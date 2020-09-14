# ---------------------------------------------------------
#   COVID ABM v3 incorporating SafeGraph data and refined testing and isolation
#   Chris Hoover (choover@berkeley.edu)
#   July 2020
# ---------------------------------------------------------

library(tidyverse)
library(data.table)
library(wrswoR)
library(dqrng)
library(fitdistrplus)
library(matrixStats)
library(lubridate)

rm(list=ls());gc()

# Adaptively set root directory to LEMMA
setwd(gsub("LEMMA.*", "LEMMA", normalizePath(getwd())))

devtools::load_all("CMH/ABM")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

# Epi data  
source("CMH/Data/Get_COVID_Cal_latest.R")

# synthetic agents from FRED/RTI
agents <- readRDS("CMH/ABM/data/sf_synthetic_agents_cbg_dt.rds")

# Make all possible locations integers
  agents$residence <- as.integer(agents$residence)
  agents$nbhd <- as.integer(agents$nbhd)
  agents$school <- as.integer(agents$school)
  agents$work <- as.integer(agents$work)
  agents$office_id <- as.integer(as.factor(agents$office_id))
  agents$class_id <- max(agents$office_id, na.rm = T) + as.integer(as.factor(agents$class_id))
  
# 1000 or so agents with both workplaces and schools (i.e. both go to school and go to work), just send them to school for simplicity  
  agents[work>0 & school>0, work:=-1]
# Replace NAs with -1  
  agents$work[is.na(agents$work)] <- -1
# Replace NAs with 0  
  agents[is.na(income), c("income", "income_bracket", "comm_bracket"):=0]
  
# Add compliance and sociality metrics
  agents[, mask := rbeta(nrow(agents), 5, 2)] # Probability of wearing a mask
  agents[, q_prob := rbeta(nrow(agents), 5, 2)] # Probability of quarantine/isolation given positive test
  agents[, sociality := rbeta(nrow(agents), 2, 2)] # Sociality metric

  setkey(agents, residence)

#Plot tests through time
#ggplot(data = sf_test) + geom_line(aes(x = Date, y = (tests/9e5)*1e5)) + theme_bw() +scale_x_date(date_breaks = "14 day") +theme(axis.text.x = element_text(angle = 45,hjust = 1))+labs(x="",y="SF Tests per 100k")

# UNCOMMENT BELOW/change size of sample TO Subset for development for faster runs/lower memory
#agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  
  
N <- nrow(agents)  

#San Francisco cbg mvmt list derived from sfgrph data
  sf_cbg_cdf <- readRDS("CMH/ABM/data/safegraph_cbg_mvmt_cdf_processed_2020-09-11.rds")
  sf_cbg_indices <- readRDS("CMH/ABM/data/safegraph_cbg_mvmt_ind_processed_2020-09-11.rds")
  sf_cbg_ids <- read_csv("CMH/Safegraph/Census_2010_CBGs_SF.csv") %>% pull(GEOID10)

#San francisco stay at home by percent by cbg derived from safegraph
  sf_sfgrph_pct_home <- readRDS("CMH/ABM/data/sfgrph_devices_pct_home_cbgs_2020-01-01_2020-08-22.rds")
  last_sfgrph <- max(sf_sfgrph_pct_home$Date)
  
#Need the simulation to run beyond when we have safegraph movement data to inform movement, so repeat older safegraph data to project in the future 
  wday(last_sfgrph)
  startover <- min(sf_sfgrph_pct_home %>% filter(wday(Date) == 1 & month(Date) == 5) %>% pull(Date))
  
  sf_sfgrph_pct_home_expand <- rbind(sf_sfgrph_pct_home,
                                     sf_sfgrph_pct_home %>% filter(Date >= startover))
  
# ---------------------------------------------------------
# Pre-process data and sim setup
# ---------------------------------------------------------
# Time frame and step
t0 <- as.Date("2020-02-01")
today <- Sys.Date()

# Key change dates
scl.close <- as.Date("2020-03-05")     #Schools closed
mask.start <- as.Date("2020-04-17")              #Mask mandate initiated
t.end <- as.Date("2020-11-01")   # Simulation end date, corresponds with 

t.tot <- as.numeric(t.end - t0)
dt = 4/24

day_of_week <- lubridate::wday(seq.Date(t0, t.end, by = "day"))
day_of_week_expand <- rep(day_of_week, each = 1/dt)
day_of_week_expand[day_of_week_expand == 1] <- "U"
day_of_week_expand[day_of_week_expand == 2] <- "M"
day_of_week_expand[day_of_week_expand == 3] <- "T"
day_of_week_expand[day_of_week_expand == 4] <- "W"
day_of_week_expand[day_of_week_expand == 5] <- "R"
day_of_week_expand[day_of_week_expand == 6] <- "F"
day_of_week_expand[day_of_week_expand == 7] <- "S"

# Break up week into 6 parts, Morning, Dayx2, Evening, Nightx2: SHOULD UPDATE IF timestep!=4/24
time_of_day <- rep(c("M", "D", "D", "E", "N", "N"), times = t.tot)  

# Binary time series for school closure active
t.scl <- c(rep(0, as.numeric(scl.close-t0)/dt),
           rep(1, as.numeric(t.end-scl.close)/dt))

# Expand safegraph-based shelter in place
sf_sfgrph_sip <- splitstackshape::expandRows(sf_sfgrph_pct_home, count = 1/dt, count.is.col = FALSE)

bta = 0.02 # transmission probability per contact from https://www.medrxiv.org/content/10.1101/2020.05.10.20097469v1.full.pdf+html ; https://doi.org/10.1073/pnas.2008373117

# Tests conducted
tests_pars <- fitdist(tail(sf_test$tests, 30), "nbinom", "mme")$estimate
  test_fx <- approxfun(0:t.tot,
                       round(c(rep(0, as.numeric(min(sf_test$Date)-t0)+1),
                         sf_test$tests,
                         rnbinom(as.numeric(t.end-max(sf_test$Date)), 
                                 mu = tests_pars[2], size = tests_pars[1]))*dt),
                       method = "linear")
  
# Slight edits to essential workers
  agents[work >0 & substr(work,1,1)!="5", essential:=1] # Essential if working in nursing facility or prison
  agents[work>0 & school>0, essential:=0] # school-aged children workers not essential
# aiming for 10-20% essential workers  
  nrow(agents[work >0 & essential == 1])/nrow(agents[work>0])
  
# ---------------------------------------------------------
# Objects to fill in sim and initialization
# ---------------------------------------------------------
  
# Initial conditions
  e.seed <- 5     #Exposed
  ip.seed <- 0    #infected pre-symptomatic
  ia.seed <- 0    #infected asymptomatic
  im.seed <- 0    #infected mildly symptomatic
  imh.seed <- 0   #infected mildly symptomatic, will become severe
  ih.seed <- 0    #infected severely symptomatic
  d.seed <- 0     #dead
  r.seed <- 0     #removed
  non.s <- e.seed + ip.seed + ia.seed + im.seed + imh.seed + ih.seed + d.seed + r.seed
  s.seed <- N - non.s
  
# Initial infection allocated among workers
  init.Es <- sample(agents[work >0, id], e.seed)   
  agents[id %in% init.Es, state:="E"]
  
# Keep track of everyone's infection status through time   
  epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
    epi_curve[1,] <- sum.inf(agents[,state])
  
# Get test data through time
  test_reports <- list()

# Keep detailed infection status through time
  infection_reports <- list()
   
# Update characteristics of initial infections  
  # Transition time
  agents[state %!in% c("S", "D", "R"), tnext:=LEMMAABM::t_til_nxt(state)]
  # State entering once transition time expires
  agents[state %!in% c("S", "D", "R"), nextstate:=LEMMAABM::next_state(state, age)]
  #Time initial infections occurred
  agents[state %!in% c("S", "D", "R"), t_infection:=dt]
  agents[, t_since_test:=0]

# Fill testing probabilities, Only necessary if testing is available in first time step of the model
  # agents[tested != 1, test_prob:=mapply(LEMMAABM::test_prob, state, income, age, race, residence_type, res_inf, t_since_test)]
  
# ---------------------------------------------------------
# Run the abm
# ---------------------------------------------------------

run.start <- Sys.time()
for(t in 2:(t.tot/dt)){
 print(t)
# Time step characteristics
  date_now <- t0+t*dt
  day_week <- day_of_week_expand[t]
  time_day <- time_of_day[t]
  sip.active <- t.sip[t]
  scl.closed <- t.scl[t]
  
# Advance transition times, time since infection, time since tested last
  agents[state %!in% c("S", "D", "R"), tnext:=tnext-dt]
  agents[state %!in% c("S", "D", "R"), t_infection:=t_infection+dt]
  agents[, t_since_test:=t_since_test+dt]
  
# Advance expired states to next state, determine new nextstate and time til next state
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), nextstate:=LEMMAABM::next_state(state, age)]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), tnext:=LEMMAABM::t_til_nxt(state)]

  print("Infections advanced")
# Implement testing (only during day time)---------------
  if(time_day != "N"){
  # Number of tests available corrected for division of day into six parts corrected for testing only occurring in day time 
    n_tests <- round(test_fx(t*dt)*3/2) 
  # New testing probabilities
    if(n_tests > 0){
      agents[,res_inf:=sum(state %in% c("Im", "Imh")), by = residence]
      agents[tested == 0 & state!= "D", test_prob:=LEMMAABM::test_prob(state, age, residence_type, res_inf)]
  # Tested agents
      testeds <- agents[,id][wrswoR::sample_int_crank(nrow(agents[tested == 0 & state != "D"]),
                                                      n_tests,
                                                      agents[tested == 0 & state != "D", test_prob])]
  # Test results and reset time since last test for those who were tested
    agents[id %in% testeds, tested:=test_sens(state, t_infection)]
    agents[id %in% testeds, t_since_test:=0]
    tested_agents <- agents[id %in% testeds,]
    test_reports[[(t-1)]] <- tested_agents[,time:=t]
    
    print("Testing conducted")
    
    }
  }
  
# Simulate infection --------------------
  if(nrow(agents[state %!in% c("S", "E", "D", "R")])>0){
# Determine locations
# Find locations of those not deceased or in the hospital  
  # Agents that are in school
    agents[school > 0 & work < 0 & state %!in% c("Ih", "D"), 
           location:=LEMMAABM::sac_location( 
             state, tested, scl.closed, sip.active,
             time_day, day_week, comm_bracket, age, sociality,
             residence, school, cbg)]
  
  # Agents that are workers only
  agents[school < 0 & work > 0 & state %!in% c("Ih", "D"), 
         location:=LEMMAABM::worker_location(state, tested,
                                             sip.active, time_day, day_week,
                                             age, res_kids, essential, sociality,
                                             comm_bracket, income_bracket, 
                                             residence, work, cbg)]
  
  # Agents that are neither in school or are working
  agents[work < 0 & school < 0 & state %!in% c("Ih", "D"),
         location:=LEMMAABM::other_location(state, tested,
                                            sip.active, time_day, 
                                            age, sociality, residence_type, comm_bracket, 
                                            residence, cbg)]
  
  agents[state %!in% c("Ih", "D") & location == cbg, 
         location:=comm_loc(location, nbhd_mat_list)]
  
  # Smaller sub-locations (offices and classrooms) for agents in workplaces or schools
  agents[state %!in% c("Ih", "D") & !is.na(location),
         small_location:=location_small(location, work, school, office_id, class_id)]
  
  print("Locations resolved")
# Determine number of individuals and transmitting individuals by location  
  agents[, n_transmitting:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = location]
  agents[n_transmitting > 0, n_present:=.N, by = location]
  
  agents[n_transmitting > 0, n_transmitting_small:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = small_location]
  agents[n_transmitting_small > 0, n_present_small:=.N, by = small_location]
  
# Determine FOI for susceptible individuals in location where someone is transmitting
  agents[n_transmitting > 0 & state == "S", FOI:=LEMMAABM::get_foi(n_transmitting, n_present, bta)]
  
# For agents sharing small location, overwrite lower FOI with small location FOI 
  agents[n_transmitting_small > 0 & state == "S", FOI:=LEMMAABM::get_foi(n_transmitting_small, n_present_small, bta)]
 
# Generate infections, update their state, sample for their nextstate and time until reaching it
    agents[FOI > 0 & state == "S", infect:=LEMMAABM::foi_infect(FOI)]
    agents[infect == 1, state:="E"]
    agents[infect == 1, nextstate:=LEMMAABM::next_state(state, age)]
    agents[infect == 1, tnext:=LEMMAABM::t_til_nxt(state)]
    
# Reset infection & location columns
    agents[, c("location", "small_location",
               "n_transmitting", "n_transmitting_small", 
               "n_present", "n_present_small",
               "FOI", "infect"):=NA_real_]
    
# Store detailed infection info
    infection_reports[[t-1]] <- agents[state %!in% c("S", "D", "R")]
    print("New infections generated")
  }
  
  epi_curve[t,] <- sum.inf(agents[,state])
  # On to the next one  
    
}  
run.end <- Sys.time()

run.end-run.start

saveRDS(rbindlist(test_reports,fill = TRUE), paste0("CMH/ABM/Analysis/Outputs/simulated_testing", Sys.Date(),".rds"))
saveRDS(rbindlist(infection_reports, fill = TRUE), paste0("CMH/ABM/Analysis/Outputs/simulated_infections", Sys.Date(),".rds"))
saveRDS(inf.dt, paste0("CMH/ABM/Analysis/Outputs/population_infection", Sys.Date(), ".rds"))
