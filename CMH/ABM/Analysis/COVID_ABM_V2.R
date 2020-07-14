# ---------------------------------------------------------
#   COVID ABM v2
#   Chris Hoover (choover@berkeley.edu)
#   July 2020
# ---------------------------------------------------------

require(tidyverse)
require(data.table)
require(wrswoR)
require(fitdistrplus)
require(matrixStats)
require(lubridate)

rm(list=ls());gc()

# Adaptively set root directory to LEMMA
setwd(gsub("LEMMA.*", "LEMMA", normalizePath(getwd())))

devtools::load_all("CMH/ABM/")

# ---------------------------------------------------------
# Load data
# ---------------------------------------------------------

# Epi data  
source("CMH/Data/Get_COVID_Cal_latest.R")

# synthetic agents from FRED/RTI
agents <- readRDS("CMH/ABM/data/sf_synthetic_agents_dt.rds")

# remove children who are mostly irrelevant to dynamics (very young and will almost exclusively be with parents)

# Subset for development for faster runs/lower memory
#agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  
  
N <- nrow(agents)  

# PCR sensitivity data
pcr_sens <- readRDS("CMH/ABM/data/PCR_Sens_Kucirka.rds")
  pcr_sens_day <- colMedians(pcr_sens)

  # Function to return estimate of probability + given +  
  pcr_sens_p1 <- approxfun(c(0:3), c(0, pcr_sens_day[1:3]))
  pcr_sens_p2<- splinefun(c(0:21), c(0, pcr_sens_day))
  pcr_sens_fun <- approxfun(seq(0,21,by=0.1), 
                            c(sapply(seq(0,3.0,by=0.1), pcr_sens_p1),
                              sapply(seq(3.1,21,by=0.1), pcr_sens_p2)))
  
  #plot(c(0:21),c(0,pcr_sens_day)) ; lines(seq(0,21,by=0.1), sapply(seq(0,21,by=0.1),pcr_sens_fun))
  
# Contact matrices from https://doi.org/10.1371/journal.pcbi.1005697
home_contacts <- readxl::read_xlsx("CMH/ABM/data/contact_matrices_152_countries/MUestimates_home_1.xlsx")
  home_c_rate <- colSums(home_contacts)

work_contacts <- readxl::read_xlsx("CMH/ABM/data/contact_matrices_152_countries/MUestimates_work_1.xlsx")
  work_c_rate <- colSums(work_contacts)

school_contacts <- readxl::read_xlsx("CMH/ABM/data/contact_matrices_152_countries/MUestimates_school_1.xlsx")
  scl_c_rate <- colSums(school_contacts)
  
other_contacts <- readxl::read_xlsx("CMH/ABM/data/contact_matrices_152_countries/MUestimates_other_locations_1.xlsx")
  other_c_rate <- colSums(other_contacts)
  
all_contacts <- readxl::read_xlsx("CMH/ABM/data/contact_matrices_152_countries/MUestimates_all_locations_1.xlsx")
  all_c_rate <- colSums(all_contacts)

# ---------------------------------------------------------
# Pre-process data and sim setup
# ---------------------------------------------------------
# Time frame and step
t0 <- as.Date("2020-02-01")
last_dat <- max(sf_all %>% na.omit() %>% pull(Date))
today <- Sys.Date()

# Beta change dates
scl.close <- as.Date("2020-03-05")     #Schools closed
sip.start <- as.Date("2020-03-15")     #SiP announced
t.end <- as.Date("2020-08-01")   # Where we're headed (for future use)

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

time_of_day <- rep(c("M", "D", "D", "E", "N", "N"), times = t.tot)  

t.sip <- c(rep(0, as.numeric(sip.start-t0)/dt),
           rep(1, as.numeric(t.end-sip.start)/dt))

t.scl <- c(rep(0, as.numeric(scl.close-t0)/dt),
           rep(1, as.numeric(t.end-scl.close)/dt))

bta = 0.2 # transmission probability per contact from https://doi.org/10.1073/pnas.2008373117

# Tests conducted
tests_pars <- fitdist(tail(sf_test$tests, 30), "nbinom", "mme")$estimate
  test_fx <- approxfun(0:t.tot,
                       round(c(rep(0, as.numeric(min(sf_test$Date)-t0)+1),
                         sf_test$tests,
                         rnbinom(as.numeric(t.end-max(sf_test$Date)), 
                                 mu = tests_pars[2], size = tests_pars[1]))*dt),
                       method = "linear")
  
# Assign essential workers
  agents[work!="X", essential:=sapply(income_bracket, function(i){rbinom(1,1,prob=1/(i*3))})]
# aiming for 10-20% essential workers  
  nrow(agents[work!="X" & essential == 1])/nrow(agents[work!="X"])
  
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
  init.Es <- sample(agents[work != "X", id], e.seed)   
  agents[id %in% init.Es, state:="E"]
  
# Keep track of everyone's infection status through time   
  inf.dt <- agents[ , .(id)]
  inf.dt[, state1:=agents[,state]]
  
# Get test data through time
  test_reports <- list()

# Keep detailed infection status through time
  infection_reports <- list()
   
   
# Update characteristics of initial infections  
  agents[state %!in% c("S", "D", "R"), tnext:=lapply(state, LEMMAABM::t_til_nxt)]
  agents[state %!in% c("S", "D", "R"), nextstate:=mapply(LEMMAABM::next_state, state, age)]
  agents[state %!in% c("S", "D", "R"), t_infection:=dt]
  agents[, t_since_test:=0]

# Fill testing probabilities 
  # Only necessary if testing is available in first time step of the model
  # agents[tested != 1, test_prob:=mapply(LEMMAABM::test_prob, state, income, age, race, residence_type, res_inf, t_since_test)]
  
# ---------------------------------------------------------
# Run the abm
# ---------------------------------------------------------

run.start <- Sys.time()
for(t in 2:(t.tot/dt)){
 print(t)
# Time step characteristics
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
  agents[tnext < 0 & state %!in% c("S", "D", "R"), nextstate:=mapply(LEMMAABM::next_state, state, age)]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), tnext:=lapply(state, LEMMAABM::t_til_nxt)]

  print("Infections advanced")
# Implement testing (only during day time)---------------
  if(time_day != "N"){
  # Number of tests available corrected for division of day into six parts corrected for testing only occurring in day time 
    n_tests <- round(test_fx(t*dt)*3/2) 
  # New testing probabilities
    if(n_tests > 0){
      agents[,res_inf:=sum(state %in% c("Im", "Imh")), by = residence]
      agents[tested == 0 & state!= "D", test_prob:=mapply(LEMMAABM::test_prob, 
                                                          state, income_bracket, age, residence_type, res_inf, t_since_test)]
  # Tested agents
      testeds <- agents[,id][wrswoR::sample_int_crank(nrow(agents[tested == 0 & state != "D"]),
                                                      n_tests,
                                                      agents[tested == 0 & state != "D", test_prob])]
  # Test results and reset time since last test for those who were tested
    agents[id %in% testeds, tested:=mapply(test_sens, state, t_infection)]
    agents[id %in% testeds, t_since_test:=0]
    tested_agents <- agents[id %in% testeds,]
    test_reports[[(t-1)]] <- tested_agents[,time:=t]
    
    print("Testing conducted")
    
    }
  }
  
    
# Simulate infection --------------------
  if(nrow(agents[state %!in% c("S", "E", "D", "R")])>0){
# Determine locations
  agents[school != "X" & work != "X" & state %!in% c("Ih", "D"),
         location:=mapply(LEMMAABM::sac_worker_location,
                          state, tested,
                          scl.closed, sip.active, time_day, day_week,
                          comm_bracket, essential, 
                          residence, school, work, comm)]
  
  agents[school != "X" & work == "X" & state %!in% c("Ih", "D"), 
         location:=mapply(LEMMAABM::sac_location, 
                          state, tested, scl.closed, sip.active,
                          time_day, day_week, comm_bracket, age,
                          residence, school, comm)]
  
  agents[school == "X" & work != "X" & state %!in% c("Ih", "D"), 
         location:=mapply(LEMMAABM::worker_location,
                          state, tested,
                          sip.active, time_day, day_week,
                          age, res_kids, essential, 
                          comm_bracket, income_bracket, 
                          residence, work, comm)]
  
  agents[work == "X" & school == "X" & state %!in% c("Ih", "D"),
         location:=mapply(LEMMAABM::other_location, 
                          state, tested,
                          sip.active, time_day, 
                          age, residence_type, comm_bracket, 
                          residence, comm)]
  
  print("Locations resolved")
# Determine number of transmitting individuals by location  
  agents[, n_transmitting:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = location]
# Determine FOI for susceptible individuals in location where someone is transmitting
  agents[n_transmitting > 0 & state == "S", FOI:=mapply(LEMMAABM::get_foi, location, n_transmitting, 
                                                        res_size, work_size, school_size, comm_size, 
                                                        MoreArgs = list(trans_rate = bta))]
# Generate infections, update their state, sample for their nextstate and time until reaching it
    agents[n_transmitting > 0 & state == "S", infect:=sapply(FOI, LEMMAABM::foi_infect)]
    agents[infect == 1, state:="E"]
    agents[infect == 1, nextstate:=mapply(LEMMAABM::next_state, state, age)]
    agents[infect == 1, tnext:=lapply(state, LEMMAABM::t_til_nxt)]
    
# Reset infection columns
    agents[, c("n_transmitting", "FOI", "infect"):=NA_real_]
# Store detailed infection info
    infection_reports[[t-1]] <- agents[state %!in% c("S", "D", "R")]
    print("New infections generated")
  }
  
  state_time <- paste0("state",t)
    inf.dt[, (state_time):=agents[,state]]
  # On to the next one  
    
}  
run.end <- Sys.time()

run.end-run.start

saveRDS(rbindlist(test_reports), paste0("CMH/ABM/Analysis/Outputs/simulated_testing", Sys.Date(),".rds"))
saveRDS(rbindlist(infection_reports), paste0("CMH/ABM/Analysis/Outputs/simulated infections", Sys.Date(),".rds"))
saveRDS(inf.dt, paste0("CMH/ABM/Analysis/Outputs/population_infection", Sys.Date(), ".rds"))
