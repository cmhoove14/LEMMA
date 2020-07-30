library(tidyverse)
library(data.table)
library(wrswoR)
library(dqrng)
library(fitdistrplus)
library(matrixStats)
library(lubridate)
library(parallel)

rm(list=ls());gc()

# Adaptively set root directory to LEMMA
setwd(gsub("LEMMA.*", "LEMMA", normalizePath(getwd())))

devtools::load_all("CMH/ABM/")

# ---------------------------------------------------------
# Load data & Setup
# ---------------------------------------------------------

# Epi data  
source("CMH/Data/Get_COVID_Cal_latest.R")

# synthetic agents from FRED/RTI
agents <- readRDS("CMH/ABM/data/sf_synthetic_agents_dt.rds")

# agents <- agents[agents$residence %in% sample(agents$residence, 1e4, replace = F)]

N <- nrow(agents)  
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
  
  setkey(agents, residence)

#San Francisco neighborhoods matrix list
 nbhd_mat_list <- readRDS("CMH/ABM/data/sf_nbhd_mat_list.rds")

# PCR sensitivity data
pcr_sens <- readRDS("CMH/ABM/data/PCR_Sens_Kucirka.rds")
  pcr_sens_day <- colMedians(pcr_sens)

  # Function to return estimate of probability + given +  
  pcr_sens_p1 <- approxfun(c(0:3), c(0, pcr_sens_day[1:3]))
  pcr_sens_p2<- splinefun(c(0:21), c(0, pcr_sens_day))
  pcr_sens_fun <- approxfun(seq(0,21,by=0.1), 
                            c(sapply(seq(0,3.0,by=0.1), pcr_sens_p1),
                              sapply(seq(3.1,21,by=0.1), pcr_sens_p2)))
  
# Time frame and step
t0 <- as.Date("2020-02-01")
last_dat <- max(sf_all %>% na.omit() %>% pull(Date))
today <- Sys.Date()

# Key change dates
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

# Break up week into 6 parts, Morning, Dayx2, Evening, Nightx2: SHOULD UPDATE IF timestep!=4/24
time_of_day <- rep(c("M", "D", "D", "E", "N", "N"), times = t.tot)  

# Binary time series for sip and school closure active
t.sip <- c(rep(0, as.numeric(sip.start-t0)/dt),
           rep(1, as.numeric(t.end-sip.start)/dt))

t.scl <- c(rep(0, as.numeric(scl.close-t0)/dt),
           rep(1, as.numeric(t.end-scl.close)/dt))

#bta = 0.02 # transmission probability per contact from https://www.medrxiv.org/content/10.1101/2020.05.10.20097469v1.full.pdf+html ; https://doi.org/10.1073/pnas.2008373117

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
# Parallel setup and export to cluster
# ---------------------------------------------------------
n_sims_per_par <- 1
bta_E0_grid <- expand.grid(bta = c(0.01,0.025,0.05,0.1),
                           E0 = c(1,3,5))  

bta_E0_sweeps <- bta_E0_grid %>% slice(rep(1:n(), each = n_sims_per_par))

#Setup for running jobs across parallel nodes in cluster
start.time <- Sys.time()

#sim_results <- LEMMAABM::covid_abm(bta = bta_E0_grid[1,1], E0 = bta_E0_grid[2,2],Ip0 = 0, Ia0 = 0, Im0 = 0, Imh0 = 0, Ih0 = 0, R0 = 0, D0 = 0,agents, nbhd_mat_list,t.tot, dt, day_of_week_fx = day_of_week_expand,time_of_day_fx = time_of_day, SiP_fx = t.sip, scl_fx = t.scl, test_fx)

sim_results <- apply(bta_E0_grid, 1, 
                     FUN = function(x) LEMMAABM::covid_abm(bta = x[1], E0 = x[2], 
                                                           Ip0 = 0, Ia0 = 0, Im0 = 0, Imh0 = 0, Ih0 = 0, R0 = 0, D0 = 0,
                                                           agents, nbhd_mat_list,
                                                           t.tot, dt, day_of_week_fx = day_of_week_expand, 
                                                           time_of_day_fx = time_of_day, SiP_fx = t.sip, scl_fx = t.scl,
                                                           test_fx))


end.time <- Sys.time()

end.time-start.time
saveRDS(sim_results, paste0("CMH/ABM/Analysis/Outputs/bta_e0_calibration",Sys.time(),".rds"))
