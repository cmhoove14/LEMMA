library(tidyverse)
library(data.table)
library(wrswoR)
library(dqrng)
library(fitdistrplus)
library(matrixStats)
library(lubridate)
library(fastmatch)
#library(LEMMAABM)

rm(list=ls());gc()

# Adaptively set root directory to LEMMA
setwd(gsub("LEMMA.*", "LEMMA", normalizePath(getwd())))

# Epi data  
source("CMH/Data/Get_COVID_Cal_latest.R")

# ABM functions
source("CMH/ABM/R/ABM_V3.R")

# synthetic agents from FRED/RTI
agents <- readRDS("CMH/ABM/data/sf_synthetic_agents_cbg_dt.rds")

# Remove agents with unassignable cbg (most of these are on treasure island)
  agents <- agents[!is.na(cbg),]

# Edit agents dt to get rid of superfluous columns
  agents <- agents[, c("t_since_test", "work_size", "office_size", "school_size", "class_size", "comm_size", 
                       "income_bracket", "comm_income", "comm_bracket", "nbhd"):=NULL]
  
# Edit agents to add necessary column(s)
  agents[, t_symptoms := 0]
  
# Make all possible locations integers
  agents$residence <- as.integer(agents$residence)
  agents$school <- as.integer(agents$school)
  agents$work <- as.integer(agents$work)
  agents$office_id <- as.integer(as.factor(agents$office_id))
  agents$class_id <- max(agents$office_id, na.rm = T) + as.integer(as.factor(agents$class_id))
  
# 1000 or so agents with both workplaces and schools (i.e. both go to school and go to work), just send them to school for simplicity  
  agents[work>0 & school>0, work:=-1]
# Replace NAs with -1  
  agents$work[is.na(agents$work)] <- -1

# Slight edits to essential workers
  agents[work >0 & substr(work,1,1)!="5", essential:=1] # Essential if working in nursing facility or prison
  agents[work>0 & school>0, essential:=0] # school-aged children workers not essential
# aiming for 10-20% essential workers  
  nrow(agents[work >0 & essential == 1])/nrow(agents[work>0])
    
  setkey(agents, residence)

#Plot tests through time
#ggplot(data = sf_test) + geom_line(aes(x = Date, y = (tests/9e5)*1e5)) + theme_bw() +scale_x_date(date_breaks = "14 day") +theme(axis.text.x = element_text(angle = 45,hjust = 1))+labs(x="",y="SF Tests per 100k")

# UNCOMMENT BELOW/change size of sample TO Subset for development for faster runs/lower memory
#agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  
  
N <- nrow(agents)  

#San Francisco cbg mvmt list derived from sfgrph data
  sf_cbg_cdf <- readRDS("CMH/ABM/data/safegraph_cbg_mvmt_cdf_processed_2020-09-11.rds")
  #sf_cbg_indices <- readRDS("CMH/ABM/data/safegraph_cbg_mvmt_ind_processed_2020-09-11.rds")
  sf_cbg_ids <- read_csv("CMH/Safegraph/Census_2010_CBGs_SF.csv") %>% pull(GEOID10)

#San francisco stay at home by percent by cbg derived from safegraph
  sf_sfgrph_pct_home <- readRDS("CMH/ABM/data/sfgrph_devices_pct_home_cbgs_2020-01-01_2020-08-22.rds")
  last_sfgrph <- max(sf_sfgrph_pct_home$Date)
  
#Need the simulation to run beyond when we have safegraph movement data to inform movement, so repeat older safegraph data to project in the future 
  wday(last_sfgrph)
  startover <- min(sf_sfgrph_pct_home %>% filter(wday(Date) == 1 & month(Date) == 5) %>% pull(Date))
  
  sf_sfgrph_pct_home_expand <- as.data.table(rbind(sf_sfgrph_pct_home,
                                                   sf_sfgrph_pct_home %>% filter(Date >= startover) %>% mutate(Date = Date + 1 + (last_sfgrph-startover))))
  
  data.table::setkey(sf_sfgrph_pct_home_expand, origin_census_block_group)
  
  sf_cbg_cdf_expand <- abind::abind(sf_cbg_cdf[,,1:dim(sf_cbg_cdf)[3]], 
                                    sf_cbg_cdf[,,(dim(sf_cbg_cdf)[3]-(last_sfgrph-startover)):(dim(sf_cbg_cdf)[3])])

#Get rid of older large objects  
  rm(sf_sfgrph_pct_home)
  rm(sf_cbg_cdf)
  gc()
  
# ---------------------------------------------------------
# Pre-process data and sim setup
# ---------------------------------------------------------
# Time frame and step
t0 <- as.Date("2020-03-01")
today <- Sys.Date()

# Key change dates
scl.close <- as.Date("2020-03-05")   # Schools closed
SiP.start <- as.Date("2020-03-15")   # Shelter in Place started
mask.start <- as.Date("2020-04-17")  # Mask mandate initiated
t.end <- as.Date("2020-12-01")       # Simulation end date

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

# Transmission probabilities across different edges https://www.medrxiv.org/content/10.1101/2020.05.10.20097469v1.full.pdf
  trans.hh <- 0.1
  trans.work <- 0.01
  trans.school <- 0.02
  trans.other <- 0.002

# For troubleshooting
agents2 <- as.data.frame(agents)
  
# Call ABM_V3 function
  abm_v3_test_run <- covid_abm_v3_no_test(bta_hh = trans.hh, bta_scl = trans.school, bta_work = trans.work, bta_other = trans.other,       
                                          E0 = 10, Ip0 = 5, Ia0 = 5, Im0 = 1, Imh0 = 0, Ih0 = 0, R0 = 0, D0 = 0,                  
                                          agents_dt = agents, cbg_cdf_array = sf_cbg_cdf_expand, cbg_ids = sf_cbg_ids, stay_home_dt = sf_sfgrph_pct_home_expand,          
                                          t0 = t0, t.tot = t.tot, dt = dt, day_of_week_fx = day_of_week_expand, time_of_day_fx = time_of_day, 
                                          SiP.start = SiP.start, scl.close = scl.close, mask.start = mask.start, 
                                          mask_fx = mask_fx, quar_fx = quar_fx, social_fx = social_fx)  

# For troubleshooting
agents <- as.data.table(agents2)
  
bta_hh = trans.hh ; bta_scl = trans.school ; bta_work = trans.work ; bta_other = trans.other ; E0 = 10 ; Ip0 = 5 ; Ia0 = 5 ; Im0 = 1 ; Imh0 = 0 ; Ih0 = 0 ; R0 = 0 ; D0 = 0 ; agents_dt = agents ; cbg_cdf_array = sf_cbg_cdf_expand ; cbg_ids = sf_cbg_ids ; stay_home_dt = sf_sfgrph_pct_home_expand ; t.tot = t.tot ; dt = dt ; day_of_week_fx = day_of_week_expand ; time_of_day_fx = time_of_day ; SiP.start = SiP.start ; scl.close = scl.close ; mask.start = mask.start ; mask.red = 0.6 ; mask_fx = mask_fx ; quar_fx = quar_fx ; social_fx = social_fx
  