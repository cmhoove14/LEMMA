# Run UCSC STAN Model
# -----------------------------------------------------

require(tidyverse)
require(rstan)

# Rstan prep
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7 -mtune=corei7')

# -----------------------------------------------------

# Data get

# -----------------------------------------------------

# Case and hosptialization data for SF
sf_hosp <- read.csv(url("https://data.sfgov.org/resource/nxjg-bhem.csv")) %>% 
  mutate(Date = as.Date(reportdate),
         type = ifelse(dphcategory == "ICU", "ICU", "HOSP"),
         conf = ifelse(covidstatus == "PUI", "PUI", "CONF"),
         hosp_stat = paste(type, conf, sep = "_")) %>% 
  pivot_wider(names_from = hosp_stat,
              values_from = patientcount) %>%
  group_by(Date) %>% 
  summarise(ICU_PUI = sum(ICU_PUI, na.rm = T),
            ICU_CONF = sum(ICU_CONF, na.rm = T),
            HOSP_PUI = sum(HOSP_PUI, na.rm = T),
            HOSP_CONF = sum(HOSP_CONF, na.rm = T)) %>% 
  arrange(Date) %>% 
  mutate(cumICUconf = cumsum(ICU_CONF),
         cumICUpui = cumsum(ICU_PUI),
         cumHOSPconf = cumsum(HOSP_CONF),
         cumHOSPpui = cumsum(HOSP_PUI))

sf_case <- read.csv(url("https://data.sfgov.org/resource/tvq9-ec9w.csv")) %>% 
  mutate(Date = as.Date(date)) %>% 
  pivot_wider(names_from = case_disposition,
              values_from = case_count) %>% 
  group_by(Date) %>% 
  summarise(Cases = sum(Confirmed, na.rm = T),
            Deaths = sum(Death, na.rm = T)) %>% 
  arrange(Date) %>% 
  mutate(cum_case = cumsum(Cases),
         cum_death = cumsum(Deaths))

# Save for plotting
sf_long <- sf_case %>% 
  left_join(sf_hosp, by = "Date") %>% 
  pivot_longer(-Date, names_to = "metric", values_to = "count")

saveRDS(sf_long,
       file = paste("CMH/UCSC_mod/sf_data_", Sys.Date(), ".RDS", sep = ""))
# -----------------------------------------------------

# Data prep

# -----------------------------------------------------
#List to fill with inputs for stan
seir_inputs <- list()

# the start date at which to start the simulation
  startdate_str = as.Date('2020-02-16')

# Timeframe for simulation. Can replace Sys.Date() with any date at which to cut off model fitting
  itoday_1based = Sys.Date()-startdate_str
# specify the date up to when beta is estimated
  seir_inputs[['itoday']] = as.numeric(itoday_1based)

# the number of days to run the model for
  seir_inputs[['nt']] = 200
  
# Put data into list
seir_inputs[['nobs']] <- nrow(sf_case)
seir_inputs[['tobs']] <- as.numeric(sf_case$Date - startdate_str)
seir_inputs[['obs_I']] <- sf_case$cum_case
seir_inputs[['obs_Rmort']] <- sf_case$cum_death

seir_inputs[['nhobs']] <- nrow(sf_hosp)
seir_inputs[['thobs']] <- as.numeric(sf_hosp$Date - startdate_str)
seir_inputs[['obs_Hmod']] <- sf_hosp$HOSP_CONF
seir_inputs[['obs_Hicu']] <- sf_hosp$ICU_CONF
  
# the number of age groups
seir_inputs[['nage']] = 1 

# Note: The inputs below must be lists or arrays, even if just one age group is specified 
# (the enclosing "[]" are not optional).

# the population for each age group
seir_inputs[['npop']] = array(883305)
# the fraction of hospitalized cases (ICU + non-ICU)
seir_inputs[['frac_hosp']] = array(0.07)
# the fraction of ICU cases
seir_inputs[['frac_icu']] = array(0.02)
# the death rate of infected
seir_inputs[['frac_mort']] = array(0.01)
# the fraction of asymptomatic cases
seir_inputs[['frac_asym']] = array(0.178)

# a parameter modifying the prior uncertainty in the fractions above (set to 1000.0 for `nage` > 1)
seir_inputs[['alpha_multiplier']] = 100.0

# mean duration in "exposed" stage
seir_inputs[['mu_duration_lat']] = 5.0
# standard deviation (sd) of duration in "exposed" stage
seir_inputs[['sigma_duration_lat']] = 2.0
# mean duration in "infectious" stage for asymptomatic cases
seir_inputs[['mu_duration_rec_asym']] = 7.0
# sd of duration in "infectious" stage for asymptomatic cases
seir_inputs[['sigma_duration_rec_asym']] = 5.0
# mean duration in "infectious" stage for mild cases
seir_inputs[['mu_duration_rec_mild']] = 7.0
# sd of duration in "infectious" stage for mild cases
seir_inputs[['sigma_duration_rec_mild']] = 5.0
# mean duration in "infectious" stage for hospitalized cases
seir_inputs[['mu_duration_pre_hosp']] = 5.0
# sd of duration in "infectious" stage for hospitalized cases
seir_inputs[['sigma_duration_pre_hosp']] = 1.0
# mean duration in hospital for non-ICU cases
seir_inputs[['mu_duration_hosp_mod']] = 7.0
# sd of duration in hospital for non-ICU cases
seir_inputs[['sigma_duration_hosp_mod']] = 1.0
# mean duration in hospital for ICU cases
seir_inputs[['mu_duration_hosp_icu']] = 16.0
# sd of duration in hospital for ICU cases
seir_inputs[['sigma_duration_hosp_icu']] = 4.0

# mean fraction of tested infectious
seir_inputs[['mu_frac_tested']] = 0.25
# sd fraction of tested infectious
seir_inputs[['sigma_frac_tested']] = 0.1

# lambda parameter for initial conditions of "exposed"
seir_inputs[['lambda_ini_exposed']] = 0.3

# mean initial beta estimate
seir_inputs[['mu_beta1']] = 0.2
# sd initial beta estimate
seir_inputs[['sigma_beta1']] = 0.02

# distance in time between the knots used to construct the splines 
seir_inputs[['dknot']] = 10

# spline mode (must be 1 or 2)
# splinemode = 1: estimate beta up to today
# splinemode = 2: estimate beta up to dknot days before today, then assume constant value up to today
seir_inputs[['splinemode']] = 1

# number of interventions
seir_inputs[['ninter']] = 1

# Note: The inputs below must be lists or arrays, even if one intervention is specified 
# (the enclosing "[]" are not optional). For zero interventions 

# start time of each interventions
seir_inputs[['t_inter']] = array(as.numeric(itoday_1based+5))
# length of each intervention
seir_inputs[['len_inter']] = array(10)
# mean change in beta through intervention 
seir_inputs[['mu_beta_inter']] = array(1.0)
# sd change in beta through intervention
seir_inputs[['sigma_beta_inter']] = array(0.5)

# -----------------------------------------------------

# Stan fit

# -----------------------------------------------------

stan_seir_fit <- stan(
  file = "CMH/UCSC_mod/UCSC_SEIR.stan",
  data = seir_inputs,
  thin = 5,
  chains = 3,
  warmup = 1000,
  iter = 5000,
  cores = 3,
  refresh = 100
)

stan_obj <- extract(stan_seir_fit, permuted = T)

saveRDS(stan_obj,
        file = paste("CMH/UCSC_mod/stan_fit_", Sys.Date(), ".RDS", sep = ""))
