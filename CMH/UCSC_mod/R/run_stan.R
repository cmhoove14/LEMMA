# -----------------------------------------------------

#  Function to run stan model
  
# -----------------------------------------------------

run_stan <- function(start.date,
                     end.date,
                     case_dat,
                     hosp_dat,
                     pars,
                     stan_file){

# the start date at which to start the simulation
  startdate_str = as.Date(start.date)

# Timeframe for simulation. Can replace Sys.Date() with any date at which to cut off model fitting
  itoday_1based = Sys.Date()-startdate_str
  
# specify the date up to when beta is estimated
  pars[['itoday']] = as.numeric(itoday_1based)

# the number of days to run the model for
  pars[['nt']] = as.numeric(as.Date(end.date) - as.Date(start.date))
  
# Put data into list
  pars[['nobs']] <- nrow(case_dat)
  pars[['tobs']] <- as.numeric(case_dat$Date - startdate_str)
  pars[['obs_I']] <- case_dat$cum_case
  pars[['obs_Rmort']] <- case_dat$cum_death
  
  pars[['nhobs']] <- nrow(hosp_dat)
  pars[['thobs']] <- as.numeric(hosp_dat$Date - startdate_str)
  pars[['obs_Hmod']] <- hosp_dat$HOSP_CONF
  pars[['obs_Hpui']] <- hosp_dat$HOSP_PUI
  pars[['obs_Hicu']] <- hosp_dat$ICU_CONF
  pars[['obs_Hpui_icu']] <- hosp_dat$ICU_PUI
  
# Note: The inputs below must be lists or arrays, even if one intervention is specified 
# (the enclosing "[]" are not optional). For zero interventions 

# start time of each interventions
  pars[['t_inter']] = array(as.numeric(itoday_1based+5))
  
# NOTE: all other parameterss should already be in pars  
  
# Run the stan model  
stan_run <- stan(
  file = stan_file,
  data = pars,
  thin = 5,
  chains = 3,
  warmup = 1000,
  iter = 5000,
  cores = 3,
  refresh = 100
)

stan_obj <- extract(stan_run, permuted = T)

return(stan_obj)

}

