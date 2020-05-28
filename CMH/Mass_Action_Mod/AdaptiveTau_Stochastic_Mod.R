#Load packages and other files ##########
library(adaptivetau)

# List of transitions #######
transitions = list(
  c(S = -1, E = 1),     #Susceptible becomes exposed
  c(E = -1, Ip = 1),    #Exposed becomes Infected, pre-symptomatic
  c(E = -1, Ia = 1),    #Exposed becomes Infected, asymptomatic
  c(Ip = -1, Is = 1),   #Infected, pre-symptomatic becomes Infected, severe
  c(Ip = -1, Im = 1),   #Infected, pre-symptomatic becomes Infected, mild
  c(Is = -1, H = 1),    #Infected, severe becomes Infected, hospitalized
  c(Ia = -1, R = 1),    #Infected, asymptomatic becomes recovered
  c(Im = -1, R = 1),    #Infected, mild becomes recovered
  c(H = -1, D = 1),     #Hospitalized, severe becomes Dead
  c(H = -1, R = 1))     #Hospitalized becomes recovered

trans_simp = list(
  c(S = -1, E = 1),     #Susceptible becomes exposed
  c(E = -1, Is = 1),    #Exposed becomes Infected, symptomatic
  c(E = -1, Ia = 1),    #Exposed becomes Infected, asymptomatic
  c(Is = -1, H = 1),    #Infected symptomatic becomes hospitalized
  c(Ia = -1, R = 1),    #Infected, asymptomatic becomes recovered
  c(Is = -1, R = 1),    #Infected, symptomatic becomes recovered
  c(H = -1, D = 1),     #Hospitalized, severe becomes Dead
  c(H = -1, R = 1))     #Hospitalized becomes recovered

# Rate siulators #########
rates_base <- function(x, p, t) {
  S = x['S']
  E = x['E']
  Ip = x['Ip']
  Ia = x['Ia']
  Is = x['Is']
  Im = x['Im']
  H = x['H']
  R = x['R']
  D = x["D"]
  
  N = p['N']            #population size
  beta = p['beta']      #transmission rate
  cp = p['cp']          #Relative contact rate between S and Ip
  ca = p['ca']          #Relative contact rate between S and Ia
  cs = p['cs']          #Relative contact rate between S and Is
  cm = p['cm']          #Relative contact rate between S and Im
  alpha = p['alpha']    #rate of symptomatic case
  gam = p['gam']        #1/serial interval
  lambda_a = p['la']    #rate of recovery of asymptomatic cases
  lambda_s = p['ls']    #rate of hospitalization for severe cases
  lambda_m = p['lm']    #rate of recovery for mild cases
  lambda_p = p['lp']    #1/(latent period - serial interval) = time from pre-symptomatic to symptomatic (either mild or severe)
  rho = p['rho']        #fraction of symptomatic cases that are mild
  delta = p['delta']    #1/time spent in hospital for severe cases
  mu = p['mu']          #mortality rate of hospitalized cases
  
  return(c(S*beta*(Ip*cp+Ia*ca+Is*cs+Im*cm)/N, #Susceptible becomes exposed
           E*(1-alpha)*gam,                    #Exposed becomes Infected, pre-symptomatic
           E*alpha*gam,                        #Exposed becomes Infected, asymptomatic
           Ip*lambda_p*(1-rho),                #Infected, pre-symptomatic becomes Infected, severe
           Ip*lambda_p*rho,                    #Infected, pre-symptomatic becomes Infected, mild
           Is*lambda_s,                        #Infected, severe becomes Infected, hospitalized
           Ia*lambda_a,                        #Infected, asymptomatic becomes recovered
           Im*lambda_m,                        #Infected, mild becomes recovered
           H*delta*mu,                         #Hospitalized becomes Dead
           H*delta*(1-mu)))                    #Hospitalized becomes recovered
}

rates_simp <- function(x, p, t){
  S = x['S']
  E = x['E']
  Is = x['Is']
  Ia = x['Ia']
  H = x['H']
  R = x['R']
  D = x["D"]
  
  N = p['N']            #population size
  beta = p['beta']      #transmission rate
  ca = p['ca']          #Relative contact rate between S and Ia
  cs = p['cs']          #Relative contact rate between S and Is
  alpha = p['alpha']    #rate of symptomatic case
  gam = p['gam']        #1/incubation period
  lambda_a = p['la']    #rate of recovery of asymptomatic cases
  lambda_s = p['ls']    #recovery rate of symptomatic, non-hospitalized cases
  lambda_h = p['lh']    #hospitalization rate of symptomatic cases
  rho = p['rho']        #fraction of symptomatic cases that are hospitalized
  delta = p['delta']    #1/time spent in hospital for severe cases
  mu = p['mu']          #mortality rate of hospitalized cases
  
  return(c(S*beta*(Ia*ca+Is*cs)/N, #Susceptible becomes exposed
           E*(1-alpha)*gam,                    #Exposed becomes Infected, symptomatic
           E*alpha*gam,                        #Exposed becomes Infected, asymptomatic
           Is*lambda_h*rho,                    #Infected, symptomatic becomes hospitalized
           Ia*lambda_a,                        #Infected, asymptomatic becomes recovered
           Is*lambda_s*(1-rho),                #Infected, symptomatic recovers
           H*delta*mu,                         #Hospitalized becomes Dead
           H*delta*(1-mu)))                    #Hospitalized becomes recovered
  
}
# Parameters
pars_base <- c(N = 1e6,            #population size
               beta = 0.5,      #transmission rate
               cp = 1.2,         #Relative contact rate between S and Ip
               ca = 1.2,          #Relative contact rate between S and Ia
               cs = 0.8,          #Relative contact rate between S and Is
               cm = 1,          #Relative contact rate between S and Im
               alpha = 0.3,    #rate of symptomatic case
               gam = 1/4,        #1/serial interval
               la = 0.15,    #rate of recovery of asymptomatic cases
               ls = 1/5,    #rate of hospitalization for severe cases
               lm = 0.15,    #rate of recovery for mild cases
               lp = 0.8,    #1/(latent period - serial interval) = time from pre-symptomatic to symptomatic (either mild or severe)
               rho = 0.95,        #fraction of symptomatic cases that are mild
               delta = 1/12,  #1/time spent in hospital for severe cases
               mu = 0.14      #mortality rate of hospitalized cases
  
)

pars_simp <- c(N = 1e6,            #population size
               beta = 0.5,         #transmission rate
               ca = 1.2,           #Relative contact rate between S and Ia
               cs = 1.0,           #Relative contact rate between S and Is
               alpha = 0.317,      #proportion of asymptomatic case
               gam = 1/5,          #1/incubation period
               la = 0.15,          #rate of recovery of asymptomatic cases
               ls = 1/22,          #recovery rate of non severe cases
               lh = 1/5,           #rate of hospitalization for symptomatic cases
               rho = 0.20,         #fraction of symptomatic cases that are hospitalized
               delta = 1/12,       #1/time spent in hospital for severe cases
               mu = 0.14           #mortality rate of hospitalized cases
)

#Function to simulate and return dataframe
stoch.sim = function(init, trans, rates, pars, t_sim){
  
  ssa.sim <- adaptivetau::ssa.adaptivetau(init.values = init, 
                                          transitions = trans,
                                          rateFunc = rates,
                                          params = pars,
                                          tf=t_sim)
  
  return(ssa.sim)
}  


# Run stanford model and plot
init_vars <- c("S" = 1e6-10,
               "E" = 10,
               "Ip" = 0,
               "Ia" = 0,
               "Is" = 0,
               "Im" = 0,
               "H" = 0,
               "R" = 0,
               "D" = 0)

init_runs <- bind_rows(lapply(1:100, function(i){
  as.data.frame(stoch.sim(init = init_vars, 
                          trans = transitions, 
                          rates = rates_base,
                          pars = pars_base,
                          t_sim = 300)) %>% 
    mutate(sim = i)
  }))

init_runs %>% 
  mutate(I = Is+Ia+H) %>% 
  select(time, I, sim) %>% 
  ggplot(aes(x = time, y = I, group = as.factor(sim))) +
    geom_line(size = 0.2, col = "grey50") +
    theme_bw() +
    labs(x="time",
         y = "cases",
         title = "infecteds through time, no interventions")

init_runs %>% 
  select(time, H, sim) %>% 
  ggplot(aes(x = time, y = H, group = as.factor(sim))) +
    geom_line(size = 0.2, col = "grey50") +
    theme_bw() +
    labs(x="time",
         y = "hospitalizations",
         title = "hospitalizations through time, no interventions")

init_runs %>% 
  select(time, D, sim) %>% 
  ggplot(aes(x = time, y = D, group = as.factor(sim))) +
    geom_line(size = 0.2, col = "grey50") +
    theme_bw() +
    labs(x="time",
         y = "deaths",
         title = "deaths through time, no interventions")
