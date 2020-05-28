# ---------------------------------------------------------
#   COVID mass action model forced with movement data
#   Chris Hoover (choover@berkeley.edu)
#   May 2020
# ---------------------------------------------------------

covid_odes = function(t, n, p) { 

  # States
    S = n[1]
    E = n[2]
    Ip = n[3]
    Ia = n[4]
    Is = n[5]
    Im = n[6]
    Ih = n[7]
    H = n[8]
    D = n[9]
    R = n[10]
  
  # Parameters
    N = p[['N']]             #population size
    beta = p[['beta']]       #transmission rate
    cp = p[['cp']]           #Relative contact rate between S and Ip
    ca = p[['ca']]           #Relative contact rate between S and Ia
    cm = p[['cm']]           #Relative contact rate between S and Im
    cs = p[['cs']]           #Relative contact rate between S and Is
    sigma = p[['sigma']]     #1/serial interval
    alpha = p[['alpha']]     #proportion asymptomatic
    lambda = p[['lambda']]   #1/(incubation period-serial interval) to model pre-symptomatic transmission
    rho = p[['rho']]         #Proportion symptomatic requiring hospitalization
    delta_1 = p[['delta_1']] #convenience parameter: 1/(delta1 + delta2) = time between symptom onset and hospitalization
    delta_2 = p[['delta_2']] #1/(delta1 + delta2) = time between symptom onset and hospitalization
    gam_a = p[['gam_a']]     #1/time to recovery (non-infectiousness) for asymptomatics
    gam_m = p[['gam_m']]     #1/time to recovery for mild cases
    gam_h = p[['gam_h']]     #1/time to removed (recovered or deceased) for hospitalized
    mu = p[['mu']]           #proportion of hospitalized cases who die

  #Movement forcing function
    m = descartes_force(t)
  
  # Model equations
    dSdt = -S*beta*m*(Ip*cp+Ia*ca+Im*cm+Is*cs)/N
    
    dEdt = S*beta*m*(Ip*cp+Ia*ca+Im*cm+Is*cs)/N - E*sigma
    
    dIpdt = E*sigma - Ip*(alpha*lambda+(1-alpha)*lambda)
    
    dIadt = Ip*alpha*lambda - Ia*gam_a
    
    dIsdt = Ip*(1-alpha)*lambda - Is*(rho*delta_1+(1-rho)*delta_1)
    
    dImdt = Is*(1-rho)*delta_1 - Im*gam_m

    dIhdt = Is*rho*delta_1 - Ih*delta_2

    dHdt = Ih*delta_2 - H*(mu*gam_h + (1-mu)*gam_h)
    
    dDdt = H*mu*gam_h
    
    dRdt = Ia*gam_a+Im*gam_m+H*(1-mu)*gam_h

    return(list(c(dSdt, dEdt, dIpdt, dIadt, dIsdt, dImdt, dIhdt, dHdt, dDdt, dRdt)))
    
} 
