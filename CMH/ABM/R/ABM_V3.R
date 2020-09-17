# ---------------------------------------------------------
#   COVID ABM v3 incorporating SafeGraph data and refined testing and isolation
#   Chris Hoover (choover@berkeley.edu)
#   Sept 2020
# ---------------------------------------------------------

#------------------
# Helpers/Utils
#------------------
#' @title Not in 
#' 
#' @description Opposite of `%in%`
#' 
#' @return function for opposite of `%in%``
#' @export

`%!in%` <- Negate(`%in%`)

#' @title Summarize Infection
#' 
#' @description Function to summarize infection vector with individual states into aggregate
#' 
#' @param x infections status vector from `ABM` run
#' 
#' @return vector of length 9 with integer counts of number of agents in each class
#' @export

sum.inf <- function(x){
  
    S = sum(x == "S")
    E = sum(x == "E")
    Ip = sum(x == "Ip")
    Ia = sum(x == "Ia")
    Im = sum(x == "Im")
    Imh = sum(x == "Imh")
    Ih = sum(x == "Ih")
    D = sum(x == "D")
    R = sum(x == "R")

  return(c(S, E, Ip, Ia, Im, Imh, Ih, D, R))
}

#' @title Get mask compliance probabilities
#' 
#' @description Function to generate random mask compliance
#' 
#' @param n_agents number of agents to generate probabilities for
#' 
#' @return vector of length n_agents with randomly generated probabilities of wearing mask following implementation of mask mandate
#' @export

mask_fx <- function(n_agents){
  rbeta(n_agents, 5, 2)
}

#' @title Quarantine compliance 
#' 
#' @description Function to generate random quarantine compliance
#' 
#' @param n_agents number of agents to generate probabilities for
#' 
#' @return vector of length n_agents with randomly generated probabilities of quarantine following symptom onset/contact/positive test
#' @export

quar_fx <- function(n_agents){
  rbeta(n_agents, 5, 2)
}

#' @title Sociality
#' 
#' @description Function to generate random sociality metric
#' 
#' @param n_agents number of agents to generate probabilities for
#' 
#' @return vector of length n_agents with randomly generated sociality
#' @export

social_fx <- function(n_agents){
  rbeta(n_agents, 2, 2)
}

#-------------------------
# Infection functions
#-------------------------
#' @title latent period
#'  
#' @description Draw from distribution of proposed latent periods. Used in `next.state`
#' 
#' @param n_E Number of draws
#' @param shape.l Shape of gamma distribution
#' @param scale.l Scale of gamma distribution
#' 
#' @return numeric of time spent in latent period (E)
#' @export
#'        

t_latent <- function(n_E, shape.l=5, scale.l=1){
  rgamma(n_E, shape.l, scale.l)
} 
      
#' @title Presymptomatic period
#'  
#' @description Draw from distribution of proposed presymptomatic periods (basically incubation-latent periods) to model presymptomatic transmission. Used in `next.state`
#' 
#' @param n_Ip Number of draws
#' @param shape Shape of gamma distribution
#' @param scale Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_presymp <- function(n_Ip, shape.ip=2,scale.ip=1){
  rgamma(n_Ip, shape.ip, scale.ip)
} 

#' @title Asymptomatic period
#'  
#' @description Draw from distribution of proposed asymptomatic period. Used in `next.state`
#' 
#' @param n_Ia Number of draws
#' @param shape.ia Shape of gamma distribution
#' @param scale.ia Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_asymp <- function(n_Ia, shape.ia = 6, scale.ia = 1){
  rgamma(n_Ia, shape.ia, scale.ia)
} 

#' @title Mild infection duration
#'  
#' @description Draw from distribution of proposed length spent with minor symptoms. Used in `next.state`
#' 
#' @param n_Im number of draws
#' @param exp.im parameter of exponential distribution for time spent with mild symptoms
#' 
#' @return numeric of time spent in mildly symptomatic (Im)
#' @export
#'        
t_msymp <- function(n_Im, exp.im = 1/6) {
  rexp(n_Im, exp.im)
}

#' @title Symptom onset to hospitalization
#'  
#' @description Draw from distribution of proposed time spent with minor symptoms before being hospitalized for severe infections (i.e. those with B(1,p.sevsymp==1)). Used in `next.state`
#' 
#' @param n_Imh number of draws
#' @param shape.imh shape of gamma distn
#' @param scale.imh scale of gamma distn
#' 
#' @return numeric of time spent mildly symptomatic before hospitalization (Imh)
#' @export
#'        
t_mtosev <- function(n_Imh,shape.imh = 7, scale.imh = 2){
  rgamma(n_Imh, shape.imh, scale.imh)
}

#' @title time spent severely symptomatic (in hospital)
#'  
#' @description Draw from distribution of proposed time spent with severe symptoms before dying or being discharged. Used in `next.state`
#' 
#' @param n_Ih number of draws
#' @param shape.ih shape of gamma distn
#' @param scale.ih scale of gamma distn
#' 
#' @return numeric of time spent hospitalized (Ih)
#' @export
#'        
t_sevsymp <- function(n_Ih, shape.ih = 3, scale.ih = 0.25){
  rgamma(n_Ih, shape.ih, scale.ih)
}

#' @title Probability of symptomatic infection
#'  
#' @description Age-dependent probabilities of having symptoms. Used in `next.state`
#' 
#' @param age age of person
#' 
#' @return numeric of probability of having symptoms.
#' @export
#'
p_symp <- function(age){
  n <- length(age)
  n[age %in% c(0:9)] <- 0.03
  n[age %in% c(10:19)] <- 0.05
  n[age %in% c(20:29)] <- 0.3
  n[age %in% c(30:39)] <- 0.55
  n[age %in% c(40:49)] <- 0.61
  n[age %in% c(50:59)] <- 0.75
  n[age %in% c(60:69)] <- 0.89
  n[age %in% c(70:79)] <- 0.9
  n[age >= 80] <- 0.9
  
  return(n)
}

#' @title Probability of severely symptomatic (will be hospitalized) infection
#'  
#' @description Age-dependent probabilities of having severe symptoms. Used in `next.state`
#' 
#' @param age age of person
#' 
#' @return numeric of probability of having severe symptoms.
#' @export
#'
p_sevsymp <- function(age){
  n <- length(age)
  n[age %in% c(0:9)] <- 0.004
  n[age %in% c(10:19)] <- 0.004
  n[age %in% c(20:29)] <- 0.01
  n[age %in% c(30:39)] <- 0.04
  n[age %in% c(40:49)] <- 0.09
  n[age %in% c(50:59)] <- 0.13
  n[age %in% c(60:69)] <- 0.19
  n[age %in% c(70:79)] <- 0.2
  n[age >= 80] <- 0.25 
}

#' @title Probability of death
#'  
#' @description Age-dependent probabilities of dying given hospitalization (Ih) Used in `next.state`; from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified
#' 
#' @param age age of person
#' 
#' @return numeric of probability of dying.
#' @export
#'
p_mort <- function(age) {
    n <- length(age)
    n[age %in% c(0:9)] <- 0.01
    n[age %in% c(10:19)] <- 0.0205
    n[age %in% c(20:29)] <- 0.031
    n[age %in% c(30:39)] <- 0.0475
    n[age %in% c(40:49)] <- 0.0785
    n[age %in% c(50:59)] <- 0.12
    n[age %in% c(60:69)] <- 0.186
    n[age %in% c(70:79)] <- 0.3
    n[age >= 80] <- 0.45
}

#' @title Time til Next state
#' 
#' @description Take input infection state the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' 
#' @return vector time to be spent in infection state. Time should be converted to `numeric` for subsequent use
#' @export

t_til_nxt <- function(pre.status){
  n <- vector("numeric", length(pre.status))
  n_E <- sum(pre.status == "E")
  n_Ip <- sum(pre.status == "Ip")
  n_Ia <- sum(pre.status == "Ia")
  n_Im <- sum(pre.status == "Im")
  n_Imh <- sum(pre.status == "Imh")
  n_Ih <- sum(pre.status == "Ih")
  
  n[pre.status == "E"] <- t_latent(n_E)
  n[pre.status == "Ip"] <- t_presymp(n_Ip)
  n[pre.status == "Ia"] <- t_asymp(n_Ia)
  n[pre.status == "Im"] <- t_msymp(n_Im)
  n[pre.status == "Imh"] <- t_mtosev(n_Imh)
  n[pre.status == "Ih"] <- t_sevsymp(n_Ih)

  return(n)
}  

#' @title Next State
#' 
#' @description Take input infection state and age and return the next infection state and the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' @param age age of individual for probability of moving to different states
#' 
#' @return vector the next state
#' @export

next_state <- function(pre.status, age){
  n <- length(pre.status)
  post.status <- rep("R", n)
  p_symps <- p_symp(age)
  p_sevsymps <- p_sevsymp(age)
  p_deads <- p_mort(age)
  p1 <- dqrunif(n)
  p2 <- dqrunif(n)
  
  post.status[pre.status == "E"] <- "Ip"
  post.status[pre.status == "Ip" & p1>p_symps] <- "Ia"
  post.status[pre.status == "Ip" & p1<p_symps & p2>p_sevsymps] <- "Im"
  post.status[pre.status == "Ip" & p1<p_symps & p2<p_sevsymps] <- "Imh"
  post.status[pre.status == "Imh"] <- "Ih"
  post.status[pre.status == "Ih" & p1<p_deads] <- "D"

  return(post.status)
}

#' @title Person's FOI in large areas
#'  
#' @description Determine FOI from locations with infectious individuals in large areas: schools, workplaces, communities (as opposed to corresponding small areas: classrooms, offices, homes)
#' 
#' @param I  Location of this person
#' @param N number f infectious people transmitting
#' @param bta transmission probability
#'  
#' @return FOI for this person
#' @export
#'        
get_foi <- function(I, N, bta){
  sum(I*bta, na.rm = T)/N
}

#' @title Simulate Infection
#'  
#' @description Randomly determine if infection occurs from FOI
#' 
#' @param foi  Person's FOI
#'  
#' @return binary of infection occurring or not
#' @export
#'        

foi_infect <- function(foi){
  p <- dqrunif(length(foi))
  return(as.numeric(p<(1-exp(-foi))))
}

#' @title Transmission probability in different settings and by mask use or not
#'  
#' @description Get vector of transmission probabilities from people in different locations and mask wearing compliance
#' 
#' @param location  Person's location
#' @param residence  Person's residence
#' @param work  Person's work
#' @param school  Person's school
#' @param trans_res  
#' @param trans_school  
#' @param trans_work  
#' @param trans_other  
#' @param mask  vector (binary) of who is wearing mask
#' @param mask_red  reduction in transmission probability if wearing mask
#'  
#' @return binary of infection occurring or not
#' @export
#'        

get_trans_prob <- function(location, residence, work, school,
                           trans_res, trans_work, trans_school, trans_other, mask, mask_red){
  t.prob <- trans_other*(1-mask_red*mask)
  t.prob[location == residence] <- trans_res
  t.prob[location == school] <- trans_school*(1-mask_red*mask[location == school])
  t.prob[location == work] <- trans_work*(1-mask_red*mask[location == work])
  
  return(t.prob)
}


#' @title Person's FOI in large areas
#'  
#' @description Determine FOI from locations with infectious individuals in large areas: schools, workplaces, communities (as opposed to corresponding small areas: classrooms, offices, homes)
#' 
#' @param I  Location of this person
#' @param N number f infectious people transmitting
#' @param bta transmission probability
#'  
#' @return FOI for this person
#' @export
#'        
get_foi <- function(I, N, bta){
  (I/N)*bta
}

#' @title Isolate individuals
#'  
#' @description Determine if agent(s) will quarantine based on their characteristics
#' 
#' @param q_prob  Person's quarantine probability assigned from quar_fx
#' @param t_symptoms Length of time person has been experiencing symptoms
#' @param tested If person has been notified of a positive test result
#' @param essential If person is an essential worker
#' @param SiP whether shelter in place is active
#'  
#' @return binary of person quarantining or not
#' @export
#'        

quar_iso <- function(q_prob, t_symptoms, tested, essential, SiP){
  n <- length(q_prob)
  r <- dqrng::dqrunif(n)
  
  symp_prob <- 1+t_symptoms*0.1 # Longer symptoms leads to higher probability of isolating
  
  p <- q_prob*symp_prob*(1+tested*0.5)*(1-essential*SiP) # Test positive increases q_prob probability by 50% ; Assume essential workers do not isolate
  
  return(as.numeric(r<p))
}

#-------------------------
# Network/movement functions
#-------------------------
#' @title Sample location
#'  
#' @description Function to determine agent's location given present model conditions. Works for agents with 3 possible locations residence/home, community and either work or school. Function passes location ids plus probability of home and community, with probability work/ school then implied
#' 
#' @param l_res residence location id
#' @param l_com community location id
#' @param l_scl work/school location id
#' @param p_res probability of being at residence
#' @param p_com probability of being in community
#' 
#' @return vector of location ids
#' @export
#' 
scl_wrk_loc <- function(l_res, l_com, l_scl_wrk, p_res, p_com){
  n <- length(l_res)
  #print(n)
  u <- dqrunif(n)
  samp <- l_com
  index <- u < p_res
  samp[index] <- l_res[index]
  index <- u > (p_res + p_com)
  samp[index] <- l_scl_wrk[index]
  return(samp)
  
}


#' @title Simulate school-aged children location
#'  
#' @description Simulate location of school-aged children depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param tested test status of individual 1/0
#' @param scl schools closed? 1/0
#' @param SiP shelter in place active? 1/0
#' @param time_day time of day (night or day)
#' @param day_week day of the week (U, M, T, W, R, F, or S)
#' @param age age of person
#' @param sociality agent sociality
#' @param res_id id of this individual's residence
#' @param scl_id id of this individual's school
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), school (S), or community (C)
#' @export
#'        
sac_location <- function(inf.state, tested,
                         scl, SiP, home_prob, time_day, day_week,
                         age, sociality,
                         res_id, scl_id, comm_id){
  
  n <- length(inf.state)
  
# Children always at home at night and in the mornings  
  if(time_day == "M"){
    probs <- cbind(rep(1,n), rep(0,n))
    
# Children are in school during the day if it's open and it's a weekday    
  } else if(scl == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    probs <- cbind(rep(0,n), rep(0,n))
    
# Children are randomly either still at school, in the community or at home in the evening during the week if schools are open  
  } else if(scl == 0 & time_day == "E" & day_week %in% c("M", "T", "W", "R", "F")){
    probs <- cbind(rep(0.33333,n), rep(0.33333,n))
    
# Weekend-like dynamics if school is closed, but SiP not in effect or if it's the weekend: children can be at home or in the community during the day and evening
  } else if((scl == 1 & SiP == 0) | (day_week %in% c("S", "U") & SiP == 0)){
    probs <- cbind(rep(0.5,n), rep(0.5,n))
    
# Shelter in place, sac most likely at home, but age and community dependent chance of being in community    
  } else if(SiP == 1){
    age_prob <- 1/(20-age) # More likely to be in community if older
    comm_prob <- age_prob*sociality*(1-home_prob)
    # Renormalize to 1
    comm_prob <- comm_prob/max(comm_prob)
    
    probs <- cbind(1-comm_prob, comm_prob)
    
  } else {
    stop("Situation not recognized for School agent")
  }
  
  # School agents who are sick or quarantined/isolated stay home
  at.home <- inf.state %in% c("Im", "Imh") | tested == 1
  location <- rep(NA_integer_, n)
  location[at.home] <- res_id[at.home]
  location[!at.home] <- scl_wrk_loc(res_id[!at.home],comm_id[!at.home], scl_id[!at.home],  probs[!at.home, 1], probs[!at.home, 2])

  return(location)
}

#' @title Simulate worker locations
#'  
#' @description Simulate location of school-aged children depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param tested test status of individual 1/0
#' @param SiP shelter in place active? 1/0
#' @param time_day time of day (night or day)
#' @param day_week day of the week (U, M, T, W, R, F, or S)
#' @param age age of person
#' @param kids number of kids in household
#' @param essential is the individual in an essential workforce? 1/0
#' @param sociality agent sociality
#' @param res_id id of this individual's residence
#' @param work_id id of this individual's workplace
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), or community (C)
#' @export
#'        
worker_location <- function(inf.state, tested,
                            SiP, home_prob, time_day, day_week,
                            age, essential, sociality,
                            res_id, work_id, comm_id){
  
    n <- length(inf.state)
    
  # home probability adjusted for age: older age groups more likely to be at home
    home_prob <- home_prob*(age/100)*(1-sociality)
  # Normalize to 1
    home_prob <- home_prob/max(home_prob)

# Workers location community or home during the week in the morning/evening
  if(SiP == 0 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(home_prob, 1-home_prob)
    
# Workers at work during the weekday outside of shelter in place
  } else if(SiP == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(rep(0,n), rep(0,n))
    
# Workers location (community, home, or work) during the weekend, non-night
  } else if(SiP == 0 & day_week %in% c("S", "U")){

    probs = cbind(home_prob, 1-home_prob)
    
# Workers location during SiP weekday non-nights
  } else if(SiP == 1 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(home_prob, 1-home_prob)
    
# Workers at work during the SiP if essential
  } else if(SiP == 1 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(home_prob*(1-essential), (1-home_prob)*(1-essential))
    
# Workers location community or home during the weekend, non-night
  } else if(SiP == 1 & day_week %in% c("S", "U")){

    probs = cbind(home_prob, 1-home_prob)
    
  } else {
    stop("Worker situation not recognized")
  }
    
# Determine locations    
# Workers who are sick or quarantined/isolated stay home
  at.home <- inf.state %in% c("Im", "Imh") | tested == 1
  location <- rep(NA_integer_, n)
  location[at.home] <- res_id[at.home]
  
  location[!at.home] <- scl_wrk_loc(res_id[!at.home], comm_id[!at.home], work_id[!at.home], probs[!at.home, 1], probs[!at.home, 2])
  
  return(location)

}

#' @title Simulate location of individuals who are not workers or school-aged
#'  
#' @description Simulate location of school-aged children depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param tested test status of individual 1/0
#' @param SiP shelter in place active? 1/0
#' @param time_day time of day (night or day)
#' @param age age of person
#' @param sociality agent sociality
#' @param res_type type of residence (H, C, P, or N)
#' @param res_id id of this individual's residence
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), school (S), or community (C)
#' @export
#'        
other_location <- function(inf.state, tested,
                           SiP, home_prob,  
                           age, sociality, res_type,  
                           res_id, comm_id){
  n <- length(inf.state)
  # home probability adjusted for age: older age groups more likely to be at home
    home_prob <- home_prob*(age/100)*(1-sociality)
  # Normalize to 1
    p.home <- home_prob/max(home_prob)

  # Assign locations  
  location <- comm_id  
  at.home <- inf.state %in% c("Im", "Imh") | tested == 1 | res_type %in% c("P", "N")
  location[at.home] <- res_id[at.home]
  
  l.probs <- dqrunif(n)
  at.home2 <- l.probs < p.home
  location[at.home2] <- res_id[at.home2]
  
  # Assume college students leave once SiP announced
  left <- SiP == 1 & res_type == "C"
  location[left] <- NA_integer_
  
  return(location)
}

#' @title Small location
#'  
#' @description Function to allocate agent locations to smaller subunits( offices and classes) from larger areas (workplaces and schools)
#' 
#' @param l_res residence location id
#' @param l_com community location id
#' @param l_scl work/school location id
#' @param p_res probability of being at residence
#' @param p_com probability of being in community
#' 
#' @return vector of location ids
#' @export
#' 
location_small <- function(l_id, work_id, school_id, office_id, class_id){
  classes <- l_id == school_id
  offices <- l_id == work_id

  l_id[classes] <- class_id[classes]
  l_id[offices] <- office_id[offices]
  
  return(l_id)
  
}

#' @title Community location function
#'  
#' @description Draws random uniform variable between 0 and 1, looks in CDF matrix to determine corresponding probability of visiting CBG, finds corresponding CBG visited in index matrix
#' 
#' @param mat_cdf cumulative probability matrix with rows corresponding to CDF of individual residing in row and visiting column
#' @param cbgs cbg names coresponding to rows of cbg_cdf
#' @param agent_cbgs cbgs of agents which are candidates to interact in the community
#' 
#' @return 
#' @export
#' 

GetCBGVisit <- function(mat_cdf, cbgs, agent_cbgs) {
  cbg_indices <- fmatch(agent_cbgs, cbgs)
  n <- length(cbg_indices)
  r <- dqrng::dqrunif(n)
  mat_cdf_expand <- mat_cdf[cbg_indices,]
  cbgs[max.col(r < mat_cdf_expand, "first")]
}

# microbenchmark::microbenchmark(match(agents[, cbg], sf_cbg_ids), fmatch(agents[, cbg], sf_cbg_ids))

#-------------------------
# Testing functions
#-------------------------

#-------------------------
# Model setup
#-------------------------

#-------------------------
# Main model sim function
#-------------------------
#' @title ABM V3 no testing 
#' 
#' @description Function to run agent based model
#' 
#' @param bta_hh houshold transmission probability
#' @param bta_scl school transmission probability
#' @param bta_work work transmission probability
#' @param bta_other other transmission probability
#' @param E0 number of initial exposed agents
#' @param Ip0 number of initial pre-symptomatic infections agents
#' @param Ia0 number of initial asymptomatic infectious agents
#' @param Im0 number of initial mildy symptomatic agents
#' @param Imh0 number of initial mildly symptomatic, will become severely symptomatic agents
#' @param Ih0 number of initial hospitalized agents
#' @param R0 number of initial recovered agents
#' @param D0 number of initial diceased agents
#' @param agents_dt data.table of agents with necessary characteristics including: TODO: FILL THESE IN
#' @param cbg_cdf_array array with dimensions cbgs x cbgs x t.tot in which entries represent probabilities of moving from CBG i to CBG j. Used in `GetCBGVisit` function
#' @param cbg_ids vector relating row numbers of `cbg_cdf_array` to actual cbg codes
#' @param stay_home_dt data table with columns `Date`, `origin_census_block_group`, and `pct_home` (E.g. derived from safegraph data) to use for social distancing/stay at home compliance
#' @param t0 start date of simulation
#' @param t.tot total time to run simulation (in days)
#' @param dt time step (defaults to 1/6, needs editing if otherwise)
#' @param day_of_week_fx function which takes t returns character of day of week (U,M,T,W,R,F,S)
#' @param SiP.start Date that shelter in place started
#' @param scl.close date that schools were closed
#' @param mask.start Date that mask mandate was introduced
#' @param mask.red reduction in transmission probability if wearing mask
#' @param mask_fx fnction to assign probabilities of wearing a mask following mask mandate
#' @param quar_fx function to assign probability an agent quarantines following exposure, positive test, or symptom start
#' @param social_fx function to generate random sociality metrics
#' 
#' 
#' @return list with two objects: an epi curve with counts of agents in each state through time and a dataframe with infection reports for every new infection (e.g. entry created whenever agent transitions from S to E)
#' @export
  
  covid_abm_v3_no_test <- function(bta_hh, bta_scl, bta_work, bta_other,                 # transmission probabilities for different settings
                                   E0, Ip0, Ia0, Im0, Imh0, Ih0, R0, D0,                  # Starting conditions
                                   agents_dt, cbg_cdf_array, cbg_ids, stay_home_dt,            # Agents dt, mvmt matrix, stay_home function
                                   t0, t.tot, dt, day_of_week_fx, time_of_day_fx, SiP.start, scl.close, mask.start, mask.red, # Times and key dates
                                   mask_fx, quar_fx, social_fx){
# Initial conditions
  e.seed <- E0     #Exposed
  ip.seed <- Ip0    #infected pre-symptomatic
  ia.seed <- Ia0    #infected asymptomatic
  im.seed <- Im0    #infected mildly symptomatic
  imh.seed <- Imh0   #infected mildly symptomatic, will become severe
  ih.seed <- Ih0    #infected severely symptomatic
  d.seed <- D0     #dead
  r.seed <- R0     #removed
  non.s <- e.seed + ip.seed + ia.seed + im.seed + imh.seed + ih.seed + d.seed + r.seed
  s.seed <- N - non.s
  
  agents <- agents_dt
  
# Initial infection allocated among workers
  init.Es <- sample(agents[work >0, id], e.seed)   
  init.Ips <- sample(agents[work >0, id], ip.seed)   
  init.Ias <- sample(agents[work >0, id], ia.seed)   
  init.Ims <- sample(agents[work >0, id], im.seed)   
  init.Imhs <- sample(agents[work >0, id], imh.seed)   
  init.Ihs <- sample(agents[work >0, id], ih.seed)   
  init.Ds <- sample(agents[work >0, id], d.seed)   
  init.Rs <- sample(agents[work >0, id], r.seed)   

  agents[id %in% init.Es, state:="E"]
  agents[id %in% init.Ips, state:="Ip"]
  agents[id %in% init.Ias, state:="Ia"]
  agents[id %in% init.Ims, state:="Im"]
  agents[id %in% init.Imhs, state:="Imh"]
  agents[id %in% init.Ihs, state:="Ih"]
  agents[id %in% init.Ds, state:="D"]
  agents[id %in% init.Rs, state:="R"]
  
# Keep track of everyone's infection status through time   
  epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
    epi_curve[1,] <- sum.inf(agents[,state])
  
# Keep record of infection events
  infection_reports <- list()
   
# Update characteristics of initial infections  
  # Transition time
  agents[state %!in% c("S", "D", "R"), tnext:=t_til_nxt(state)]
  # State entering once transition time expires
  agents[state %!in% c("S", "D", "R"), nextstate:=next_state(state, age)]
  #Time initial infections occurred
  agents[state %!in% c("S", "D", "R"), t_infection:=dt]

# Add compliance and sociality metrics, start people with no known contacts
  agents[, mask := mask_fx(nrow(agents))] # Probability of wearing a mask
  agents[, q_prob := quar_fx(nrow(agents))] # Probability of quarantine/isolation given positive test
  agents[, sociality := social_fx(nrow(agents))] # Sociality metric
  agents[, contact := 0]
  
# Run simulation    
for(t in 2:(t.tot/dt)){
 print(t)
  
# Time step characteristics
  date_now <- t0+t*dt
  agents[, Date:=date_now]
  date_num <- as.numeric(floor(date_now-as.Date("2020-01-01")))
  day_week <- day_of_week_fx[t]
  time_day <- time_of_day_fx[t]
  scl.closed <- ifelse(date_now > scl.close, 1, 0)
  SiP.active <- ifelse(date_now > SiP.start, 1, 0)
  mask.mandate <- ifelse(date_now > mask.start, 1, 0)

# Advance transition times, time since infection,time since symptoms started
  agents[state %!in% c("S", "D", "R"), tnext:=tnext-dt]
  agents[state %!in% c("S", "D", "R"), t_infection:=t_infection+dt]
  agents[state %in% c("Im", "Imh"), t_symptoms:=t_symptoms+dt]

# Advance expired states to next state, determine new nextstate and time til next state
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), nextstate:=next_state(state, age)]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), tnext:=t_til_nxt(state)]

  print("Infections advanced")
  
# Simulate infection --------------------
  # If noone transmitting, skip. Assume agents inactive at night
  if(nrow(agents[state %!in% c("S", "E", "D", "R")])>0 & time_day != "N"){
    
# Quarantine ------------
  agents[t_symptoms > 0 | tested > 0 | contact > 0, quarantine:= quar_iso(q_prob, t_symptoms, tested, essential, SiP.active)]
    
# Determine locations ---------------
  # Get CBG-based stay at home compliance 
  home.today <- stay_home_dt[Date == as.character(date_now),]
  home.mean <- mean(home.today$pct_home)
  home_agents <- merge(agents, home.today, by.x = "cbg", by.y = "origin_census_block_group", all.x = TRUE)
  home_agents[is.na(pct_home), pct_home:=home.mean]
  agents[,pct_home:=home_agents[,pct_home]]

  # Find locations of those not deceased or in the hospital  
    # Agents that are in school
    agents[school > 0 & work < 0 & state %!in% c("Ih", "D"), 
           location:=sac_location( 
             state, tested, scl.closed, SiP.active, pct_home,
             time_day, day_week, age, sociality,
             residence, school, cbg)]

    # Agents that are workers only
    agents[school < 0 & work > 0 & state %!in% c("Ih", "D"), 
           location:=worker_location(state, tested,
                                     SiP.active, pct_home, time_day, day_week,
                                     age, essential, sociality,
                                     residence, work, cbg)]
    
    # Agents that are neither in school or are working
    agents[work < 0 & school < 0 & state %!in% c("Ih", "D"),
           location:=other_location(state, tested,
                                    SiP.active, pct_home, 
                                    age, sociality, residence_type,  
                                    residence, cbg)]
    
    #Agents that are quarantined stay at home
    agents[quarantine == 1, location:=residence]
    
    # Probabilistically move agents in community to other communities based on safegraph movement
    cbg_moves <- GetCBGVisit(cbg_cdf_array[,,date_num], cbg_ids, 
                             agents[state %!in% c("Ih", "D") & location == cbg, location])
    
    agents[state %!in% c("Ih", "D") & location == cbg, 
           location:=cbg_moves]
    
    # Smaller sub-locations (offices and classrooms) for agents in workplaces or schools
    agents[state %!in% c("Ih", "D") & !is.na(location),
           small_location:=location_small(location, work, school, office_id, class_id)]
  
  print("Locations resolved")
  
# Determine number of individuals and transmitting individuals by location  
  agents[, n_transmitting:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = location]
  agents[n_transmitting > 0, n_present:=.N, by = location]
  
  agents[n_transmitting > 0, n_transmitting_small:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = small_location]
  agents[n_transmitting_small > 0, n_present_small:=.N, by = small_location]
  
# Determine FOI in locations where someone is transmitting
  agents[,wear.mask:=0]
  
  if(mask.mandate == 1){ # Only really care about people transmitting wearing a mask
    agents[state %in% c("Ip", "Ia", "Im", "Imh"), wear.mask:=rbinom(.N, 1, mask)]
  } 
  
  agents[state %in% c("Ip", "Ia", "Im", "Imh"), 
         trans_prob := get_trans_prob(location, residence, work, school,
                                      trans_res = trans.hh, trans_work = trans.work, trans_school = trans.school, trans_other = trans.other, 
                                      mask = wear.mask, mask_red = mask.red)]

  agents[n_transmitting > 0, FOI:=get_foi(n_transmitting, n_present, trans_prob), by = location]
  
# For agents sharing small location, overwrite lower FOI with small location FOI 
  agents[n_transmitting_small > 0, FOI:=get_foi(n_transmitting_small, n_present_small, trans_prob), by = small_location]
  
# Document KNOWN contacts in proportion to number of people in location where transmission was occuring
  agents[, n_symptomatic:=sum(state %in% c("Im", "Imh")), by = location]
  agents[, n_symptomatic_small:=sum(state %in% c("Im", "Imh")), by = small_location]
  agents[n_symptomatic > 0, contact_prob := 1/n_symptomatic]
  agents[n_symptomatic_small > 0, contact_prob := 1/n_symptomatic_small]
  
  agents[contact_prob > 0, contact := rbinom(.N, 1, contact_prob)]
  
# Generate infections, update their state, sample for their nextstate and time until reaching it
    agents[FOI > 0 & state == "S", infect:=foi_infect(FOI)]
    agents[infect == 1, state:="E"]
    agents[infect == 1, nextstate:=next_state(state, age)]
    agents[infect == 1, tnext:=t_til_nxt(state)]
    
# Store detailed infection info
    infection_reports[[t-1]] <- agents[infect == 1,]
    print("New infections generated")
    
# Reset infection & location columns
    agents[, c("location", "small_location",
               "n_transmitting", "n_transmitting_small", 
               "n_symptomatic", "n_symptomatic_small", "contact_prob",
               "n_present", "n_present_small", "pct_home", "wear.mask",
               "FOI", "infect"):=NULL]
  }
  
  epi_curve[t,] <- sum.inf(agents[,state])
  # On to the next one  
    
}    
  fin_out <- list()
  fin_out[["epi_curve"]] <- epi_curve
  fin_out[["infections"]] <- rbindlist(infection_reports,fill=TRUE)
  
  return(fin_out)
  
}
  

#------------------
# ABM function with testing
#------------------
#' @title ABM V3 no testing 
#' 
#' @description Function to run agent based model
#' 
#' @param bta_hh houshold transmission probability
#' @param bta_scl school transmission probability
#' @param bta_work work transmission probability
#' @param bta_other other transmission probability
#' @param E0 number of initial exposed agents
#' @param Ip0 number of initial pre-symptomatic infections agents
#' @param Ia0 number of initial asymptomatic infectious agents
#' @param Im0 number of initial mildy symptomatic agents
#' @param Imh0 number of initial mildly symptomatic, will become severely symptomatic agents
#' @param Ih0 number of initial hospitalized agents
#' @param R0 number of initial recovered agents
#' @param D0 number of initial diceased agents
#' @param agents_dt data.table of agents with necessary characteristics including: TODO: FILL THESE IN
#' @param cbg_cdf_array array with dimensions cbgs x cbgs x t.tot in which entries represent probabilities of moving from CBG i to CBG j. Used in `GetCBGVisit` function
#' @param cbg_ids vector relating row numbers of `cbg_cdf_array` to actual cbg codes
#' @param stay_home_dt data table with columns `Date`, `origin_census_block_group`, and `pct_home` (E.g. derived from safegraph data) to use for social distancing/stay at home compliance
#' @param t0 start date of simulation
#' @param t.tot total time to run simulation (in days)
#' @param dt time step (defaults to 1/6, needs editing if otherwise)
#' @param day_of_week_fx function which takes t returns character of day of week (U,M,T,W,R,F,S)
#' @param SiP.start Date that shelter in place started
#' @param scl.close date that schools were closed
#' @param mask.start Date that mask mandate was introduced
#' @param mask.red Reduction in transmission probability from wearing mask
#' @param mask_fx fnction to assign probabilities of wearing a mask following mask mandate
#' @param quar_fx function to assign probability an agent quarantines following exposure, positive test, or symptom start
#' @param social_fx function to generate random sociality metrics
#' 
#' 
#' @return list with two objects: an epi curve with counts of agents in each state through time and a dataframe with infection reports for every new infection (e.g. entry created whenever agent transitions from S to E)
#' @export
  
covid_abm_v3_testing <- function(bta_hh, bta_scl, bta_work, bta_other,                 # transmission probabilities for different settings
                                 E0, Ip0, Ia0, Im0, Imh0, Ih0, R0, D0,                  # Starting conditions
                                 agents_dt, cbg_cdf_array, cbg_ids, stay_home_dt,            # Agents dt, mvmt matrix, stay_home function
                                 t0, t.tot, dt, day_of_week_fx, time_of_day_fx, SiP.start, scl.close, mask.start, mask_red, # Times and key dates
                                 mask_fx, quar_fx, social_fx){
  # Initial conditions
  e.seed <- E0     #Exposed
  ip.seed <- Ip0    #infected pre-symptomatic
  ia.seed <- Ia0    #infected asymptomatic
  im.seed <- Im0    #infected mildly symptomatic
  imh.seed <- Imh0   #infected mildly symptomatic, will become severe
  ih.seed <- Ih0    #infected severely symptomatic
  d.seed <- D0     #dead
  r.seed <- R0     #removed
  non.s <- e.seed + ip.seed + ia.seed + im.seed + imh.seed + ih.seed + d.seed + r.seed
  s.seed <- N - non.s
  
  agents <- agents_dt
  
# Initial infection allocated among workers
  init.Es <- sample(agents[work >0, id], e.seed)   
  init.Ips <- sample(agents[work >0, id], ip.seed)   
  init.Ias <- sample(agents[work >0, id], ia.seed)   
  init.Ims <- sample(agents[work >0, id], im.seed)   
  init.Imhs <- sample(agents[work >0, id], imh.seed)   
  init.Ihs <- sample(agents[work >0, id], ih.seed)   
  init.Ds <- sample(agents[work >0, id], d.seed)   
  init.Rs <- sample(agents[work >0, id], r.seed)   

  agents[id %in% init.Es, state:="E"]
  agents[id %in% init.Ips, state:="Ip"]
  agents[id %in% init.Ias, state:="Ia"]
  agents[id %in% init.Ims, state:="Im"]
  agents[id %in% init.Imhs, state:="Imh"]
  agents[id %in% init.Ihs, state:="Ih"]
  agents[id %in% init.Ds, state:="D"]
  agents[id %in% init.Rs, state:="R"]
  
# Keep track of everyone's infection status through time   
  epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
    epi_curve[1,] <- sum.inf(agents[,state])
  
# Keep record of infection events
  infection_reports <- list()

# Keep track of test data through time
  test_reports <- list()

   
# Update characteristics of initial infections  
  # Transition time
  agents[state %!in% c("S", "D", "R"), tnext:=t_til_nxt(state)]
  # State entering once transition time expires
  agents[state %!in% c("S", "D", "R"), nextstate:=next_state(state, age)]
  #Time initial infections occurred
  agents[state %!in% c("S", "D", "R"), t_infection:=dt]

# Add compliance and sociality metrics, start people with no known contacts
  agents[, mask := mask_fx(nrow(agents))] # Probability of wearing a mask
  agents[, q_prob := quar_fx(nrow(agents))] # Probability of quarantine/isolation given positive test
  agents[, sociality := social_fx(nrow(agents))] # Sociality metric
  agents[, contact := 0]

# Run simulation    
for(t in 2:(t.tot/dt)){
 print(t)
  
# Time step characteristics
  date_now <- t0+t*dt
  agents[, Date:=date_now]
  date_num <- as.numeric(floor(date_now-as.Date("2020-01-01")))
  day_week <- day_of_week_fx[t]
  time_day <- time_of_day_fx[t]
  scl.closed <- ifelse(date_now > scl.close, 1, 0)
  SiP.active <- ifelse(date_now > SiP.start, 1, 0)
  mask.mandate <- ifelse(date_now > mask.start, 1, 0)
  
# Advance transition times, time since infection,time since symptoms started
  agents[state %!in% c("S", "D", "R"), tnext:=tnext-dt]
  agents[state %!in% c("S", "D", "R"), t_infection:=t_infection+dt]
  agents[state %in% c("Im", "Imh"), t_symptoms:=t_symptoms+dt]

# Advance expired states to next state, determine new nextstate and time til next state
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), nextstate:=next_state(state, age)]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), tnext:=t_til_nxt(state)]

  print("Infections advanced")
  
# Implement testing in the morning for simplicity---------------
  if(time_day == "M"){
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
    tested_agents <- agents[id %in% testeds,]
    test_reports[[(t-1)]] <- tested_agents[,time:=t]
    
    print("Testing conducted")
    
    }
  }
  
  
# Simulate infection --------------------
  # If noone transmitting, skip. Assume agents inactive at night
  if(nrow(agents[state %!in% c("S", "E", "D", "R")])>0 & time_day != "N"){
    
# Quarantine ------------
  agents[t_symptoms > 0 | tested > 0 | contact > 0, quarantine:= quar_iso(q_prob, t_symptoms, tested, essential, SiP.active)]
    
# Determine locations ---------------
  # Get CBG-based stay at home compliance 
  home.today <- stay_home_dt[Date == as.character(date_now),]
  home.mean <- mean(home.today$pct_home)
  home_agents <- merge(agents, home.today, by.x = "cbg", by.y = "origin_census_block_group", all.x = TRUE)
  home_agents[is.na(pct_home), pct_home:=home.mean]
  agents[,pct_home:=home_agents[,pct_home]]

  # Find locations of those not deceased or in the hospital  
    # Agents that are in school
    agents[school > 0 & work < 0 & state %!in% c("Ih", "D"), 
           location:=sac_location( 
             state, tested, scl.closed, SiP.active, pct_home,
             time_day, day_week, age, sociality,
             residence, school, cbg)]

    # Agents that are workers only
    agents[school < 0 & work > 0 & state %!in% c("Ih", "D"), 
           location:=worker_location(state, tested,
                                     SiP.active, pct_home, time_day, day_week,
                                     age, essential, sociality,
                                     residence, work, cbg)]
    
    # Agents that are neither in school or are working
    agents[work < 0 & school < 0 & state %!in% c("Ih", "D"),
           location:=other_location(state, tested,
                                    SiP.active, pct_home, 
                                    age, sociality, residence_type,  
                                    residence, cbg)]
    
    #Agents that are quarantined stay at home
    agents[quarantine == 1, location:=residence]

    # Probabilistically move agents in community to other communities based on safegraph movement
    cbg_moves <- GetCBGVisit(cbg_cdf_array[,,date_num], cbg_ids, 
                             agents[state %!in% c("Ih", "D") & location == cbg, location])
    
    agents[state %!in% c("Ih", "D") & location == cbg, 
           location:=cbg_moves]
    
    # Smaller sub-locations (offices and classrooms) for agents in workplaces or schools
    agents[state %!in% c("Ih", "D") & !is.na(location),
           small_location:=location_small(location, work, school, office_id, class_id)]
  
  print("Locations resolved")
  
# Determine number of individuals and transmitting individuals by location  
  agents[, n_transmitting:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = location]
  agents[n_transmitting > 0, n_present:=.N, by = location]
  
  agents[n_transmitting > 0, n_transmitting_small:=sum(state %in% c("Ip", "Ia", "Im", "Imh")), by = small_location]
  agents[n_transmitting_small > 0, n_present_small:=.N, by = small_location]
  
# Determine FOI in locations where someone is transmitting
  agents[,wear.mask:=0]
  
  if(mask.mandate == 1){ # Only really care about people transmitting wearing a mask
    agents[state %in% c("Ip", "Ia", "Im", "Imh"), wear.mask:=rbinom(.N, 1, mask)]
  } 
  
  agents[state %in% c("Ip", "Ia", "Im", "Imh"), 
         trans_prob := get_trans_prob(location, residence, work, school,
                                      trans_res = trans.hh, trans_work = trans.work, trans_school = trans.school, trans_other = trans.other, 
                                      mask = wear.mask, mask_red = mask.red)]

  agents[n_transmitting > 0, FOI:=get_foi(n_transmitting, n_present, trans_prob), by = location]
  
# For agents sharing small location, overwrite lower FOI with small location FOI 
  agents[n_transmitting_small > 0, FOI:=get_foi(n_transmitting_small, n_present_small, trans_prob), by = small_location]
  
# Document KNOWN contacts in proportion to number of people in location where transmission was occuring
  agents[, n_symptomatic:=sum(state %in% c("Im", "Imh")), by = location]
  agents[, n_symptomatic_small:=sum(state %in% c("Im", "Imh")), by = small_location]
  agents[n_symptomatic > 0, contact_prob := 1/n_symptomatic]
  agents[n_symptomatic_small > 0, contact_prob := 1/n_symptomatic_small]
  
  agents[contact_prob > 0, contact := rbinom(.N, 1, contact_prob)]
  
# Generate infections, update their state, sample for their nextstate and time until reaching it
    agents[FOI > 0 & state == "S", infect:=foi_infect(FOI)]
    agents[infect == 1, state:="E"]
    agents[infect == 1, nextstate:=next_state(state, age)]
    agents[infect == 1, tnext:=t_til_nxt(state)]
    
# Store detailed infection info
    infection_reports[[t-1]] <- agents[infect == 1,]
    print("New infections generated")
    
# Reset infection & location columns
    agents[, c("location", "small_location",
               "n_transmitting", "n_transmitting_small", 
               "n_symptomatic", "n_symptomatic_small", "contact_prob",
               "n_present", "n_present_small", "pct_home", "wear.mask",
               "FOI", "infect"):=NULL]
  }
  

  
  epi_curve[t,] <- sum.inf(agents[,state])
  
  gc()  
  # On to the next one  
    
}    
  fin_out <- list()
  fin_out[["epi_curve"]] <- epi_curve
  fin_out[["infections"]] <- rbindlist(infection_reports,fill=TRUE)
  fin_out[["linelist_tests"]] <- rbindlist(test_reports,fill = TRUE)
  
  return(fin_out)
  
}
  