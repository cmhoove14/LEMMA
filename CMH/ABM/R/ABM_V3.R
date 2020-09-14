#------------------
# Helpers/Utils
#------------------
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

#-------------------------
# Infection functions
#-------------------------

#-------------------------
# Network/movement functions
#-------------------------
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
#' @param comm_bracket income bracket of the community (census tract)
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
                         scl, SiP, time_day, day_week,
                         comm_bracket, age, sociality,
                         res_id, scl_id, comm_id){
  
  n <- length(inf.state)
  
# Children always at home at night and in the mornings  
  if(time_day %in% c("N", "M")){
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
    inc_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    comm_prob <- age_prob*inc_prob*sociality
    
    probs <- cbind(1-comm_prob, comm_prob)
    
  } else {
    stop("Situation not recognized for School agent")
  }
  
  # School agents who are sick or tested positive stay home
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
#' @param comm_bracket income bracket of the community (census tract)
#' @param income_bracket income bracket of the community (census tract)
#' @param res_id id of this individual's residence
#' @param work_id id of this individual's workplace
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), or community (C)
#' @export
#'        
worker_location <- function(inf.state, tested,
                            SiP, time_day, day_week,
                            age, kids, essential, sociality,
                            comm_bracket, income_bracket, 
                            res_id, work_id, comm_id){
  
    n <- length(inf.state)

  # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
  
# Workers location (community, home, or work) during the week in the morning/evening
  if(SiP == 0 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(age_prob+kids_prob,
                  comm_bracket*sociality,
                  income_prob)
    
# Workers location (community, home, or work) during the week during the day
  } else if(SiP == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(income_prob,
                 comm_bracket*sociality,
                 kids_prob+age_prob)
    
# Workers location (community, home, or work) during the week during the night
  } else if(SiP == 0 & time_day == "N" & day_week %in% c("M", "T", "W", "R", "F")){

    probs = cbind((kids_prob+age_prob)*3,
                  comm_bracket*sociality,
                  income_prob)
    
# Workers location (community, home, or work) during the weekend, non-night
  } else if(SiP == 0 & time_day != "N" & day_week %in% c("S", "U")){

    probs = cbind(kids_prob,
                  (age_prob+comm_bracket)*sociality,
                  income_prob)
    
# Workers location (community, home, or work) during the weekend night
  } else if(SiP == 0 & time_day == "N" & day_week %in% c("S", "U")){

    probs = cbind((age_prob+kids_prob)*2,
                  comm_bracket*sociality,
                  income_prob)
    
# Workers location during SiP weekday non-nights
  } else if(SiP == 1 & time_day != "N" & day_week %in% c("M", "T", "W", "R", "F")){

    probs = cbind((age_prob+kids_prob)*2,
                  comm_bracket*sociality,
                  income_prob*essential)
    
# Workers location during SiP weekday nights
  } else if(SiP == 1 & time_day == "N" & day_week %in% c("M", "T", "W", "R", "F")){

    probs = cbind((age_prob+kids_prob)*4,
                 comm_bracket*sociality,
                 income_prob*essential)
    
# Workers location during SiP weekend non-nights
  } else if(SiP == 1 & time_day != "N" & day_week %in% c("S", "U")){

    probs = cbind(age_prob+kids_prob,
                  comm_bracket*sociality,
                  income_prob*essential)
    
# Workers location during SiP weekend nights
  } else if(SiP == 1 & time_day == "N" & day_week %in% c("S", "U")){
    probs = cbind((age_prob+kids_prob)*3,
                  comm_bracket*sociality,
                  income_prob*essential)
  } else {
    stop("Worker situation not recognized")
  }
    
# Determine locations    
# Workers who are sick or tested positive stay home
  at.home <- inf.state %in% c("Im", "Imh") | tested == 1
  location <- rep(NA_integer_, n)
  location[at.home] <- res_id[at.home]
  
  t_prob <- rowSums(probs)
  norm_probs <- probs/t_prob
    
  location[!at.home] <- scl_wrk_loc(res_id[!at.home], comm_id[!at.home], work_id[!at.home], norm_probs[!at.home, 1], norm_probs[!at.home, 2])
  
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
#' @param comm_bracket income bracket of the community (census tract)
#' @param res_id id of this individual's residence
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), school (S), or community (C)
#' @export
#'        
other_location <- function(inf.state, tested,
                           SiP, time_day, 
                           age, sociality, res_type, comm_bracket, 
                           res_id, comm_id){
  n <- length(inf.state)
  age_prob <- ifelse(age <10, 0.9, ifelse(age>10&age<16, 0.5, 0.1))
  comm_prob <- 1/(5-comm_bracket) # more likely to be in community if in lower income community (4 quartiles of income brackets)
  soc_prob <- 1-sociality # More social = less likely to be at home

  if(SiP == 0 & time_day != "N"){
    p.home = age_prob*comm_prob*soc_prob
    
  } else if(SiP == 0 & time_day == "N"){
    p.home = age_prob*comm_prob*soc_prob*2
    
  } else if(SiP == 1 & time_day == "N"){
    p.home = 1
    
  } else if(SiP == 1 & time_day != "N"){
    p.home = age_prob*comm_prob*soc_prob*2
    
  } else {
    stop("Situation not recognized for non-worker, non-school agent")
  }
    
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


#' @title Community location utility function
#'  
#' @description Draws random uniform variable between 0 and 1, looks in CDF matrix to determine corresponding probability of visiting CBG, finds corresponding CBG visited in index matrix
#' 
#' @param mat_cdf cumulative probability matrix with rows corresponding to CDF of individual residing in row and visiting column
#' @param mat_index index of cbg to which individual residing in row might move to
#' 
#' @return 
#' @export
#' 

GetNbhd <- function(mat_cdf, mat_index) {
  n <- nrow(mat_cdf)
  r <- dqrng::dqrunif(n)
  index <- max.col(r < mat_cdf, "first")
  mat_index[cbind(1:n, index)]
}

#-------------------------
# Testing functions
#-------------------------


#-------------------------
# Main model sim function
#-------------------------
covid_abm_v3 <- function(bta, E0, Ip0, Ia0, Im0, Imh0, Ih0, R0, D0,                  # Starting conditions
                         agents, nbhd_mat_list,                                      # Agents dt and mvmt matrix
                         t.tot, dt, day_of_week_fx, time_of_day_fx, SiP_fx, scl_fx,  # Times and key dates
                         test_fx){
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
    epi_curve[1,] <- LEMMAABM:::sum.inf(agents[,state])
  
# Keep track of test data through time
  test_reports <- list()

# Keep track of state transitions through time
  transition_reports <- list()
  
# Keep record of infection events
  infection_reports <- list()
   
# Update characteristics of initial infections  
  # Transition time
  agents[state %!in% c("S", "D", "R"), tnext:=LEMMAABM::t_til_nxt(state)]
  # State entering once transition time expires
  agents[state %!in% c("S", "D", "R"), nextstate:=LEMMAABM::next_state(state, age)]
  #Time initial infections occurred
  agents[state %!in% c("S", "D", "R"), t_infection:=dt]
  agents[, t_since_test:=0]

# Run simulation    
for(t in 2:(t.tot/dt)){
# print(t)
# Time step characteristics
  day_week <- day_of_week_fx[t]
  time_day <- time_of_day_fx[t]
  sip.active <- SiP_fx[t]
  scl.closed <- scl_fx[t]
  
# Advance transition times, time since infection, time since tested last
  agents[, time:=t]
  agents[state %!in% c("S", "D", "R"), tnext:=tnext-dt]
  agents[state %!in% c("S", "D", "R"), t_infection:=t_infection+dt]
  agents[, t_since_test:=t_since_test+dt]
  
# Save record of state transitions
  transition_reports[[t-1]] <- agents[tnext < 0 & state %!in% c("S", "D", "R"), .(id,age,race,state,nextstate,tested,test_public,test_private,
                                                                                  residence,lat,lon,work,school,nbhd,ct,zip,time)]

  
# Advance expired states to next state, determine new nextstate and time til next state
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), nextstate:=LEMMAABM::next_state(state, age)]
  agents[tnext < 0 & state %!in% c("S", "D", "R"), tnext:=LEMMAABM::t_til_nxt(state)]

# print("Infections advanced")
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
    agents[id %in% testeds, tested:=LEMMAABM::test_sens(state, t_infection)]
    agents[id %in% testeds, t_since_test:=0]
    test_reports[[(t-1)]] <- agents[id %in% testeds, .(id,age,race,state,nextstate,tested,test_public,test_private,
                                                       residence,lat,lon,work,school,nbhd,ct,zip,time)]
    
# print("Testing conducted")
    
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
             residence, school, nbhd)]
  
  # Agents that are workers only
  agents[school < 0 & work > 0 & state %!in% c("Ih", "D"), 
         location:=LEMMAABM::worker_location(state, tested,
                                             sip.active, time_day, day_week,
                                             age, res_kids, essential, sociality,
                                             comm_bracket, income_bracket, 
                                             residence, work, nbhd)]
  
  # Agents that are neither in school or are working
  agents[work < 0 & school < 0 & state %!in% c("Ih", "D"),
         location:=LEMMAABM::other_location(state, tested,
                                            sip.active, time_day, 
                                            age, sociality, residence_type, comm_bracket, 
                                            residence, nbhd)]
  
  agents[state %!in% c("Ih", "D") & location == nbhd & !is.na(location), 
         location:=comm_loc(location, nbhd_mat_list)]
  
  # Smaller sub-locations (offices and classrooms) for agents in workplaces or schools
  agents[state %!in% c("Ih", "D") & !is.na(location),
         small_location:=LEMMAABM::location_small(location, work, school, office_id, class_id)]
  
# print("Locations resolved")
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
    
# Save record of new infection events
    infection_reports[[t-1]] <- agents[infect == 1, .(id,age,race,infect,
                                                      location,small_location, n_transmitting, n_transmitting_small, FOI,
                                                      residence,lat,lon,work,school,nbhd,ct,zip,time)]
    
# Reset infection & location columns
    agents[, c("location", "small_location",
               "n_transmitting", "n_transmitting_small", 
               "n_present", "n_present_small",
               "FOI", "infect"):=NA_real_]
    
    #print("New infections generated")
  }
  
  epi_curve[t,] <- LEMMAABM:::sum.inf(agents[,state])
  # On to the next one  
}    
  fin_out <- list()
  fin_out[["epi_curve"]] <- epi_curve
  fin_out[["linelist_tests"]] <- rbindlist(test_reports,fill = TRUE)
  fin_out[["transitions"]] <- rbindlist(transition_reports,fill=TRUE)
  fin_out[["infections"]] <- rbindlist(infection_reports,fill=TRUE)
  
  return(fin_out)
  
}