#' @title Agent BAsed Model simulation
#'  
#' @description Simulate agents based model of COVID19 transmission from a population of synthetic agents
#' 
#' @param bta beta parameter = transmission probability
#' @param E0 initial number of exposed agents, to be allocated among working population
#' @param Ip0 initial number of pre-symptomatic agents, to be allocated among working population
#' @param Ia0 initial number of asymptomatic agents, to be allocated among working population
#' @param Im0 initial number of mildly symptomatic agents, to be allocated among working population
#' @param Imh0 initial number of mildly symptomatic-pre-hospitalized agents, to be allocated among working population
#' @param Ih0 initial number of hospitalized agents, to be allocated among working population
#' @param R0 initial number of recovered agents, to be allocated among working population
#' @param D0 initial number of deceased agents, to be allocated among working population
#' @param agents data.table of synthetic agents, must contain columns `age`, `race`, `state`, `nextstate`, `tnext`, `t_infection`, `test_prob`, `tested`, `t_since_test`, `q_prob`, `sociality`, `residence`, `residence_type`, `res_inf`, `income`, `work`, `school`, `nbhd`, `office_id`, `class_id`, `essential`, `comm_bracket`, and `income_bracket` 
#' @param nbhd_mat_list list derived from neighborhoods matrix with each neighborhoods neighbors. See `comm_loc` and `GetNbhd` functions
#' @param t.tot total time period in days to run simulation
#' @param dt time step of simulation, default is 4/24. Be sure to match day_of_week_fx and time_of_day_fx appropriately since total time steps will equal `t.tot/dt`
#' @param day_of_week_fx function returning the day of week of at time step `t`
#' @param time_of_day_fx function returning the time of day at time step `t`   
#' @param SiP_fx function returning shelter-in-place status of population at `t`   
#' @param scl_fx function returning school closure status at time step `t`   
#' @param test_fx function returning the number of tests conducted at time step `t`   
#' 
#' @return 
#' @export
#' 

covid_abm <- function(bta, E0, Ip0, Ia0, Im0, Imh0, Ih0, R0, D0,
                      agents, nbhd_mat_list,
                      t.tot, dt, day_of_week_fx, time_of_day_fx, SiP_fx, scl_fx,
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