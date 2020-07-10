#' @title Simulate school-aged children who are also workers' location
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
#' @param essential is an essential worker?
#' @param res_id id of this individual's residence
#' @param scl_id id of this individual's school
#' @param work_id id of this individual's school
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), school (S), or community (C)
#' @export
#'        
sac_worker_location <- function(inf.state, tested,
                                scl, SiP, time_day, day_week,
                                comm_bracket, essential, 
                                res_id, scl_id, work_id, comm_id){
# Children workers who are sick or tested positive stay home 
  if(inf.state %in% c("Im", "Imh") | tested == 1){
    location = res_id
# Children workers are in school during the day if it's open and it's a weekday    
  } else if(scl == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    location = scl_id
# Children workers are randomly either still at school, in the community, at work or at home in the evening during the week if schools are open  
  } else if(scl == 0 & time_day %in% c("E", "M") & day_week %in% c("M", "T", "W", "R", "F")){
    location = c(scl_id, comm_id, res_id, work_id)[dqsample.int(4, 1)]
# Children workers are likely at home, maybe at work on school nights
  } else if(scl == 0 & time_day == "N" & day_week %in% c("M", "T", "W", "R", "F")){
    location = ifelse(dqrunif(1,0,1)>0.1, work_id, res_id)
# Weekend-like dynamics if school is closed, but SiP not in effect or if it's the weekend: children workers can be at home, at work or in the community during the morning, day and evening
  } else if((scl == 1 & SiP == 0 & time_day != "N") | (day_week %in% c("S", "U") & SiP == 0 & time_day != "N")){
    location = c(comm_id, res_id, work_id)[dqsample.int(3, 1)]
# Children workers are likely at home, maybe at work on weekend-like nights
  } else if((scl == 1 & SiP == 0 & time_day == "N") | (day_week %in% c("S", "U") & SiP == 0 & time_day == "N")){
    location = ifelse(dqrunif(1,0,1)>0.2, work_id, res_id)
# Children Workers location during SiP non-nights
  } else if(SiP == 1 & time_day != "N"){
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(10,
                                                                             5*essential,
                                                                             5-comm_bracket))]
# Children Workers location during SiP nights
  } else if(SiP == 1 & time_day == "N"){
     # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(10,
                                                                             2*essential,
                                                                             (5-comm_bracket)/2))]
  } else {
    location = NA
  }
  return(location)
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
#' @param comm_bracket income bracket of the community (census tract)
#' @param age age of person
#' @param res_id id of this individual's residence
#' @param scl_id id of this individual's school
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), school (S), or community (C)
#' @export
#'        
sac_location <- function(inf.state, tested,
                         scl, SiP, time_day, day_week,
                         comm_bracket, age, 
                         res_id, scl_id, comm_id){
# Children who are sick or tested positive stay home; children always at home at night and in the mornings  
  if(inf.state %in% c("Im", "Imh") | tested == 1 | time_day %in% c("N", "M")){
    location = res_id
# Children are in school during the day if it's open and it's a weekday    
  } else if(scl == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    location = scl_id
# Children are randomly either still at school, in the community or at home in the evening during the week if schools are open  
  } else if(scl == 0 & time_day == "E" & day_week %in% c("M", "T", "W", "R", "F")){
    location = c(scl_id, comm_id, res_id)[dqsample.int(3, 1)]
# Weekend-like dynamics if school is closed, but SiP not in effect or if it's the weekend: children can be at home or in the community during the day and evening
  } else if((scl == 1 & SiP == 0) | (day_week %in% c("S", "U") & SiP == 0)){
    location = c(comm_id, res_id)[dqsample.int(2, 1)]
# Shelter in place, sac most likely at home, but age and community dependent chance of being in community    
  } else if(SiP == 1){
    age_prob <- 1/(20-age) # More likely to be in community if older
    comm_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    location = ifelse(dqrunif(1,0,1)>(age_prob*comm_prob), comm_id, res_id)
  } else {
    location = NA
  }
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
                            age, kids, essential, 
                            comm_bracket, income_bracket, 
                            res_id, work_id, comm_id){
# Workers who are sick or tested positive stay home
  if(inf.state %in% c("Im", "Imh") | tested == 1){
    location = res_id
# Workers location (community, home, or work) during the week in the morning/evening
  } else if(SiP == 0 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
  # Higher probability of at home if older and/or with kids. Higher probability of at work if lower income job, higher probability of in community if lower income community  
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(age_prob+kids_prob,
                                                                             income_prob,
                                                                             comm_bracket))]
# Workers location (community, home, or work) during the week during the day
  } else if(SiP == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
  # Switch work and income probabilities  
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(income_prob,
                                                                             kids_prob+age_prob,
                                                                             comm_bracket))]
# Workers location (community, home, or work) during the week during the night
  } else if(SiP == 0 & time_day == "N" & day_week %in% c("M", "T", "W", "R", "F")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c((kids_prob+age_prob)*3,
                                                                             income_prob,
                                                                             comm_bracket))]
# Workers location (community, home, or work) during the weekend, non-night
  } else if(SiP == 0 & time_day != "N" & day_week %in% c("S", "U")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(kids_prob,
                                                                             income_prob,
                                                                             age_prob+comm_bracket))]
# Workers location (community, home, or work) during the weekend night
  } else if(SiP == 0 & time_day == "N" & day_week %in% c("S", "U")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c((age_prob+kids_prob)*2,
                                                                             income_prob,
                                                                             comm_bracket))]
# Workers location during SiP weekday non-nights
  } else if(SiP == 1 & time_day != "N" & day_week %in% c("M", "T", "W", "R", "F")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c((age_prob+kids_prob)*2,
                                                                             income_prob*essential,
                                                                             comm_bracket))]
# Workers location during SiP weekday nights
  } else if(SiP == 1 & time_day == "N" & day_week %in% c("M", "T", "W", "R", "F")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c((age_prob+kids_prob)*4,
                                                                             income_prob*essential,
                                                                             comm_bracket))]
# Workers location during SiP weekend non-nights
  } else if(SiP == 1 & time_day != "N" & day_week %in% c("S", "U")){
    # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c(age_prob+kids_prob,
                                                                             income_prob*essential,
                                                                             comm_bracket))]
# Workers location during SiP weekend nights
  } else if(SiP == 1 & time_day == "N" & day_week %in% c("S", "U")){
     # all these things roughly normalized to 4 to input into sampling weights
    age_prob <- age/20 # Older more likely to be at home
    kids_prob <- ifelse(kids>0, 4, 1) # Have kids more likely to be at home
    comm_prob <- 5-comm_bracket # lower income brackets, higher values
    income_prob <- 5-income_bracket # lower income brackets, higher values
    
    # Most likely at home
    location = c(res_id, work_id, comm_id)[wrswoR::sample_int_crank(3, 1, 
                                                                    prob = c((age_prob+kids_prob)*3,
                                                                             income_prob*essential,
                                                                             comm_bracket))]
  } else {
    location = NA
  }
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
                           age, res_type, comm_bracket, 
                           res_id, comm_id){
  # Workers who are sick or tested positive stay home
  if(inf.state %in% c("Im", "Imh") | tested == 1 | res_type %in% c("P", "N")){
    location = res_id
  } else if(SiP == 1 & res_type == "C"){
    location = NA # College dormitories irrelevant once SiP starts, assume students leave
  } else if(SiP == 0 & res_type == "C"){
    location = ifelse(dqrunif(1,0,1)>0.5, comm_id, res_id)
  } else if(SiP == 0 & res_type == "H" & time_day != "N"){
    age_prob <- ifelse(age <10, 0.05, ifelse(age>10&age<16, 0.1, 0.5))
    comm_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    location = ifelse(dqrunif(1,0,1)>(age_prob*comm_prob), comm_id, res_id)
  } else if(SiP == 0 & res_type == "H" & time_day == "N"){
    age_prob <- ifelse(age <10, 0, ifelse(age>10&age<16, 0.05, 0.1))
    comm_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    location = ifelse(dqrunif(1,0,1)>(age_prob*comm_prob), comm_id, res_id)
  } else if(SiP == 1 & res_type == "H" & time_day == "N"){
    age_prob <- ifelse(age <10, 0, ifelse(age>10&age<16, 0.01, 0.05))
    comm_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    location = ifelse(dqrunif(1,0,1)>(age_prob*comm_prob), comm_id, res_id)
  } else if(SiP == 1 & res_type == "H" & time_day != "N"){
    age_prob <- ifelse(age <10, 0.01, ifelse(age>10&age<16, 0.05, 0.1))
    comm_prob <- 1/comm_bracket # more likely to be in community if in lower income community (4 quartiles of income brackets)
    location = ifelse(dqrunif(1,0,1)>(age_prob*comm_prob), comm_id, res_id)
  } else {
    location = NA
  }
  return(location)
}


#' @title Quarantine symptomatics
#'  
#' @description Restrict contact of people with symptoms to their household with some compliance probability influenced by whether they're able to work from home
#' 
#' @param inf.stat Infection status vector
#' @param base.net Relationship matrix
#' @param q.prob Vector of length N corresponding to individuals' probability of quarantining given symptoms
#' 
#' @return Updated contact matrix with quarantining of symptomatics implemented  
#' @export
#'        

quarantine_symptomatics <- function(inf.stat, base.net, q.prob){
  out.mat <- base.net
  
  #Indices of individuals who are infected with symptoms  
  infection.indices <- which(inf.stat %in% c("Im", "Imh"))
  
  #Bernoulli trial for those with symptoms on whether they'll quarantine
  q10 <- sapply(q.prob[infection.indices], function(p){ rbinom(1,1,p) })
  
  #Indices of individuals who quarantine themselves
  quarantine.indices <- infection.indices[q10 == 1]
  
  #Change home relation status to "Q" for quarantined
  out.mat[which(out.mat[, quarantine.indices] == "H")] <- "Q"
  
  #Eliminate all other relationships
  out.mat[which(out.mat[, infection.indices] %!in% c("Q", "0"))] <- "0"

  return(out.mat)
}

#' @title Generate random edges in network
#'  
#' @description Function to generate random edges on top of network
#' 
#' @param start.net Network matrix to add random edges to
#' @param r.rate Rate at which individuals add random edges (similar to sociality)
#' @param n.prob individual probabilities of being involved in random edge generation
#' @param r.trans.rate Numeric indicating probability a random contact between a susceptible and infectious results in a new infection
#' 
#' @return Updated contact matrix with random edges added  
#' @export
#'        

add_r_edges <- function(start.net, r.rate, n.prob, r.trans.rate){
  new.net <- start.net
  #Number of random edges generated
    n.r.edges <- rbinom(1, nrow(start.net), r.rate)
  #Allocate random edges to network with individual probability correspondng to n.prob
    r.pairs <- matrix(sample(nrow(start.net), 2*n.r.edges, replace = TRUE, prob = n.prob), ncol=2)
  #If edge already exists in another capacity, keep it  
    pre.edges <- new.net[r.pairs]
    post.edges <- ifelse(pre.edges == 0, r.trans.rate, pre.edges)
    new.net[r.pairs] <- post.edges

  #Make symmetrical  
    new.net <- sym_mat(new.net)
  
  return(new.net)
}
