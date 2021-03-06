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

#' @title Community location
#'  
#' @description Function to randomly determine location if agent is not at work, home, or school. 10% chance of visiting neighboring community, 1% chance of visiting some random community.
#' 
#' @param comm community (neighborhood) of residence
#' @param nbhd_mat_list list of neighbors for all neighborhoods
#' 
#' @return location of agent if they choose to be in the community
#' @export
#' 

comm_loc <- function(comm, nbhd_mat_list){
  out <- comm
  nobs <- length(comm)
  cp <- dqrunif(nobs)

  # Determine if agent is in own neighborhood, neighboring neighborhood, or randomly chosen neighborhood with probabilities 89%, 10%, 1% respectively
  in_nbrs <- comm[cp > 0.89 & cp < 0.99]
  in_rand <- comm[cp > 0.99]

  # For each agent visiting neighboring neighborhood, sample from neighbors
  out[in_nbrs] <- GetNbhd(nbhd_mat_list$cdf[comm[in_nbrs], ], nbhd_mat_list$index[comm[in_nbrs], ])

  # For each agent visiting random neighborhood, sample randomly
  out[in_rand] <- comm[sample.int(n = length(in_rand), size = length(in_rand), replace = T)]
  
  return(out)
}

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
#' @param sociality relative sociality of agent
#' @param comm_bracket income bracket of the community (census tract)
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
                                sociality, comm_bracket, 
                                res_id, scl_id, work_id, comm_id){
# Children who are sick or tested positive stay home; children always at home at night and in the mornings  
  if(inf.state %in% c("Im", "Imh") | tested == 1 | time_day %in% c("N", "M")){
    location = res_id
  } else if(SiP == 1){
  # Probability of following SiP function of sociality and income quartile 
    sip.flw <- as.numeric(dqrunif(1,0,1) > sociality/comm_bracket)
    location = ifelse(sip.flw == 1, res_id, comm_id)
# Children workers are in school during the day if it's open and it's a weekday    
  } else if(scl == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    location = scl_id
# Children workers are randomly either still at school, in the community, at work or at home in the evenings and mornings during the week if schools are open  
  } else if(scl == 0 & time_day %in% c("E", "M") & day_week %in% c("M", "T", "W", "R", "F")){
    location = c(scl_id, comm_id, res_id, work_id)[dqsample.int(4, 1)]
# Weekend-like dynamics if school is closed, but SiP not in effect or if it's the weekend: children workers can be at home, at work or in the community during the morning, day and evening
  } else if((scl == 1 & SiP == 0) | (day_week %in% c("S", "U") & SiP == 0)){
    location = c(comm_id, res_id, work_id)[dqsample.int(3, 1)]
  } else {
    location = NA
  }
  # TODO: Implement variability in community location with comm_loc function above
  # if(location == comm_id){location = comm_loc(comm_id)}
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
