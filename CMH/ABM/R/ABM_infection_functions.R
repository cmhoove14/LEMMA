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
  (I/N)*bta
}

#' @title (DEPRECATED) Person's FOI in small areas
#'  
#' @description Determine FOI from locations with infectious individuals in small areas: classrooms, offices, homes
#' 
#' @param location  Location of this person
#' @param n_transmitting number f infectious people transmitting
#' @param res_size population of this person's residence
#' @param office_size population of this person's office
#' @param class_size population of this person's class
#' @param trans_prob transmission probability
#'  
#' @return FOI for this person
#' @export
#'        
# TODO: Inform this with age-stratified contact matrices
get_foi_small_area <- function(location, n_transmitting, res_size, office_size, class_size, trans_prob){
  l_type <- substring(location, 1, 1)
  l_lngth <- nchar(location)

  l_pop <- res_size
  l_pop[l_type=="4" & l_lngth > 5] <- class_size[l_type=="4" & l_lngth > 5]
  l_pop[l_type=="5" & l_lngth > 5] <- office_size[l_type=="5" & l_lngth > 5]
  
  FOI <- (n_transmitting/l_pop)*trans_prob
  
  return(FOI)
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

#' @title Generate Infections
#' 
#' @description Generate new infections given contact matrix and indices of people who are infectious. Restricts the contact matrix (`contact.mat`) to include only those columns corresponding to infectious people based on `inf.ind` Then goes through each row and performs a bernoulli trial based on the transmission probability in each cell Equivalent to a chance of infection for every contact if contact exists (else the cell is 0)Returns a vector of row indicies corresponding to new infections to enter the E compartment Infection multiplier allows alteration of the infection probability, e.g. if contacts are with asymptomatics and want to model reduced infectiousness
#' 
#' @param contact.mat matrix representing contact network where entries are transmission probabilities
#' @param inf.ind indices of individuals who are infectious. Matrix will be subset to these columns to simulate infectious contacts
#' @param inf.multiplier multiplier to apply to the infection probabilities in the matrix e.g. to simulate reduction in infectiousness among asymptomatic individuals
#' 
#' @return vector of indicies of new infections (e.g. people who have moved from S to E)
#' @export 
#' 

new_infection <- function(contact.mat, inf.ind, inf.multiplier){
  #drop=F allows apply to proceed even if length(inf.ind=1) since apply expects a matrix 
  #across each row, apply a bernoulli trial to all columns corresponding to infectious individuals
  init.Es <- t(apply(contact.mat[,inf.ind,drop=F], 1, function(e){ 
    sapply(e, function(beta) rbinom(1, 1, (1-exp(-beta*inf.multiplier)))) 
  }))
  # Make sure summing over rows since if inf.ind=1, above returns a 1xN matrix rather than Nx1 rows
  if(dim(init.Es)[1] == 1){
    new.Es <- colSums(init.Es)
  } else {
    new.Es <- rowSums(init.Es)
  }
                   
  return(new.Es)
}

#' @title ABM Test
#' 
#' @description Test individuals with different sampling weights. TODO: Implement testing uncertainty, esp. with regards to time since being infected
#' 
#' @param inf.states vector of individual infection states
#' @param test.prior vector of prior testing statuses
#' @param test.probs vector of sampling weights corresponding to S, E, Ip, Ia, Im, Imh, Ih, and R classes, in that order
#' @param n.tests numeric of number of tests conducted
#' 
#' @return vector of NAs and Ts with Ts corresponding to individuals who tested positive
#' @export
#' 

test_folks <- function(inf.states, test.prior, test.probs, n.tests){
  weights <- as.numeric(replace(inf.states, 
                                c("S", "E", "Ip", "Ia", "Im", "Imh", "Ih", "R"), 
                                test.probs)[inf.states])

  weights[!is.na(test.prior)] <- 0
  weights[is.na(weights)] <- 0
  
  tested <- wrswoR::sample_int_crank(length(inf.states),
                                     n.tests,
                                     weights)
  
  pos.tests <- tested[which(inf.states[tested] %in% c("Ip", "Ia", "Im", "Imh", "Ih"))]
  
  test.prior[pos.tests] <- paste0(inf.states[pos.tests], "T")
  
  return(test.prior)
}
