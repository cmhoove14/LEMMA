#' @title latent period
#'  
#' @description Draw from distribution of proposed latent periods. Used in `next.state`
#' 
#' @param shape.l Shape of gamma distribution
#' @param scale.l Scale of gamma distribution
#' 
#' @return numeric of time spent in latent period (E)
#' @export
#'        

t_latent <- function(shape.l=11, scale.l=2){
  rgamma(1, shape.l, scale.l)
} 
      
#' @title Presymptomatic period
#'  
#' @description Draw from distribution of proposed presymptomatic periods (basically incubation-latent periods) to model presymptomatic transmission. Used in `next.state`
#' 
#' @param shape Shape of gamma distribution
#' @param scale Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_presymp <- function(shape.ip=1,scale.ip=2){
  rgamma(1, shape.ip, scale.ip)
} 

#' @title Asymptomatic period
#'  
#' @description Draw from distribution of proposed asymptomatic period. Used in `next.state`
#' 
#' @param shape.ia Shape of gamma distribution
#' @param scale.ia Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_asymp <- function(shape.ia = 6, scale.ia = 1){
  rgamma(1, shape.ia, scale.ia)
} 

#' @title Mild infection duration
#'  
#' @description Draw from distribution of proposed length spent with minor symptoms. Used in `next.state`
#' 
#' @param exp.im parameter of exponential distribution for time spent with mild symptoms
#' 
#' @return numeric of time spent in mildly symptomatic (Im)
#' @export
#'        
t_msymp <- function(exp.im = 1/9) {
  rexp(1, exp.im)
}

#' @title Symptom onset to hospitalization
#'  
#' @description Draw from distribution of proposed time spent with minor symptoms before being hospitalized for severe infections (i.e. those with B(1,p.sevsymp==1)). Used in `next.state`
#' 
#' @param shape.imh shape of gamma distn
#' @param scale.imh scale of gamma distn
#' 
#' @return numeric of time spent mildly symptomatic before hospitalization (Imh)
#' @export
#'        
t_mtosev <- function(shape.imh = 7, scale.imh = 2){
  rgamma(1, shape.imh, scale.imh)
}

#' @title time spent severely symptomatic (in hospital)
#'  
#' @description Draw from distribution of proposed time spent with severe symptoms before dying or being discharged. Used in `next.state`
#' 
#' @param shape.ih shape of gamma distn
#' @param scale.ih scale of gamma distn
#' 
#' @return numeric of time spent hospitalized (Ih)
#' @export
#'        
t_sevsymp <- function(shape.ih = 3, scale.ih = 0.25){
  rgamma(1, shape.ih, scale.ih)
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
p_symp <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.03,
                                          age %in% c(10:19) ~ 0.05,
                                          age %in% c(20:29) ~ 0.3,
                                          age %in% c(30:39) ~ 0.55,
                                          age %in% c(40:49) ~ 0.61,
                                          age %in% c(50:59) ~ 0.75,
                                          age %in% c(60:69) ~ 0.89,
                                          age %in% c(70:79) ~ 0.9,
                                          age >= 80 ~ 0.9)}

#' @title Probability of severely symptomatic (will be hospitalized) infection
#'  
#' @description Age-dependent probabilities of having severe symptoms. Used in `next.state`
#' 
#' @param age age of person
#' 
#' @return numeric of probability of having severe symptoms.
#' @export
#'
p_sevsymp <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.004,
                                             age %in% c(10:19) ~ 0.004,
                                             age %in% c(20:29) ~ 0.01,
                                             age %in% c(30:39) ~ 0.04,
                                             age %in% c(40:49) ~ 0.09,
                                             age %in% c(50:59) ~ 0.13,
                                             age %in% c(60:69) ~ 0.19,
                                             age %in% c(70:79) ~ 0.2,
                                             age >= 80 ~ 0.25)}

#' @title Probability of death
#'  
#' @description Age-dependent probabilities of dying given hospitalization (Ih) Used in `next.state`; from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified
#' 
#' @param age age of person
#' 
#' @return numeric of probability of dying.
#' @export
#'
p_mort <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.01,
                                          age %in% c(10:19) ~ 0.0205,
                                          age %in% c(20:29) ~ 0.031,
                                          age %in% c(30:39) ~ 0.0475,
                                          age %in% c(40:49) ~ 0.0785,
                                          age %in% c(50:59) ~ 0.12,
                                          age %in% c(60:69) ~ 0.186,
                                          age %in% c(70:79) ~ 0.3,
                                          age >= 80 ~ 0.45)}

#' @title Next State
#' 
#' @description Take input infection state and age and return the next infection state and the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' @param age age of individual for probability of moving to different states
#' 
#' @return vector with two entries, the first is the next state, the second is the time to be spent in that state. Time should be converted to `numeric` for subsequent use
#' @export

next_state <- function(pre.status, age){
  if (pre.status == "E"){
    post.status <- "Ip"
    next.time <- t_presymp()
  } else if (pre.status == "Ip"){
    symp <- rbinom(1, 1, p_symp(age = age))
    if(symp == 0){
      post.status <- "Ia"
      next.time <- t_asymp()
    } else {
      sevsymp <- rbinom(1, 1, p_sevsymp(age = age))
      if(sevsymp == 1){
        post.status <- "Imh"
        next.time <- t_mtosev()
      } else {
        post.status <- "Im"
        next.time <- t_msymp()
      }
    }
  } else if (pre.status == "Imh"){
    post.status <- "Ih"
    next.time <- t_sevsymp()
  } else if (pre.status == "Ih"){
    died <- rbinom(1, 1, p_mort(age = age))
    if(died == 1){
      post.status <- "D"
      next.time <- NA_real_
    } else {
      post.status <- "R"
      next.time <- NA_real_
    }
  } else if (pre.status == "Im"){
    post.status <- "R"
    next.time <- NA_real_
  } else if (pre.status == "Ia"){
    post.status <- "R"
    next.time <- NA_real_
  }  
  return(c(post.status, next.time))
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
