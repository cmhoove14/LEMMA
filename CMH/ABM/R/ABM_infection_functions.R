#' @title Next State
#' 
#' @description Take input infection state and age and return the next infection state and the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' @param age age of individual for probability of moving to different states
#' 
#' @return vector with two entries, the first is the next state, the second is the time to be spent in that state. Time should be converted to `numeric` for subsequent use
#' @export

next.state <- function(pre.status, age){
  if (pre.status == "E"){
    post.status <- "Ip"
    next.time <- t.presymp()
  } else if (pre.status == "Ip"){
    symp <- rbinom(1, 1, p.symp(age = age))
    if(symp == 0){
      post.status <- "Ia"
      next.time <- t.asymp()
    } else {
      sevsymp <- rbinom(1, 1, p.sevsymp(age = age))
      if(sevsymp == 1){
        post.status <- "Imh"
        next.time <- t.mtosev()
      } else {
        post.status <- "Im"
        next.time <- t.msymp()
      }
    }
  } else if (pre.status == "Imh"){
    post.status <- "Ih"
    next.time <- t.sevsymp()
  } else if (pre.status == "Ih"){
    died <- rbinom(1, 1, p.mort(age = age))
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

new.infection <- function(contact.mat, inf.ind, inf.multiplier){
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
