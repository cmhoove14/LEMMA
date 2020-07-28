#' @title Network degree
#' 
#' @description get distribution of individual degrees from input network matrix
#' 
#' @param net.met matrix representing contact network
#' 
#' @return vector of individual degrees (number of contacts)
#' @export

get_degree <- function(net.mat){
  net.mat[which(net.mat>0)] <- 1
  rowSums(net.mat)
}

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


#' @title Symmetric matrix
#' 
#' @description make an unsymmetrical matrix symmetrical  
#' 
#' @param mat input matrix
#' 
#' @return symmetric matrix
#' @export

sym_mat <- function(mat){
  mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
  return(mat)
}

#' @title Not in 
#' 
#' @description Opposite of `%in%`
#' 
#' @return function for opposite of `%in%``
#' @export

`%!in%` <- Negate(`%in%`)
