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
#' @description Function to summarize infection matric through time to give number of individuals in each state at each time 
#' 
#' @param inf.mat infections status matrix from `ABM` run
#' 
#' @return matrix with columns corresponding to each infection state and rows filled with number people in each state at each time
#' @export

sum_inf_mat <- function(inf.mat){
  t.sim <- 1:ncol(inf.mat)
  out <- t(apply(inf.mat,2,function(x){
    S = sum(x == "S")
    E = sum(x == "E")
    Ip = sum(x == "Ip")
    Ia = sum(x == "Ia")
    Im = sum(x == "Im")
    Imh = sum(x == "Imh")
    Ih = sum(x == "Ih")
    D = sum(x == "D")
    R = sum(x == "R")
    
    return(c("S" = S, 
             "E" = E, 
             "Ip" = Ip,
             "Ia" = Ia, 
             "Im" = Im,
             "Imh" = Imh,
             "Ih" = Ih,
             "D" = D,
             "R" = R))
  }))
  
  return(cbind(t.sim, out))
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
