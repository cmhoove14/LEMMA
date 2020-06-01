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
