#' @title COVID ABM
#' 
#' @description Simulate COVID transmission across a network of N individuals
#' 
#' @param t.tot total time to run simulation
#' @param dt time step of simulation to run
#' @param pop.ages vector corresponding to age of each individual
#' @param q.probs vector of individual probabilities of quarantining
#' @param n.probs vector of individual probabilities of being in random edge generation
#' @param inf.mat infection status matrix with column 1 filled with initial infection status, N rows, and t.tot/dt columns
#' @param transition.mat transition times matrix (time until moving into next state) with column 1 filled with initial times remaining in starting states, N rows, and t.tot/dt columns
#' @param net.mat contact network array with dimensions NxNxt.tot/dt such that each slice is a symmetric matrix representing contact between 
#' 
#' @return Matrix with columns corresponding to each infection state and rows filled with number people in each state at each time over which the simulation was run
#' @export
#' 

covid_abm <- function(t.tot, dt, 
                      pop.ages, q.probs, n.probs,
                      inf.mat, transition.mat, net.mat){

# Run simulation
  for(t in 2:(t.tot/dt)){

# Advance transition times
    t.til.nxt[,t] <- t.til.nxt[,(t-1)]-dt
  
  # Advance expired states to next state 
    nexts <- which(t.til.nxt[,t] < 0) # states that expired
    if(length(nexts >0)){
      next.mat <- t(mapply(next_state, inf.mat[nexts,(t-1)], pop.ages[nexts])) # function returns new states and time spent in new state
      inf.mat[nexts,t] <- next.mat[,1] # update infection status matrix
      t.til.nxt[nexts,t] <- as.numeric(next.mat[,2]) # update waiting time matrix
    }
    
# Update network based on epi advances and interventions
  #Quarantine symptomatics   
  t.net <- quarantine_symptomatics(inf.mat[,t], relation.mat, q.probs)  
  
  # Eliminate school connections if schools closed and increase hh contacts
    if(t.sc <= (t*dt) & (t*dt) <= t.sc.end){
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh*trans.hh.sc
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob.sc, n.probs, trans.other)
      
  # Eliminate work and school connections if schools closed and increase hh contacts
    } else if(t.sip <= (t*dt) & (t*dt) <= t.sip.end){
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh*trans.hh.sip
      t.net[which(t.net == "W")] <- 0
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob.sip, n.probs, trans.other)
      
  # If not in shelter in place or school closure, network remains the same plus random connections    
    } else {
      t.net[which(t.net %in% c("H", "Q"))] <- trans.hh
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- trans.school
    # Add random network component to network
      t.net <- add_r_edges(t.net, r.net.prob, n.probs, trans.other)
    } 
    
  #store resulting network in network array
    class(t.net) <- "numeric"
    net.mat[,,t] <- t.net

#Generate new infections across network 
  # Indices of different types of transmitters
    a.p.transmitters <- which(inf.mat[,t] %in% c("Ip", "Ia"))  
    m.s.transmitters <- which(inf.mat[,t] %in% c("Im", "Imh"))  
   
  # Bernouli trial across all rows times transmitter columns where p(infection)~contact
    # Transmission from pre/asymtpomatic infections with reduction in transmissibility 
      if(length(a.p.transmitters) > 0){
        new.Es1 <- new_infection(net.mat[,,t], a.p.transmitters, trans.asymp)
      } else {
        new.Es1 <- rep(0, N)
      }

    # Transmissions from mild/severely symptomatic individuals
      if(length(m.s.transmitters) > 0){
        new.Es2 <- new_infection(net.mat[,,t], m.s.transmitters, 1)
      } else {
        new.Es2 <- rep(0, N)
      }

  # Index of individuls who enter E compartment (new infections)
    new.Es <- which(new.Es1+new.Es2 > 0 & inf.mat[,(t-1)] == "S")
    
  # Update infection status matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix for new Es from sample of latent period dist'n
    t.til.nxt[new.Es,t] <- sapply(1:length(new.Es), t_latent)
  
  # Anyone who hasn't changed infection state remains the same
    sames <- which(is.na(inf.mat[,t]))
    inf.mat[sames,t] <- inf.mat[sames,(t-1)]
      
  # On to the next one  
  }
  # Return infection status and network matrices
  return(list("infection" = inf.mat,
              "network" = net.mat))     

}