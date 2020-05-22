# ---------------------------------------------------------
#   COVID ABM
#   Chris Hoover (choover@berkeley.edu)
#   May 2020
# ---------------------------------------------------------

# FUNCTIONS (to be moved to R package infrastructure)
# ---------------------------------------------------------

# Function to handle new state transitions
# Basically a bunch of ifelse statements to advance from one state to the next once time in one state expires
next.state <- function(pre.status, age){
  if (pre.status == "E"){
    post.status <- "Ip"
    next.time <- t.presymp()
  } else if (pre.status == "Ip"){
    asymp <- rbinom(1, 1, p.asymp(age = age))
    if(asymp == 1){
      post.status <- "Ia"
      next.time <- t.asymp()
    } else {
      sevsymp <- p.sevsymp(age = age)
      if(sevsymp == 1){
        post.status <- "Ims"
        next.time <- t.mtosev()
      } else {
        post.status <- "Im"
        next.time <- t.msymp()
      }
    }
  } else if (pre.status == "Ia"){
    post.status <- "R"
    next.time <- NA_real_
  } else if (pre.status == "Ims"){
    post.status <- "Is"
    next.time <- t.sevsymp()
  } else if (pre.status == "Im"){
    post.status <- "R"
    next.time <- NA_real_
  } else if (pre.status == "Is"){
    post.status <- "R"
    next.time <- NA_real_
  }
  return(c(post.status, next.time))
}

#Function to make matrix symmetrical
sym.mat <- function(mat){
  mat[lower.tri(mat)] = t(mat)[lower.tri(mat)]
  return(mat)
}

# Function to generate random edges on top of network
#TODO: introduce heterogeneity into who generates random edges
add.r.edges <- function(start.net, r.rate, r.trans.rate){
  new.net <- start.net
  #Number of random edges generated
    n.r.edges <- rbinom(1, nrow(start.net), r.rate)
  #Allocate random edges to network. Could add probs argument here to make some individuals more likely to have random contacts
    r.pairs <- matrix(sample(nrow(start.net), 2*n.r.edges, replace = TRUE), ncol=2)
  #If edge already exists in another capacity, keep it  
    pre.edges <- new.net[r.pairs]
    post.edges <- ifelse(pre.edges == 0, r.trans.rate, pre.edges)
    new.net[r.pairs] <- post.edges

  #Make symmetrical  
    new.net <- sym.mat(new.net)
  
  return(new.net)
}

new.infection <- function(){
  
}



# ---------------------------------------------------------
# SETUP
# TODO: incorporate testing frequency/teting regime
# TODO: get netrowk parameters for pre/post intervention, incorporate into network dynamics
# ---------------------------------------------------------

# Number of people, time frame, and time step
  N <- 1000
  t.tot <- 200
  dt <- 1
  
# Intervention parameters 
  # Timing
    t.sc <- 20       # Time at which schools are closed
    t.sip <- 40      # Time at which shelter in place occurs
    t.sc.end <- t.tot+1  # Time of return to schools
    t.sip.end <- t.tot+1  # Time of shelter in place lift

  # Network parameters
    trans.hh.sc <- 1.2     # increase in hh transmission when schools are closed
    trans.hh.sip <- 1.3    # increase in hh transmission when sheltered in place
    r.net.prob <- 0.5      # Probability of random interaction pre-intervention
    r.net.prob.sip <- 0.05 # Probability of random interaction while in shelter in place
    
# Initial conditions
  e.seed <- 10    #Exposed
  ip.seed <- 0    #infected pre-symptomatic
  ia.seed <- 0    #infected asymptomatic
  im.seed <- 0    #infected mildly symptomatic
  ims.seed <- 0   #infected mildly symptomatic, will become severe
  is.seed <- 0    #infected severely symptomatic
  r.seed <- 0     #removed
  s.seed <- N - e.seed - ip.seed - ia.seed - im.seed - ims.seed - is.seed - r.seed
  
# Testing frequency and timing  
  
# Parameters and distributions 
  # Transmission probabilities across different edges
    trans.hh <- 0.1
    trans.work <- 0.05
    trans.school <- 0.075
    trans.other <- 0.025
    trans.asymp <- 0.61  #Reduction in transmission probability from pre/asymptomatics (https://doi.org/10.1101/2020.03.15.20036582)
  
  # Infection progression parameters all either exponentially or erlang distributed in order to compare to mass-action analogs
    # Latent period
      t.latent <- function(...) {rgamma(1, 11, 2)} # erlang distribution with mean 5.5 days, low variance
      
    # Presymptomatic period (basically incubation period-latent period, this could maybe use some work)
      t.presymp <- function(...) {rgamma(1, 1, 2)} # erlang distribution with mean 0.5 days
      
    # Probability of being asymptomatic vs. becoming mildly symptomatic
      p.asymp <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.03,
                                                 age %in% c(10:19) ~ 0.05,
                                                 age %in% c(20:29) ~ 0.3,
                                                 age %in% c(30:39) ~ 0.55,
                                                 age %in% c(40:49) ~ 0.61,
                                                 age %in% c(50:59) ~ 0.75,
                                                 age %in% c(60:69) ~ 0.89,
                                                 age %in% c(70:79) ~ 0.9,
                                                 age >= 80 ~ 0.9)}
      
    # Time spent asymptomatic before recovery 
      t.asymp <- function(...) {rgamma(1, 6, 1)} # erlang distribution with mean 6 days, longish tail
      
    # Probability of progressing from mildly to severely symptomatic
      p.sevsymp <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.004,
                                                 age %in% c(10:19) ~ 0.004,
                                                 age %in% c(20:29) ~ 0.01,
                                                 age %in% c(30:39) ~ 0.04,
                                                 age %in% c(40:49) ~ 0.09,
                                                 age %in% c(50:59) ~ 0.13,
                                                 age %in% c(60:69) ~ 0.19,
                                                 age %in% c(70:79) ~ 0.2,
                                                 age >= 80 ~ 0.25)}
      
    # Time spent mildly symptomatic before recovery if B(1, p.sevsymp)=1
      t.msymp <- function(...) {rexp(1, 1/9)}
      
    # Time spent mildly symptomatic before progressing to severely symptomatic if B(1, p.sevsymp)=1
      t.mtosev <- function(...) {rgamma(1, 7, 2)}
      
    # Time spent severely symptomatic before recovery
      t.sevsymp <- function(...) {rgamma(1, 40, 4)} # erlang distribution with mean 10 days, low variance
        
# Base network matrix and Network matrix through time
  base.net <- matrix(data = 0, ncol = N, nrow = N)
  net.mat <- array(data = 0, dim = c(N,N,t.tot/dt))
  
# Infection status through time
  inf.mat <- matrix(data = NA, nrow = N, ncol = t.tot/dt)

# Event times
  t.til.nxt <- matrix(data = NA, nrow = N, ncol = t.tot/dt)

# ---------------------------------------------------------  
  
# NETWORK CHARACTERISTICS (code adapted from http://epirecip.es/epicookbook/chapters/karlsson/r) 
#TODO: FIll THESE IN WITH EMPIRICAL/SITUATIONAL DATA  
#TODO: How to inform formation of random edges within network? e.g. heterogeneity between individuals in terms of their random contacts
  
# ---------------------------------------------------------
  
#Example age distribution data    
  pop.props <- c(2.9+2.9,2.7+2.4,3.4+3.4,3.1+3,3.1+3.5,3.1+2.9,2.7+3.2,2.6+1.9,1.4+1+0.6+0.1+0)
  names(pop.props) <- c(seq(0,70,10), 80)  # 80 is anyone older than 80
  pop.ages <- as.numeric(sample(names(pop.props), size = N, replace = TRUE, prob = pop.props))

# example family size proportions
  num.siblings.props <- c(0.19,0.48,0.23,0.10)
  names(num.siblings.props) <- c(0,1,2,3)

# School characteristics 
  average.class.size.school <- 20
  
# Relational vectors to fill  
  family.membership <- rep(NA, N)
  work.membership <- rep(NA, N)
  school.membership <- rep(NA, N)
  other.contacts <- rep(NA, N)
  
# Assign relationships to individuals
# Families (e.g. households)  
  set.seed(430)  
  fam.id <- 1
  while(sum(is.na(family.membership[pop.ages<20]))>0){ # While there are unassigned children
  # Get number of children in family
    n.children <- sample(as.numeric(names(num.siblings.props)), 1, prob=num.siblings.props) + 1 
  # Find unassigned children to assign
    child.index <- which((pop.ages<20) & is.na(family.membership))[1:n.children]
    
  # Find unassigned parents to assign children to
# NOTE: This ends up being a bit weird because can have parents who are 19 and children who are 18  
    
    parent.index <- which((pop.ages>=20) & (pop.ages<=70) & is.na(family.membership))[1:2]
  # Assign family id to children and parents
    family.membership[c(child.index, parent.index)] <- fam.id
  # Start with next family id
    fam.id <- fam.id + 1
  }

# assign work assuming 15 workplaces with skewed distribution of workers per workplace
  n.working <- sum(pop.ages>=20 & pop.ages<=70)
  is.working <- which(pop.ages>=20 & pop.ages<=70)
  work.membership[is.working] <- sample(1:15, n.working, replace = T, prob = dpois(1:15, lambda = 5))

#assign schools/daycares
# for 0 to 9 year olds
  class.id <- 1
  is.in.school1 <- which(pop.ages==0)
  family.ids <-  unique(family.membership[is.in.school1])
  class.size <- 0
  for(f in family.ids){
    # Place children from the same family in the same school
    index <- is.in.school1[family.membership[is.in.school1]==f]
    school.membership[index] <- class.id
    class.size <- class.size + length(index)
    # Once class size reaches limit,start over with new class
    if(class.size>average.class.size.school){
      class.size <- 0
      class.id <- class.id + 1
    }
  }

# for 10 to 19 yrs
  is.in.school2 <- which(pop.ages==10)
  family.ids <-  unique(family.membership[is.in.school2])
  class.size <- 0
  for(f in family.ids){
    index <- is.in.school2[family.membership[is.in.school2]==f]
    school.membership[index] <- class.id
    class.size <- class.size + length(index)
    if(class.size>average.class.size.school){
      class.size <- 0
      class.id <- class.id + 1
    }
  }

# ---------------------------------------------------------  
  
# GENERATE NETWORK

# ---------------------------------------------------------
    
# First generate network matrix where cells are transmission probabilities
# combn function returns all pairwise combinations of individuals in the network membership index   
  
# household network
for(f in unique(family.membership[!is.na(family.membership)])){
  f.index <- which(family.membership==f)
  base.net[t(combn(f.index, 2))] <- trans.hh
}

# work network
for(w in unique(work.membership[!is.na(work.membership)])){
  w.index <- which(work.membership==w)
  if(length(w.index)==1){
    NULL
  } else {
    base.net[t(combn(w.index, 2))] <- trans.work
  }
}

# school network
for(s in unique(school.membership[!is.na(school.membership)])){
  s.index <- which(school.membership==s)
  base.net[t(combn(s.index, 2))] <- trans.school
}

# Make network matrix symmetric
  base.net[lower.tri(base.net)] = t(base.net)[lower.tri(base.net)]
    
# ---------------------------------------------------------

# SIMULATE TRANSMISSION

# ---------------------------------------------------------

# Initialize infection and waiting time matrices    
init.infection <- sample(c(rep("E", e.seed),
                           rep("Ip", ip.seed),
                           rep("Ia", ia.seed),
                           rep("Im", im.seed),
                           rep("Ims", ims.seed),
                           rep("Is", is.seed),
                           rep("R", r.seed),
                           rep("S", s.seed)), N, replace = FALSE)
  
  inf.mat[,1] <- init.infection  
  t.til.nxt[,1] <- sapply(inf.mat[,1], function(i){
    dplyr::case_when(i == "E" ~ t.latent(), 
                     i == "Ip" ~ t.presymp(), 
                     i == "Ia" ~ t.asymp(), 
                     i == "Im" ~ t.msymp(), 
                     i == "Ims" ~ t.mtosev(), 
                     i == "Is" ~ t.sevsymp(), 
                     TRUE ~ NA_real_)
  })

# Add random network component to starting network
  t1.net <- add.r.edges(base.net, r.net.prob, trans.other)
  #store starting network
  net.mat[,,1] <- t1.net

# Run simulation
for(t in 2:(t.tot/dt)){
#Advance transition times
    t.til.nxt[,t] <- t.til.nxt[,(t-1)]-dt
  
  # Advance expired states to next state 
    nexts <- which(t.til.nxt[,t] < 0) # states that expired
    next.mat <- t(mapply(next.state, inf.mat[nexts,(t-1)], pop.ages[nexts])) # function returns new states and time spent in new state
    inf.mat[nexts,t] <- next.mat[1,] # update infection status matrix
    t.til.nxt[nexts,t] <- as.numeric(next.mat[,2]) # update waiting time matrix
    
# Update network based on epi advances and interventions
  t.net <- base.net  
  # Eliminate school network connections if schools closed and increase hh contacts
    if(t.sc <= t & t <= t.sc.end){
      t.net[which(t.net == trans.school)] <- 0
    }  
  # Eliminate work network connections if shelter in place  
    if(t.sip <= t & t <= t.sip.end){
      t.net[which(t.net == trans.work)] <- 0
    }  
    

    
#Generate new infections across network 
  # Indices of pre/asymptomatic transmitters
    a.p.transmitters <- which(inf.mat[,t] %in% c("Ip", "Ia"))  
    m.s.transmitters <- which(inf.mat[,t] %in% c("Im", "Is"))  
   
  # Bernouli trial across all rows times transmitter columns where p(infection)~contact
    # Transmission from pre/asymtpomatic infections with reduction in transmissibility 
    if(length(a.p.transmitters) > 0){
      new.Es1 <- rowSums(t(apply(net.mat[,a.p.transmitters,t], 1, function(e){ 
        sapply(e, function(beta) rbinom(1, 1, (1-exp(-beta*trans.asymp)))) 
      })))
    } else {
      new.Es1 <- 0
    }  

    # Transmissions from mild/severely symptomatic individuals
    if(length(m.s.transmitters) > 0){
      new.Es2 <- rowSums(t(apply(net.mat[,m.s.transmitters,t], 1, function(e){ 
        sapply(e, function(beta) rbinom(1, 1, (1-exp(-beta)))) 
      })))
    } else {
      new.Es2 <- 0
    }  
      
    #Index of individuls who enter E compartment
    new.Es <- which(new.Es1+new.Es2 > 0)
    
  # Update infection matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix from sample of latent period dist'n
    t.til.nxt[new.Es,t] <- sapply(1:length(new.Es), t.latent)
      
}