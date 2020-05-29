# ---------------------------------------------------------
#   COVID ABM
#   Chris Hoover (choover@berkeley.edu)
#   May 2020
# ---------------------------------------------------------

# FUNCTIONS (to be moved to R package infrastructure)
# ---------------------------------------------------------
setwd("CMH/ABM/")
devtools::load_all()

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

# ---------------------------------------------------------
# SETUP
# TODO: incorporate testing frequency/testing regime
# ---------------------------------------------------------

# Number of people, time frame, and time step
  N <- 1000
  t.tot <- 100
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
    r.net.prob.sc <- 0.4  # Probability of random interaction while schools are closed
    r.net.prob.sip <- 0.05 # Probability of random interaction while in shelter in place
    
# Initial conditions
  e.seed <- 1     #Exposed
  ip.seed <- 0    #infected pre-symptomatic
  ia.seed <- 0    #infected asymptomatic
  im.seed <- 0    #infected mildly symptomatic
  imh.seed <- 0   #infected mildly symptomatic, will become severe
  ih.seed <- 0    #infected severely symptomatic
  d.seed <- 0
  r.seed <- 0     #removed
  s.seed <- N - e.seed - ip.seed - ia.seed - im.seed - imh.seed - ih.seed - d.seed - r.seed
  
#TODO: Testing frequency and timing  
  
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
      
    # Probability of becoming mildly symptomatic vs. being asymptomatic
      p.symp <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.03,
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
      
    # Time spent severely symptomatic before recovery/death
      t.sevsymp <- function(...) {rgamma(1, 36, 3)} # erlang distribution with mean 12 days, low variance
      
    # Probability of dying given hospitalization from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified 
      p.mort <- function(age) {dplyr::case_when(age %in% c(0:9) ~ 0.01,
                                                age %in% c(10:19) ~ 0.0205,
                                                age %in% c(20:29) ~ 0.031,
                                                age %in% c(30:39) ~ 0.0475,
                                                age %in% c(40:49) ~ 0.0785,
                                                age %in% c(50:59) ~ 0.12,
                                                age %in% c(60:69) ~ 0.186,
                                                age %in% c(70:79) ~ 0.3,
                                                age >= 80 ~ 0.45)}
        
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
  num.siblings.props <- dnbinom(0:10, mu = 1.2, size = 1000)
  names(num.siblings.props) <- c(0:10)

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
# NOTE: This ends up being a bit weird because can have parents who are 20-29 and children who are 10-19  
    
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
for(h in unique(family.membership[!is.na(family.membership)])){
  h.index <- which(family.membership==h)
  base.net[t(combn(h.index, 2))] <- "H"
}

# work network
for(w in unique(work.membership[!is.na(work.membership)])){
  w.index <- which(work.membership==w)
  if(length(w.index)==1){ # If one worker in the workplace, messes this up so just skip
    NULL
  } else {
    base.net[t(combn(w.index, 2))] <- "W"
  }
}

# school network
for(s in unique(school.membership[!is.na(school.membership)])){
  s.index <- which(school.membership==s)
  base.net[t(combn(s.index, 2))] <- "S"
}

# Make network matrix symmetric
  base.net <- sym.mat(base.net)
  
# ---------------------------------------------------------

# SIMULATE TRANSMISSION

# ---------------------------------------------------------

# Initialize infection and waiting time matrices    
init.infection <- sample(c(rep("E", e.seed),
                           rep("Ip", ip.seed),
                           rep("Ia", ia.seed),
                           rep("Im", im.seed),
                           rep("Imh", imh.seed),
                           rep("Ih", ih.seed),
                           rep("R", r.seed),
                           rep("S", s.seed)), N, replace = FALSE)
  
# Fill infection and state transition matrices based on initial conditions  
  inf.mat[,1] <- init.infection  
  t.til.nxt[,1] <- sapply(inf.mat[,1], function(i){
    dplyr::case_when(i == "E" ~ t.latent(), 
                     i == "Ip" ~ t.presymp(), 
                     i == "Ia" ~ t.asymp(), 
                     i == "Im" ~ t.msymp(), 
                     i == "Imh" ~ t.mtosev(), 
                     i == "Ih" ~ t.sevsymp(), 
                     TRUE ~ NA_real_)
  })

#Prepare network for t1  
  # Replace contacts with transmission probabilities
    t1.net <- base.net
    t1.net[which(t1.net == "H")] <- trans.hh
    t1.net[which(t1.net == "W")] <- trans.work
    t1.net[which(t1.net == "S")] <- trans.school
  # Add random network component to starting network
    t1.net <- add.r.edges(t1.net, r.net.prob, trans.other)
    class(t1.net) <- "numeric"
  #store starting network
    net.mat[,,1] <- t1.net

# Run simulation
for(t in 2:(t.tot/dt)){
  print(t)
# Advance transition times
    t.til.nxt[,t] <- t.til.nxt[,(t-1)]-dt
  
  # Advance expired states to next state 
    nexts <- which(t.til.nxt[,t] < 0) # states that expired
    if(length(nexts >0)){
      next.mat <- t(mapply(next.state, inf.mat[nexts,(t-1)], pop.ages[nexts])) # function returns new states and time spent in new state
      inf.mat[nexts,t] <- next.mat[,1] # update infection status matrix
      t.til.nxt[nexts,t] <- as.numeric(next.mat[,2]) # update waiting time matrix
    }
    
# Update network based on epi advances and interventions
# TODO: Implement infection influence on contact    
  t.net <- base.net  
  
  #indices of individuals with symptoms
  inf.w.symptoms <- which(inf.mat[,t] %in% c("Im", "Imh"))
  
  # Eliminate school connections if schools closed and increase hh contacts
    if(t.sc <= (t*dt) & (t*dt) <= t.sc.end){
      t.net[which(t.net == "H")] <- trans.hh*trans.hh.sc
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add.r.edges(t.net, r.net.prob.sc, trans.other)
      
  # Eliminate work and school connections if schools closed and increase hh contacts
    } else if(t.sip <= (t*dt) & (t*dt) <= t.sip.end){
      t.net[which(t.net == "H")] <- trans.hh*trans.hh.sip
      t.net[which(t.net == "W")] <- 0
      t.net[which(t.net == "S")] <- 0
    # Add random network component to network
      t.net <- add.r.edges(t.net, r.net.prob.sip, trans.other)
      
  # If not in shelter in place or school closure, network remains the same plus random connections    
    } else {
      t.net[which(t.net == "H")] <- trans.hh
      t.net[which(t.net == "W")] <- trans.work
      t.net[which(t.net == "S")] <- trans.school
    # Add random network component to network
      t.net <- add.r.edges(t.net, r.net.prob, trans.other)
    } 
    
  #store resulting network in network array
    class(t.net) <- "numeric"
    net.mat[,,t] <- t.net

#Generate new infections across network 
  # Indices of different types of transmitters transmitters
    a.p.transmitters <- which(inf.mat[,t] %in% c("Ip", "Ia"))  
    m.s.transmitters <- which(inf.mat[,t] %in% c("Im", "Imh"))  
   
  # Bernouli trial across all rows times transmitter columns where p(infection)~contact
    # Transmission from pre/asymtpomatic infections with reduction in transmissibility 
      if(length(a.p.transmitters) > 0){
        new.Es1 <- new.infection(net.mat[,,t], a.p.transmitters, trans.asymp)
      } else {
        new.Es1 <- rep(0, N)
      }

    # Transmissions from mild/severely symptomatic individuals
      if(length(m.s.transmitters) > 0){
        new.Es2 <- new.infection(net.mat[,,t], m.s.transmitters, 1)
      } else {
        new.Es2 <- rep(0, N)
      }

  # Index of individuls who enter E compartment (new infections)
    new.Es <- which(new.Es1+new.Es2 > 0 & inf.mat[,(t-1)] == "S")
    
  # Update infection status matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix for new Es from sample of latent period dist'n
    t.til.nxt[new.Es,t] <- sapply(1:length(new.Es), t.latent)
  
  # Anyone who hasn't changed infection state remains the same
    sames <- which(is.na(inf.mat[,t]))
    inf.mat[sames,t] <- inf.mat[sames,(t-1)]
      
  # On to the next one  
}
 
test <- sum.inf.mat(inf.mat)       
