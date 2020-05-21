# ---------------------------------------------------------
#   COVID ABM
#   Chris Hoover (choover@berkeley.edu)
#   May 2020
# ---------------------------------------------------------

# SETUP
# TODO: incorporate testing frequency/teting regime
# TODO: fit/parameterize/get data for epidemiological parameters
# TODO: get netrowk parameters for pre/post intervention, incorporate into network dynamics
# ---------------------------------------------------------

# Number of people, time frame, and time step
  N <- 1000
  t <- 200
  dt <- 1
  
# Intervention parameters 
  t.sc <- 20     # Time at which schools are closed
  t.sip <- 40    # Time at which shelter in place occurs

# Initial conditions
  e.seed <- 10    #Exposed
  ip.seed <- 0    #infected pre-symptomatic
  ia.seed <- 0    #infected asymptomatic
  im.seed <- 0    #infected mildly symptomatic
  is.seed <- 0    #infected severely symptomatic
  r.seed <- 0     #removed
  s.seed <- N - e.seed - ip.seed - ia.seed - im.seed - is.seed - r.seed
  
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
                                                 age > 80 ~ 0.9)}
      
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
                                                 age > 80 ~ 0.25)}
      
    # Time spent mildly symptomatic before recovery if B(1, p.sevsymp)=1
      t.msymp <- function(...) {rexp(1, 9)}
      
    # Time spent mildly symptomatic before progressing to severely symptomatic if B(1, p.sevsymp)=1
      t.mtosev <- function(...) {rgamma(1, 7, 2)}
      
    # Time spent severely symptomatic before recovery
      t.sevsymp <- function(...) {rgamma(1, 40, 4)} # erlang distribution with mean 10 days, low variance
        
  # Network parameters
    r.net.prob <- 0.5     # Probability of random interaction pre-intervention
    r.net.prob.sip <- 0.05 # Probability of random interaction while in shelter in place

# Network matrix through time
  base.net <- matrix(data = 0, ncol = N, nrow = N)
  net.mat <- array(data = 0, dim = c(N,N,t/dt))
  
# Infection status through time
  inf.mat <- matrix(data = 0, nrow = N, ncol = t/dt)

# ---------------------------------------------------------  
  
# NETWORK CHARACTERISTICS (code adapted from http://epirecip.es/epicookbook/chapters/karlsson/r) 
#TODO: FIll THESE IN WITH EMPIRICAL/SITUATIONAL DATA  
#TODO: How to inform formation of random edges within network? e.g. heterogeneity between individuals in terms of their random contacts
  
# ---------------------------------------------------------
  
#Example age distribution data    
  pop.props <- c(2.9+2.9,2.7+2.4,3.4+3.4,3.1+3,3.1+3.5,3.1+2.9,2.7+3.2,2.6+1.9,1.4+1,0.6+0.1+0)
  names(pop.props) <- c(seq(0,90,10)[-1], "90+")
  pop.ages <- as.numeric(sample(names(pop.props), N, replace = TRUE, prob = pop.props))

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
  while(sum(is.na(family.membership[pop.ages<=20]))>0){ # While there are unassigned children
  # Get number of children in family
    n.children <- sample(as.numeric(names(num.siblings.props)), 1, prob=num.siblings.props) + 1 
  # Find unassigned children to assign
    child.index <- which((pop.ages<=20) & is.na(family.membership))[1:n.children]
    
  # Find unassigned parents to assign children to
# NOTE: This ends up being a bit weird because can have parents who are 19 and children who are 18  
    
    parent.index <- which((pop.ages>20) & (pop.ages<=50) & is.na(family.membership))[1:2]
  # Assign family id to children and parents
    family.membership[c(child.index, parent.index)] <- fam.id
  # Start with next family id
    fam.id <- fam.id + 1
  }

# assign work assuming 15 workplaces with skewed distribution of workers per workplace
  n.working <- sum(pop.ages>20 & pop.ages<65)
  is.working <- which(pop.ages>20 & pop.ages<65)
  work.membership[is.working] <- sample(1:15, n.working, replace = T, prob = dpois(1:15, lambda = 5))

#assign schools
# for 5 to 10 year olds
  class.id <- 1
  is.in.school1 <- which((pop.ages>=5) & (pop.ages<10))
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

# for 10 to 20 yrs
  is.in.school2 <- which((pop.ages>=10) & (pop.ages<=20))
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

# random network
  #Number of random edges. Maybe make r.net.prob a distribution so there's heterogeneity in who has random contacts?
    #e.g. sum(sapply(r.probs, function(p){rbinom(1, 1, p)}))
    n.r.pairs <- rbinom(1, N, r.net.prob)
  #Allocate random edges to network. Could add probs argument here to make some individuals more likely to have random contacts
    r.pairs <- matrix(sample(N, 2*n.r.pairs, replace = TRUE), ncol=2)
  #If edge already exists in another capacity, keep it  
    pre.edges <- base.net[r.pairs]
    post.edges <- ifelse(pre.edges == 0, trans.other, pre.edges)
    base.net[r.pairs] <- post.edges

# Make network matrix symmetric
  base.net[lower.tri(base.net)] = t(base.net)[lower.tri(base.net)]
  net.mat[ , , 1] <- base.net
  
# ---------------------------------------------------------

# SIMULATE TRANSMISSION

# ---------------------------------------------------------
  
init.infection <- sample(c(rep("E", e.seed),
                           rep("Ip", ip.seed),
                           rep("Ia", ia.seed),
                           rep("Im", im.seed),
                           rep("Is", is.seed),
                           rep("R", r.seed),
                           rep("S", s.seed)), N, replace = FALSE)
  
inf.mat[,1] <- init.infection  
  
for(t in 2:(t/dt)){
  transmitters <- which(inf.mat[,(t-1)] == "I")
  trans.mat <- as.matrix(ifelse(inf.mat[,(t-1)]=="I",1,0), ncol = 1)%*%net.mat[ , , (t-1)]
}