# ---------------------------------------------------------
#   COVID ABM with testing
#   Chris Hoover (choover@berkeley.edu)
#   June 2020
# ---------------------------------------------------------

devtools::load_all("CMH/ABM/")

# ---------------------------------------------------------
# SETUP
# TODO: incorporate testing frequency/testing regime
# ---------------------------------------------------------
set.seed(430)
# Number of people, key dates, transmission probability 
  N <- 9e4
  risks <- rbeta(N, 2, 5)
  
  hist(risks, breaks = 30)
  
  beta.init <- 1/N
  
  pop.props <- c(0.06+0.061, 0.065+0.066, 0.066+0.5*0.139, 0.5*0.139+0.5*0.127, 0.127, 0.5*0.127+0.066, 0.063+0.5*0.093, 0.5*0.093+0.5*0.048,0.5*0.048+0.019)
  names(pop.props) <- c(seq(0,70,10), 80)  # 80 is anyone older than 80
  pop.ages <- as.numeric(sample(names(pop.props), size = N, replace = TRUE, prob = pop.props))

# Key Dates
t0 <- as.Date("2020-02-15")

# Beta change dates
schools.close <- as.Date("2020-03-05") #Schools closed
SiP.start <- as.Date("2020-03-15")     #SiP announced
SiP2 <- as.Date("2020-04-15")          #Another knot
SiP3 <- as.Date("2020-05-15")          #Another knot
today <- as.Date(Sys.Date()-1)         #Another knot today
t.end <- as.Date("2020-07-15")         # Where we're headed (for future use)

t.tot <- as.numeric(t.end - t0)
  dt <- 1
  

beta.time <- cbind(-as.numeric(t0-c(t0, schools.close,
                                    SiP.start, SiP2, SiP3, today, t.end)),
                   log(beta.init*c(1, 1, 0.65, 0.28, 0.27, 0.25, 0.4)))

beta_smooth <- splinefun(x = beta.time[,1],
                         y = beta.time[,2],
                         method = "natural")

beta_fx <- approxfun(c(1:t.tot),
                     c(rep(log(beta.init),beta.time[2,1]), sapply(c(beta.time[2,1]+1):t.tot,
                                                                  beta_smooth)))

plot(c(1:t.tot), exp(sapply(1:t.tot, beta_fx)), type = "l")
  points(x = beta.time[,1],
         y = exp(beta.time[,2]),
         pch = 17, col = "red")
# ---------------------------------------------------------
# Functions
# ---------------------------------------------------------

# Simulate infection across population  
sim.infection <- function(inf.vec, risks, bta){
  I.tot <- sum(inf.vec %in% c("Ip", "Ia", "Im", "Imh"))
  inf.probs <- 1-exp(-bta*I.tot*risks)
  S.vec <- which(inf.vec == "S")
  StoEs <- sapply(S.vec, function(p){
    rbinom(1,1,inf.probs[p])
  })
  return(S.vec[which(StoEs == 1)])
}  

# Function from here to speed up random sampling w/o replacement: https://stackoverflow.com/questions/15113650/faster-weighted-sampling-without-replacement 
weighted_Random_Sample <- function(
    .data,
    .weights,
    .n
    ){

    key <- runif(length(.data)) ^ (1 / .weights)
    return(.data[order(key, decreasing=TRUE)][1:.n])
}
  
# Test people   
test_folks <- function(inf.stat, test_probs, n_tests){
  
}

# Quarantine people with symptoms and people who are tested
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

# ---------------------------------------------------------
# Model prep
# ---------------------------------------------------------

# Infection status through time
  inf.mat <- matrix(data = NA, nrow = N, ncol = t.tot/dt)

# Event times
  t.til.nxt <- matrix(data = NA, nrow = N, ncol = t.tot/dt)
  
# Initial conditions
  e.seed <- 6     #Exposed
  ip.seed <- 1    #infected pre-symptomatic
  ia.seed <- 0    #infected asymptomatic
  im.seed <- 0    #infected mildly symptomatic
  imh.seed <- 0   #infected mildly symptomatic, will become severe
  ih.seed <- 0    #infected severely symptomatic
  d.seed <- 0     #dead
  r.seed <- 0     #removed
  s.seed <- N - e.seed - ip.seed - ia.seed - im.seed - imh.seed - ih.seed - d.seed - r.seed

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
    if(i == "E"){
      t_latent()
    } else if(i == "Ip"){
      t_presymp()
    } else if(i == "Ia"){
      t_asymp()
    } else if(i == "Im"){
      t_msymp()
    } else if(i == "Imh"){
      t_mtosev()
    } else if(i == "Ih"){
      t_sevsymp()
    } else {
      NA
    }
  })

# ---------------------------------------------------------
# Run simulation
# ---------------------------------------------------------

for(t in 2:(t.tot/dt)){
  print(t)
# Advance transition times
    t.til.nxt[,t] <- t.til.nxt[,(t-1)]-dt
  
  # Advance expired states to next state 
    nexts <- which(t.til.nxt[,t] < 0) # states that expired
    if(length(nexts >0)){
      next.mat <- t(mapply(next_state, inf.mat[nexts,(t-1)], pop.ages[nexts])) # function returns new states and time spent in new state
      inf.mat[nexts,t] <- next.mat[,1] # update infection status matrix
      t.til.nxt[nexts,t] <- as.numeric(next.mat[,2]) # update waiting time matrix
    }
    
  # Anyone who hasn't changed infection state remains the same
    sames <- which(is.na(inf.mat[,t]))
    inf.mat[sames,t] <- inf.mat[sames,(t-1)]

# Simulate infection
  beta_t <- exp(beta_fx(t))  
  new.Es <- sim.infection(inf.mat[,t], risks, beta_t)

  # Update infection status matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix for new Es from sample of latent period dist'n
    t.til.nxt[new.Es,t] <- as.numeric(replicate(length(new.Es), t_latent()))
  
  # On to the next one  
}
 
test <- sum_inf_mat(inf.mat)
