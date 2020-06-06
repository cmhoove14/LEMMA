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
  N <- 9e5
  risks <- rbeta(N, 2, 2)
  bta <- 1
  
  pop.props <- c(0.06+0.061, 0.065+0.066, 0.066+0.5*0.139, 0.5*0.139+0.5*0.127, 0.127, 0.5*0.127+0.066, 0.063+0.5*0.093, 0.5*0.093+0.5*0.048,0.5*0.048+0.019)
  names(pop.props) <- c(seq(0,70,10), 80)  # 80 is anyone older than 80
  pop.ages <- as.numeric(sample(names(pop.props), size = N, replace = TRUE, prob = pop.props))

# Key Dates
  t0 <- as.Date("2020-02-15")
  
# Beta change dates
  schools.close <- as.Date("2020-03-05") #Schools closed
  SiP.start <- as.Date("2020-03-15")     #SiP announced
  SiP.cont <- as.Date("2020-05-01")      #Another knot
  today <- Sys.Date()                     #Another knot
  
  t.end <- as.Date("2020-07-15")         # Where we're headed (for future use)

  t.tot <- as.numeric(t.end - t0)
  dt <- 1
  
  beta_time <- cbind(-as.numeric(t0-c(t0, schools.close,
                                      SiP.start, SiP.cont, today,t.end)),
                     c(1, 1, 0.6, 0.4, 0.5, 0.7))
  
  beta_smooth <- splinefun(x = beta_time[2:6,1],
                           y = beta_time[2:6,2],
                           method = "natural")

  beta_fx <- approxfun(c(1:t.tot),
                       c(rep(1,beta_time[2,1]), sapply(c(beta_time[2,1]+1):t.tot,
                                                       beta_smooth)))
  
plot(c(1:t.tot), sapply(1:t.tot, beta_fx), type = "l")
  points(x = beta_time[,1],
         y = beta_time[,2],
         pch = 17, col = "red")

# Infection status through time
  inf.mat <- matrix(data = NA, nrow = N, ncol = t.tot/dt)

# Event times
  t.til.nxt <- matrix(data = NA, nrow = N, ncol = t.tot/dt)
  
# Initial conditions
  e.seed <- 10     #Exposed
  ip.seed <- 5    #infected pre-symptomatic
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

sim.infection <- function(inf.vec, risks, bta){
  I.tot <- sum(inf.vec %in% c("Ip", "Ia", "Im", "Imh"))
  inf.probs <- (I.tot/N*bta*risks)
  S.vec <- which(inf.vec == "S")
  StoEs <- sapply(S.vec, function(p){
    rbinom(1,1,inf.probs[p])
  })
  return(which(StoEs == 1))
}  
  
# Run simulation
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

# Simulate infection
  beta_t <- beta_fx(t)  
  new.Es <- sim.infection(inf.mat[,t], risks, beta_t)
    
  # Update infection status matrix with new Es  
    inf.mat[new.Es,t] <- "E"
    
  # Update transition times matrix for new Es from sample of latent period dist'n
    t.til.nxt[new.Es,t] <- sapply(1:length(new.Es), t_latent)
  
  # Anyone who hasn't changed infection state remains the same
    sames <- which(is.na(inf.mat[,t]))
    inf.mat[sames,t] <- inf.mat[sames,(t-1)]
      
  # On to the next one  
}
 
test <- sum_inf_mat(inf.mat)
