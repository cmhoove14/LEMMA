require(surveillance)
require(tidyverse)

source("CMH/Data/Get_COVID_Cal_counties_latest.R")
load("CMH/Data/t_i_h_gamma.Rdata")

#PMF of the time from infection to hospitalization truncated at thirty days
dmax <- 30
inc.pmf <- c(0,dgamma(c(1:dmax), t_i_h_gamma_pars[1], t_i_h_gamma_pars[2]))

#Function to sample from the incubation time
rincu <- function(n) {
  sample(0:dmax, size=n, replace=TRUE, prob=inc.pmf)
}

# Hospital admissions (the "onsets") as an sts object
CA_H_Adm <- CA_hosp %>% 
  mutate(Date = as.Date(todays_date, format = "%Y-%m-%d"),
         H_admits = previous_days_covid_confirmed_patients,
         H_suspects = previous_days_suspected_covid_patients) %>% 
  dplyr::select(Date, county, H_admits, H_suspects) %>% 
  filter(!is.na(H_admits) & !is.na(Date)) %>% 
  arrange(county, Date)

SF_H <- CA_H_Adm %>% 
  filter(county == "San Francisco") %>% 
  pull(H_admits)

SF_H_pad0 <- c(rep(0,dmax), SF_H)

sts <- new("sts", 
           epoch=1:length(SF_H_pad0),
           observed=matrix(SF_H_pad0,ncol=1))

#Plot the "outbreak"
  plot(sts, xaxis.labelFormat=NULL, legend=NULL)

#Call non-parametric back-projection function 
bpnp.control <- list(k=2,
                     eps=rep(0.0001,2),
                     iter.max=rep(500,2),
                     B=300,
                     eq3a.method="C")

sts.bp <- backprojNP(sts, incu.pmf=inc.pmf,
                     control=bpnp.control)

# Function to get back projection estimate  
backproj <- function(dat.vec, pmf, bpnp.control){
  dat.sts <- new("sts", 
                 epoch=1:length(dat.vec),
                 observed=matrix(dat.vec,ncol=1))
  
  sts.bp <- backprojNP(dat.sts, incu.pmf=pmf,
                       control=bpnp.control)
  
  return(sts.bp)

}

#Function to plot estimates from bootstrapped back projections
backproj.plot <- function(dat.vec, sts.obj){
  sts.sum <- as_tibble(t(apply(sts.obj@lambda, 1, function(x){
    t.sum <- summary(x[1,])
    return(c(t.sum[c(2,3,5)]))
  }))) %>% 
    mutate(obs = dat.vec,
           time = dplyr::row_number())
  
  sts.plot <- sts.sum %>% 
  ggplot() +
    theme_bw() +
    geom_col(aes(x = time, y = obs)) +
    geom_line(aes(x = time, y = Median), size = 1.2) +
    geom_ribbon(aes(x = time, ymin = `1st Qu.`, ymax = `3rd Qu.`),
                fill = "grey60", alpha = 0.3)
  
  return(sts.plot)
}

# PLot resulting estimate of onset dates
plot(sts.bp,
     xaxis.labelFormat=NULL,
     legend=NULL,
     lwd=c(1,1,2),
     lty=c(1,1,1),
     main="")

#Do the convolution for the expectation (e.g. validate estimates)
mu <- matrix(0,ncol=ncol(sts.bp),nrow=nrow(sts.bp))
#Loop over all series
for (j in 1:ncol(sts.bp)) { 
  #Loop over all time points
  for (t in 1:nrow(sts.bp)) {
    #Convolution, note support of inc.pmf starts at zero (move idx by 1)
    i <- seq_len(t)
    mu[t,j] <- sum(inc.pmf[t-i+1] * upperbound(sts.bp)[i,j],na.rm=TRUE)
  }
}
#Show the fit
lines(1:nrow(sts.bp)-0.5,mu[,1],col="green",type="s",lwd=3)

plot.stsBP <- function(stsBP) {
  maxy <- max(observed(stsBP),upperbound(stsBP),stsBP@ci,na.rm=TRUE)
  plot(upperbound(stsBP),type="n",ylim=c(0,maxy), ylab="Cases",xlab="time")
  if (!all(is.na(stsBP@ci))) {
    polygon( c(1:nrow(stsBP),rev(1:nrow(stsBP))),
             c(stsBP@ci[2,,1],rev(stsBP@ci[1,,1])),col="lightgray")
  }
  lines(upperbound(stsBP),type="l",lwd=2)
  legend(x="topleft",
         c(expression(lambda[t])),
         lty=c(1),col=c(1),fill=c(NA),border=c(NA),lwd=c(2),
         bty = "n")

  invisible()
}

#Plot the result of k=0 and add truth for comparison. No CIs available
plot.stsBP(sts.bp)
