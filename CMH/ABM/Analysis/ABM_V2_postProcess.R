library(data.table)
library(tidyverse)
library(ggplot2)

bta_e0_cal <- readRDS("CMH/ABM/Analysis/Outputs/bta_e0_calibration2020-07-31.rds")
states <- c("S", "E", "Ip", "Ia", "Im", "Imh", "Ih", "D", "R")
dt <- 4/24
start.date <- as.Date("2020-02-01")

# Should reflect matrix of parameter sweeps 
n_sims_per_par <- 1
bta_E0_grid <- expand.grid(bta = c(0.05, 0.1, 0.2),
                           E0 = c(1,3,5))  

bta_E0_sweeps <- bta_E0_grid %>% slice(rep(1:n(), each = n_sims_per_par))

n_I <- sapply(1:nrow(bta_E0_sweeps), function(i){
  nrow(bta_e0_cal[[i]][["infections"]])
})
n_It <- sapply(1:nrow(bta_E0_sweeps), function(i){
  sum(bta_e0_cal[[i]][["linelist_tests"]][["tested"]])
})

cbind(bta_E0_sweeps,n_I,n_It)

# Run which is closest to observed
bta01_e1 <- bta_e0_cal[[4]][["epi_curve"]]
  colnames(bta01_e1) <- states

bta01_e1 <- as.data.frame(bta01_e1) %>% 
  mutate(I = Ip+Ia+Im+Imh+Ih,
         time = row_number(),
         time_day = time*dt,
         date = start.date+time_day) %>% 
  group_by(date) %>% 
  summarise(across(c("S", "I", "R")),median)

  ggplot()  