library(data.table)
library(tidyverse)
library(ggplot2)

# Get SF data
if(sum(grepl(Sys.Date(), list.files("CMH/Data"))) == 0){
  source("CMH/Data/Get_COVID_Cal_latest.R")
  save.image(file = paste("CMH/Data/CA_SF_data", Sys.Date(), ".Rdata"))
} else {
  load(paste("CMH/Data/CA_SF_data", Sys.Date(), ".Rdata"))
}

bta_e0_cal <- readRDS("CMH/ABM/Analysis/Outputs/bta_01_e0_3_n1002020-08-03.rds")
states <- c("S", "E", "Ip", "Ia", "Im", "Imh", "Ih", "D", "R")
dt <- 4/24
start.date <- as.Date("2020-02-01")

abm_cases <- bind_rows(lapply(1:length(bta_e0_cal), function(i){
  df <- as.data.frame(bta_e0_cal[[i]][["infections"]]) %>% 
    mutate(time_day = time*dt,
           date = start.date+time_day,
           iter = i)
  
  return(df)

}))

abm_cases_day <- abm_cases %>% 
  mutate(date_char = as.character(date)) %>% 
  group_by(iter, date_char) %>% 
  summarise(I = n()) %>% 
  mutate(date = as.Date(date_char))

ggplot() +
  geom_col(data = sf_case, 
           aes(x = Date, y = Cases),
           col = "darkblue", fill = "blue", alpha = 0.7) +
  geom_line(data = abm_cases_day,
            aes(x = date, y = I, group = factor(iter)),
            col = "grey20", alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
#  ylim(c(0,200)) +
  labs(title = "Infection curves for 100 ABM runs")

ggplot() +
  geom_col(data = sf_case, 
           aes(x = Date, y = Cases),
           col = "darkblue", fill = "blue", alpha = 0.7) +
  geom_line(data = abm_cases_day,
            aes(x = date, y = I, group = factor(iter)),
            col = "grey20", alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  ylim(c(0,200)) +
  labs(title = "Infection curves for 100 ABM runs")

epi_curves <- bind_rows(lapply(1:length(bta_e0_cal), function(i){
  df <- as.data.frame(bta_e0_cal[[i]][["epi_curve"]])
  
  colnames(df) <- states
  
  df.out <- df %>% 
    mutate(I = Ip+Ia+Im+Imh+Ih,
           time = row_number(),
           time_day = time*dt,
           date = start.date+time_day,
           iter = i)
  
  return(df.out)
}))

epi_curve_hosp <- epi_curves %>% 
  mutate(date_char = as.character(date)) %>% 
  group_by(iter, date_char) %>% 
  summarise(H = mean(Ih)) %>% 
  mutate(date = as.Date(date_char))

ggplot() +
  geom_col(data = sf_hosp, 
           aes(x = Date, y = HOSP_tot),
           col = "grey20", fill = "grey50", alpha = 0.7) +
  geom_line(data = epi_curve_hosp,
            aes(x = date, y = H, group = factor(iter)),
            col = "darkblue", alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Hospitalization curves for 100 ABM runs")


epi_curves_day <- epi_curves %>% 
  mutate(date_char = as.character(date)) %>% 
  group_by(iter, date_char) %>% 
  summarise(across(all_of(c(states, "I")), mean)) %>% 
  mutate(date = as.Date(date_char))

  ggplot() +
    geom_line(data = epi_curves_day, 
              aes(x = date, y = I, group = factor(iter)),
              col = "grey20", alpha = 0.3) +
    geom_line(data = sf_case,
              aes(x = Date, y = Cases))
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Infection curves for 100 ABM runs")

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