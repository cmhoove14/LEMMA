require(tidyverse)

CA_cases <- read.csv(url("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"))

CA_tests <- read.csv(url("https://data.ca.gov/dataset/efd6b822-7312-477c-922b-bccb82025fbe/resource/b6648a0d-ff0a-4111-b80b-febda2ac9e09/download/statewide_testing.csv"))

CA_hosp <- read.csv(url("https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv"))

sf_hosp <- read.csv(url("https://data.sfgov.org/resource/nxjg-bhem.csv")) %>% 
  mutate(Date = as.Date(reportdate),
         type = ifelse(dphcategory == "ICU", "ICU", "HOSP"),
         conf = ifelse(covidstatus == "PUI", "PUI", "CONF"),
         hosp_stat = paste(type, conf, sep = "_")) %>% 
  pivot_wider(names_from = hosp_stat,
              values_from = patientcount) %>%
  group_by(Date) %>% 
  summarise(ICU_PUI = sum(ICU_PUI, na.rm = T),
            ICU_CONF = sum(ICU_CONF, na.rm = T),
            HOSP_PUI = sum(HOSP_PUI, na.rm = T),
            HOSP_CONF = sum(HOSP_CONF, na.rm = T)) %>% 
  arrange(Date) %>% 
  mutate(HOSP_tot = ICU_CONF + HOSP_CONF,
         HOSP_max = ICU_CONF + HOSP_CONF + ICU_PUI + HOSP_PUI,
         cumICUconf = cumsum(ICU_CONF),
         cumICUpui = cumsum(ICU_PUI),
         cumHOSPconf = cumsum(HOSP_CONF),
         cumHOSPpui = cumsum(HOSP_PUI))

sf_case <- read.csv(url("https://data.sfgov.org/resource/tvq9-ec9w.csv")) %>% 
  mutate(Date = as.Date(specimen_collection_date)) %>% 
  padr::pad(., start_val = as.Date("2020-03-01")) %>%  
  pivot_wider(names_from = case_disposition,
              values_from = case_count) %>% 
  group_by(Date) %>% 
  summarise(Cases = sum(Confirmed, na.rm = T),
            Deaths = sum(Death, na.rm = T)) %>% 
  arrange(Date) %>% 
  mutate(cum_case = cumsum(Cases),
         cum_death = cumsum(Deaths))

sf_test <- read.csv(url("https://data.sfgov.org/resource/nfpa-mg4g.csv")) %>% 
  mutate(Date = as.Date(specimen_collection_date)) %>% 
  arrange(Date) %>% 
  mutate(cum_tests = cumsum(tests),
         cum_pos = cumsum(pos))

sf_all <- sf_test %>% 
  dplyr::select(Date, tests, pos, neg, pct, indeterminate, cum_tests, cum_pos) %>%
  full_join(sf_case, by = "Date") %>% 
  full_join(sf_hosp, by = "Date") %>% 
  mutate(time = as.integer(Date - as.Date("2020-02-29"))) %>% 
  filter(time >0)
