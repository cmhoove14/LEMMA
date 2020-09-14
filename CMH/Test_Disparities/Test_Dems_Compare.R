library(data.table)
library(tidyverse)
library(geojsonsf)

sfc19 <- fread("CMH/ABM/data/secure/DeidentifiedCovidTesting7-8-2020.csv") %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         race_age_zip = paste(race, age, zip, sep = "_"))

sf_zips <- geojson_sf("https://data.sfgov.org/api/geospatial/u5j3-svi6?method=export&format=GeoJSON") %>% 
  filter(po_name == "SAN FRANCISCO")

sf_test_sites <- geojson_sf("https://data.sfgov.org/resource/dtit-7gp4.geojson?$where=point+is+not+null")


sf_dems <- sfc19 %>% 
  group_by(race_age_zip) %>% 
  summarise(dem_pop = first(demo.zip.pop))

sfc19_sum <- sfc19 %>% 
  group_by(date,age,race,zip) %>% 
  summarise(n_tests = n(),
            n_pos = sum(pos),
            pos_rate = n_pos/n_tests)
