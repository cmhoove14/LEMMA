library(sf)
library(tidycensus)
library(tidyverse)
library(data.table)
library(geojsonsf)

sfbg.sf <- tigris::block_groups(state = "06",
                                county = "075",
                                year = 2010,
                                class = "sf")

#Save crs to match coordinates
sf.crs <- st_crs(sfbg.sf)

# Load testing site locations in SF and determine CBG of testing sites
sf_test_sites <- geojson_sf("https://data.sfgov.org/resource/dtit-7gp4.geojson?$where=point+is+not+null") %>% 
  st_set_crs(sf.crs)

#mapview::mapview(list(sfbg.sf, sf_test_sites), layer.name = c("SF Census Block GRoups", "SF Covid Testing Sites"))

sf_test_bgs <- sf_test_sites %>% 
  st_join(., sfbg.sf)

sf_test_bgs_vec <- sf_test_bgs %>% pull(GEOID10)

saveRDS(sf_test_bgs_vec, file = "CMH/Safegraph/sf_test_site_bgs.rds")
