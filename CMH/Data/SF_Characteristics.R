# ---------------------------------------------------
# Get demographic and other characteristics of SF on census tract level to inform agent based model of covid transmission
# Chris Hoover
# June 2020
# ---------------------------------------------------
library(tidycensus)
library(maptools)
library(rgdal)
library(readxl)
library(tidyverse)


census_api_key("7ba1ba2ca9e00272159d7baa683b488ae13ce15e")
sf.census <- fips_codes %>% 
  filter(state == "CA" & county == "San Francisco County")

# ---------------------------------------------------
# Calenviroscreen data
# ---------------------------------------------------
ces3.sf <- readxl::read_xlsx("CMH/Data/ces3results.xlsx", sheet = 1) %>% 
  filter(`California County` == "San Francisco")

ces3.dem <- readxl::read_xlsx("CMH/Data/ces3results.xlsx", sheet = 4)
colnames(ces3.dem) <- c("CT", "CES3.score", "CES3.per", "CES3.per.range",
                           "Pop", "County", "age.lt10", "age.11_64", "age.gt65",
                           "per.hispanic", "per.white", "per.black", "per.native", "per.asian", "per.other")

ces3.dem.sf <- ces3.dem %>% 
  filter(County == "San Francisco")

ces3.all <- ces3.sf %>% 
  left_join(ces3.dem.sf, by = c("Census Tract" = "CT")) %>% 
  mutate(GEOID = str_c("0", `Census Tract`)) %>% 
  dplyr::select(c("GEOID", "CES3.score", "CES3.per", "CES3.per.range",
                  "Pop", "County", "age.lt10", "age.11_64", "age.gt65",
                  "per.hispanic", "per.white", "per.black", "per.native", "per.asian", "per.other"))

# ---------------------------------------------------
# Household size by SF census tracts
# ---------------------------------------------------
sf.acs18.hh <- tidycensus::get_acs(geography = "tract", 
                                   table = "B11016", 
                                   state = "CA", 
                                   county = sf.census$county_code, 
                                   survey = "acs5") %>% 
  mutate(hh.type = case_when(variable == "B11016_001" ~ NA_character_,
                             variable == "B11016_002" ~ NA_character_,
                             variable == "B11016_003" ~ "hh.fam2",
                             variable == "B11016_004" ~ "hh.fam3",
                             variable == "B11016_005" ~ "hh.fam4",
                             variable == "B11016_006" ~ "hh.fam5",
                             variable == "B11016_007" ~ "hh.fam6",
                             variable == "B11016_008" ~ "hh.fam7p",
                             variable == "B11016_009" ~ NA_character_,
                             variable == "B11016_010" ~ "hh.non1",
                             variable == "B11016_011" ~ "hh.non2",
                             variable == "B11016_012" ~ "hh.non3",
                             variable == "B11016_013" ~ "hh.non4",
                             variable == "B11016_014" ~ "hh.non5",
                             variable == "B11016_015" ~ "hh.non6",
                             variable == "B11016_016" ~ "hh.non7p")) %>% 
  drop_na() %>% 
  dplyr::select(GEOID, estimate, hh.type) %>% 
  pivot_wider(names_from = hh.type,
              values_from = estimate)

# ---------------------------------------------------
# Median household income in past 12 months
# ---------------------------------------------------
sf.acs18.income <- tidycensus::get_acs(geography = "tract", 
                                       variables = "B19013_001", 
                                       state = "CA", 
                                       county = sf.census$county_code, 
                                       survey = "acs5") %>% 
  mutate(hh.income = estimate) %>% 
  dplyr::select(GEOID, hh.income)

# ---------------------------------------------------
# SF census tracts shapefile for spatial network
# ---------------------------------------------------
sf.sf <- tigris::tracts(sf.census$state_code,
                        sf.census$county_code,
                        class = "sf",
                        refresh = TRUE)

sf.sf <- sf.sf %>% 
  filter(AWATER < 5e7)

sf.sp <- as(sf.sf, Class = "Spatial")

# Get neighborhood matrix
sf.coords <- sp::coordinates(sf.sp)  # Coordinates of polygon centers
sf.nbs <- spdep::poly2nb(sf.sp)  # Neighbour relationships based on a triangulation
sf.wmat <- spdep::nb2mat(sf.nbs)  # Weight matrix between the nodes.
row.names(sf.wmat) <- sf.sf$GEOID
colnames(sf.wmat) <- rownames(sf.wmat)

sf.wmat.df <- as_tibble(sf.wmat) %>%
  mutate(GEOID = rownames(sf.wmat))

# ---------------------------------------------------
# Join all together  
# ---------------------------------------------------
sf.cts.all <- sf.wmat.df %>% 
  left_join(ces3.all, by = "GEOID") %>% 
  left_join(sf.acs18.hh, by = "GEOID") %>% 
  left_join(sf.acs18.income, by = "GEOID")
