library(sf)
library(tidycensus)
library(tidyverse)
library(data.table)
library(geojsonsf)

#----------------------------------------
# Load all FRED synthetic population data  
#----------------------------------------
sf.ppl <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_people.csv")
sf.hh <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_households.csv")
sf.work <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_workplaces.csv")
sf.scl <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_schools.csv")
sf.gq <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_groupQuarters.csv")
sf.gq.ppl <- read.csv("CMH/Data/SF_synthetic_pop/FRED_SF_groupQuartersPeople.csv")

#----------------------------------------
# Get SF census tracts and zip codes shapefiles and determine neighborhood matrix
#----------------------------------------
gc()

#San frnacisco block groups
sf.sf <- tigris::block_groups("06",
                              "075",
                              year = 2010,
                              class = "sf")

sf.sf <- sf.sf %>% 
  filter(AWATER10 < 5e7)

#Save crs to match coordinates
sf.crs <- st_crs(sf.sf)

# Bay area zip codes
bay_zips <- geojson_sf("https://data.sfgov.org/api/geospatial/u5j3-svi6?method=export&format=GeoJSON") %>% 
  st_set_crs(sf.crs)

#----------------------------------------
# Determine neighborhoods and zips of households
#----------------------------------------
sf.hh.sf <- sf.hh %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf %>% dplyr::select(GEOID10)) %>% 
  st_join(., bay_zips %>% dplyr::select(zip)) %>% 
  left_join(sf.hh %>% dplyr::select(c("sp_id", "latitude", "longitude")),
            by = "sp_id")

#----------------------------------------
# Determine nbhds of workplaces
#----------------------------------------
sf.work.sf <- sf.work %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf %>% dplyr::select(GEOID10))

#----------------------------------------
# Determine nbhds of schools
#----------------------------------------
sf.scl.sf <- sf.scl %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf %>% dplyr::select(GEOID10))

#----------------------------------------
# Determine nbhds of group quarters
#----------------------------------------
sf.gq.sf <- sf.gq %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf %>% dplyr::select(GEOID10)) %>% 
  left_join(sf.gq %>% dplyr::select(c("sp_id", "latitude", "longitude")),
            by = "sp_id")

#----------------------------------------
# Determine workplaces close to group quarters and schools in order to assign some workers to working at group quarters and schools
#----------------------------------------
sf.work.gq <- st_distance(sf.gq.sf, sf.work.sf)
  gq.work.ids <- apply(sf.work.gq, 1, which.min)
  sf.work$work_loc <- sf.work$sp_id
  sf.work$work_loc[gq.work.ids] <- sf.gq$sp_id

sf.work.scl <- st_distance(sf.scl.sf, sf.work.sf)
  scl.work.ids <- apply(sf.work.scl, 1, which.min)
  sf.work$work_loc[scl.work.ids] <- sf.scl$sp_id
  
#----------------------------------------
# Load testing site locations in SF and get nearest test site for everyone
#----------------------------------------
sf_test_sites <- geojson_sf("https://data.sfgov.org/resource/dtit-7gp4.geojson?$where=point+is+not+null") %>% 
  st_set_crs(sf.crs)

sf_test_dist_public <- st_distance(sf.hh.sf, sf_test_sites %>% filter(location_type == "Public"))
  public_test_site <- sf_test_sites$id[which(sf_test_sites$location_type == "Public")][apply(sf_test_dist_public, 1, which.min)]
  
sf_test_dist_private <- st_distance(sf.hh.sf, sf_test_sites %>% filter(location_type == "Private"))
  private_test_site <- sf_test_sites$id[which(sf_test_sites$location_type == "Private")][apply(sf_test_dist_private, 1, which.min)]
  
  sf.hh.sf <- sf.hh.sf %>% 
    mutate(test_public = public_test_site,
           test_private = private_test_site)
  
#----------------------------------------
# Merge datasets
#----------------------------------------
# Group quarters people to their quarters
sf.gq.res <- sf.gq.ppl %>% 
  mutate(race = NA,   # Add columns necessary for join to sf.ppl
         relate = NA, 
         school_id = -1,
         work_id = -1) %>% 
  left_join(sf.gq.sf %>% st_set_geometry(.,NULL) %>% 
              mutate(hh_income = NA), 
            by = c("sp_gq_id" = "sp_id")) %>% 
  rename("sp_res_id" = "sp_gq_id",
         "cbg" = "GEOID10") %>% 
  dplyr::select(-c(persons, stcotrbg)) %>% 
  dplyr::relocate(hh_income, .after = cbg)

#People to their workplaces and schools
sf.ppl.res <- sf.ppl %>% 
  mutate(gq_type = "H",
         school_id = as.numeric(ifelse(school_id == "X", -1, school_id)),
         work_id = as.numeric(ifelse(work_id == "X", -1, work_id))) %>% 
  left_join(sf.hh.sf %>% st_set_geometry(., NULL) %>% 
              dplyr::select(-hh_race), 
            by = c("sp_hh_id" = "sp_id")) %>% 
  dplyr::select(-c(stcotrbg)) %>% 
  rename("sp_res_id" = "sp_hh_id",
         "cbg" = "GEOID10")  

sf.full <- bind_rows(sf.ppl.res, sf.gq.res) %>% 
  left_join(sf.work %>% 
              dplyr::select(sp_id, work_loc) %>% 
              mutate(sp_id = sp_id), 
            by = c("work_id" = "sp_id")) %>% 
  left_join(sf.scl %>% 
              dplyr::select(sp_id, stco) %>% 
              mutate(sp_id = sp_id) %>% 
              dplyr::select(-stco),
            by = c("school_id" = "sp_id"))

#Reduce race categories
sf.full <- sf.full %>% 
  mutate(race = case_when(race == 1 ~ 1,        # White
                          race == 2 ~ 2,        # Black
                          race %in% c(8,9) ~ 3, # Hispanic/Latinx
                          race %in% c(5,6) ~ 4, # Asian & Pacific islander
                          race == 3 ~ 5,      # Native American
                          race == 4 ~ 6,      # Other
                          race == 7 ~ 7))     # Two or more

#----------------------------------------
# Load Mission study data for info on essential workers
#----------------------------------------
mission <- read.csv("CMH/Test_Disparities/Data/secure/MissionStudy_Analytic_Dataset_Without_PHI.csv") %>% 
  mutate(essential = ifelse((workimpact != 1 & occupation %in% c(3,4)) | (workimpact == 2 & occupation != 1), 1, 0))

essential_glm <- glm(essential ~ as.factor(income) + as.factor(ethnicity),
                     data = mission, family = "binomial")

predict_essential <- function(income, ethnicity){
  ess_prob <- exp(predict(essential_glm, newdata = data.frame(income = income, ethnicity = ethnicity)))
  return(as.numeric(dqrng::dqrunif(1,0,1) < ess_prob))
}

#----------------------------------------
set.seed(430)
# data.table of agents
  agents <- data.table(
    id = 1:nrow(sf.full),
    age = sf.full$age,
    race = sf.full$race,
    state = 'S',
    nextstate = 'S',
    tnext = 0,
    t_infection = 0,
    test_prob = 0,
    tested = 0,
    t_since_test = 0,
    residence = sf.full$sp_res_id,
    residence_type = sf.full$gq_type,
    network = sf.full$relate,
    lat = sf.full$latitude,
    lon = sf.full$longitude,
    income = sf.full$hh_income,
    work = sf.full$work_loc,
    school = sf.full$school_id,
    cbg = sf.full$cbg,
    zip = sf.full$zip
  )
  
# No negative incomes
  agents[income <0, income:=0]

# Add age groups
  agents[, age_group5:=cut(agents$age, c(seq(0,75, by = 5),120), labels = FALSE, include.lowest = TRUE)]
  agents[, age_group10:=cut(agents$age, c(seq(0,70, by = 10),120), labels = FALSE, include.lowest = TRUE)]
   
# Add individuals per residence  
  agents[, res_size:=.N, by=residence]
  
# Add kids per residence  
  agents[, res_kids:=sum(age %in% c(0:18)), by=residence]

# Add adults per residence  
  agents[, res_adults:=sum(age %in% c(19:64)), by=residence]
    
# Add elders per residence  
  agents[, res_elders:=sum(age %in% c(65:120)), by=residence]
  
# Add income bracket  
  agents[residence_type == "H", income_bracket:=cut(income, c(0, 50, 100, 10000)*1000, labels = FALSE, include.lowest = TRUE)]
  
# Add workplace size
  agents[work > 0, work_size:=.N, by = work]

# Add offices to workplaces
  agents[work > 0, office:=sapply(work_size, function(w) sample(1:floor(w/10), 1))]
  agents[work > 0, office_id:=paste0(work, "_", office)]
  agents[work > 0, office_size:=.N, by = office_id]
  
  agents[,office:=NULL]
    
# Add essential workers  
  agents[work > 0, essential:=mapply(predict_essential, income_bracket, race)]
  
  sum(agents$essential,na.rm = T)/nrow(agents %>% filter(work>0))
  
# Add school size
  agents[school > 0, school_size:=.N, by = school]
  
# Add students to classrooms  
  agents[school > 0, school_ages:=cut(age, seq(2, 18, 2), labels = F)]
  agents[school > 0, school_age_id:=paste0(school, "_",school_ages)]
  agents[school > 0, school_age_size:=.N, by = school_age_id]
  agents[school > 0, class:=sapply(school_age_size, function(s) sample(1:floor(s/25), 1))]
  agents[school > 0, class_id:=paste0(school_age_id, "_", class)]
  agents[school > 0, class_size:=.N, by = class_id]
  
  agents[, c("school_ages", "school_age_id", "school_age_size", "class"):=NULL]
  
# Add cbg population  
  agents[, comm_size:=.N, by = cbg]
  
# Add income per cbg
  agents[, comm_income:=median(income, na.rm = T), by = cbg]
  
# Add income brackets in nbhds
  agents[, comm_bracket:=cut(comm_income, quantile(comm_income, probs = 0:4/4, na.rm = T), labels = FALSE, include.lowest = TRUE)]
  
  saveRDS(agents, "CMH/ABM/data/sf_synthetic_agents_cbg_dt.rds")
