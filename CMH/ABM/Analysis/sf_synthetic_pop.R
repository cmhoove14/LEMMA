require(sf)
require(tidycensus)
require(tidyverse)
require(data.table)

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
# Get SF census tracts shapefile and determine neighborhood matrix
#----------------------------------------
gc()
sf.sf <- tigris::tracts("06",
                        "075",
                        year = 2010,
                        class = "sf")

sf.sf <- sf.sf %>% 
  filter(AWATER10 < 5e7)
#Save crs to match coordinates

sf.crs <- st_crs(sf.sf)

# Hexagonal grid of 10km^2 area for neighborhoods
sf.grid.10km <- st_make_grid(sf.sf, cellsize = .001087,
                             what = "centers", square = FALSE) %>% 
  st_as_sf() %>% 
  mutate(nbhd = row_number())

#st_area(sf.grid.10km)
#plot(sf.grid.10km)

#----------------------------------------
# Determine neighborhoods of households
#----------------------------------------
sf.hh.sf <- sf.hh %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.grid.10km)

# Get neighborhood matrix
sf.sp <- as(sf.grid.10km, "Spatial")
sf.coords <- sp::coordinates(sf.sp)  # Coordinates of polygon centers
sf.nbs <- spdep::poly2nb(sf.sp)  # Neighbour relationships based on triangulation
sf.wmat <- spdep::nb2mat(sf.nbs)  # Weight matrix between the nodes.
saveRDS(sf.wmat, "CMH/ABM/data/sf_nbhd_distance_matrix.rds")

#----------------------------------------
# Determine nbhds of workplaces
#----------------------------------------
sf.work.sf <- sf.work %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.grid.10km)

#----------------------------------------
# Determine nbhds of schools
#----------------------------------------
sf.scl.sf <- sf.scl %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.grid.10km)

#----------------------------------------
# Determine nbhds of group quarters
#----------------------------------------
sf.gq.sf <- sf.gq %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.grid.10km)

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
# Merge datasets
#----------------------------------------
# Group quarters people to their quarters
sf.gq.res <- sf.gq.ppl %>% 
  mutate(race = NA,   # Add columns necessary for join to sf.ppl
         relate = NA, 
         school_id = "X",
         work_id = "X") %>% 
  left_join(sf.gq.sf %>% st_set_geometry(.,NULL) %>% 
              mutate(hh_income = NA), 
            by = c("sp_gq_id" = "sp_id")) %>% 
  rename("sp_res_id" = "sp_gq_id",
         "ct" = "stcotrbg") %>% 
  dplyr::select(-persons) %>% 
  dplyr::relocate(hh_income, .after = ct)

#People to their workplaces and schools
sf.ppl.res <- sf.ppl %>% 
  mutate(gq_type = "H") %>% 
  left_join(sf.hh.sf %>% st_set_geometry(., NULL) %>% 
              dplyr::select(-hh_race), 
            by = c("sp_hh_id" = "sp_id")) %>% 
  rename("sp_res_id" = "sp_hh_id",
         "ct" = "stcotrbg")  

sf.full <- bind_rows(sf.ppl.res, sf.gq.res) %>% 
  left_join(sf.work %>% 
              dplyr::select(sp_id, work_loc) %>% 
              mutate(sp_id = as.character(sp_id)), 
            by = c("work_id" = "sp_id")) %>% 
  left_join(sf.scl %>% 
              dplyr::select(sp_id, stco) %>% 
              mutate(sp_id = as.character(sp_id)) %>% 
              dplyr::select(-stco),
            by = c("school_id" = "sp_id")) %>% 
  mutate(GEOID = str_c("0", substr(as.character(ct), 1, nchar(as.character(ct))-1)))

sf.full %>% filter(school_id != "X") %>% 
  mutate(child_grp = cut(age, seq(2, 18, by = 2), labels = FALSE),
         school_age = paste0(school_id, "_", child_grp)) %>% 
  group_by(school_age) %>% 
  summarise(n_students = n()) %>% arrange(-n_students)

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
    q_prob = rbeta(nrow(sf.full), 5, 2),
    sociality = rbeta(nrow(sf.full), 2, 2),
    residence = sf.full$sp_res_id,
    residence_type = sf.full$gq_type,
    income = sf.full$hh_income,
    work = sf.full$work_loc,
    school = sf.full$school_id,
    nbhd = sf.full$nbhd,
    ct = sf.full$GEOID
  )
  
# No negative incomes
  agents[income <0, income:=0]

# Add age groups
  agents[, age_group5:=cut(agents$age, c(seq(0,75, by = 5),120), labels = FALSE, include.lowest = TRUE)]
    
# Add individuals per residence  
  agents[, res_size:=.N, by=residence]
  
# Add kids per residence  
  agents[, res_kids:=sum(age %in% c(0:18)), by=residence]

# Add adults per residence  
  agents[, res_adults:=sum(age %in% c(19:64)), by=residence]
    
# Add elders per residence  
  agents[, res_elders:=sum(age %in% c(65:120)), by=residence]
  
# Add income per person in residences  
  agents[, income_pp:=income/res_size]
  
# Add income bracket  
  agents[residence_type == "H", income_bracket:=cut(income_pp, quantile(income_pp, probs = 0:4/4), labels = FALSE, include.lowest = TRUE)]
  
# Add workplace size
  agents[work != "X", work_size:=.N, by = work]

# Add offices to workplaces
  agents[work != "X", office:=sapply(work_size, function(w) sample(1:floor(w/10), 1))]
  agents[work != "X", office_id:=paste0(work, "_", office)]
  agents[work != "X", office_size:=.N, by = office_id]
  
  agents[,office:=NULL]
    
# Add income per workplace  
  agents[work != "X", work_income:=median(income, na.rm = T), by = work]
  
# Add school size
  agents[school != "X", school_size:=.N, by = school]
  
# Add students to classrooms  
  agents[school != "X", school_ages:=cut(age, seq(2, 18, 2), labels = F)]
  agents[school != "X", school_age_id:=paste0(school, "_",school_ages)]
  agents[school != "X", school_age_size:=.N, by = school_age_id]
  agents[school != "X", class:=sapply(school_age_size, function(s) sample(1:floor(s/25), 1))]
  agents[school != "X", class_id:=paste0(school_age_id, "_", class)]
  agents[school != "X", class_size:=.N, by = class_id]
  
  agents[, c("school_ages", "school_age_id", "school_age_size", "class"):=NULL]
  
# Add nbhd population  
  agents[, comm_size:=.N, by = nbhd]
  
# Add income per nbhd
  agents[, comm_income:=median(income, na.rm = T), by = nbhd]
  
# Add income brackets in nbhds
  agents[, comm_bracket:=cut(comm_income, quantile(comm_income, probs = 0:4/4, na.rm = T), labels = FALSE, include.lowest = TRUE)]
  
  saveRDS(agents, "CMH/ABM/data/sf_synthetic_agents_dt.rds")
