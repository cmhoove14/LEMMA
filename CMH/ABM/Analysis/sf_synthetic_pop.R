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

# Get neighborhood matrix
sf.sp <- as(sf.sf, "Spatial")
sf.coords <- sp::coordinates(sf.sp)  # Coordinates of polygon centers
sf.nbs <- spdep::poly2nb(sf.sp)  # Neighbour relationships based on a triangulation
sf.wmat <- spdep::nb2mat(sf.nbs)  # Weight matrix between the nodes.
rownames(sf.wmat) <- sf.sf$GEOID
colnames(sf.wmat) <- rownames(sf.wmat)

sf.wmat.df <- as_tibble(sf.wmat) %>%
  mutate(GEOID = rownames(sf.wmat))

saveRDS(sf.wmat.df, "CMH/ABM/data/sf_ct_distance_matrix.rds")

# Visualize 
num_nbs <- length(sf.nbs)
threshold <- 0.1
sp::plot(sf.sp)
points(sf.coords, pch = 16, cex = 0.5)

adj_matrix <- sf.wmat>threshold
dimnames(adj_matrix) <- NULL

edges <- which(adj_matrix, arr.ind = TRUE)
apply(edges, 1, function(r){
  lines(sf.coords[r,1], sf.coords[r,2], col="blue")
})

#----------------------------------------
# Determine CTs of workplaces
#----------------------------------------
sf.work.sf <- sf.work %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf)

sp::plot(sf.sp)
  points(as(sf.work.sf, "Spatial"), pch = 17, cex = 0.4, col = "orange")

#----------------------------------------
# Determine CTs of group quarters
#----------------------------------------
sf.gq.sf <- sf.gq %>% 
  st_as_sf(., coords = c("longitude", "latitude")) %>% 
  st_set_crs(sf.crs) %>% 
  st_join(., sf.sf)

sp::plot(sf.sp)
  points(as(sf.gq.sf, "Spatial"), pch = 18, cex = 0.7, col = "darkred")

#----------------------------------------
# Distance matrix between workplaces and group quarters in order to assign some workers to working at group quarters
#----------------------------------------
work_gq_dist_mat <- st_distance(sf.work.sf, sf.gq.sf)
class(work_gq_dist_mat) <- "numeric"
  
#----------------------------------------
# Merge datasets
#----------------------------------------
sf.gq.res <- sf.gq.ppl %>% 
  mutate(race = NA,   # Add columns necessary for join to sf.ppl
         relate = NA, 
         school_id = "X",
         work_id = "X") %>% 
  left_join(sf.gq %>% mutate(hh_income = NA), 
            by = c("sp_gq_id" = "sp_id")) %>% 
  rename("sp_res_id" = "sp_gq_id",
         "res.lat" = "latitude",
         "res.lon" = "longitude",
         "ct" = "stcotrbg") %>% 
  dplyr::select(-persons) %>% 
  dplyr::relocate(hh_income, .after = ct)

sf.ppl.res <- sf.ppl %>% 
  mutate(gq_type = "H") %>% 
  left_join(sf.hh %>% dplyr::select(-hh_race), 
            by = c("sp_hh_id" = "sp_id")) %>% 
  rename("sp_res_id" = "sp_hh_id",
         "res.lat" = "latitude",
         "res.lon" = "longitude",
         "ct" = "stcotrbg")  

sf.full <- bind_rows(sf.ppl.res, sf.gq.res) %>% 
  left_join(sf.work %>% 
              mutate(sp_id = as.character(sp_id)) %>% 
              rename("work.lat" = "latitude",
                     "work.lon" = "longitude"), 
            by = c("work_id" = "sp_id")) %>% 
  left_join(sf.scl %>% 
              mutate(sp_id = as.character(sp_id)) %>% 
              rename("scl.lat" = "latitude",
                     "scl.lon" = "longitude") %>% 
              dplyr::select(-stco),
            by = c("school_id" = "sp_id")) %>% 
  mutate(GEOID = str_c("0", substr(as.character(ct), 1, nchar(as.character(ct))-1)))

#----------------------------------------
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
    q_prob = 0,
    residence = sf.full$sp_res_id,
    residence_type = sf.full$gq_type,
    income = sf.full$hh_income,
    work = sf.full$work_id,
    school = sf.full$school_id,
    comm = sf.full$GEOID
  )

# No negative incomes
  agents[income <0, income:=0]

# Add age groups
  agents[, age_group5:=cut(agents$age, c(seq(0,75, by = 5),105), labels = FALSE, include.lowest = TRUE)]
    
# Add individuals per residence  
  agents[, res_size:=.N, by=residence]
  
# Add kids per residence  
  agents[, res_kids:=sum(age %in% c(0:18)), by=residence]

# Add adults per residence  
  agents[, res_adults:=sum(age %in% c(19:64)), by=residence]
    
# Add elders per residence  
  agents[, res_elders:=sum(age %in% c(65:110)), by=residence]
  
# Add income per person in residences  
  agents[, income_pp:=income/res_size]
  
# Add income bracket  
  agents[residence_type == "H", income_bracket:=cut(income_pp, quantile(income_pp, probs = 0:4/4), labels = FALSE, include.lowest = TRUE)]
  
# Add workplace size
  agents[work != "X", work_size:=.N, by = work]
  
# Add income per workplace  
  agents[work != "X", work_income:=median(income, na.rm = T), by = work]
  
# Add school size
  agents[school != "X", school_size:=.N, by = school]
  
# Add CT population  
  agents[, comm_size:=.N, by = comm]
  
# Add income per CT
  agents[, comm_income:=median(income, na.rm = T), by = comm]
  
# Add income brackets in CTs
  agents[, comm_bracket:=cut(comm_income, quantile(comm_income, probs = 0:4/4), labels = FALSE, include.lowest = TRUE)]
  
  saveRDS(agents, "CMH/ABM/data/sf_synthetic_agents_dt.rds")
