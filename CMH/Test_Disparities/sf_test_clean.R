library(tidyverse)
library(lubridate)
library(readxl)
library(geojsonsf)

# -------------------------------- 
# Data Load 
# -------------------------------- 

# Get SF testing sites 
sf_test_sites <- geojson_sf("https://data.sfgov.org/resource/dtit-7gp4.geojson?$where=point+is+not+null")

#Get SF testing data
sf_test_8_27 <- read_csv("CMH/Test_Disparities/Data/secure/ModelingTestingDataSet_08_27_2020.csv")

# -------------------------------- 
# Data Clean geography, dates, race/eth
# -------------------------------- 
# filter out testing data from outside of California, and not from SF
only_sf<-sf_test_8_27 %>% 
  filter(OrderingFacilityState=="CA")

only_sf<-only_sf %>% 
  filter(tolower(only_sf$OrderingFacilityCity) %in% grep("(san francisco)|^(sf)", unique(tolower(sf_test_8_27$OrderingFacilityCity)), value=TRUE)[1:4])

#Birthdate and test date cleaning
# Specdate has some errors and missingness while specdate filled appears to have no errors and is completely filled so will use it as the date the test was conducted
only_sf <- only_sf %>% 
    mutate(birthdate = as.Date(birth),
           testdate = as.Date(specdatefilled),
           testmonth = lubridate::month(testdate),
           testweek = lubridate::week(testdate),
           age = lubridate::time_length((testdate-birthdate), "years"),
           age_group5 = cut(age, c(seq(0,75, by = 5),120), labels = FALSE, include.lowest = TRUE),
           age_group10 = cut(age, c(seq(0,70, by = 10),120), labels = FALSE, include.lowest = TRUE),
           age_group = cut(age, c(0,18,65,120), labels = FALSE, include.lowest = TRUE))

# Race data
# Function from Josh
CodeRace2 <- function(race) {
  race <- as.integer(race)
  stopifnot(race %in% c(NA, 0:11))
  race2 <- rep("other", length(race)) # 4=Native American, 7=Multiethnic, 10="Other"
  race2[race == 5] <- "Latinx"
  race2[race == 1] <- "Black"
  race2[race == 6] <- "White"
  race2[race %in% c(2, 3)] <- "Asian"
  race2[race %in% c(NA, 8, 9, 11)] <- NA_character_
  return(race2)
}

only_sf$RaceEthnicityCode <- CodeRace2(only_sf$RaceEthnicity)

# -------------------------------- 
# Organize testing sites/facilities
# -------------------------------- 

# string data cleaning
address<-tolower(only_sf$OrderingFacilityAddressStreet)
address<-gsub("street", "st", address)
address<-gsub("avenue", "ave", address)
address<-gsub("boulevard", "blvd", address)
address<-gsub("portreo", "portero", address)
address<-gsub("burhnham", "burnham", address)
address<-gsub("laguna honda blvd", "laguna honda", address)
address<-gsub("plz", "plaza", address)
address<-gsub("(?<= (st)|(ave)|(ct)|(blvd))[^a-z].*$", "", address, perl = TRUE)
address<-gsub("pier 30 lot - lot #30", "599 the embarcadero", address)   #Edited this to reflect pier 30=embarcadero testing site
address<-gsub("2333 buchanan", "2333 buchanan st", address)
address<-str_remove(address, "[,.-]")

site_address<-tolower(sf_test_sites$address)
site_address<-gsub("street", "st", site_address)
site_address<-gsub("avenue", "ave", site_address)
site_address<-gsub("boulevard", "blvd", site_address)
site_address<-gsub("1450 noriega", "1450 noriega st", site_address)
site_address<-gsub("(?<= (st)|(ave)|(ct)|(blvd))[^a-z].*$", "", site_address, perl = TRUE)
site_address <- gsub("pier 30", "599 the embarcadero", site_address)

only_sf<-only_sf %>% 
  mutate(cleaned_names=tolower(OrderingFacilityName)) %>% 
  mutate(cleaned_address=address)

sf_test_sites<-sf_test_sites %>% 
  mutate(cleaned_names=tolower(name)) %>% 
  mutate(cleaned_address=site_address)

# merge testing data with testing sites
test_sites<-sf_test_sites %>% dplyr::select(cleaned_address, eligibility, location_type, testing_hours, popup_or_permanent, sample_collection_method)
sf_tests_sites<-left_join(only_sf, test_sites, by='cleaned_address')
sf_tests_sites<-sf_tests_sites %>% dplyr::select(-c(ZIP, OrderingFacilityAddressStreet, OrderingFacilityCity, OrderingFacilityState))

# Test facility type
test_address <- sf_tests_sites$cleaned_address
agency <- tolower(sf_tests_sites$OrderingFacilityName)
test_agency <- rep("other", nrow(sf_tests_sites))
test_facility <- rep("other", nrow(sf_tests_sites))

# SF Appointment only public test sites
  test_agency[grepl("599 the embarcadero", test_address)] <- "sf_appt"  # Embarcadero test site
  test_agency[grepl("600 7th st", test_address)] <- "sf_appt"  # SOMA test site

  test_facility[grepl("599 the embarcadero", test_address)] <- "sf_embarcadero"  # Embarcadero test site
  test_facility[grepl("600 7th st", test_address)] <- "sf_soma"  # SOMA test site

# SF Neighborhood test sites from here https://sf.gov/find-out-about-your-covid-19-testing-options
  test_agency[grepl("3850 17th st", test_address)] <- "sf_nbhd"  # Castro mission
  test_agency[grepl("845 jackson st", test_address)] <- "sf_nbhd"# Chinese hospital
  test_agency[grepl("1181 golden gate ave", test_address)] <- "sf_nbhd" # Maxine hall
  test_agency[grepl("240 shotwell st", test_address)] <- "sf_nbhd"  # Mission nbhd health
  test_agency[grepl("1050 wisconsin", test_address)] <- "sf_nbhd"  # Potrero hill 
  test_agency[grepl("2401 keith st", test_address)] <- "sf_nbhd"  # southeast health
  test_agency[grepl("330 ellis", test_address)] <- "sf_nbhd"  # Tenderloin testing at Glide
  test_agency[grepl("1001 potrero", test_address)] <- "sf_nbhd"  # ZSFG
  test_agency[grepl("799 moscow st", test_address)] <- "sf_nbhd"  # excelsior testing site in august

  test_facility[grepl("3850 17th st", test_address)] <- "sf_castro"  # Castro mission
  test_facility[grepl("845 jackson st", test_address)] <- "sf_chinese"# Chinese hospital
  test_facility[grepl("1181 golden gate ave", test_address)] <- "sf_maxine" # Maxine hall
  test_facility[grepl("240 shotwell st", test_address)] <- "sf_mission"  # Mission nbhd health
  test_facility[grepl("1050 wisconsin", test_address)] <- "sf_potrero"  # Potrero hill 
  test_facility[grepl("2401 keith st", test_address)] <- "sf_missionbay"  # southeast health
  test_facility[grepl("330 ellis", test_address)] <- "sf_tenderloinglide"  # Tenderloin testing at Glide
  test_facility[grepl("1001 potrero", test_address)] <- "sf_zsfg"  # ZSFG
  test_facility[grepl("799 moscow st", test_address)] <- "sf_excelsior"  # excelsior testing site
    
# SF_CCC sites
  test_agency[agency %in% c("healthright 360",
                            "native american hc",
                            "south of market health center",
                            "san francisco free clinic")] <- "sf_ccc"
# Northeast Medical Services (NEMS) sf_ccc sites
  test_agency[grepl("1520 stockton st", test_address)] <- "sf_ccc"
  test_agency[grepl("1450 noriega st", test_address)] <- "sf_ccc"

  
# Private insurance testing based on ordering agency
  test_agency[grepl("sutter", agency)] <- "sutter"
  test_agency[grepl("cpmc", agency)] <- "sutter"
  test_agency[grepl("one medical", agency)] <- "one_medical"
  test_agency[grepl("2175 market st", test_address)] <- "one_medical"
  test_agency[grepl("kaiser", agency)] <- "kaiser"
  test_agency[grepl("sanfranmedicalcntr", agency)] <- "kaiser"
  test_agency[grepl("kfh", agency)] <- "kaiser"
  test_agency[grepl("1600 owens st", test_address)] <- "kaiser"
  test_agency[grepl("ucsf", agency)] <- "ucsf"
  test_agency[grepl("carbon", agency)] <- "carbon"

  test_facility[grepl("sutter", agency)] <- "sutter"
  test_facility[grepl("cpmc", agency)] <- "sutter"
  test_facility[grepl("one medical", agency)] <- "one_medical"
  test_facility[grepl("2175 market st", test_address)] <- "one_medical"
  test_facility[grepl("kaiser", agency)] <- "kaiser"
  test_facility[grepl("sanfranmedicalcntr", agency)] <- "kaiser"
  test_facility[grepl("1600 owens st", test_address)] <- "kaiser"
  test_facility[grepl("kfh", agency)] <- "kaiser"
  test_facility[grepl("ucsf", agency)] <- "ucsf"
  test_facility[grepl("carbon", agency)] <- "carbon"
    
# Check on remaining unassigned test agencies  
sf_tests_sites %>% 
  mutate(test_agency = test_agency) %>% 
  filter(test_agency == "other") %>% 
  group_by(OrderingFacilityName, cleaned_address) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% View()

# SFDPH at 50 ivy st counted as neighborhood testing site
  test_agency[grepl("50 ivy st", test_address)] <- "sf_nbhd"

#SFDPH Bayview testing site (seems to have been discontinued?)
  test_agency[grepl("1751 carroll ave", test_address)] <- "sf_nbhd"	

#SFDPH Sunnyvale testing site (seems to have been discontinued?)
  test_agency[grepl("1652 sunnydale ave", test_address)] <- "sf_nbhd"	
    
#SFDPH Mission testing site with different address/Thursdays only testing site
  test_agency[grepl("701 alabama st", test_address)] <- "sf_nbhd"	

#SFDPH OMI testing site
  test_agency[grepl("50 broad st", test_address)] <- "sf_nbhd"	
    
#SFDPH Potrero site with different address 
  test_agency[grepl("107 dakota st", test_address)] <- "sf_nbhd"	
  
# Laguna Honda skilled nursing facility
  test_agency[grepl("375 laguna honda", test_address)] <- "sf_lagunahonda"
  
# Dignity health: St Mary's and St francis hospitals
  test_agency[grepl("900 hyde st", test_address)] <- "dignity"
  test_agency[grepl("2250 hayes st", test_address)] <- "dignity"
  test_agency[grepl("1199 bush st", test_address)] <- "dignity" # Bayspring medical group in St. Francis hospital
  test_agency[grepl("1801 divisadero st", test_address)] <- "dignity" # Lower Pac Heights urgent care
  test_agency[grepl("2288 market st", test_address)] <- "dignity" # Castro Dignity urgent care
  test_agency[grepl("1085 valencia st", test_address)] <- "dignity" # Valencia Dignity urgent care
  test_agency[grepl("199 west portal ave", test_address)] <- "dignity" # West Portal Dignity urgent care
  test_agency[grepl("2395 lombard st", test_address)] <- "dignity" # Lombard Dignity urgent care
  test_agency[grepl("2895 diamond st", test_address)] <- "dignity" # Glen Park Dignity urgent care
  test_agency[grepl("st marys", agency)] <- "dignity"
  test_agency[grepl("st mary's", agency)] <- "dignity"
  test_agency[grepl("st. marys", agency)] <- "dignity"
  test_agency[grepl("st francis", agency)] <- "dignity"
  
# Carbon Health
  test_agency[grepl("1998 market st", test_address)] <- "carbon" 

# Circle Medical
  test_agency[grepl("333 1st st", test_address)] <- "circle" 
  
# City bay urgent care  
  test_agency[grepl("2131 irving st", test_address)] <- "city_bay_urgent" 
    
# CVS
  test_agency[grepl("377 32nd ave", test_address)] <- "cvs" 
  
# Forward Health
  test_agency[grepl("180 sutter st", test_address)] <- "fwd_health" 
  
# Sutter Pacific Medical Foundation Lab (SPMFLAB)/Sutter institute for health and healing https://www.sutterhealth.org/find-location/facility/institute-for-health-healing-2300-california-street  
  test_agency[grepl("2300 california st", test_address)] <- "sutter"
  
# Check again
sf_tests_sites %>% 
  mutate(test_agency = test_agency) %>% 
  filter(test_agency == "other") %>% 
  group_by(OrderingFacilityName, cleaned_address) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% View()
  
# Mobile collection site appears to be testing alot of people in hospitals, not sure what else is in that category
# 1600 Owens ave "SanFranciscoMission" seems to be a Kaiser facility in MIssion Bay, but not certain

# Finalize addition of test agency info  
sf_tests_sites <- sf_tests_sites %>% 
  mutate(test_agency = test_agency)  
  
