library(here)
library(tidyverse)
library(geojsonsf)
library(readxl)

# Get SF testing sites 
sf_test_sites <- geojson_sf("https://data.sfgov.org/resource/dtit-7gp4.geojson?$where=point+is+not+null")

# de identified testing dataset, inital clean of zip codes
# CHANGE FILE PATH
sf_test_deID<-as.data.frame(read_excel(path="C:/Users/Sophia/Documents/COVID Research/SFTesting_08_27_2020_deID.xlsx")) %>%
  mutate(zip5 = substr(ZIP, 1, 5))
# sf_test_deID <- read_csv("CMH/Test_Disparities/Data/secure/SFTesting_08_27_2020_deID.csv") %>% 
#   mutate(zip5 = substr(ZIP, 1, 5))

# Task 1: Clean the dates for `birth`, `specdate`, and `specdatefilled` columns ----------------------------------
# Might have to go back to the csv/excel for this as it seems to be in a weird format.
# Goal is to have it in canonical yyyy-mm-dd form

# Task 2: See what you can do in terms of matching up testing facilities in sf_test_deID with those in the sf_test_sites----------------------------
# We want to compare who's getting testing and what their positivity rates are across different testing facilities, so need to come up with a way to 
# classify the testing facilities. First thing you might try is matching up the `OrderingFacilityName` in the sf_test_deID dataset with the "name" 
# column in the sf_test_sites dataset. Could also try doing this based on the address columns in each dataset (example code below). We ultimately 
# want a couple columns: `Facility type` e.g. hospital, pharmacy, clinic, COVID specific testing site, etc. ; `Facility eligibility` 
# (this is already in the `sf_test_sites` dataset as ligibility and as `location_type`) based on whether specific health insurance is required or not. 
# The sf_test_sites call to the url above also only pulls currently available testing sites in SF, so might also look at their website and see if there's 
# any archive of additional testing sites that are not currently operating

# filter out testing data from outside of California, and not from SF
only_sf<-sf_test_deID %>% filter(sf_test_deID$OrderingFacilityState=="CA")
only_sf<-only_sf %>% filter(tolower(only_sf$OrderingFacilityCity) %in% grep("(san francisco)|^(sf)", unique(tolower(sf_test_deID$OrderingFacilityCity)), value=TRUE)[1:4])

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
address<-gsub("pier 30 lot - lot #30", "pier 30", address)
address<-gsub("2333 buchanan", "2333 buchanan st", address)
address<-str_remove(address, "[,.-]")

site_address<-tolower(sf_test_sites$address)
site_address<-gsub("street", "st", site_address)
site_address<-gsub("avenue", "ave", site_address)
site_address<-gsub("boulevard", "blvd", site_address)
site_address<-gsub("1450 noriega", "1450 noriega st", site_address)
site_address<-gsub("(?<= (st)|(ave)|(ct)|(blvd))[^a-z].*$", "", site_address, perl = TRUE)

only_sf<-only_sf %>% mutate(cleaned_names=tolower(OrderingFacilityName)) %>% mutate(cleaned_address=address)
sf_test_sites<-sf_test_sites %>% mutate(cleaned_names=tolower(name)) %>% mutate(cleaned_address=site_address)

# merge testing data with testing sites
test_sites<-sf_test_sites %>% select(cleaned_address, eligibility, location_type, testing_hours, popup_or_permanent, sample_collection_method)
by_address<-left_join(only_sf, test_sites, by='cleaned_address')
by_address<-by_address %>% select(-c(ZIP, OrderingFacilityAddressStreet, OrderingFacilityCity, OrderingFacilityState, cleaned_names))

write.csv(by_address, file="C:/Users/Sophia/Documents/COVID Research/SFTesting_08_27_2020_deID_matchedsites.csv")

# `%notin%` <- Negate(`%in%`)
# unique(only_sf$cleaned_address[only_sf$cleaned_address %in% sf_test_sites$cleaned_address])
# unique(only_sf$cleaned_address[only_sf$cleaned_address %notin% sf_test_sites$cleaned_address])
# # number testing data unmatched to current sites
# sum(is.na(by_address$location_type))
# # number testing data matched to current sites
# sum(!is.na(by_address$location_type))

# sf_test_deID[sf_test_deID$OrderingFacilityAddressStreet %in% sf_test_sites$address,]
# sf_test_deID[sf_test_deID$OrderingFacilityAddressStreet %in% sf_test_sites$address,]


