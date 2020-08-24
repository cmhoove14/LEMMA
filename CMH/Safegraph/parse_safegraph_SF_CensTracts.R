library(data.table)
library(readr)
library(jsonlite)

GetCT <- function(block_group) substr(block_group, start = 1, stop = 11)

ToCounty1 <- function(origin, visits) {
  visits_vec <- unlist(jsonlite::fromJSON(visits))
  data.table(origin, visits = names(visits_vec), num_visits = visits_vec)
}

ToCounty <- function(row, dt) {
  ToCounty1(dt[row, origin_census_block_group], dt[row, destination_cbgs])
}

sf_cts <- read_csv("Census_2010_Tracts.csv")
sf_ct_dt <- as.data.table(expand.grid(origin_cts = sf_cts$GEOID10,
                                      visit_cts = sf_cts$GEOID10))

# Functions returns number of devices in i with home in j on day t
CountyConnects <- function(csv){
  sfgrph <- data.table(readr::read_csv(csv))
  sfgrph[, state_cnty_fips:=substr(origin_census_block_group, start = 1, stop = 5)]
  sfgrph[, ct_fips:=substr(origin_census_block_group, start = 1, stop = 11)]
  
  sfgrph_sf <- sfgrph[state_cnty_fips == "06075"]

  dt <- rbindlist(lapply(1:nrow(sfgrph_sf), ToCounty, dt = sfgrph_sf))
  dt[, origin_ct := GetCT(origin)]
  dt[, visits_ct := GetCT(visits)]

  dt2 <- dt[substr(visits_ct,1,5)=="06075", .(num_visits = sum(num_visits)), by = c("origin_ct", "visits_ct")]
  
  dt2 <- merge.data.table(sf_ct_dt, dt2, all.x = TRUE,
                          by.x = c("origin_cts", "visit_cts"), 
                          by.y = c("origin_ct", "visits_ct"))
  
  dt2 <- data.table::dcast(dt2, formula = "origin_cts ~ visit_cts", value.var = "num_visits")
  stopifnot(all.equal(dt2$origin_ct, colnames(dt2)[-1])) #first column is "origin_county"
  
  mat <- as.matrix(dt2[, -1])
  rownames(mat) <- dt2$origin_ct
  mat[is.na(mat)] <- 0
  
  return(mat)

}

files <- list.files("2020")
csvs <- files[grepl(".csv", files)]

dates <- as.Date(substr(csvs, 1, 10))

start <- min(dates)
end <- max(dates)

days <- as.integer(end-start)

fill <- array(data = NA, dim = c(197,197,days))

for(i in 1:days){
  idate <- dates[i]
  
  ifile <- paste0("2020/", files[grepl(idate, files)])
    
  if(length(ifile) == 1){
    fill[,,i] <- CountyConnects(ifile)
  } else if(length(ifile) == 0){
    print("No file found for ", idate)
  } else{
    print(paste0("More than one file found for ", idate))
  }
  
}

saveRDS(fill, paste0("SFCensTractsMvmt", start, "to", end, ".rds"))
