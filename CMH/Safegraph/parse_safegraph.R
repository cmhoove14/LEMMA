library(data.table)
library(readr)
library(jsonlite)

GetCounty <- function(block_group) substr(block_group, start = 3, stop = 5)

ToCounty1 <- function(origin, visits) {
  visits_vec <- unlist(jsonlite::fromJSON(visits))
  data.table(origin, visits = names(visits_vec), num_visits = visits_vec)
}

ToCounty <- function(row, dt) {
  ToCounty1(dt[row, origin_census_block_group], dt[row, destination_cbgs])
}

# Functions returns number of devices in i with home in j on day t
CountyConnects <- function(csv){
  sfgrph <- data.table(readr::read_csv(csv))
  sfgrph[, state_fips:=substr(origin_census_block_group, start = 1, stop = 2)]
  sfgrph[, cnty_fips:=substr(origin_census_block_group, start = 3, stop = 5)]
  sfgrph[, ct_fips:=substr(origin_census_block_group, start = 1, stop = 11)]
  
  sfgrph_ca <- sfgrph[state_fips == "06"]

  dt <- rbindlist(lapply(1:nrow(sfgrph_ca), ToCounty, dt = sfgrph_ca))
  dt[, origin_county := GetCounty(origin)]
  dt[, visits_county := GetCounty(visits)]
  dt[, visits_state := substr(visits, 1, 2)]
  dt <- dt[visits_state == "06"] #we might want to do something with this later

  dt2 <- dt[, .(num_visits = sum(num_visits)), by = c("origin_county", "visits_county")]
  dt2 <- data.table::dcast(dt2, formula = "origin_county ~ visits_county", value.var = "num_visits")
  stopifnot(all.equal(dt2$origin_county, colnames(dt2)[-1])) #first column is "origin_county"
  mat <- as.matrix(dt2[, -1])
  rownames(mat) <- dt2[, origin_county]
  mat[is.na(mat)] <- 0
  
  return(mat)

}

files <- list.files("2020")
csvs <- files[grepl(".csv", files)]

dates <- as.Date(substr(csvs, 1, 10))

start <- min(dates)
end <- max(dates)

days <- as.integer(end-start)

fill <- array(data = NA, dim = c(58,58,days))

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

saveRDS(fill, paste0("CACountyMvmt", start, "to", end, ".rds"))
