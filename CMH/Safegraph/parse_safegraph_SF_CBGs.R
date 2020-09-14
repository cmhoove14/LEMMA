library(data.table)
library(readr)
library(jsonlite)

ToCBG1 <- function(origin, visits) {
  visits_vec <- unlist(jsonlite::fromJSON(visits))
  data.table(origin, visits = names(visits_vec), num_visits = visits_vec)
}

ToCBG <- function(row, dt) {
  ToCBG1(dt[row, origin_census_block_group], dt[row, destination_cbgs])
}

sf_cbgs <- read_csv("Census_2010_CBGs_SF.csv")
sf_cbg_dt <- as.data.table(expand.grid(origin_cbgs = sf_cbgs$GEOID10,
                                       visit_cbgs = sf_cbgs$GEOID10))

# Functions returns number of devices in i with home in j on day t
cbg_visits <- function(csv){
  sfgrph <- data.table(readr::read_csv(csv))
  sfgrph[, state_cnty_fips:=substr(origin_census_block_group, start = 1, stop = 5)]

  sfgrph_sf <- sfgrph[state_cnty_fips == "06075"]

  dt <- rbindlist(lapply(1:nrow(sfgrph_sf), ToCBG, dt = sfgrph_sf))

  dt2 <- merge.data.table(sf_cbg_dt, dt, all.x = TRUE,
                          by.x = c("origin_cbgs", "visit_cbgs"), 
                          by.y = c("origin", "visits"))
  
  dt2 <- data.table::dcast(dt2, formula = "origin_cbgs ~ visit_cbgs", value.var = "num_visits")
    stopifnot(all.equal(dt2$origin_cbgs, colnames(dt2)[-1])) 
  
  mat <- as.matrix(dt2[, -1])
  rownames(mat) <- dt2$origin_cbgs
  mat[is.na(mat)] <- 0
  
  return(mat)

}

files <- list.files("2020")
csvs <- files[grepl(".csv", files)]

dates <- as.Date(substr(csvs, 1, 10))

start <- min(dates)
end <- max(dates)

days <- as.integer(end-start)

fill <- array(data = NA, dim = c(nrow(sf_cbgs),nrow(sf_cbgs),days))

for(i in 1:days){
  idate <- dates[i]
  
  ifile <- paste0("2020/", files[grepl(idate, files)])
    
  if(length(ifile) == 1){
    fill[,,i] <- cbg_visits(ifile)
  } else if(length(ifile) == 0){
    print("No file found for ", idate)
  } else{
    print(paste0("More than one file found for ", idate))
  }
  
}

saveRDS(fill, paste0("SFCensBlockGroupsMvmt", start, "to", end, ".rds"))
