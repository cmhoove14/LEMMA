library(ggplot2)
library(ggthemes)

#assume RunCACounties-SF was just run - sets max.date
date.set <- as.character(max.date - 12)
#date.set <- as.character(seq(as.Date("2020/3/15"), as.Date("2020/8/11"), by = "3 days"))
filestr <- paste0("../LEMMA_SF_run/Rt map ", as.character(max(date.set)))
grDevices::pdf(file = paste0(filestr, ".pdf"), width = 9.350, height = 7.225)
for (date1 in date.set) {
  print(date1)
  file.set <- list.files("Forecasts/", "*.xlsx")
  county.set <- gsub(".xlsx", "", file.set)
  dt <- data.table(county = county.set, Rt = NA_real_)
  for (county1 in county.set) {
    x <- as.data.table(readxl::read_excel(paste0("Forecasts/", county1, ".xlsx"), sheet = "rt"))
    rt1 <- x[date == date1, `50%`]
    if (length(rt1) == 0) {
      rt1 <- NA
    }
    dt[county == county1, Rt := rt1]
  }



  dt[, subregion := tolower(county)]
  ca <- map_data("county", "california")
  ca <- as.data.table(ca)

  ca <- merge(ca, dt, by = "subregion", all.x = T)
  ca[, Rt := pmin(1.5, Rt)]
  ca[, Rt := pmax(0.8, Rt)]
  print(ggplot(ca, aes(long, lat)) + geom_polygon(aes(group = group, fill = Rt), colour = "black") + theme_map() + scale_fill_gradient(low = "green2", high = "red", limits = c(0.8, 1.5)) + ggtitle(date1))
}
dev.off()

bay.area <- c("San Francisco", "San Mateo",  "Alameda", "Contra Costa", "Santa Clara", "Marin")
print(dt[county %in% bay.area, .(county, Re = round(Rt, 2))])
write.csv(dt[!is.na(Rt), .(county, Rt = round(Rt, 2))], file = paste0(filestr, ".csv"), row.names = F)
