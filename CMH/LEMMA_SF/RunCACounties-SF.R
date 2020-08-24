library(data.table)

setwd("../LEMMA-Forecasts")
input.file <- "Inputs/SF.xlsx"
run.now <- T #if F assume just ran RunCAcounties.R
if (run.now) {
  sf <- LEMMA::CredibilityIntervalFromExcel(input.file)
} else {
  sf <- county.results[["San Francisco"]]
}

max.date <- max(sf$inputs$obs.data$date)
outfile <- paste0("../LEMMA_SF_run/Outputs/SF-", max.date)
file.copy("Forecasts/San Francisco.pdf", paste0(outfile, ".pdf"), overwrite = T)
file.copy("Forecasts/San Francisco.xlsx", paste0(outfile, ".xlsx"), overwrite = T)
file.copy(input.file, paste0(outfile, "-input.xlsx"), overwrite = T)

source('../LEMMA_SF_run/Rt over time map - single.R')

if (F) {
  sf$inputs$model.inputs$end.date <- as.Date("2020/12/31")
  scen_nochange <- LEMMA:::ProjectScenario(sf, new.int=NULL, paste0(outfile, "_noChange"))

  value <- max(scen_nochange$posterior.quantiles$hosp[, "50%"])
  date <- names(which.max(scen_nochange$posterior.quantiles$hosp[, "50%"]))

  int.today <- data.table(mu_t_inter = max.date + 1, sigma_t_inter = 0.0001, mu_beta_inter = 0.7, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
  scen_today <- LEMMA:::ProjectScenario(sf, new.int=int.today, paste0(outfile, "_actToday_0.7"))

  int.twoweeks <- data.table(mu_t_inter = max.date + 15, sigma_t_inter = 0.0001, mu_beta_inter = 0.7, sigma_beta_inter = 0.0001, mu_len_inter = 7, sigma_len_inter = 2)
  scen_twoweeks <- LEMMA:::ProjectScenario(sf, new.int=int.twoweeks, paste0(outfile, "_actTwoWeeks_0.7"))

  ShortDate <- function(d) as.character(as.Date(d), format = "%b%e")

  GetText <- function(scen) {
    GetQ <- function(q) {
      hosp <- scen$posterior.quantiles$hosp[, paste0(q, "%")]
      hosp[1:(which(names(hosp) == max.date))] <- 0
      value <- round(max(hosp), -1)
      date <- names(which.max(hosp))
      deaths <- round(max(scen$posterior.quantiles$deaths[, paste0(q, "%")]), -1)
      list(value = value, date = ShortDate(date), deaths = deaths)
    }
    paste0("we project a median peak hospitalization of ", GetQ(50)$value, " on ", GetQ(50)$date, " and ", GetQ(50)$deaths, " deaths in 2020. 95% quantile: peak hospitalization of ", GetQ(95)$value, " on ", GetQ(95)$date, " and ", GetQ(95)$deaths, " deaths in 2020.\n")
  }

  cat("95% quantile Aug 1:\n", round(sf$posterior.quantiles$hosp["2020-08-01", "95%"], -1), "\n")
  today.plus30 <- as.character(max.date + 30)
  cat("\n\nWith no change", GetText(scen_nochange))
  cat("30 day forecast (", ShortDate(today.plus30), "): Median hospitalization ", round(scen_nochange$posterior.quantiles$hosp[today.plus30, "50%"], -1), ", 95% quantile hospitalization ", round(scen_nochange$posterior.quantiles$hosp[today.plus30, "95%"], -1), "\n", sep = "")
  cat("\nUnder an intervention that reduces Re by 30% starting today", GetText(scen_today))
  cat("\nUnder an intervention that reduces Re by 30% starting in two weeks", GetText(scen_twoweeks))
}

if (F) {
  #if SF deaths need updating
  source('Code/GetCountyData.R')
  dt=GetCountyData("")
  write.csv(dt[county == "San Francisco" & date >= as.Date("2020/3/23"), .(max.date=max(date))], row.names = F)
  write.csv(dt[county == "San Francisco" & date >= as.Date("2020/3/23"), .(deaths.conf)], row.names = F)
}
