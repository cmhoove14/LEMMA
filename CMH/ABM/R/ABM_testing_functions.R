#' @title Testing probability
#'  
#' @description Estimate testing probability as function of agent characteristics and infection status
#' 
#' @param inf.status Infection status 
#' @param income_bracket income
#' @param age age
#' @param res_type type of residence
#' @param res_inf residence infection status
#' @param last_test time since person was last tested
#' 
#' @return sampling weight for whether or not tested
#' @export
#'        

test_prob <- function(inf.status, income_bracket, age, res_type, res_inf, last_test){
  if(inf.status %in% c("S", "E", "Ip", "Ia", "R")){
    inf.weight = 1  # no symptoms base weight
  } else if(inf.status %in% c("Im", "Imh")){
    inf.weight = 10 # symptoms 10 times more likely for testing
  } else {
    inf.weight = 50 # hospitalized 50 times more likely for testing
  }
  
# Households with higher incomes
# Nursing homes and prisons more likely to be tested  
  if(res_type == "H"){
    res.weight = 1
    income.weight = income_bracket
  } else if(res_type == "C"){
    res.weight = 1
    income.weight = 1
  } else {
    res.weight = 10
    income.weight = 1
  }
  
# Older age groups more likely to be tested  
  age.weight = age/10
# Individuals with known infection in household more likely to be tested  
  hh.inf.weight = res_inf*10
# Individuals without a recent test more likely to be tested  
  last.test.weight = last_test/28
  
  samp_weight_rate <- inf.weight+res.weight+income.weight+age.weight+hh.inf.weight+last.test.weight
  
  return(rpois(1, samp_weight_rate))
}

#' @title False Negative Rate
#'  
#' @description Estimate probability of testing positive given true positive. Depends on function `pcr_sens_fun` which takes days since infection as input and returns probability of positive test given true positive.
#' 
#' @param inf.status Infection status 
#' @param t.since.infection time since being infected
#' 
#' @return binary of whether test is positive 1 or negative 0
#' @export
#'        

test_sens <- function(inf.status, t.since.infection){
  ifelse(inf.status %in% c("S", "D", "R"),
         0, rbinom(1,1,pcr_sens_fun(t.since.infection)))
}

#' @title Quarantine probability
#'  
#' @description Estimate probability someone will stay home
#' 
#' @param inf.status Infection status 
#' @param test.status hh infection status
#' @param income income
#' @param age age
#' @param res_type type of residence
#' @param hh.inf hh infection status
#' 
#' @return sampling weight for whether or not tested
#' @export
#'        

quar_prob <- function(inf.status, income, age, race, res_type, hh.inf){
  if(inf.status %in% c("S", "E", "Ip", "Ia", "R")){
    inf.weight = 1
  } else if(inf.status %in% c("Im", "Imh")){
    inf.weight = 10
  } else {
    inf.weight = 50
  }
  
  if(res_type == "H"){
    res.weight = 1
    income.weight = income/1e5
    race.weight = ifelse(race == 1, 2, 1)
  } else if(res_type == "C"){
    res.weight = 1
    income.weight = 1
    race.weight = 1
  } else {
    res.weight = 10
    income.weight = 1
    race.weight = 1
  }
  
  age.weight = age/10
  hh.inf.weight = hh.inf*10
  
  samp_weight_rate <- inf.weight+res.weight+income.weight+age.weight+race.weight+hh.inf.weight
  
  return(rpois(1, samp_weight_rate))
}
