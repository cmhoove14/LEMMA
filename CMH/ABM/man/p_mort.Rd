% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ABM_infection_functions.R
\name{p_mort}
\alias{p_mort}
\title{Probability of death}
\usage{
p_mort(age)
}
\arguments{
\item{age}{age of person}
}
\value{
numeric of probability of dying.
}
\description{
Age-dependent probabilities of dying given hospitalization (Ih) Used in `next.state`; from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified
}
