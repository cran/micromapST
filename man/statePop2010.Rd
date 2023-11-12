\name{statePop2010}
\docType{data}
\alias{statePop2010}
\title{US State Population for 2010}
\description{
   US State 2010 population data by race and Hispanic ethnicity.
}
\usage{data(statePop2010)}
\format{
  A data frame with 51 observations (one per state) on the following 6 variables per state:
  \describe{
    \item{Hisp}{an integer count of the Hispanic population}
    \item{white}{an integer count of the white population}
    \item{black}{an integer count of the black population}
    \item{OtherWBH}{an integer count of the population other than white, 
    black or Hispanic}
    \item{pctHisp}{a numeric percentage of the Hispanic population to the 
    total population}
    \item{pctOtherWBH}{a numeric percentage of the population other than white, 
    black, or Hispanic}
  }
  Each row has the state 2 character abbreviation as its row name.
}
\details{
   The dataset contains 51 records, one for each state. The data represents 
   the population count or percentage of the total population 
   by race and Hispanic ethnicity within
   the state. This dataset is used by the 
   micromapSEER examples with the "USStatesDF" border group.
}
\source{United States Census Bureau, Population Total by State, by Race, Combinations of Two Races, 
 and not Hispanic or Latino, 2010 (Summary File 1, Table QT-P4), URL = http://factfinder2.census.gov/.
}
\keyword{datasets}
