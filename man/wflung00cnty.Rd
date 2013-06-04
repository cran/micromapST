\name{wflung00cnty}
\alias{wflung00cnty}
\docType{data}
\title{Lung cancer mortality data for white females, by county, 2000-4}
\description{
   Counts and rates of lung cancer mortality data among white women, 
   aggregated for 2000-4 by county.
}
\usage{wflung00cnty}
\format{
  A data frame with 2577 observations on the following 6 variables.
  \describe{
    \item{\code{fips}}{a numeric vector of 5 digit fips codes identifying the state and the county}
    \item{\code{rate}}{a numeric vector of age-adjusted rates by county during 2000-4 for white females}
    \item{\code{count}}{a numeric vector of the number of white female lung cancer deaths during 2000-4 by county}
    \item{\code{pop}}{a numeric vector of the white female population in the county during 2000}
    \item{\code{stcode}}{a numberic vector of the 2 digit state fips code}
    \item{\code{stabr}}{a character vector of the 2 character state postal code}
  }
}
\details{
  The rates on this file are directly age adjusted to the US 2000 
  standard population and are expressed as the number of 
  deaths per 100,000 person-years. Counties with from 1 to 9 deaths are suppressed (deleted from the file).
  This dataset is used by the micromapST examples.
}
\source{
  Surveillance Research Program, National Cancer Institute SEER*Stat 
  software (http://www.seer.cancer.gov/seerstat), November 2007 
  data submission, released April 2008. Data originally provided 
  to NCI by the National Center for Health Statistics.
}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting, LLC, Gaithersburg, MD}
\references{
   FIP 6-4 Codes
}
\keyword{datasets}
