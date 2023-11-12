\name{wflung00and95}
\docType{data}
\alias{wflung00and95}
\title{Lung cancer mortality data for white females, 2000-4 and 1995-9}
\description{
Counts and rates of age-adjusted (2000 U.S. standard) lung cancer mortality data among white women, aggregated for 1995-9 and 2000-4.
  }
\usage{data(wflung00and95)}
\format{
  A data frame with 51 observations, 1 for each state + DC, 
  on the following 12 variables.
  \describe{
    \item{Rate.00}{a numeric vector of age-adjusted rates by state during 2000-4 for white females}
    \item{Count.00}{a numeric vector of the number of white female lung cancer deaths during 2000-4}
    \item{Lower.00}{a numeric vector of the 95\% confidence interval lower bound for white female 2000-4 rates}
    \item{Upper.00}{a numeric vector of the 95\% confidence interval upper bound for white female 2000-4 rates}
    \item{Pop.00}{a numeric vector of the white female population during 2000}
    \item{StdErr.00}{a numeric vector of the standard error of the white female 2000-4 rates}
    \item{Rate.95}{a numeric vector of age-adjusted rates by state during 1995-9 for white females}
    \item{Count.95}{a numeric vector of the number of white female lung cancer deaths during 1995-9}
    \item{Lower.95}{a numeric vector of the 95\% confidence interval lower bound for white female 1995-9 rates}
    \item{Upper.95}{a numeric vector of the 95\% confidence interval upper bound for white female 1995-9 rates}
    \item{Pop.95}{a numeric vector of the white female population estimates for 1995}
    \item{StdErr.95}{a numeric vector of the standard error of the white female 1995-9 rates}
  }
}
\details{
 The rates on this file are directly age adjusted to the US 2000 
 standard population and are expressed as the number of deaths per 100,000 person-years.  
 The row names are the 2 character postal codes for the states.   
 The data represents the rates for two periods of time: 2000 to 2004 and 1995 to 1999.
 This dataset is used in the \var{micromapSEER} examples using the border group of "USStatesDF".
}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting, LLC, Gaithersburg, MD}
\source{Surveillance Research Program, National Cancer Institute 
   SEER*Stat software (\url{https://www.seer.cancer.gov/seerstat}), 
   November 2007 data submission, released April 2008. Data 
   originally provided to NCI by the National Center for Health 
   Statistics.
}
\keyword{datasets}