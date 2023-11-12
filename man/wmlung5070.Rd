\name{wmlung5070}
\docType{data}
\alias{wmlung5070}
\title{
  Lung cancer mortality data for white males, 
  1950-69 and 1970-94
}
\description{
   Counts and rates of lung cancer mortality data among white men by state, aggregated for 1950-1969 and 1970-1994
}
\usage{data(wmlung5070)}
\format{
  A data frame with 51 observations, 1 for each state + DC, 
  on the following 5 variables.
  \describe{
    \item{RATEWM_50}{a numeric vector, state age-adjusted rates during 1950-69}
    \item{COUNTWM_50}{a numeric vector, the number of lung cancer deaths during 1950-69}
    \item{RATEWM_70}{a numeric vector, state age-adjusted rates during 1970-94}
    \item{COUNTWM_70}{a numeric vector, the number of lung cancer deaths during 1970-94}
    \item{PERCENT}{a numeric vector of the percent change in rate from 1950-69 to 1970-94}
  }
}
\details{
  The rates on this file are directly age adjusted to the US 1970 
  standard population and are expressed as the number of 
  deaths per 100,000 person-years.  The row names are the 
  2 character postal codes for the states. Note that the data 
  currently available on the NCI web site are from a
  later data submission and so may differ slightly 
  (in first decimal place) from the rates provided here 
  due to corrections to the dataset after its first publication.
  The name of each row is the state abbreviation - 2 characters.
  This dataset is used by the \var{micromapSEER} examples using the border group of "USStatesDF".
}
\source{
   Surveillance Research Program, National Cancer Institute 
   SEER*Stat software (\url{https://www.seer.cancer.gov/seerstat}), 
   November 2007 data submission, released April 2008. Data 
   originally provided to NCI by the National Center for Health 
   Statistics.
}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting, LLC, Gaithersburg, MD}
\references{
  Devesa SS, Grauman DJ, Blot WJ, Pennello GA, Hoover RN, 
  Fraumeni, JF Jr. Atlas of cancer mortality
  in the United States: 1950-94, NIH Publication 99-4564, 
  Bethesda, MD: National Cancer Institute
}
\keyword{datasets}