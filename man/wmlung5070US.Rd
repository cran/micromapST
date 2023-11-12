\name{wmlung5070US}
\docType{data}
\alias{wmlung5070US}
\title{
  U.S. lung cancer mortality data for white males, 1950-1969 and 1970-1994
}
\description{
  Count and age-adjusted rate of lung cancer mortality among white men for the total U.S.,
  aggregated for 1950-69 and 1970-94.
}
\usage{data(wmlung5070US)}
\format{
  A data frame with 1 observations on the following 5 variables.
  \describe{
    \item{RATEWM_50}{a numeric vector, US age adjusted mortality rates for 1950-1969}
    \item{COUNTWM_50}{a numeric vector, US number of cases from 1950-1969}
    \item{RATEWM_70}{a numeric vector, US age adjusted mortality rates for 1970-1994}
    \item{COUNTWM_70}{a numeric vector, US number of cases from 1970-1994}
    \item{PERCENT}{a numeric vector, change from 1950-1969 to 1970-1994 US rates.}
  }
}
\details{
 see wmlung5070 for further details.
  The row name is always \var{US} indicating US rates.
  This dataset is used by the \var{micromapSEER} examples using the border group of "USStatesDF".
}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting, LLC, Gaithersburg, MD}
\references{
  None
}
\keyword{datasets}