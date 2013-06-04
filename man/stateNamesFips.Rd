\name{stateNamesFips}
\alias{stateNamesFips}
\docType{data}
\title{State Names (abbrevated and full) and US State FIPS codes.}
\description{
stateNamesFips database of the USA state FIPS codes, and the associated full and abbreviated
state names.
}
\usage{stateNamesFips}
\format{
  A data frame with 51 observations of the following 2 variables.
  \describe{
    \item{\code{fips}}{a numeric vector, 2 digit state FIPS code.}
    \item{\code{ab}}{a character vector, 2 character State Abbreviation (postal code)}
  }
}
\details{
  stateNamesFips dataset provides a table to permit the translation of FIPS codes,
  abbreviated state name (2 characters), and full state names in the stateMicromap
  function. Although the FIPS codes cover territories and other areas, only the FIPS codes
  for the 50 states and DC are included in this dataset.
}

\source{NIST - Federal Information Processing Standards}
\references{NIST FIPS 6-4 Standards} 
\keyword{datasets}
