\name{AfricaPopData}
\docType{data}
\alias{AfricaPopData}
\title{Test data for the Africa border Group}
\description{
This dataset contains the population and country data for the 52
countries in the African border group.
  }
\usage{data(AfricaPopData)}
\format{
  A data frame with 52 observations, 1 for each African country,
  on the following "x" variables.
  \describe{
    \item{Rank}{an integer rank of the country in Africa.}
    \item{Name}{a character vector containing the Africa Country
    Name.} \item{Abbr}{a character vector containing the African
    Country Abbreviation.}
    \item{Projection}{a numeric vector of the number of the
    county's population} \item{AvrRelGw}{a numeric vector of the
    average relative population growth.}
    \item{AvrAbsGw}{a numeric vector of the average absolute
    population growth.} \item{Est2Time}{a numeric vector of the
    estimated time to double the population - years.}
    \item{OfficialPop}{a numeric vector of the official
    population.} \item{MMDDYY}{the date the information was last
    updated.}
    \item{PercOf}{a numeric vector representing the percentage
    the country's population is to the total population of
    Africa.}
  }
}
\details{
 This dataset was pulled from wikipedica on the population
 numbers for African countries.}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting,
LLC, Gaithersburg, MD}
\keyword{datasets}