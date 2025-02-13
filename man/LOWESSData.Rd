\name{LOWESSData}
\docType{data}
\alias{LOWESSData}
\title{Test data for US LOWESS ScatDot glyphs.}
\description{
This dataset contains the data for the USStatesBG for testing and demonstrations
of the ScatDat LOWESS line feature in the glyph.}
\usage{data(LOWESSData)}
\format{
  A data frame with 51 observations, 1 for each US State and DC area, 
  on the following 3 variables.  The row.names are set to the strings in the "ABBR" column.
  \describe{
    \item{x}{the x coordinate value.}
    \item{y}{the y coordinate value.}
    \item{Abbr}{a character vector containing the abbreviated names for each US state and DC.}
  }
}
\details{
 This dataset was manually created to provide set of x, y and w data point to test and demostrate how 
 a LOWESS line can be implement in the ScatDot glyph with the USStatesBG border group.}
\keyword{datasets}