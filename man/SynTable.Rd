\name{SynTable}
\alias{SynTable}
\docType{data}
\title{This data set contains a synonym table to help translate common incorrect location id 
strings}
\description{
When the user supplied data does not contain the exact Name or Abbr string to match the
name table information.  The data row cannot be linked or map to the micromapST
graphics.  An example is the many ways the District of Columbia in the U. S. is 
identified.  Originally special code was used to identify these mismatches and 
correct them.  The SynTable dataset now provide a open method to address the problem.
The common string representing an area can be entered into the table and the correct Name and Abbr
equivalent.  When a data row's name does not match the name table, the Synonym Table is 
referenced to see it there is an alternative value to use.
}
\usage{data(SynTable)}
\format{
  A data frame with 51 observations (one per state) on the following 7 variables.
  \describe{
    \item{\code{locid}}{a character string representing the name of the row in the user's provided data.frame.}
    \item{\code{Name}}{a character string containing the equivalent name table Name column value.}
    \item{\code{Abbr}}{a character string containing the equivalent name table Abbr column value.}
   }
}
\author{
Jim Pearson, Statnet Consulting, Gaithersburg, MD 20877 
}
\seealso{
  \link{micromapST}
}
\keyword{datasets}
