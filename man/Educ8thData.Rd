\name{Educ8thData}
\docType{data}
\alias{Educ8thData}
\title{Education Survey of 8th Grade Proficiency in Math}
\description{
   Math Proficiency Survey Results for 8th Graders in 2011 by State
}
\usage{data(Educ8thData)}
\format{
  A data frame with 51 observations (one per state) on the following 7 variables.
  \describe{
    \item{\code{StAbbrev}}{a character string representing the 2 character state Id for this row.}
    \item{\code{State}}{a character string factor of the state full name.}
    \item{\code{avgscore}}{a numeric vector of average proficiency score for each state}
    \item{\code{PctBelowBasic}}{a numeric vector of percentage of students with below basic scores}
    \item{\code{PctAtBasic}}{a numeric vector of percentage of students at basic proficiency.}
    \item{\code{PctProficient}}{a numeric vector of percentage of students at the proficient level}
    \item{\code{PctAdvanced}}{a numeric vector of percentage of students scoring at the advanced level.}
  }
}
\details{

   The dataset contains 51 records, one for each state/area. The data represents 
   the percentage of 8th grade students in 2011 in that state who tested at 
   each proficiency level in math:
   less than basic, basic, proficient and advanced.
   The row name is the state abbreviation - 2 characters.
   This dataset is used by the \var{micromapSEER} examples using the "USStatesDF" border group.
}
\source{
    The National Center for Education Statistics, Department of Education\cr
    http://nces.ed.gov/nationsreportcard/states/
}
\keyword{datasets}
