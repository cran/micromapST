\name{wflung00and95US}
\docType{data}
\alias{wflung00and95US}
\title{wflung 2000 to 2004 and 1995 to 1999 US data}
\description{
   Counts and age-adjusted rate of white female lung cancer for the total U.S.
   for the aggregated periods 1995-9 and 2000-4.
}
\usage{data(wflung00and95US)}
\format{
  A data frame with 1 observation on the following 13 variables.
  \describe{
    \item{Rate.00}{a numeric vector, state rate for 2000-2004}
    \item{Count.00}{a numeric vector, state number of cases for 2000-2004}
    \item{Lower.00}{a numeric vector, lower end point for 95\% confidence interval}
    \item{Upper.00}{a numeric vector, upper end point for 95\% confidence interval}
    \item{Pop.00}{a numeric vector, state population fro 2000-2004}
    \item{StdErr.00}{a numeric vector, state standard error}
    \item{Rate.95}{a numeric vector, state rate for 1995-1999}
    \item{Count.95}{a numeric vector, state number of cases for 1995-1999}
    \item{Lower.95}{a numeric vector, lower end point for 95\% confidence interval}
    \item{Upper.95}{a numeric vector, upper end point for 95\% confidence interval}
    \item{Pop.95}{a numeric vector, state population for 2000-2004}
    \item{StdErr.95}{a numeric vector, state standard error}
  }
}
\details{
  See documentation for wflung00and95 for more details.
  The row name is the associated state abbreviation - 2 characters.
  This dataset is used in the \var{micromapSEER} examples using a border group of "USStatesDF"..
}
\author{Linda W. Pickle and Jim Pearson of StatNet Consulting, LLC, Gaithersburg, MD}
\source{Surveillance Research Program, National Cancer Institute 
   SEER*Stat software (\url{https://www.seer.cancer.gov/seerstat}), 
   November 2007 data submission, released April 2008. Data 
   originally provided to NCI by the National Center for Health 
   Statistics.
}
\references{none}
\keyword{datasets}

