\name{stateNationVisBorders}
\alias{stateNationVisBorders}
\docType{data}
\title{US National Boundary Points}
\description{stateNationVisBorders - A set of boundary points for the US, its 50 states and DC.
}
\usage{stateNationVisBorders}
\format{
  A data frame with 108 elements of the following 2 variables.
  \describe{
    \item{x}{the x coordinate for the boundary point}
    \item{y}{the y coordinate for the boundary point}
  }
}
\details{
  stateNationVisBorders datasets contains the list of points to be able to plot the
  borders of the US in the micromap panel.  Lines are drawn between each point with the
  last line drawn from the final point to the first point. The order of the point is
  important.  The outer points of each state should coincide with points used in the
  national US boundary. The boundaries have been smoothed following Monmonier's visibility
  map.
}
\source{
Mark Monmonier's visibility map, modified by Daniel B. Carr, George Mason University,
Fairfax, VA
}
\references{
Monmonier M, Mapping it out: Expository cartography for the humanities and social
 sciences. Chicago, University of Chicago Press, 1993, p. 178.
 }
\keyword{datasets}
