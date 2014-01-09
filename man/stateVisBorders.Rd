\name{stateVisBorders}
\alias{stateVisBorders}
\docType{data}
\title{U.S. State boundary points}
\description{
stateVisBorders contains a set of boundary points for each US states. The file contains a set of
boundary points to outline each US state when a map is plotted in the micromapST.
}
\format{A data frame with 434 entries for the 50 US states and the District of Columbia. 
  A state is represented by multiple boundary points. 
  \describe{
    \item{st}{a character vector of the name of the state - 2 characters}
    \item{x}{the x coordinate for the boundary point}
    \item{y}{the y coordinate for the boundary point}
  }
}
\details{
The \var{stateVisBorders} dataset is critical to drawing the state boundaries on the maps. Each
row represents a state and the data a collection of points scales to use on the micromapST
US and state graphic panel. The state boundary is drawn by connecting the points in the order in dataset, with
the final point connected to the first point. Each state must have at least three
points.  The points should co-incide with neighboring state points to eliminate any
holes between states. The boundaries have been smoothed following Monmonier's visibility map.
}
\source{Mark Monmonier's visibility map, modified by Daniel B. Carr, George Mason University, Fairfax, VA}
\references{
Monmonier M, Mapping it out: Expository cartography for the humanities and social
sciences. Chicago, University of Chicago Press, 1993, p. 178.
}
\keyword{datasets}
