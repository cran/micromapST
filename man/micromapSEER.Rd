\name{micromapSEER}
\alias{micromapSEER}
\title{A function to create linked micromaps for the 20 U. S. Seer Registries}
\description{
The \var{micromapSEER} function or the \var{micromapST} function with the \option{bordGrp} set to \var{USSeerBG} 
can be used to create linked micromaps for the 20 U. S. Seer Registries.  
}
\usage{micromapSEER(statsDFrame,panelDesc,...)}
\details{
A release of micromapST was custom modified for the NCI Seer projects. Later the cusstom changes 
were integrated into the standard micromapST and the micromapSEER only version was discontinued. 
This micromapSEER shell call allows existing code to continue to work without modification.
}
\arguments{
  \item{statsDFrame}{- data frame of the data required for creating the graphical glyphics with one 
  row per Seer Area being mapped.  Each row is linked to the boundary data using a Seer Area abbreviation or name.}
  \item{panelDesc}{- data frame containing information and pointers for each glyphic column to be generated.
  The data frame specifies the type of glyphic and the columns in the \var{statsDFrame} data frame that 
  contain the data for the glyphic or the external data structure via the panelData list.}
  \item{...}{- the remaining parameters required and used by the \var{micromapST} function call.}
}
\author{
Daniel B. Carr, George Mason University, Fairfax VA, and Jim Pearson, StatNet Consulting,
LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
}
\value{None}
\keyword{functions}
