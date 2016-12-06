\name{micromapGSetPanelDef}
\alias{micromapGSetPanelDef}
\title{function to build the micromapGPanelDef data.frame}
\description{
The micromapGSetPanelDef function generates a data.frame to supplement
the micromapGDefaults structure.  The resulting structure along with 
the values from micromapGSetDefaults are copies into the package's memory
and used by the "panel functions" to manage the panels during the setup and glyphics 
creation.
}

\usage{micromapGSetDefaults()}

\details{
The variable in the structure are::
 \describe{
   \item{Var 1}{for the state maps in each group of 5 states that 
      make up one panel row.}
   \item{Var 2}{for the median state (in the middle)}
   \item{Var 3}{for the cumulative hightlight color for the states already 
   presented in other panel rows.}
 }
 is not recommended.  See the micromapGDefault description for more information on these
 lists.
 More to come.
}
\author{
Daniel B. Carr, George Mason University, Fairfax VA, and Jim Pearson, StatNet Consulting,
LLC, Gaithersburg, MD 
}
\value{\link{micromapGSetPanelDef}}
\keyword{functions}
