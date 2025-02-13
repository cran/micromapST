\name{glyph-rank}
\alias{RANK}
\alias{glyph-rank}
\title{The rank glyph creates a graphic of the area rankings.}
\description{
The \var{rank} glyph presents a ranking number for each area in the linked micromap.  
The rank number presented is the ranking value based on the sortVar results.  

This glyph needs to be rewritten to 
properly handle order ties in the data consistantly.
}
\details{
  The ranking function does not use any data or label or reference column information from the \var{panelDesc} data.frame.
  
  The ranking is based on the resulting sorted order specified in the sortVar call parameter.
   
  The \var{statsDFrame} and \var{panelDesc} data.frames reside in the global environment and automatically 
  accessible to the process along with several other major structures.
  
}
\author{
  Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
}
\value{None}
\keyword{functions}
