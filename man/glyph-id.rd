\name{glyph-id}
\alias{ID}
\alias{glyph-id}
\title{The id glyph creates a graphic of a names or abbreviations of each area in the map.}
\description{
The \var{id} glyph creates a simple column containing the ID using the Abbreviation or Full name
of each area in the Border Group dataset.  The call parameter \option{plotname} tells micromapST which
label to use in the ID glyph column.  The label and the color representing the area are place 
in alignment with the statistical graphics in the group/row. 

The only \var{panelDesc} rows (variables) used by the ID glyph are: "lab3" and "lab4" values 
from the \var{panelDesc}
data.frame.  All of the other rows are ignored and the values for this glyph should be set to "NA".
}
\details{
  For the id glyph, creates a location id label in the color associated with the area being presented in 
  the statistical graphics in neigbhooring columns.  The label can be the Full Name or the Abbreviation
  assigned to the area in the border group dataset. 
  
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
