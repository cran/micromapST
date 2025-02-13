\name{glyph-mapxxxx}
\alias{map}
\alias{mapcom}
\alias{maptail}
\alias{mapmedian}
\alias{glyph-mapxxxx}
\title{The collection of map glyphs creates a graphic of a micromap plots.}
\description{

REWRITE
The \var{map} collection of glyphs generate small maps from the boundary group for use in creating the 
linked micromaps and the statistical plots of the data.  The collection of map glyphs do not use any values 
from the \var{col1}, \var{col2}, \var{col3}, \var{lab1}, \var{lab2}, \var{lab3}, \var{lab4}, \var{reftxt}, and 
\var{refval}, \var{panelData}, and \var{parm} vectors in the \var{panelDesc}. If any of there are present
in the \var{panelDesc} data.frame, they value should be set to "NA".
Each map is drawn in a single group/row section of the graphic representing up to 5 areas.  
A color is assigned to each "ACTIVE" area that is presented and carried over to the associated graphic 
in the columns on the left or right of the map.
}
\details{
  The only call parameter is \var{j} carrying the graphic column number being drawn.  \var{j} is used by 
  all of the panel functions to scale, outline, and select the area within the total graphic page to
  draw a column of the map glyphs, one per group/row.  
 
  The map collection consist of four ways the maps are used to present the area information moving down the page.
  1) Map - is a simple map with out any extra areas colored in.   Only the "active" areas being presented 
  in the graphics on either side
  are colored.  The same color is used in the statistical graphic to line the area to the data.
  2) MapCum - is map, is the same as the "MAP" glypic, but after an area has been referenced, 
  it is colored yellow to 
  let the user know the area was presented in a group/row above.
  3) MapTail - this map is the same as the MapCum in that it accumulate areas that have been presented.  
  However, at the median point, the areas shaded are changed to the areas that have not been presented 
  before to area to be repesented.  As the graphics continue, the number of yellow area decreases. 
  In the last row, only the presented states are colored and no area are colored yellow.
  4) MapMedian - this map colors all of the areas with values above the median (as sorted) a very 
  pale red. All of the areas below the median   are colore a vary pale blue.  The areas being 
  presented are still colored with a unique color and linked to the statistical graphic using
  that color.
  
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
