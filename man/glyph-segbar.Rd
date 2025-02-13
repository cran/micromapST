\name{glyph-segbar}
\alias{SEGBAR}
\alias{glyph-segbar}
\title{The Segbar glyph creates a graphic of a stacked bar chart dot plot of x values.}
\description{
The \var{segbar} glyph creates a set of stacked bars with each bar in the stack with a length equal to the 
data value provided in the \var{statsDFrame} data.frame.  Since the stacked bar diagrams can contain multiple
bars, the data in the \var{statsDFrame} must provide the data in consecutive columns.  The first column
in the series is specified in the \var{panelDesc} data.frame in variable col1.  The last column in the sequence
is specified in variable col2. The stacked bars can contain from 2 to 9 bars.

The segbar graphic starts the first bar at the left edge of the panel.  Each bar is added to the right starting
at the end of the previous bar.  The width of the panel is determined based on the space require to fit all of the 
segment stacked bars for all of the areas. Each bar is filled with the same color as the area the data represents.  
The transparency of the color varies from left to right and a line is used to separate each bar for an area.
}
\details{
  The segment stacked bars only use the \var{panelDesc} col1, col2, lab1, lab2, lab3, lab4, refVal, and refTxt variable.
  The glyph uses the R draw rectangle function to draw and fill each part of the stack.
  
  The \var{statsDFrame} and \var{panelDesc} data.frames reside in the global environment and automatically
  accessible to the process along with several other major structures.
  
  In the \var{details} data.frames there is a \var{SNBar.varht} (a logical variable) that is used to indicate 
  to the gryphic whether to use variable height bars in the gryphic or not.  
  By default the value is set to \var{FALSE}.
  When set to \var{TRUE}, all Center Segmented Stacked Bar Charts will have their bars vary in height from the left to
  the right.
 
}
\author{
  Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
}
\value{None}
\keyword{functions}
