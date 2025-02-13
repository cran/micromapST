\name{glyph-ctrbar}
\alias{glyph-ctrbar}
\alias{CTRBAR}
\title{The ctrbar glyph creates a centered stacked bar diagram centered about the value of zero.}
\description{
The \var{ctrbar} glyph creates a stacked bar diagram, where the centered segment stacked bar (ctrbar) 
glyph displays the bars in the order of the data in the statsDFrame data.frame.  Each row is assumed to have a total 
value of 100 or at least equal values of all of the segments.  Each row represents the same number of segment 
with the same total value or proprotion of participants in a study being examined.  
The \var{ctrbar} identifies the center of the set of bars, either between two bars or the middle of an odd number of bars.  
The glyph lines up the middle of the segment or the boundary between two segments to allow comparison of the data between 
each row (or area). The values of each boundary is provided in a column in the statsDFrame data.frame.  In panelDesc, the 
name or number of the FIRST data column is provided in the col1 column.  The last column in the series is specified 
in the col2 column.  The values may be specified as statsDRrame column names or numbers.  
}
\details{
  The glyph is called with a \var{j} identifying the graphic column number being drawn.  \var{j} is used by 
  all of the panel functions to scale, outline, and select the area within the total graphic page to
  draw a column of the ctrbar glyphs, one per group/row.  It is also used to reference the panelDesc 
  column to determine what type of glyph to draw, what variables (via col1, col2, and col3 rows), what labels to be used 
  for the header and trailer titles when drawing the graphic.  
  The graphic is a simple bar plot of the data points provide to the ctrbar routines.
  
  The data must be sorted from lowest to highest values. 
  
  For the ctrbar glyph, the col1 and col2 column names/numbers must be supplied and represent the first and last
  data columns in the \var{statsDFrame} data.frame.   
  
  In the \var{details} data.frames there is a \var{CBar.varht} (a logical variable) that is used to indicate 
  to the gryphic whether to use variable height bars in the gryphic or not.  By default the value is set to 
  \var{FALSE}.
  When set tos \var{TRUE}, all Center Segmented Stacked Bar Charts will have their bars vary in height from the left to
  the right.
  
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
