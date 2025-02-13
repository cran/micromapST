\name{glyph-arrow}
\alias{glyph-arrow}
\alias{ARROW}
\title{The arrow glyph creates a graphic of a two data points, from a beginning point to an end point.}
\description{
The \var{arrow} glyph creates a simple arrow with pointing heads from a beginning point (col1 value) to 
the ending point (col2 value).  If the beginning and ending point values are the same, a solid dot will
be used to represent the two points.  The point value is provided through the col1 and col2 in 
\var{panelDesc} data.frame providing named/numbers of columns in the \var{statsDFrame} user provided data structure.  
Only the points related to the area rows associated to the group/row are used to create the arrow for that area.
If the median consist of only a single area, 
the single arrow is plotted in the group/row. 
The \var{panelDesc} variables col1 or col2 provide the name/number of the data columns in the 
\var{statsDFrame} data.frame supplied by the user.
}
\details{
  The col1 snd col2 \var{panelDesc} variable provide the name/number of the column in the \var{statsDFrame} 
  containing data values used to plot the beginning and end points of each arrow.  The beginning value may 
  be higher than the ending point to identify arrow going down between the col1 and col2 time points.
  
  The user can specify the lab1, lab2, lab3, lab4, refval, and reftxt information/values in the 
  appropriate column \var{j} related to the glyph in the \var{panelDesc} provide additional information 
  on how to create the glyph graphic column. The information is used to create the column headers,
  trailers, and reference information. 
  
  An X-axis is drawn above and below the column based on the data provided in 
  the col1 an col2 columns in the \var{statsDFrame} provided by the user.
  
  For the arrow glyph, the col1 and col2 column names/numbers must be supplied.   

  \var{panelDesc} variable rows not used are the col3, panelData, and parm variable rows. If these
  rows are present the value for the \var{arrow} glyph should be set to NA.
  
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
