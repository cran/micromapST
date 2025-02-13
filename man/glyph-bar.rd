\name{glyph-bar}
\alias{glyph-bar}
\alias{BAR}
\title{The bar glyph creates a graphic of a single data point as the height of the bar.}
\description{
The \var{bar} glyph creates a simple bar with the starting point for the bar being zero.  The value is defined by 
the col1 column name/number in the \var{statsDFrame} data.frame provided by the user.  The value may be a positive 
or negative number. The glyph determines the range of the values and sets the width of the graphic box accordingly.
The zero point reference will be the same for all bars drawn for the glyphic. If the median consist of only a single area and bar, 
the single bar will be plotted in the group/row. 
The col1 data must be numeric values and can be used to sort the order of the areas using the sortVar call parameter.
}
\details{
  The col1 \var{panelDesc} parameter is indexed by \var{j} to obtain the data column's name/number of the data in 
  the \var{statsDFrame} data.frame to use for the height of the bar.  The bar is draw horizontally with negative values on the left 
  and positive values on the right.  \var{j} is used to index into \var{panelDesc} to get the 
  user specified lab1, lab2, lab3, lab4, refval, and reftxt information to create the column headers,
  trailers, and reference information. An X-axis is drawn above and below the column based on the data provided in 
  the col1 column of the \var{statsDFrame} by the user.
  
  For the bar glyph, the col1 value of the column name/numbers in the \var{statsDFrame} data.frame must be provided.
  The data values must be numeric.
  
  \var{panelDesc} rows not used by a glyph should be set to NA if not used.  The bar glyph does not
  use the col2, col3, panelData, and parm rows. 
  
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
