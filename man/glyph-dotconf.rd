\name{glyph-dotconf}
\alias{DOTCONF}
\alias{glyph-dotconf}
\title{The dotconf glyph creates a graphic of a single data point as a dot with a line through the dot from the low to high confidence limits.}
\description{
The \var{dotconf} glyph is an extension to the dot glyph graphic.  The \var{dotconf} glyph uses two additional data
columns in the \var{statsDFrame} user data data.frame to provide the low (col2) and high (col3) confidence interval 
values. col1 still represent the value of the dot's position in the graph. 
A line is drawn through the dot from the low confidence limit (provided by col2) to the highest confidence limit (provided by col3).  
The data values obtained from the the column names/numbers in col1, col2, and col3 variables in the \var{panelDesc} data.frame 
point to the columns containing the data in the \var{statsDFrame} data.frame provided by the user.  
All of the data must be numberic.  
The lowest confidence limit must be less than the data value for 
the dot position and the highest confidence limit must be greater than the data value for the dot.
The range of the data for all three data columns is used to define the X-Axis range for the graphic column. 
The dot and the limits are drawn on the same row associated with the color used for the area it represents.  
The dot will be filled with the color of the related area in the geographic map.
If the median consist of only a single area, only one area row and graphic will be plotted.
The col1, col2, and col3 column in the \var{statsDFrame} data.frame must be numeric values and can be
used to sort the order of the areas using the sortVar call parameter.
}
\details{
  The col1 \var{panelDesc} parameter is indexed by \var{j} to obtain the data column's names/numbers of the col1, col2, and col3 data in 
  the \var{statsDFrame} data.frame to plot the dot and confidence interval.   The col1 referenced data column is the value of the dot,
  the col2 referenced data is the low interval, and the col3 referenced data is the high interval.  \var{j} is used to index into \var{panelDesc} to get the 
  user specified lab1, lab2, lab3, lab4, refval, and reftxt information to create the column headers,
  trailers, and reference information. An X-axis is drawn above and below the column based on the data provided in 
  the col1, col2, and col3 referenced column of data in the \var{statsDFrame} by the user.
  
  For the dotconf glyph, the col1, col2, and col3 column name/numbers refereninc the \var{statsDFrame} data.frame must be provided and valid.
  All data in these columns must be numeric.
  
  \var{panelDesc} rows not used by the glyph should be set to NA.  The dotconf glyph does not
  use the panelData, and parm rows. 
  
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
