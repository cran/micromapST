\name{glyph-dotse}
\alias{DOTSE}
\alias{glyph-dotse}
\title{The dotse glyph creates a graphic of a single data point and a line representing the standard error of the data.}
\description{
The \var{dotse} glyph is also based on the \var{dat} gryph.  In the \var{dotse} graphic, \var{col2} 
in the \var{panelDesc} data.frame provides the standard error value for the row.  Using the standard error
value, \var{dotse} creates a dot at the data point specified by col1 and a line through the dot representing the 
standard error associated with the dot's data.  Value of the dot and it's standard error value is provided 
in the \var{statsDFrame} data columns referenced by the column name/number is \var{panelDesc} col1 and col2 rows. 
The upper and lower values are calculated based on 

\preformatted{
       zval <- stats::qnorm( .5 + Dot.SE / 200 )  # where Dot.SE is defaulted to 95
       inc  <- zval * col2 referenced data
       upper <- col1 referenced data + inc
       lower <- col1 referenced data - inc
}       

The \var{dotse} is plotted at the col1 data value from \var{statsDFrame}.  The line is drawn from the lower to upper 
calculated values based on the significant error values provided in the \var{statsDFrame} column referenced 
by the \var{panelDesc} col2 row. The range of the X-Axis of the graphic is based on the values of all of the data
points to be graphed from all of the area.  All of the data must be numberic.  
The dot will be filled with the color of the related area in the geographic map.
If the median consist of only a single area, only one area row and graphic will be plotted.
The data in the referred \var{statsDFrame} by col1 and col2  must be numeric values and can be
used to sort the order of the areas using the sortVar call parameter.
}
\details{
  The col1 \var{panelDesc} parameter is indexed by \var{j} to obtain the data column's names/numbers 
  of the col1 and col2 referred columns in 
  the \var{statsDFrame} data.frame to plot the dot, calculate the standard error lower and upper values.   
  The col1 referenced data column is the value of the dot,
  the col2 referenced data is the low interval, and the col3 referenced data is the high interval.  
  \var{j} is used to index into \var{panelDesc} to get the 
  user specified lab1, lab2, lab3, lab4, refval, and reftxt information to create the column headers,
  trailers, and reference information. An X-axis is drawn above and below the column based on the data provided in 
  the col1 and col2 referenced column of data in the \var{statsDFrame} by the user.
  
  For the dotse glyph, the col1 and col2 column name/numbers refereninc the \var{statsDFrame} data.frame 
  must be provided and valid.   All data in these columns must be numeric.
  
  \var{panelDesc} rows not used by the glyph should be set to NA.  The dotse glyph does not
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
