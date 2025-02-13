\name{glyph-dotsignif}
\alias{DOTSIGNIF}
\alias{glyph-dotsignif}
\title{The dotsignif glyph creates a graphic of a single data point as a dot and overlay the dot with a "X" if the data value is significant.}
\description{
The \var{dotsignif} glyph is the same as the \var{dot} glyphic, but adds the option of testing 
for significants and overlaying the dot with an "X" the dot's value is.  The dot is placed using the data in column name/number
in the \var{statsDFrame} user provided data.  The data in \var{col2} is tested to determine if the dot's value
is significant.  The comparison is done against the standard pvalue of 0.05.  This value can be changed in 
the \var{details} call parmater.  The value of the dot is in the column named/numbers in the \var{panelDesc} data.frame's
\var{col1} in the \var{statsDFrame} data.frame.  
The dot is considered significant if the value provided in \var{statsDFrame} 
column referenced by the column name/number provided in the \var{col2} value is greater than the pvalue variable.  
The default pvalue is 0.05.  If the col2 referenced data is > then the reference pvalue (0.05), 
the an "X" is overploted on the dot.
The range of the data provided is used to set the range of the X-Axis.
The dot is plotted in line with the associated area's color in the group/row.
If the median consist of only a single area and dot, only one area row is plotted.
The col1 and col2 column names/numbers must be provided and the data in the \var{statsDFrame} must be numeric values.
The statsDFrame columns can be used to sort the order of the areas using the sortVar call parameter.
}
\details{
  The col1 \var{panelDesc} parameter is indexed by \var{j} to obtain the data column's name/number of the data in 
  the \var{statsDFrame} data.frame to plot the dot.  The data in the \var{statsDFrame} referred to by col2 is compared with the 
  set pvalue to determine if the value of the dot is significant.  If it is, an "X" is ploted over the dot to indicate significance.
  \var{j} is used to index into \var{panelDesc} to get the user specified lab1, lab2, lab3, lab4, refval, and reftxt 
  information to create the column headers,
  trailers, and reference information. An X-axis is drawn above and below the column based on the data provided in 
  the col1 column of the \var{statsDFrame} by the user.
  
  For the dotsignif glyph, the col1 and col2 values of column name/numbers must referrence columns in the \var{statsDFrame} data.frame 
  and the data in these columns must be numeric.
  
  \var{panelDesc} rows not used by the glyph should be set to NA.  The dotsignif glyph does not
  use the col3, panelData, and parm rows. 
  
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
