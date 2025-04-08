\name{glyph-dot}
\alias{dot}
\alias{glyph-dot}
\title{The dot glyph create a graphic of a single data point as a dot on the graphic between the lowest and highest values.}
\description{
The \var{dot} glyph creates a simple dot at the point on the graphic at the specified data point from the column name or number provided 
in the \var{col1} row in \var{panelDest} data.frame in the \var{statsDFrame} data.frame.
The range of the data provided is used 
to set the range of the X-Axis and the dot is plotted in line with the associated area's color in the group/row.
If the median consist of only a single area and dot, only one area row is plotted.
The col1 data must be numeric values and can be used to sort the order of the areas using the \var{sortVar} call parameter.
}
\details{
  The col1 variable for the glyph column in \var{panelDesc} parameter links to the data column containing the dot value 
  the \var{statsDFrame} data.frame.  \var{j} is used to index into \var{panelDesc} to get the 
  user specified \var{lab1}, \var{lab2}, \var{lab3}, \var{lab4}, \var{refval}, and \var{reftxt} information to 
  create the column headers,
  trailers, and reference information. An X-axis is drawn above and below the column based on the data provided in 
  the \var{col1} column of the \var{statsDFrame} by the user.
  
  For the dot glyph, the \var{col1} value of the column name/numbers in the \var{statsDFrame} data.frame must be provided.
  The data values must be numeric.
  
  This glyph routine creates both the "dot" and "dotsignif" graphs.  
  If the \var{dsignif} call parameter is set to FALSE, the function will not check for \var{col2} and 
  associated data in the \var{statsDFrame} for \var{pvalues} to check the significance of the 
  dot value data.  If TRUE, the function creates the dotsignif glyph creation.
  
  \var{panelDesc} rows not used by the glyph should be set to NA.  The "dot" glyph does not
  use the\var{col2}, \var{col3}, \var{panelData}, and \var{parm} rows in the \var{panelDesc} data.frame. 
  
  The following is an example of a section of a boxplot glyph column in a linked micromap:
  
  \if{html}{\figure{ExamDot.png}{options: height="150" width="150"}}
  \if{latex}{\figure{ExamDot.png}{options: width=150px, height=150px}}

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
