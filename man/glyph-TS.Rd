\name{glyph-TS}
\alias{TS}
\alias{glyph-TS}
\alias{glyph-TSconf}
\title{The TS glyph creates a time series plot for up to 64 time periods for the data based on the provided matrix.}
\description{
The \var{ts} and \var{tsconf} glyphs create time series plot of data for each area over time with or without confidence
internals.  The plot is of a "y" value at a time ("x") where you can have as many samples as you want.  
When each area is presented for the analysis, the time series line will be highlighted in the panel.
If you provided and requested confidence interval, a line is drawn for the upper and lower values over time and the
space between the original value ("Y") and the upper and lower values are shaded in with a lighter variation of the 
color assigned to the area for presentation.  

Since the data required for this glyph is more complicated, it is collected into a 3 dimensional matrix and 
passed to the glyph code via the \var{panelData} variable in the \var{panelDesc} data.frame.
}
\details{
  The data structure used to carry the time series data with confidence intervals is a 3 dimensional
  matrix. The first level index is by the area the data is associated.  The second level under the area is the time 
  series matrixes representing the data for each time period being graphed.  The third level under each time 
  period is A vector containg the "x", "y", "hy", and "ly" values a the time point for the area.  The "x" is time point
  idenfier, "y" is the observed value, "hy" is the high confidence value, and "lY" is the low confidence value.
  It is best to assemble the matrix for the time series for each area, then gather the areas in to the final matrix.
  
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
