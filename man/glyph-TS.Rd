\name{glyph-ts}
\alias{ts}
\alias{glyph-ts}
\alias{glyph-tsconf}
\title{The TS and TSCONF glyph creates a time series plot for up to 64 time 
   periods for the data based on the provided matrix.}
\description{
The \var{ts} and \var{tsconf} glyphs create time series plot of data for 
each area over time with or without confidence internals.  The plot is of 
a "y" value at a time ("x") where you can have as many samples as 
you want.  
When each area is presented for the analysis, the time series line will be
highlighted in the panel. If you provided and requested confidence interval, 
a line is drawn for the upper and lower values over time and the
space between the original value ("Y") and the upper and lower values 
are shaded in with a lighter variation of the 
color assigned to the area for presentation.  

Since the data required for this glyph is more complicated, it is 
collected into a 3 dimensional matrix and passed to the glyph code via 
the \var{panelData} variable in the \var{panelDesc} data.frame.
}
\details{
  The data structure used to carry the time series data with confidence 
  intervals is a 3 dimensional matrix. The first level index is by the area 
  the data is associated.  The second level under the area is the time 
  series matrixes representing the data for each time period being graphed.  
  The third level under each time period is A vector containg the "x", "y", 
  "hy", and "ly" values a the time point for the area.  The "x" is time point
  idenfier, "y" is the observed value, "hy" is the high confidence value, 
  and "lY" is the low confidence value.  It is best to assemble the matrix 
  for the time series for each area, then gather the areas 
  in to the final matrix.
  
  The easiest way to construct the time series 3 dimensional array is to 
  use the following steps:
  \preformatted{
      TSAreaNames <- AreaNames
      TSArr       <- array(dim=c(51,10,2),dimnames=list(TSAreaNames))  
                            # This build an empty 3d structure to be filled in.
      
      for (index in TSAreaNames) {
          x    <- c(1,2,3,4,5,6,7,8,9,10)  # time points or "x" data values for this area.
          y    <- DATA[index,]             # pick up the 10 "y" data values for "x" points.
          TSArr[index,,1] <- x
          TSArr[index,,2] <- y   
          # if the confidence values are available, then TSArr[index,,3] 
          # and TSArr[index,,4] vectors would beadded for the high and low y values
      }  # repeat until the TSArr is fully loaded. 
      
      # verify the 3d array was build correctly
      row.names(TSArr) <- TSAreaNames   # add the row.names to the array.
 
  }
  
  If any data point is missing from the time series or the confidence data, that point will not be plotted, but the 
  rest of the time series will be plotted.
  
  The following is an example of a section of a time series glyph in a linked micromap:
    
  \if{html}{\figure{ExamTS.png}{options: width=200}}
  \if{latex}{\figure{ExamTS.png}{options: width=200px}}
  
  An example of a time series glyph with confidence intervales is: 
  
  \if{html}{\figure{ExamTSConfEdited.png}{options: width=200}}
  \if{latex}{\figure{ExamTSConfEdited.png}{options: width=200px}}

  The shaded areas of the confidence interval around a time series line is based on the color of the 
  area and line.  It is a translucent version of the original color set to 20%.  This allows all of the
  time series lines to be seen and provides a reasonable presentation of the confidence spaces when the 
  areas overlap on the graph.

  The time series can also be produced in the Black and White color mode with good results. 

  The \var{statsDFrame} and \var{panelDesc} data.frames reside in the global 
  environment and automatically 
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
