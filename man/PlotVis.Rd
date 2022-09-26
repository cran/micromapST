\name{PlotVis}
\alias{PlotVis}
\title{A function to plot the micromapST boundary format called VisBorders}
\description{
The \var{PlotVis} function will plot the boundary data contained in a border group.  The border group boundary
information is stored as a data.frame with an "x", "y", "hole", and "key" columns.  The "key" column identifies
the name table entry the boundary point and polygon belongs to.  The "x" and "y" are the coordinates for the 
boundary vertex in the polygon, and the "hole" column identifies if the polygon is a hole in the area identified by the 
"key".  At the end of each sequence of vectex for a polygon, the first vertex and the last vertex must 
be the same point.  This is followed by a "x" and "y" coordinates of c(NA,NA) to tell the turn off drawing and 
wait for the next point.
}
\usage{PlotVis ( VisB, VisCol, xTitle, xAxes, xLwd) }
\details{
More details to follow.
}
\arguments{
  \item{VisB}{- is a data frame containing a VisBorders formated collection of boundaries to be plotted.
   Refer to the section under the discussion on the \var{areaVisBorder} data.frame.
   }
  \item{VisCol}{- is a vector of the colors to fill each polygon in the VisBorder data frame. 
   One color value is required for each "NA" (end of polygon) in the VisBorders data.frame.
   }
  \item{xTitle}{- is character string value to be used for the title of the VisBorders plot.
   The default for this call parameter is NULL.
   }
  \item{xAxes}{- is a logical indicator as to whether or not the X and Y Axis should be drawn.
   The default for this call parameter is FALSE.
   }
  \item{xLwd}{- is a numerical value of the boundary line width to use when drawing the 
   areas in the map.  The value may range from .1 up to 3 times the standard width.  The default 
   value for this call parameter is 0.5.
   }
  }
\author{
  Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
}
\value{None}
\keyword{functions}
