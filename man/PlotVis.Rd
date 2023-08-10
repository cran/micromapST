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
wait for the next point. It's primary purpose is to visualize boundary data that has been prepared for 
micromapST and assist in developing new border groups for micromapST.
}
\usage{PlotVis ( VisBrd, VisCol, xTitle=NULL, xAxes=FALSE, xLwd=0.05) }
\details{
More details to follow.
}
\arguments{
  \item{VisBrd}{- is a data frame containing a VisBorders formated collection of boundaries to be plotted.
   Refer to the section under the discussion on the \var{areaVisBorder} data.frame and it's format.
   In a border group, this would be the areaVisBorder, L2VisBorder, RegVisBorder, or L3VisBorders 
   data.frame.}
  \item{VisCol}{- is a vector of the colors to fill each polygon in the VisBorder data frame. 
   One color value is required for each "NA" (end of polygon) in the VisBorders data.frame. This 
   vector must have the same number of elements at the VisBorder has polygons (point sets ending in NA.)
   
   If multiple polygons represent a single area, the function caller must compensate for 
   this in the VisCol vector. The usual strategy is to assign a color to each row in the 
   Name Table, then compare the keys in the Name Table and the keys in the VisBorder 
   and assign the color for that Key/Polygon.
   
   Warning: R's polygon function does not advance to the next color in this list, if the vectors 
   between the "NA" rows represent a point or a line, R's polygon function will not advance 
   to the next color, nothing to fill.  This will affect which colors shade each polygon.  
   }
  \item{xTitle}{- is character string value to be used for the title of the VisBorders plot.
   The default for this call parameter is NULL.  Only a single string can be used.
   }
  \item{xAxes}{- is a logical (FALSE or TRUE) indicator as to whether or not the X and Y Axis should be drawn.
   The default for this call parameter is FALSE.
   }
  \item{xLwd}{- is a numerical value of the boundary line width to use when drawing the 
   areas in the map.  The value may range from .1 up to 3 times the standard width.  The default 
   value is 0.5.
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
