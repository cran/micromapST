\name{glyph-scatdot}
\alias{SCATDOT}
\alias{glyph-scatdot}
\title{The SCATDOT glyph creates a graphic of a scatter dot plot of x and y coordinate points.}
\description{
The \var{SCATDOT} glyph creates a scatter dot plot of all of the x and y coordinates for the data referenced
by the scatdot through the col1 and col2 column numbers/names in \var{panelDesc} data.frame structure that point
to columns in the \var{statsDFrame} data.frame provide by the USER with the data. The data from each area row is 
repersented by a dot in the scatdot graphic.  The areas highlighted in the current group/row are shown 
using slightly large dots and the color of their associated area in the group/row set. 
There may be from 3 to 5 areas represented in a single group/row set and link by color to the geographic
area in the micromap drawn from the boundary data provided in the boundary group data.}

\details{
  The only internal argument is \var{j} identifying the graphic column number being drawn.  \var{j} is used by 
  all of the panel functions to scale, outline, and select the area within the total graphic page to
  draw a column of the scatdot glyphs, one per group/row.  It is also used to reference the panelDesc 
  column to retreive related information and links to be able to draw and label the scatdot graphic and column. 
  The scatdot glyph uses the \var{col1} and \var{col2} variable to identify the \var{statsDFrame} columns that contain
  the x and y coordinates for each dot representing an area.  The \var{lab1}, \var{lab2}, \var{lab3}, and \var{lab4} variables
  provide the header and trailing character strings for the column.  
  
  The graphic is a simple x-y plot of the data points provide to the scatdot routines.
  
  The \var{refval}, \var{reftxt}, and \var{panelData} variable in the panelDesc structure are not used.
  
  Scatdot glyph has the ability to add a "DIAGONAL" or "LOWESS" line to the graphic as an option.  The default is a 
  white diagonal line.   The parm variable list in the panelDesc is used by the user to request the type of line, 
  the color, R type, and R width.   In the list for the glyph column, the user starts by specifying "LINE=" to request
  "NONLINE", "DIAGONAL" or a "LOWESS" based line (upper or lower case).  If a "LOWESS" type line is desired, the 
  f value for the LOWESS function is set with "f=<value>" in the parm list. The value must be a decimal number ranging 
  from .01 to 100. The range is not checked. All \var{f} values may not work if to small or large. Check with the 
  R documentation on the "LOWESS" function in the \var{stats} package.  
  The user cannot change the "iter" parameter. It defaults to 3. 
  If the "LOWESS" function parameters are not acceptable to R, the system may terminate the micromapST function call 
  and report an error. The user can also specify the line color (line.col, any valid hex value or "R" color name), 
  line width (line.lwd, range of .5 to 2 is reasonable, default is 0.5) or the "R" line type (line.lty, 
  the default is "solid" or any of the "R" types.) There extra parameters are passed to the glyph code 
  via the panelDesc "parm=list( )" variable. This is the only panelDesc variable that must be a list of lists.   
  Because of the R language there are two ways to successfully add the \var{parm} list to \var{micromapST} package:
  
  \preformatted{
       panelDesc <- data.frame(type = c("map","scatdot"),
                               lab1 = c("US", "population"),
                               lab2 = c(NA, "1920"),
                               col1 = c(NA, "x"),
                               col2 = c(NA, "y")
                             )
       wparm <- list(NA, list(line="LOWESS",f=.77,line.col="blue",line.lwd=1,line.lty="solid"))
       panelDesc$parm <- wparm
       
     or 
  
       panelDesc <- data.frame(type = c("map","scatdot"),
                               lab1 = c("US", "population"),
                               lab2 = c(NA, "1920"),
                               col1 = c(NA, "x"),
                               col2 = c(NA, "y"),
               parm = I(list(NA, list(line="LOWESS",f=.77,line.col="blue",line.lwd=1,line.lty="solid")))
                           )
  }                             
  In the second example, the "I()" function must be used around the list structure to make sure 
  R does not parse the \var{parm} value when adding to the data.frame.
  
  The resulting panelDesc structure should look like:
  \preformatted{
 > str(panelDesc)
  'data.frame':   2 obs. of  6 variables:
 $ type: chr  "map" "scatdot"
 $ lab1: chr  "US" "population"
 $ lab2: chr  NA "1920"
 $ col1: chr  NA "x"
 $ col2: chr  NA "y"
 $ parm:List of 2
  ..$ : logi NA
  ..$ :List of 5
  .. ..$ line    : chr "LOWESS"
  .. ..$ f       : num 0.77
  .. ..$ line.col: chr "blue"
  .. ..$ line.lwd: num 1
  .. ..$ line.lty: chr "solid"
  ..- attr(*, "class")= chr "AsIs"
>   
  }
  
  For details on the lowess R function refer to the stats package documention and the lowess function.
       
  This allows the user to customize these optiona for each scatdot glyphic column, as needed.
  
  The defaults for the \var{scatdot} "parm list variable" are:
  \preformatted{
     if "line='NOLINE'"	There are no defaults.
  	
     if "line='DIAGONAL'" or "line='LOWESS'" then defaults are:	
  	
  	line.col = "white" or "gray20", respectfully
  	line.lwd = 0.5
  	line.lty = "solid"
  	
     if "line='LOWESS'" then
     
     	f = .666666
     	
     if the parameter list is empty or there is no "line=" parameter,
        "line='DIAGONAL'" is assumed.
  } 	
  The user can change any of the \var{scatdot} options using in the parm row list entry.
  
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
