\name{glyph-Boxplot}
\alias{BOXPLOT}
\alias{glyph-boxplot}
\title{The boxplot glyph creates a graphic of a scatter dot plot of x and y coordinate points.}
\description{
The \var{boxplot} glyph draws the boxplot for each of the areas being presented in the group/row.  This can 
be up to 5 areas (boxplots) at a time.  The color scheme of each boxplot matches the color 
assigned to the area being presented in this graphic row.  Each boxplot is redrawn from the attribute 
data provided in the boxplot data.frame passed to the glyph code via the \var{panelDesc} \var{panelData}
variable by name.  The boxplot data.frame is the output of the standard R boxplot function when plot= parameter 
is set FALSE.  This produces a data.frame of the information needed to draw the boxplots and outliers.  
The name of the data.frame is passed to the boxplot glyph via the \var{panelData} vector in the panelDesc
data.frame used to describe the requested linked micromap.
}
\details{
  The boxplot data.frame created by calling R's boxplot function with plot=FALSE contain a row of boxplot
  attributes for each set of data passed to it originally.  In this case it would be the data for each 
  area to be analyzed by micromapST.  The data.frame is passed by name using the \option{panelData} row in 
  the \var{panelDesc} data.frame. 
  
  For the boxplot glyph, doesnot use the col1, col2, col3, refval or reftxt rows of the \var{panelDesc} data.frame.   
  
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
