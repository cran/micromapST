\name{micromapGSetPanelDef}
\alias{micromapGSetPanelDef}
\title{function to build the micromapGPanelDef data.frame}
\description{
The micromapGSetPanelDef function generates a data.frame representing the layout 
of the columns and row groups in the total graphic.  It include the location of each
individual graphic, the spacing of the columns and row groups and spacing for the title
column headers and trailers and x axis when needed.
The resulting structure along with 
the values from micromapGSetDefaults are copies into the package's memory
and used by the "panel functions" to manage the panels during the setup and glyphics 
creation.
}

\usage{micromapGSetPanelDef(nRows,rSizeMaj,rSizeMin,rSepGap,MaxRows,UGrpPattern)}  

\arguments{
   \item{nRows}{for the number of data rows to include in the map.)}
   \item{rSizeMaj}{the maximum size of a row group in inches.  A row group represents
     a number of data rows as one horizontal set of graphs.}
   \item{rSizeMin}{the minimum size of a row group in inches.}
   \item{rSepGap}{the size of the spacing between row groups in the output graphic.}
   \item{MaxRows}{the maximum number of rows.}
   \item{UGrpPattern}{a vector of the number of rows per group.  This is specified by the 
     user as a substitute for the calculated row group pattern.  The number of rows must
     equal the number of rows in the data that match the mapping data and must have the 
     rows grouped so that the top and bottom groups have the most rows.  The pattern can then 
     slowly dimenish to 2 rows as the group approach the middle (median) row group.}
}
\details{
 Minor modifications can be made to the presentation structure through the details= call parameter, 
 but this is not recommended.  See the micromapGDefault description for more information on these
 lists.
}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\value{\link{micromapGSetPanelDef}}
\keyword{functions}
