\name{micromapGSetDefaults}
\alias{micromapGSetDefaults}
\title{function to build the micromapGDefault data.frame}
\description{
The micromapGSetDefaults function generates a data.frame containing two lists: the colors
and details list. Each list contains the operational parameters to instruct micromapST on
how to physically construct a micromapST graphic. 

The colors list contains 24 rgb colors:\cr

The 12 basic colors are:
 \describe{
   \item{6 colors}{for the state maps in each group of 6 areas that 
      make up one panel row. Currently the max is 5 areas per group.  The 6th color is 
      reserved for future use.}
   \item{1 color}{for the median state (in the middle)}
   \item{1 color}{for the cumulative hightlight color for the areas already 
   presented in other panel rows.}
   \item{2 colors}{for the when highlighting areas above and below the median.} 
   \item{1 color}{for use to shade not referenced areas in the user data.}
   \item{1 color}{for use to shade not-active areas.} 
 }
and a repeat of the 12 rgb colors with an alpha transparency value of 10\%.

The details list contains the spacing, margins, text size, etc. information to guide the
construction of the micromapST graphic. These lists may be modified by the user, but this
is not recommended.  See the micromapSTDefault description for more information on these
lists.
}
\usage{micromapGSetDefaults()}
\details{
The default colors in the colors list are: red, orange, green, greenish-blue, lavender and magenta
for the area colors, black for the median state color, and light yellow for the
cummulative hightlighting, vary pale red and green for highlight areas above and below the median,
the lightest and lighter gray to shade not referenced and not-active areas on the map.
The default values in the details list can be found in the micromapGDefaults documentation.

This function is primarily used internally by micromapST.  However, if the user wants to see
values of all of the internal variables or wants to make wholesale changes to the layout and
operational changes to the colors and details list, this function can be used to create a copy
of the full colors and details lists.

The return value is a variable equal to list(colors, details) where both colors and details are named lists.
}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\value{\link{micromapGDefaults}}
\keyword{functions}
