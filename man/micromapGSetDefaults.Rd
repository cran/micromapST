\name{micromapGSetDefaults}
\alias{micromapGSetDefaults}
\title{function to build the micromapGDefault data.frame}
\description{
The micromapGSetDefaults function generates a data.frame containing two lists: the colors
and details list. Each list contains the operational parameters to instruct micromapST on
how to physically construct a micromapST graphic. 
The colors list contains 14 rgb colors:\cr

The 7 basic colors:
 \describe{
   \item{5 colors}{for the state maps in each group of 5 states that 
      make up one panel row.}
   \item{1 color}{for the median state (in the middle)}
   \item{1 color}{for the cumulative hightlight color for the states already 
   presented in other panel rows.}
 }
and a repeat of the 7 rgb colors with an alpha transparency value of 10\%.

The details list contains the spacing, margins, text size, etc. information to guide the
construction of the micromapST graphic. These lists may be modified by the user, but this
is not recommended.  See the micromapSTDefault description for more information on these
lists.
}
\usage{micromapGSetDefaults()}
\details{
The default colors in the colors list are: red, orange, green, greenish-blue and lavender
for the state colors, black for the median state color, and light yellow for the
cummulative hightlighting on the map.
The default values in the details list can be found in the micromapGDefaults documentation.

This function is primarily used internally by micromapST.  However, if the user wants to see
values of all of the internal variables or wants to make wholesale changes to the layout and
operational changes to the colors and details list, this function can be used to create a copy
of the full colors and details lists.
}
\author{
Daniel B. Carr, George Mason University, Fairfax VA, and Jim Pearson, StatNet Consulting,
LLC, Gaithersburg, MD 
}
\value{\link{micromapGDefaults}}
\keyword{functions}
