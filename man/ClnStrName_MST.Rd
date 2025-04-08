\name{ClnStrName_MST}
\alias{ClnStrName_MST}
\title{A function to consistantly clean the character strings used for Location IDs}
\description{
The \var{ClnStrName_MST} function is used to clean the location ID character string used 
as the "Name", "Abbr", "ID", and "Alt-Abbr" location ID columns in the name table in a 
border group.  The packages automatically clean these string to make sure match are successful
as much as possible.   The function converts any punctuation or control characters to blanks.
It also converts any of the following characters "+=$^|<>~`" and will not convert "-" or "_" 
characters.  The converted string is then squished to remove leading and trailing blanks and
multiple groups of internal blanks to a single blank.

If a user wants to use to border group name table to convert their own location IDs, it is
recommended passing the original Loc ID list through the ClnStrName_MST function to 
match sure their list will match the "Name", "Abbr", "ID", or "Alt-Abbr" columns in 
the border group location ID columns.  Otherwise, most of the list may not match.

}
\usage{ClnStrName_MST(x)}
\details{
See description
}
\arguments{
  \item{x}{- a vector of one or more character string variables }
}   
\value{The function returns a vector of character strings the same size as the original vector.  
   Each entry has been edited to remove unwanted characters.}
\author{
  Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
  \link{BuildBorderGroup}
}
\keyword{functions}
