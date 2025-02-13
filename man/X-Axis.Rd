\name{X-Axis}
\alias{X-Axis}
\title{How the X-Axis is created and user controls.}
\description{
How the X-Axis is created is important to almost every glyph built by \var{micromapST}.
It provides the detailed scaling information on the data in the glyph columns.  The X-Axis needs to provide as much information
to the reader of the overall graphic as possible.  While the space is limited, \var{micromapST} has made several features available to 
help the user manage how the X-Axis presents the data information. 
}
\details{
  Tools that are available:
  \describe{
    \item{No modification}{Layout the X-Axis numbers in a straight line with no modification.  Try to present as many ticks as possible.}
    \item{Reduce Size}{If the X-Axis is very crowed, the package may reduce the font size of all of the X-Axis's to get more information
       visible to the user.  This is generally done in a manner similar to each column casting a vote to reduce the font size.}
    \item{Overlap Protection}{When number are presented at the edges of the panels, they often stretch past the ends of the panel and into
	the next panel's space.  micromap
	ST attempt to catch these problems and may elect to not present a number in that position or vote 
	for a reduction in the font size.  }
    \item{High/Low}{When there is overlap at the edges of panels and when not enough space to present a resaonable number of X-Axis label
        (points), micromapST may present all of the X-Axis labels in a one up and one down pattern.  In this way the numbers on the edge of 
        panels can overlap labels on another panel by present one value in a low position next to the panel line and the next panel present
        it's X-Axis value in an high position avoiding over printing its neighboring value.  When this is enable, all of the 
        numbers in the X-Axis will alternate being presented in a high/low position sequence.  This also allows more numbers to 
        be presented since all neighboring label have more room on the axis.  Generally the font is also reduced so that two line of 
        labels can be presented in about 1.5 lines of space.}
     \item{Scaling}{Depending on the magitude of the X-Axis numbers, scaling the number can save a lot of space and present a more readable 
        X-Axis.  micromapST support two types of scaling.   a) reduce the magnitude of the number by 100, 1000, 10,000, etc and 
        add a second line indicating what the divisor was (tens, hundreds, thousands, etc.)  b) again reduce the magnitude of the numbers
        by factors of 10s and add to the end of each number a scaling letter (H=hundread, T=thousands, M=millions, B=billions, etc.)
        This technique is used along with the High/Low and Overlap Protection options when possible.}
    }
    
  These options are all targeted at improving the readability of the X-Axis.  
  To enhance the label placement, the \var{labeling} package is used to determine the best placement for the X-Axis labels.
  micromapST makes use of the "wilkinson" function and the "extended" function based on "wilkinson".  
   
}
\author{
  Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST}
}
\value{None}
\keyword{functions}
