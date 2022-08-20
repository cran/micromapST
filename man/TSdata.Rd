\name{TSdata}
\alias{TSdata}
\docType{data}
\title{Time Series Example Dataset}
\description{
Data for Time Series Examples

The data are age-adjusted (2000 U.S. standard) female lung cancer mortality rates
(per 100,000 population) for each year from 1996 to 2010.
}
\usage{data(TSdata)}
\format{
  This dataset is an array with dimensions of 51, 15, 4.  The rownames of the array are the
  51 state and DC abbreviations (2 characters). 
  TSdata[,,1:4] contains the x (time) value, followed by the value for the line, then
  the lower 95\% confidence limit, and finally the upper 95\% confidence limit value.
}
\details{
  The first dimension [\var{st},,] of 51 elements contains each state or DC.  
  This dimension is referenced by the rownames of the array.
  
  The second dimension [,\var{t},] of \var{n} elements in this case are the time periods
  in the time series.  Our example uses the years 1996 to 2010 as the 
  time period values.  A reasonable number of points is between 20 and 30.

  The third dimension [,,\var{v}] of 2 or 4 elements is the x or y values during the time period.
  If no confidence data is provided, the third dimension is 2:
  \itemize{
  \item data[,,\var{1}] is the X value 
  \item data[,,\var{2}] is the mid-Y value (Y) 
  }
  If a confidence band is being plotted in \option{\var{tsconf}} graphs then 
  there are 4 elements.
  \itemize{
  \item data[,,\var{1}] is the X value 
  \item data[,,\var{2}] is the mid-Y value (Y) 
  \item data[,,\var{3}] is the low-Y value
  \item data[..\var{4}] is the high-Y value
  }
  For example, the x,y coordinates for year=1996 (time period = 1) for the first 
  state (\var{AK}) is TSdata[\var{1},\var{1},c(1,2)].
  
  This approach was done to allow a data matrix built for the "tsconf" glyphs 
  to be used for a \var{ts} glyphs.
  
  This data is used by micromapSEER with the "USStatesDF" border group.
  
  \preformatted{
  
  # how to create a new time series data set 
  tempTS <-read.table("...yourfilename.csv",sep=",",header=T)
  yrmat  <-matrix(rep(1996:2010,51),nrow=51,ncol=15,byrow=T)  # year labels
  ratemat<-as.matrix(
         tempTS[,c(8,13,18,23,28,33,38,43,48,53,58,63,68,73,78)]
         )
  locimat<-as.matrix(
         tempTS[,c(9,14,19,24,29,34,39,44,49,54,59,64,69,74,79)]
         )
  hicimat<-as.matrix(
         tempTS[,c(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)]
         )
  workmat<-cbind(yrmat,ratemat,locimat,hicimat)
  TSdata <-NULL   
  TSdata <-array(workmat,dim=c(51,15,4))
  # change state ab from factors to characters.
  rownames(TSdata)<-as.character(tempTS$stab)  
 }
}
\source{Surveillance, Epidemiology, and End Results (SEER) Program (www.seer.cancer.gov) 
SEER*Stat Database: Mortality - All COD, Aggregated With State, Total U.S. (1969-2010) 
(Katrina/Rita Population Adjustment), National Cancer Institute, DCCPS, Surveillance Research Program,
Surveillance Systems Branch, released April 2013.  \cr
Underlying mortality data provided by NCHS (www.cdc.gov/nchs). 
}
\keyword{datasets}