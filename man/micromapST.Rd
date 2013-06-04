\name{micromapST}
\alias{micromapST}
\title{main function to create linked micromap graphics}
\description{
The micromapST functions use graphic, lattice and RColorBrewer packages to create linked micromap plots 
for the 51 U.S. states and District of Columbia.

One plot row represents a state.  Multiple columns are used to present several graphical representations of the statistical data. 
The primary columns are a MAP and ID (state name). Statistical data in the other columns can be represented by arrows, bar charts, 
boxplots, dots, dots with confidence intervals, and dots with standard error, time series line plots with or without confidence
bands, scatter plots and horizontal stacked (segmented) bar charts.
}
%  The following must be as written or an error is generated during the build.  R expects this to MATCH the call coding - exactly.
\usage{micromapST ( stateFrame, panelDesc, 
                    rowNames  = c('ab','fips','full')[1], 
                    sortVar   = NULL, 
                    ascend    = TRUE, 
                    title     = c('', ''),
                    plotNames = c('ab','full')[2], 
                    colors    = micromapSTDefaults$colors, 
                    details   = micromapSTDefaults$details) 
  }
\arguments{
  \item{stateFrame}{a data.frame containing data for the plots. 
    Used with \code{arrow}, \code{bar}, \code{segbar}, \code{normbar}, \code{ctrbar}, 
    \code{dot}, \code{dotse}, \code{dotconf}, and \code{scatdot} plots.
    Not used with boxplots or time series plots.  \cr 
    The rownames must be state abbreviations, state names, or 2-digit FIPS codes to link to the map dataset. 
    The \code{col1}, \code{col2}, and \code{col3} variables in the panelDesc data.frame link the values in the 
    stateFrame to the panel columns.
   }
  \item{panelDesc}{a data.frame that defines the layout and content of the panels, 
    each of which contains a map or state names or statistical graphics and the location 
    of supporting statistical data in the stateFrame or panelData structures. 
    }  
  \item{rowNames}{defines the type of value used as the row.names. The options are:
    'ab', the 2 character state ID (postal code abbreviations), 'full', the 
    full state name, or 'fips', the 2-digit FIPS code for the state.  The default is 'ab'. 
    The rownames of the \code{stateFrame} and \code{panelDesc} are validated.
    No special characters should be used in any row name.
    See stateNamesFips for a list of these codes.
    }
  \item{sortVar}{defines the column number in the stateframe to be used to 
    sort the stateFrame and panelDesc data.frames before creating the state micromap.
    A vector of column numbers can be used to break ties. For Example: \code{sortVar=c(4,5)}.
    Columns 4 and 5 in the stateFrame are used in the sort.
    }
  \item{ascend}{a logical value. If \code{TRUE}, sortVar will be sorted in ascending order and if FALSE, 
    sortVar will be sorted in descending order.  
    }
  \item{title}{A vector with one or two character strings to use as the overall 
    micromap plot title.
      
    example:    \code{title = 'micromapST Title'}\cr  
             or \code{title = c('title line 1', 'title line 2')}
    }
  \item{plotNames}{defines the type of state names to be displayed.  The options are: 'ab' or 'full'.
     'ab' will display the 2 character state abbreviations,  'full' will display the full state name
     (except that the District of Columbia is shown as D.C. because of space limitations).
     The default is 'full'.
     }
  \item{colors}{the name of a color palette or 
    a vector of 14 color names or '#xxxxxx' values:
    \itemize{
      \item
      5 colors for states in a groups of 5 states,  
      \item 
      1 color for the median state,  
      \item 
      1 foreground color for non-highlighted states in the map. 
      7 translucent colors, e.g., for the time series bands.
     }
     If \code{colors = 'greys'} or \code{colors = 'grays'}, the entire plot will be done 
       in black/white/gray shades.
     If a \code{colors} vector is not provided, the package default colors will be used:
     \itemize{
       \item
       state colors: 'red', 'orange', 'green', 'greenish blue', 'lavender'
       \item
       median state color: 'black'
       \item
       non-highlighed states: 'light yellow'
     }
     It is recommended to use the default.
    }
  \item{details}{defines the spacing, line widths and other details of the plot layout and content; 
    see \code{rlStateDefaults$details} which contains the line spacing and text size, group spacing, etc.
    It is recommended to use the default.
    }
 }

\details{
The micromapST function creates a linked micromap plot for data referencing 
the 50 US States and DC geographical areas.  The function provides links from 
a US state map to several forms of graphical charts:  
dot, dot with confidence intervals, dot SE, arrow, bar chart, time series, time series
with a confidence band, horizontal stacked (segmented) bar, normalized bar and centered bar charts, scattered
dot and box plots. The data values for each column of graphs and each state and DC are 
provided in the \code{stateFrame} data.frame.  The \code{panelDesc} data.frame specifies the type 
of chart, the column numbers in the stateFrame with the statistics for the chart, 
column titles, reference values, etc.   Additional data for boxplots and time series
plots are provided through the panelData column in the panelDesc data.frame
}


\value{None}

\references{
Daniel B. Carr and Linda Williams Pickle, Visualizing Data Patterns with Micromaps, 
CRC Press, 2010
}

\author{
Daniel B. Carr, George Mason University, Fairfax VA, with contributions 
from Jim Pearson and Linda Pickle of StatNet Consulting, LLC, Gaithersburg, MD
}

\examples{

######
#   micromapST - Example # 1 - map with no cumulative shading,
#     2 columns of statistics: dot with 95% confidence interval, boxplot
#     sorted in descending order by state rates
####
# load sample data, compute boxplot
data(wflung00and95,wflung00and95US,wflung00cnty) 
wfboxlist = boxplot(split(wflung00cnty$rate,wflung00cnty$stabr),plot=FALSE)  
# set up 4 column page layout
panelDesc <- data.frame(
  type=c('map','id','dotconf','boxplot'),    
  lab1=c('','','State Rate','County Rates'),  
  lab2=c('','','and 95\% CI','(suppressed if 1-9 deaths)'), 
  lab3=c('','','Deaths per 100,000','Deaths per 100,000'), 
  col1=c(NA,NA,1,NA),col2=c(NA,NA,3,NA),col3=c(NA,NA,4,NA),     
  refVals=c(NA,NA,wflung00and95US[1,1],NA),   
  refTexts=c(NA,NA,'US Rate 2000-4',NA),       
  panelData= c('','','','wfboxlist')          
  ) 
# set up PDF output file, call package
FDir = getwd()       	
FName = paste(FDir,"/EX1-WFLung-2000-2004-State-Dot-County-Box.pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(wflung00and95,panelDesc,sortVar=1,ascend=FALSE,
       title=c("White Female Lung Cancer Mortality, 2000-2004",
                  "State Rates & County Boxplots"))  
dev.off()
##End Example 1

######
#   micromapST - Example # 2 - map with cumulative shading 
#                      from top down (mapcum), arrow and bar charts, 
#                      sorted in descending order by starting
#                      value of arrows (1950-69 rates)
####
# Load example data from package.
data(wmlung5070,wmlung5070US)  
panelDesc = data.frame(
   type=c('mapcum','id','arrow','bar'),		
   lab1=c('','','Rates in','Percent Change'),       
   lab2=c('','','1950-69 and 1970-94','1950-69 To 1970-94'),  
   lab3=c('','','Deaths per 100,000','Percent'),
   col1=c(NA,NA,1,5), 		
   col2=c(NA,NA,3,NA)		
 )
FDir = getwd()		
FName = paste(FDir,"/EX2-wmLung50-70-Arrow-Bar.pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(wmlung5070,panelDesc,sortVar=1,ascend=FALSE,
        title=c("Change in White Male Lung Cancer Mortality Rates",
                   "from 1950-69 to 1970-94")) 
dev.off()
##End Example 2

######
#   micromapST - Example # 3 - Time Series Line Plots with Confidence Bands
#     maptail option highlights states from extremes to middle state
#     read in time series data set example
####
data(TSdata)  
temprates<-data.frame(TSdata[,,2])  
# TSdata structure is array of size c(51,15,4), 
# dimensions = 51 states, 15 years, (year label, point value, low limit, high limit)
panelDesc = data.frame(                    
    type=c('maptail','id','tsconf','dot'),      
    lab1=c('','','Time Series','Female'),  
    lab2=c('','','Annual Rate per 100,000','Most Recent Rate (2010)'),  
    lab3=c('','','Years','Deaths per 100,000'), 
    lab4=c('','','Rate',''),		  
    col1=c(NA,NA,NA,15),        
    panelData =c(NA,NA,'TSdata',NA)
    )
ExTitle = c('Time Series with Confidence bands',
            'Annual Female Lung Cancer Mortality Rates, 1996-2010')
ExFile  = '/EX3-Time-Series-with-Conf'
FDir = getwd()	        	
FName = paste(FDir,ExFile,".pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(temprates,panelDesc,sortVar=15,ascend=FALSE,title=ExTitle)  
dev.off()

# end of example 3

######
#   micromapST - Example 4 - dot followed by a scatter dot columns
#     use same data as Example 3 to compare 1996 & 2010 rates
#     mapmedian option shades states above or below the median (light yellow)
####
data(TSdata)
temprates<-data.frame(TSdata[,,2])  # create simple matrix of rates for dot panel & sorting
panelDesc = data.frame(                 
    type=c('mapmedian','id','dot','scatdot'),  
    lab1=c('','','Female Lung Cancer Mortality','Comparison of Rates'),   
    lab2=c('','','Rate in 1996 (Sort Variable)',
                      'in 1996 (x axis) and 2010 (y axis)'),   
    lab3=c('','','Deaths per 100,000','Deaths per 100,000 in 1996'), 
    lab4=c('','','','Rate in 2010'),	
    col1=c(NA,NA,1,1),                 
    col2=c(NA,NA,NA,15)		
    )
ExTitle <- c( "Dot Plot for 1996, Scatter Plot Comparing 1996 to 2010",
                    "Female Lung Cancer Mortality Rates")
ExFile  <- "/EX4-Scatter-Dots"
FDir = getwd()	        	
FName = paste(FDir,ExFile,".pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(temprates,panelDesc,sortVar=1,ascend=FALSE,title=ExTitle)  
dev.off()

##End Example 4

######
#   micromapST - Example 5 - horizontal stacked (segmented) bars
#     segbar plots the input data, normbar plots percent of total
#     package computes the percents from input data
#     input for the categories for each state must be in consecutive 
#     columns of the input data.frame
####
data(statePop2010)
panelDesc = data.frame(                   
    type=c('map','id','segbar','normbar'), 
    lab1=c('','','Stacked Bar','Normalized Stacked Bar'), 
    lab2=c('','','Counts','Percent'),     
    col1=c(NA,NA,1,1),                     
    col2=c(NA,NA,4,4)		  
    )
ExFile  = "/EX5-Stkd-Bar"
FDir = getwd()	        	
FName = paste(FDir,ExFile, ".pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(statePop2010,panelDesc,sortVar=6,ascend=FALSE,
     title=c("Stacked Bars: 2010 Census Pop by Race, Sorted by Percent Other Race",
             "Categories Left to Right: Hispanic, non-Hispanic White, Black, Other"))  
dev.off()
## End Example 5

######
#   micromapST - Example 6 - centered (diverging) stacked bars
#
#     National 8th grade Math Proficiency NAEP Test Scores Data for 2011
#     source: National Center for Education Statistics, 
#     http://nces.ed.gov/nationsreportcard/naepdata/
#     bar segment values - \% in each of 4 categories: 
#           \% < Basic, \% at Basic, \% Proficient, \% Advanced
####
data(Educ8thData)  
# columns = State abbrev, State name, Avg Score, \%s \<basic, 
#           basic, proficient, advanced
panelDesc = data.frame(                 
    type=c('map','id','dot','ctrbar'),
    lab1=c('','','Avg. Scores','Math Proficiency'),         
    lab2=c('','','','<Basic, Basic, Proficient, Advanced'),  
    lab3=c('','','','\% to Left of 0           |    \% to Right'),  
    col1=c(NA,NA,3,4),col2=c(NA,NA,NA,7)   
  )
ExTitle = c("Stacked Bars: Educational Progress (NAEP) in Math, 2011, 8th Grade",
                  "Centered at Not Proficient vs. Proficient")
ExFile  = "/EX6-Educ-Centered-Bar"
FDir = getwd()	        	
FName = paste(FDir,ExFile,".pdf",sep="")
pdf(file=FName,width=7.5,height=10)
micromapST(Educ8thData,panelDesc,sortVar=3, title=ExTitle)  
dev.off()
rm(Educ8thData)

### End of example 6


}
