\name{micromapST}
\alias{micromapST}
\alias{micromapST.Version}
\title{Linked Micromap Graphics Package}
\description{
Provides a easy and quick means of creating Linked Micromaps for any collection 
of geographically associated areas.  The micromapST package 
uses the standard graphics and RColorBrewer packages to rapidly create 
highly readable linked micromap plots.
This gives the user the ability to explore different views of their data quickly. 

\var{micromapST} uses the border and name information contained in border group
datasets to define the geographical areas used in creating the linked micromaps.

The \var{micromapSEER} function is included to help users of the specialized
\var{micromapST} for NCI Seer areas called \var{micromapSEER} migrate to this package.  
The \var{micromapSEER} function, calls \var{micromapST} with the border group set 
to \var{USSeerBG} to generate linked micromaps for the 20 U. S. Seer Areas.

The \var{micromapST} contains border group datasets with the boundary and name
information for the following::
\itemize{
 \item \var{USStatesBG}   - data from the original \var{micromapST} package for . 
 the U. S. 50 states and the District of Columbia.
 \item \var{USSeerBG}     - data for the 21 U. S. Seer Areas.
 \item \var{KansasBG}     - data for the 105 counties in the state of Kansas.
 \item \var{NewYorkBG}    - data for the 62 counties in the state of New York.
 \item \var{MarylandBG}   - data for the 24 counties in the state of Maryland.
 \item \var{UtahBG}       = data for the 29 counties in the state of Utah
 \item \var{ChinaBG}      - 34 administrative areas in the country of China.
 \item \var{UKIrelandBG}  - 218 administrative areas in UK, Ireland and Isle of Man.
 \item \var{SeoulKoreaBG} - 25 districts in Seoul S. Korea.
 \item \var{AfricaBG}     - 52 countries of the Africa continent.
}

Each plot row represents a single sub-area (state, county or province) within 
the border group area. Each column can be defined to present a different 
graphical representations of the user's data.  
\cr
For linked micromaps, the primary columns are a MAP type, ID (sub-area name) 
and one or more glyphics.  The statistical data is presented in the 
glyphics columns as one of the following glyph types:
\itemize{
   \item arrows,
   \item bars,
   \item boxplots,
   \item dots,
   \item dots with confidence intervals,
   \item dots with standard error,
   \item dot with a significance marker,
   \item time series line plots with or without confidence bands,
   \item scatter plots, and
   \item horizontal stacked (segmented, centered, and normalized) bars.
 }

All border groups are distrbuted with the package as .rda datasets.
}
\usage{micromapST ( statsDFrame, 
                     panelDesc, 
                     rowNamesCol = NULL,
                     rowNames   = NULL, 
                     sortVar    = NULL, 
                     ascend     = TRUE, 
                     title      = c("", ""),
                     plotNames  = NULL,
                     axisScale  = NULL,
                     staggerLab = NULL,
                     bordGrp    = NULL,
                     bordDir    = NULL,
                     dataRegionsOnly = NULL,
                     regionsB   = NULL,
                     grpPattern = NULL,
                     maxAreasPerGrp = NULL,
                     ignoreNoMatches = FALSE,
                     colors     = NULL, 
                     details    = NULL) 
  }
\arguments{
  \item{statsDFrame}{a data.frame containing data used with the following plots/glyphs:. 
    \var{arrow}, \var{bar}, \var{segbar}, \var{normbar}, \var{ctrbar}, 
    \var{dot}, \var{dotse}, \var{dotconf}, \var{dotsignif}, 
    and \var{scatdot} plots.
    The data for the \var{boxplot} and time series plots (\var{TS} and \var{TSConf})
    is more complex and multi-dimensional and is passed to the glyph generation routines 
    via the \var{panelData} parameter (see below for more details.)\cr
    The \var{row.names} of \var{statsDFrame} data.frame are used as the link identifier between
    the data row to the map boundary data.  For U.S. state data, the link identifier must
    be the the state's 2 character abbreviations, full names, or 2-digit US FIPS codes as the \var{id}.
    For Seer Areas, the area identifier is the Seer Area abbreviates as defined in the 
    \var{USSeerBG} document. Refer to the documentation on each border group for the exact
    names, abbrevviations and ids defined for each sub-area. \cr
    The data columns in the statsDFrame are associated with each graphic using the 
    \var{col1}, \var{col2}, and \var{col3} vectors in the panelDesc data.frame by column 
    name or number.
    }
  \item{panelDesc}{a data.frame that defines the description of each column: types, 
    associated data columns in the \var{stateFrame} data.frame, column titles (top and bottom), 
    reference values and text, and names of additional data.frames
    for complex glyphics (time series and boxplots). See section on panelDesc data.frame.
    for more details. 
    }  
  \item{rowNames}{defines the type of value used as the row.names in the \var{stateFrame}. 
    The options are:  
    \itemize{
      \item \option{ab} - an abbreviation for the sub-area: 2 character state ID, 
       postal code abbreviation, ISO abbreviation, or a generally accepted 
       abbreviation for the sub-area;
      \item \option{id} - sub-area ID, numerical integer identifiers.  
       In the U.S. border groups, the state and county FIPs codes are used. 
       An alias for \option{id} is \option{FIPS}.;
      \item \option{full} - the full sub-area names;
      \item \option{alias} - partial match aliases.  Used only with the Seer Registry
       sub-area registry names as outputted by the SeerStat programs; or
      \item \option{alt_ab} - an alternate sub-area abbreviation.  
       In some areas, there area two accepted set of abbreviations.  
       If the border group has an alternate abbreviation defined, this 
       option allows the alternative abbreviation to be used as the \var{rowNames} in the data.
    }
  
    See the documentation on each border group for details on
    the full names, abbreviatations, optional alternate abbreviations 
    and numeric identifiers defined for each area in the particular 
    border group in the \var{areaNamesAbbrsIDs} R object. \cr
    The default is \option{ab}.
    
    If a border group does not have one of these sets of name data, then the corresponding 
    option will not be available.
    
    The linkage between data and boundaries is accomplished using the strings in the 
    statsDFrame column identified by \var{rowNamesCol} or if no \var{rowNamesCol} is specified in 
    the \code{row.names} information of the \var{statsDFrame}, 
  
    The linkage values are validated to against the \var{areaNamesAbbrsIDs} data.frame 
    for the specified border group for the \var{micromapST} call.
    If a sub-area in the border group is not referenced in the data, it is outlined on the map, 
    but not colored.  If the sub-area link cannot be matched, a warning message is generated and 
    the execution of the package is stopped.  Unless, the user has set the \option{noMatchIgnore} 
    call argument to TRUE and the data will be ignored.
    
    The \option{alias} option is implemented to support with the \var{USSeerBG} 
    border group and data generated by the Seer Stat program. The registry column 
    identifying the Seer Area contains the Seer Area name and additional information.
    This option along with the \var{alias} field in the border group allows the 
    package to use the registry column as the linkage to the boundary data using a  
    wildcard or partial string allows the registry column generated by Seer Stat 
    to be used as the linkage.  This option is controlled the \var{enableAlias} variable 
    in the border group's \var{areaParms} data.frame.
   }
  \item{rowNamesCol}{allows the user to specify the data.frame column that contains 
    the sub-area string to be used to link the data row to the boundary data for the sub-area.
    The \var{rowNames} option above specifies which name information (full name, abbreviation,
    id, alternate abbreviations) the sub-area string is matched to in the border group 
    name information. (see the border group documentation for more details.)  
    The \var{rowNamesCol} value must be a column number or column name within the 
    \var{statsDFrame} data.frame.  The default value for \var{rowNamesCol} is to use 
    row.names of the passed data.frame.
   }
  \item{sortVar}{defines the column name(s) or number(s) in the \var{statsDFrame} 
    data.frame to be used to sort the \var{statsDFrame} data.frames before creating 
    the state micromap.  A vector of column names or numbers can be used sort 
    on multiple columns and to break ties. 
    
    For Example: \code{sortVar=c(4,5)} where columns 4 and 5 in the \var{statsDFrame} 
    are used in the sort.
    If the user needs to sort the data based on information in the boxplot or time-series data, 
    the best practice is to copy the data into the \var{statsDFrame}.
    }
  \item{ascend}{a logical value. If \var{TRUE}, \var{sortVar} will be sorted in ascending order. 
    If \var{FALSE}, \var{sortVar} will be sorted in descending order.  
    The default value is \var{TRUE}.  
    }
  \item{title}{A character vector with one or two character strings to be used 
    as the title of the overall micromap plot page. 
     \preformatted{  
      For example:  
          \code{title = "micromapST Title"} or   
          \code{title = c("title line 1","title line 2")}
      }
    }
  \item{plotNames}{defines the type of state names to be displayed when an \var{id} glyph column 
    requested.  The options are: \option{ab} or \option{full}.  
    \option{ab} will display the area abbreviations in uppercase as defined in the border 
    groups name information. \cr 
    \option{full} will display the full area name as properly capitalized
    strings as defined in the border group name information. \cr
    (For the U.S. Stata name information, 
    the full name for District of Columbia is shown as Dist. of Col.. because of space limitations). \cr
    The default is \option{ab}.
    }
  \item{axisScale}{defines the type of axis labels to be used and if scaling is applied.  
    The acceptable values are \option{o}, \option{w}, \option{s}, and \option{sn}.  
    \option{o} is the original method using the pretty
    function limiting the values to the range of the data.  
    \option{w} is the default and uses the Wilkinson algorithm to generate the axis labels for 
    a set of data,
    \option{s} uses the wilkinson algorithm,
    adjusts the range to cover the labels, and determines a scaling to apply to the 
    labels.  If 1000 is the scaling, "in the thousands" is added to the column titles.
    \option{sn} uses the wilkinson algorithm, adjusts the range to cover the labels, and checks
    each value to determine if it should be scaled.  If it is scales, the scale identifier
    is added as a suffix. (e.g., 1240334 become 1.24M)   "w" is the wilkinson algorithm 
    without any scaling.
    }
  \item{staggerLab}{is a logical variable the controls if the axis labels are staggered and 
    drawn on on two lines instead of one.  If set to FALSE (the default), the labels are 
    not staggered.  If TRUE, two lines are used to draw the axis labels, alternating labels
    on each line.
    }
  \item{bordDir}{specifies the path to a border group dataset (.rda) that is external to the 
    \var{micromapST} package.  The name of the border group is specified in the \var{bordGrp} 
    parameter.
    }
  \item{bordGrp}{specifies which preloaded border group to use for the area names, 
     abbreviations, numeric identifier, and area boundary data.  The supported 
     border groups are \var{USStatesBG} and \var{USSeerBG}.  
     The default \var{bordGrp} value is \var{USStatesBG}.  For more information 
     on building your own border group refer to the section in this manual on the
     \var{BuildBorderGroup} function.
    }
  \item{dataRegionsOnly}{specifies to only map regions containing data when 
     \var{aP_Regions} is set to \var{TRUE}.  This indicates the name table contains
     the information to draw partial area maps of regions.  When set to \var{TRUE},
     only regions within the area are drawn that contain at least one sub-area
     with data provided by the caller.  If \var{FALSE}, the entire area is drawn.
     The default is \var{FALSE}.  Retional boundaries are not required, but suggested.
     See border group documentation for more details.
    }
  \item{regionsB}{when regional boundaries are provided, controls whether to 
     overlay the boundaries on the map.  If dataRegionsOnly is set to \var{TRUE} and 
     only a subset of regions will be mapped and regional boundaries are 
     provided, regionsB is et to \var{TRUE}.  The default is \var{FALSE}.
     Independent of \option{dataRegionsOnly}, if set to \var{TRUE}, if present,
     regional boundaries are overlayed on the map,  If set to \var{FALSE}, 
     no regional boundaries are drawn.
     See border group documentation for more details.
    }
  \item{grpPattern}{The micromapST package generates the pattern of rows to panel 
    groups automatically. The pattern is based on a maximum of 5 rows per group 
    and number of rows can only decend from the edges toward the median row.  
    The pattern generated by the package can be overriden by using the 
    \var{grpPattern} to specify the pattern to use as a numeric vector.  
    The provided vector must pass the following checks:
    a) must be a numeric vector,
    b) the sum of the values in the vector must equal the number of rows in the user 
    supplied statsDFrame data frame
    c) the maximum number of rows per panel group is 5.
    d) the number of rows per panel must be integers and decending from the outside to the 
    middle of the vector.
    }
  \item{maxAreasPerGrp}{This parameter defines the maximum number of areas to be 
    represented by a group-row in the resulting linked micromap.  
    The value can be 5 or 6.
    }
  \item{ignoreNoMatches}{The \var{micromapST} package will automatically handle 
    situations where there is no data for a sub-area in an area.  However, it 
    will stop processing if there is data for a non-existing area.  To instruct 
    the package to ignore rows of data for sub-areas that do not have 
    boundary information in the border group, set the call argument 
    \option{ignoreNoMatches} to \var{TRUE} to get the package to detail 
    the data rows from the processing.
    }
  \item{colors}{is a vector containing  a vector of 12 or 24 color 
    names or values ("#xxxxxx") or the name of a color palette. 
    The vector of 12 or 24 color names or "#xxxxxx" values 
    are used to define the colors used for:
    \itemize{
       \item The 6 colors in each group for the states and symbols in 
        the glyphs. one color per row (state).  
       \item 1 color for the median state and glyphics,  
       \item 1 foreground color for highlighted states in the map.  
        This is used to highlight states already referenced previously 
        or have meaning depend on the type of map requested. 
        The usage is as follows:
        \preformatted{
        "map"      - not used.
        "mapcum"   - highlight areas previously referenced
                     above.
        "maptail"  - highlight areas previously referenced 
                     above the median row and highlight 
                     remaining states below the median row.
        "mapmedian"- highlight all areas above the median 
                     in maps above the median row and 
                     highlight all areas below the median 
                     in maps below the median row.
         }            
       \item 2 accent colors for "mapmedian" sub-area colors for above 
       median and below median.
       \item When an 12 additional colors are specified, 
       they are used as the translucent colors in the 
       \option{tsconf} confidence intervals bands. 
       If only 12 colors are provided, the additional
       12 translucent colors are generated using a 20\% 
       transparent version of the original color.
       \preformatted{
          e.g., \code{adjustcolors(colors,0.2)}
       }
     }
     The only color palette support is a gray palette to permit 
     publication of the linked micromaps using a gray scale 
     instead of color.  By setting \var{colors} = "greys", 
     "grays", or "bw", the entire plot will be generated using 
     gray scale that has been balanced to maintain readability 
     and reproduction without the use of color printing.  
     Additional color palettes may be supported in future releases.
     
     If a \var{colors} vector is not provided, the package default 
     colors will be used:
     \itemize{
       \item 6 state colors: "red", "orange", "green", "greenish blue", "lavender"
       \item 1 median state color: "black"
       \item 1 highlighed states: "light yellow" for "map", "mapcum", "maptail"
       \item 2 highlighed states: "light red" and "light green" for "mapmedian" and
       \item 12 translucent colors using the above colors at 20\%.
     }
     It is strongly recommended to use the default.
    }
  \item{details}{defines the spacing, line widths and many other details 
    of the plot layout, structure and content; 
    see \var{micromapGDefaults$details} for more details.
    Generally \var{details} does not need to be specified, the default values 
    will always be used and are strongly recommended. However, in a few cases, 
    it may be desireable to turn off or disable a feature.  In these cases,
    the user can specify just the specific variable and value in a list and 
    pass it to \var{micromapST} via the \var{details} parameter.  For example:
    \preformatted{
     details=list(SNBar.Middle.Dot=FALSE,SBar.varht=FALSE)
    }
    The entire details variable list does not have to be passed. See the section 
    on the \var{micromapGDefaults$details} for more details.
  }
}
\details{The \var{micromapST} function creates a linked micromap plot 
for data referencing a collection of geographic related areas, 
like the 50 US States and DC geographical areas or U.S. Seer Areas.  
The function provides links from a US state map to several forms 
of graphical charts:  
dot (\option{dot}), 
dot with confidence intervals (\option{dotconf}), 
dot standard error (\option{dotse}), 
dot with significance mark (\option{dotsignif}), 
arrow (\option{arrow}), 
bar chart (\option{bar}), 
time series (\option{ts}), 
time series with a confidence band (\option{tsconf}), 
horizontal stacked (segmented) bar (\option{segbar}), 
normalized bar (\option{normbar}), 
centered bar charts (\option{ctrbar}), 
scattered dot (\option{scatdot}), and 
box plots (\option{boxplot}). 
The data values for each column of graphs and each area are 
provided in the \var{statsDFrame} data.frame.  The panelDesc data.frame specifies the type 
of chart, the column numbers in the \var{statsDFrame} with the statistics for the chart, 
column titles, reference values, etc.   Additional data for boxplots and time series
plots are provided through the panelData data.frame column. 

The following sets of data have been included in the package to support the examples and provide samples of 
data the micromapST package can utilize.

Dataset contained in the package to support the examples provides are:
 \describe{
    \item{statePop2010}{U. S. State population data for 2010.
      }
    \item{TSdata}{To be Added
      }
    \item{Educ8thData}{To be Added
      }
    \item{stateData}{To be Added
      }
    \item{wflung00and95}{White Female Lung (wflung) data from 1995 and 2000
      }
    \item{wflung00cnty}{Wflung US data for 2000 by county.
      }
    \item{wflung00and95US}{Wflung US data for 2000 and 1995.
      }
    \item{wflung5070}{Wflung  data for 50s to 70s 
      }
    \item{wflung5070US}{ Wflung  data for 50s to 70s US rates
      }
    \item{KanPopInc}{Kansas state population by county.
      }
    \item{mdPopData}{Maryland state population by county. 
      }
    \item{nyPopData}{New York state population by county.
      }
    \item{UtahPopData}{Utah state population by county.
      }
    \item{AfricaPopData}{Africa population by country.
      }
    \item{SeoulPopData}{Seoul City population by district.
      }
    \item{cnPopData}{China population by province and adminstrative district.
      }
  }

}
\value{None}
\author{
Daniel B. Carr, George Mason University, Fairfax VA, with 
contributions from Jim Pearson and Linda Pickle of StatNet 
Consulting, LLC, Gaithersburg, MD
}
\references{
  Daniel B. Carr and Linda Williams Pickle, Visualizing Data Patterns with Micromaps, CRC
  Press, 2010\cr
  Linda Williams Pickle, James B. Pearson Jr., Daniel B. Carr (2015), micromapST: Exploring and Communicating Geospatial Patterns in US State Data.,
  Journal of Statistical Software, 63(3), 1-25., \url{https://www.jstatsoft.org/v63/i03/}
}
\seealso{
  \link{micromapST},
  \link{micromapSEER}
}
\examples{

###
#
#   micromapST - Example # 01 - map with no cumulative shading,
#     2 columns of statistics: dot with 95\% confidence interval, 
#     boxplot sorted in descending order by state rates, using 
#     the default border group of "USStatesBG", with default symbols.
###

# load sample data, compute boxplot
TDir<-"c:/projects/statnet/"  # my private test PDF directory exist, don't use temp.
if (!dir.exists(TDir)) {TDir <- paste0(tempdir(),"/") }  # get a temp directory for the output 
                                # PDF files for the example.
cat("TempDir:",TDir,"\n")

   # replace this directory name with the location if you want to same 
   # the output from the examples.

utils::data(wflung00and95,wflung00and95US,wflung00cnty,envir=environment()) 

wfboxlist = graphics::boxplot(split(wflung00cnty$rate,wflung00cnty$stabr),
                      plot=FALSE) 

# set up 4 column page layout

panelDesc01 <- data.frame(
  type=c("map","id","dotconf","boxplot"),    
  lab1=c("","","State Rate","County Rates"),  
  lab2=c("","","and 95\% CI","(suppressed if 1-9 deaths)"), 
  lab3=c("","","Deaths per 100,000","Deaths per 100,000"), 
  col1=c(NA,NA,1,NA),col2=c(NA,NA,3,NA),col3=c(NA,NA,4,NA),     
  refVals=c(NA,NA,NA,wflung00and95US[1,1]),   
  refTexts=c(NA,NA,NA,"US Rate 2000-4"),       
  panelData= c("","","","wfboxlist")          
  ) 
panelDesc <- panelDesc01
# set up PDF output file, call package

ExTitle <- c("Ex01-US White Female Lung Cancer Mortality, 2000-2004", 
               "State Rates & County Boxplots")
               
grDevices::pdf(file=paste0(TDir,"Ex01-US-WFLung-2000-2004-St-DotCf-Co-Box.pdf"),
     width=7.5,height=10)

micromapST(wflung00and95, panelDesc01, sortVar=1, ascend=FALSE,
           title=ExTitle
         )  

x <- grDevices::dev.off()
#
### End Example 01

###
#
#   micromapST - Example # 02 - map with cumulative shading 
#                 from top down (mapcum), arrow and bar charts, 
#                 sorted in descending order by starting
#                 value of arrows (1950-69 rates) using default
#                 border group of "USStatesDF".  This 
#                 example also provides custom colors for the 
#                 linked micromaps, highlights, etc.
#   
###

# Load example data from package.
utils::data(wmlung5070,wmlung5070US,envir=environment())  

panelDesc02 <- data.frame(
   type=c("mapcum","id","arrow","bar"),		
   lab1=c("","","Rates in","Percent Change"),       
   lab2=c("","","1950-69 and 1970-94","1950-69 To 1970-94"),  
   lab3=c("MAPCUM","","Deaths per 100,000","Percent"),
   col1=c(NA,NA,"RATEWM_50","PERCENT"), 		
   col2=c(NA,NA,"RATEWM_70",NA)		
 )
 
colorsRgb = matrix(c(                    # the basic 7 colors.
 213,  62,  79,   #region 1: red	    #D53E4F - Rust Red
 252, 141,  89,   #region 2: orange	    #FC8D59 - Brn/Org
 253, 225, 139,   #region 3: green	    #FEE08B - Pale Brn
 153, 213, 148,   #region 4: greenish blue  #99D594 - med Green
  50, 136, 189,   #region 5: lavendar 	    #3288BD - Blue
 255,   0, 255,   #region 6                 #FF00FF - Magenta    
 .00, .00, .00,   #region 7: black for median #000000 - Black
 230, 245, 152,   #non-highlighted foreground #E6F598 - YellowGreen
 255, 174, 185,   # alternate shape upper   #FFAEB9 - Mauve
 191, 239, 255,   # alternate shape lower   #BFEFFF - Cyan
 242, 242, 242,   # lightest grey for non-referenced sub-areas  #F2F2F2
 234, 234, 234),  # lighter grey for bkg - non-active sub-areas. #EAEAEA
 
  ncol=3,byrow=TRUE)

xcolors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],
                            maxColorValue=255),
              # set solid colors
            grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],64,
                            maxColorValue=255))   
              # set translucent colors for time series.

# set up reference names for color set
names(xcolors) =c("rustred","orange","lightbrown","mediumgreen", 
                  "blue","magenta", "black","yellowgreen",
                  "mauve","cyan","lightest grey","lighter grey",
                  "l_rustred","l_orange","vlightbrown","lightgreen", 
                  "lightblue","l_black","l_yelgreen","l_mauve",
                  "l_cyan","l_lightest grey","l_lighter grey")       

ExTitle <- c("Ex02-US Change in White Male Lung Cancer Mortality Rates",
                   "from 1950-69 to 1970-94-Diff colors")

grDevices::pdf(file=paste0(TDir,"Ex02-US WmLung50-70-Arrow-Bar.pdf"),width=7.5,height=10)

micromapST(wmlung5070,panelDesc02,sortVar=1,ascend=FALSE,
            title=ExTitle, colors=xcolors
            ) 

x <- grDevices::dev.off()
#
### End Example 02

\dontrun{
###
#
#   micromapST - Example # 03 - Time Series Line Plots with 
#     Confidence Bands maptail option highlights states from extremes 
#     to middle state read in time series data set example using the 
#     default border group of "USStatesDF".
#
###

# Load example data from package.
utils::data(TSdata,envir=environment())  
temprates     <-  data.frame(TSdata[,,2])  

# TSdata structure is array of size c(51,15,4), 
# dimensions = 51 states, 15 years, (year label, point value, low limit, 
# high limit)

panelDesc03   <- data.frame(                    
    type=c("maptail","id","tsconf","dot"),      
    lab1=c("","","Time Series","Female"),  
    lab2=c("","","Annual Rate per 100,000","Most Recent Rate (2010)"),  
    lab3=c("","","Years","Deaths per 100,000"), 
    lab4=c("","","Rate",""),		  
    col1=c(NA,NA,NA,15),        
    panelData =c(NA,NA,"TSdata",NA)
    )
ExTitle <- c("Ex03-US Time Series with Confidence bands",
               "Annual Female Lung Cancer Mortality Rates, 1996-2010")

grDevices::pdf(file=paste0(TDir,"Ex03-US Time-Series-with-Conf.pdf"),
    width=7.5,height=10)

micromapST(temprates,panelDesc03,sortVar="P15",ascend=FALSE,
           title=ExTitle)  

x <- grDevices::dev.off()
#
### End Example 03
}

###
#
#   micromapST - Example # 03a - Time Series Line Plots with 
#     Confidence Bands maptail option highlights states from extremes 
#     to middle state read in time series data set example using the 
#     default border group of "USStatesDF".
#
#   Specify the x-Axis values are dates and to format them as dates.
###

# Load example data from package.
utils::data(TSdata,envir=environment())  
temprates      <-  data.frame(TSdata[,,2])   # y rate

# In the original package TS data, the x data was not 
# a date value, it was the year number. To be able to demonstrate
# the X-Axis Date format labeling, these were changed to Date values
# by effectively substracting 1970-1-1 from the year value. 

####
#
#  Example 3a - Building TS conf array and converting years
#  into date values for the X-Axis and labels.
# 
# example of build a TS Conf array.
#
#  Using the old TSdata array as a starting point and source of data,
#  but build an entirely new TS array structure in a similar manner
#  that might be used to build your own time series array.
#

data(TSdata)   # get old array
TSAreas      <- row.names(TSdata)  # one per area (index 1)
NewArray     <- array(dim=c(51,15,4),dimnames=list(TSAreas))  
  # this is for 51 states, 15 samples/observations, and 4 values per sample.

for (inx in seq(1,length(TSAreas)))  { # loop once per area
  
  Samp     <-  TSdata[inx,,]            # samples for an area
            # each sample has 15 observations of 4 values.
            # value 1 is the X axis data or the DATE of the observation
  Samp[,1]  <- as.Date(paste0(as.character(Samp[,1]),"-01-01"))   
    # convert simple year number to date
  NewArray[inx,,] <- Samp
  
}

# setting the attribute "xIsDate" on array to TRUE, signals micromapST 
# the user wants to see the x-axis values as dates.

\dontrun{
attr(NewArray,"xIsDate") <- TRUE

# TSdata and NewArray structures are arrays of size c(51,15,4), 
# dimensions = 51 states, 15 years, (year label, point value, low limit, high limit)

panelDesc03a   <- data.frame(                    
    type=c("maptail","id","tsconf","dot"),      
    lab1=c("","","Time Series (YYYY-MM)","Female"),    
      # recommend adding to the column title a note about the date format used.
    lab2=c("","","Annual Rate per 100,000","Most Recent Rate (2010)"),  
    lab3=c("","","Years","Deaths per 100,000"), 
    lab4=c("","","Rate",""),		  
    col1=c(NA,NA,NA,15),        
    panelData =c(NA,NA,"NewArray",NA)
    )
    
ExTitle <- c("Ex03a-US Time Series with Confidence bands with time (yyyy-mm)",
               "Annual Female Lung Cancer Mortality Rates, 1996-2010")

grDevices::pdf(file=paste0(TDir,"Ex03a-US Time-Series-with-Conf wDates.pdf"),
    width=7.5,height=10)

micromapST(temprates,panelDesc03a,sortVar="P15",ascend=FALSE,
           axisScale="s",
           title=ExTitle)  

x <- grDevices::dev.off()
}
#
### End Example 03a

###
#
#   micromapST - Example 04 - dot followed by a scatter dot columns
#     use same data as Example 3 to compare 1996 & 2010 rates
#     mapmedian option shades states above or below the median 
#     (light yellow) using the default border group of "USStatesBG"
#
#   USES data loaded for Example 03 (temprates).
#
####

# Load example data from package.
utils::data(TSdata,envir=environment())  
temprates      <-  data.frame(TSdata[,,2])   # y rate

panelDesc04 <- data.frame(                 
    type=c("mapmedian","id","dot","scatdot"),  
    lab1=c("","","Female Lung Cancer Mortality","Comparison of Rates"),   
    lab2=c("","","Rate in 1996 (Sort Variable)",
                      "in 1996 (x axis) and 2010 (y axis)"),   
    lab3=c("","","Deaths per 100,000","Deaths per 100,000 in 1996"), 
    lab4=c("","","","Rate in 2010"),	
    col1=c(NA,NA,1,1),                 
    col2=c(NA,NA,NA,15)		
    )
    
ExTitle <- c("Ex04-US Dot Plot for 1996, Scatter Plot Comparing 1996 to 2010",
               "Female Lung Cancer Mortality Rates")

FName <- paste0(TDir,"Ex04-US FLCMR Scatter-Dots-1996-2010.pdf")
grDevices::pdf(file=FName,width=7.5,height=10)

micromapST(temprates,panelDesc04,sortVar=1,ascend=FALSE,title=ExTitle)  

x <- grDevices::dev.off()
#
### End Example 04

###
#
#   micromapST - Example 05 - horizontal stacked (segmented) bars
#     segbar plots the input data, normbar plots percent of total
#     package computes the percents from input data
#     input for the categories for each state must be in consecutive 
#     columns of the input data.frame using the default border group 
#     of "USStatesBG"
####

# Load example data from package.
utils::data(statePop2010,envir=environment())

panelDesc05 <- data.frame(                   
    type=c("map","id","segbar","normbar"), 
    lab1=c("","","Stacked Bar","Normalized Stacked Bar"), 
    lab2=c("","","Counts","Percent"),     
    col1=c(NA,NA,"Hisp","Hisp"),                     
    col2=c(NA,NA,"OtherWBH","OtherWBH")		  
    )
ExTitle <- c("Ex05-Stkd Norm Bars: 2010 Census Pop by Race, Sorted by Cnt Other Race",
             "Cat-L to R: Hispanic, non-Hisp White, Black, Other-sn-varbar")

grDevices::pdf(file=paste0(TDir,"Ex05-US Stkd-Norm Bar-var-height.pdf"),
     width=7.5,height=10)

micromapST(statePop2010, panelDesc05, sortVar="OtherWBH", ascend=FALSE,
           title= ExTitle,
           details=list(SNBar.varht=TRUE), axisScale="sn" )  
     
x <- grDevices::dev.off()
#
### End Example 05

\dontrun{
###
#
#   micromapST - Example 06 - horizontal stacked (segmented) bars
#     segbar plots the input data, normbar plots percent of total
#     package computes the percents from input data
#     input for the categories for each state must be in consecutive 
#     columns of the input data.frame using the default border group
#     of "USStatesBG".
#
#     Turning off the variable bar height and the midpoint dot features
#     in the horizontal stacked bars (segmented)
#
#  USES data loaded for Example 05 above - statePop2010.
#
###

# Reuse data loaded for Example 5 above.

panelDesc06= data.frame(                   
      type=c("map","id","segbar","normbar"), 
      lab1=c("","","Stacked Bar","Normalized Stacked Bar"), 
      lab2=c("","","Counts","Percent"),     
      col1=c(NA,NA,"Hisp","Hisp"),                     
      col2=c(NA,NA,"OtherWBH","OtherWBH")		  
    )
   
ExTitle <- c("Ex06-Stacked Norm Bars: 2010 Census Pop by Race, Sorted by Other Race",
              "Cat-L to R: Hisp, non-Hisp White, Black, Other,ID-diamond")

grDevices::pdf(file=paste0(TDir,"Ex06-Stkd-Norm-Bar-fixedheight-nodot.pdf"),
    width=7.5,height=10)

micromapST(statePop2010,panelDesc06,sortVar=4,ascend=FALSE,
            title= ExTitle,
            details=list(SNBar.Middle.Dot=FALSE,SNBar.varht=FALSE,Id.Dot.pch=23)
     )  
x <- grDevices::dev.off()
#
### End Example 06
}

###
#
#   micromapST - Example 07 - centered (diverging) stacked bars
#
#     National 8th grade Math Proficiency NAEP Test Scores Data for 2011
#     source: National Center for Education Statistics, 
#     http://nces.ed.gov/nationsreportcard/naepdata/
#     bar segment values - \% in each of 4 categories: 
#           \% < Basic, \% at Basic, \% Proficient, \% Advanced
#     using the default border group of "USStatesBG".
####

# Load example data from package.
utils::data(Educ8thData,envir=environment())  

# columns = State abbrev, State name, Avg Score, \%s \<basic, 
#           basic, proficient, advanced

panelDesc07 <- data.frame(                 
    type=c("map","id","dot","ctrbar"),
    lab1=c("","","Avg. Scores","Math Proficiency"),         
    lab2=c("","","","<Basic, Basic, Proficient, Advanced"),  
    lab3=c("","","","\% to Left of 0           | \% to Right"),  
    col1=c(NA,NA,"avgscore","PctBelowBasic"),
    col2=c(NA,NA,NA,"PctAdvanced")   
  )
  
ExTitle <- c("Ex07-US Dot Stkd Bars:Educational Progress (NAEP) in Math-2011, 8th Grade",
             "Centered at Not-Prof vs. Prof")

grDevices::pdf(file=paste0(TDir,"Ex07-US Dot-Centered-Bar Educ.pdf"),width=7.5,height=10)

micromapST(Educ8thData,panelDesc07,
             sortVar=3, 
             title=ExTitle)  

x <- grDevices::dev.off()
#
### End of example 07

\dontrun{
###
#
#  Example 08 - use of state.x77 data table as data source
#     Data does not contain a row for DC, a missing sub-area.
#     Example also uses a smaller then 7.5 x 10 graphic space.
#
###

utils::data(state,envir=environment())

stateData <- as.data.frame(state.x77)

rownames(stateData) <- state.abb

panelDesc08 <- data.frame(type = c("maptail", "id", "dot"),
                   lab1 = c("", "", "Murder"),
                   lab3 = c("", "", "Murders per 100K Population"),
                   col1 = c(NA, NA, 5))
                   
ExTitle <- c("Ex08-US LM Plot of Murders in the United States",
                      "No DC row entry.")

grDevices::pdf(file = paste0(TDir,"Ex08_US state.x77_no_DC.pdf"), 
    width = 5, height = 9) 

micromapST(stateData, panelDesc08, 
           sortVar = 5, ascend = FALSE,
           title = ExTitle)

x <- grDevices::dev.off()

#
### End Example 08
}

###
#
#  Example 09 - US state map based on data from state.x77 table with
#     DC row added to complete data.frame, but with missing values (NAs).
#     The DC row will be sorted to the bottom of the list size 
#     it does not contain any data.
#
#   Used data and the panelDesc data.frames (stateData and panelDesc16) 
#   used in example 09.
#
###

panelDesc09 <- data.frame(type = c("maptail", "id", "dot"),
                   lab1 = c("", "", "Murder"),
                   lab3 = c("", "", "Murders per 100K Population"),
                   col1 = c(NA, NA, 5))
                   
# add DC as 51st state with missing data "NA" to stateData.  

rm(state)

utils::data(state,envir=environment())

stateData <- as.data.frame(state.x77)
rownames(stateData) <- state.abb

stateData <- rbind(stateData, DC = rep(NA, 8))   
                  # missing values for DC row.

ExTitle <- c("Ex09-US-LM Plot of Murders in the United States",
                  "DC row added with NA, decending.")

grDevices::pdf(file =paste0(TDir,"Ex09_US_state.x77_DCasNA_D.pdf"), 
     width = 5, height = 9) 

micromapST(stateData, panelDesc09, 
           sortVar = 5, ascend = FALSE,
           title = ExTitle)

x <- grDevices::dev.off()
#
### End Example 09

###
#
#  Example # 10 - Maps Seer Registries using the micromapST function 
#   with the bordGrp = "USSeerBG".  
#
###

# Load example data from package.
utils::data(Seer18Area,envir=environment())

# set up 4 column page layout

panelDesc10 = data.frame(
  type=c("mapcum","id","dotsignif","arrow")    
  ,lab1=c("","","Rate Trend APC", "Rate Change")  
  ,lab2=c("","","Dot-Signif","2002-06 to 2007-11") 
  ,lab3=c("","","","") 
  ,col1=c(NA,NA,"RateTrendAPC","Rate20022006")
  ,col2=c(NA,NA,"pValue", "Rate20072011")
  ) 

ExTitle    <- c("Ex10-SeerStat Data-2002-6 and 2007-11",
                     "Dot with Signif., Arrow and Bar")

grDevices::pdf(file=paste0(TDir,"Ex10-SeerStat-DotSignif.pdf"),width=7.5,height=10)

micromapST(Seer18Area,panelDesc10,
        sortVar="Rate20022006",ascend=FALSE,
        title=ExTitle,
        rowNames="alias",rowNamesCol='Registry', 
        bordGrp="USSeerBG",
        plotNames="ab")

x <- grDevices::dev.off()
#
#  Both calls are effectively identical.
#
### End of example 10

###
#
#  Example # 11 - Counties in Kansas on an 11 x 17 page
#
###

# Load example data from package.
utils::data(KansPopInc,envir=environment())

# Four Column Layout:  Map, ID, Dot, and Dot
panelDesc11 = data.frame(
  type=c("map","id","dot",        "dot")    
  ,lab1=c("",     "",  "Population", "Average Inc.")  
  ,lab2=c("",     "",  "in 2000",    "per year") 
  ,lab3=c("",     "",  "People",     "") 
  ,col1=c(NA,     NA,  "Pop",        "AvgInc")
  ) 

ExTitle    <- c("Ex11-Kansas Pop data 11x17",
                  "Current Pop and Average Inc - scaling=e")

grDevices::pdf(file=paste0(TDir,"Ex11-Kansas_Population_and_Income-11x17.pdf"), 
     width=10, height=16)  
          # tabloid size page (11x17) to handle 105 counties.

#  Use default scaling = "e" and no staggered labels, 
#  Use full county names for data to boundary matching, 
#  but presented abbreviated county names 
#  in "id" glyphic column with large page.

micromapST(KansPopInc, panelDesc11, 
        sortVar=c("AvgInc","Pop"), ascend=FALSE, 
        title=ExTitle,
        rowNames="full", rowNamesCol='County',
        bordGrp="KansasBG",
        plotNames="ab")

x <- grDevices::dev.off()        
#
### End Example 11

###
#
#   micromapST - Example 12 - A linked micromap of the counties of 
#      the state of Maryland using the border group "MarylandBG". 
#      The MarylandPopInc data is shown using two dot glypics - current 
#       population and average increase per county.
#      A "maptail" state map is used to show the counties in relationship
#      to the median county as sorted by the 1970 population.
###

utils::data(mdPopData,envir=environment())

# set up 5 column page layout

panelDesc12 = data.frame(
  type=c("maptail","id","dot","dot","arrow")    
  ,lab1=c("","","Population", "Population","Change")  
  ,lab2=c("","","in 1970","in 2000", "from 1970 to 2000") 
  ,lab3=c("","","","","") 
  ,col1=c(NA,NA,"X1970","X2010","X1970")
  ,col2=c(NA,NA,"","","X2010")
 ) 

ExTitle       <- c("Ex12-Maryland Population-map",
                      "1970 and 2010 Pop and Change,stag,sn")

grDevices::pdf(file=paste0(TDir,"Ex12-MD Pop 1970 and 2010 plus change-map.pdf"), 
     width=7.5, height=10.5) 

micromapST(mdPopData, panelDesc12, 
        sortVar=2, ascend=FALSE, 
        title=ExTitle,
        rowNames="full", rowNamesCol='County',
        bordGrp="MarylandBG", 
        axisScale="sn", staggerLab=TRUE,
        plotNames="ab")

x <- grDevices::dev.off()        
#
### End Example 12

###
#
#   micromapST - Example 13 - A linked micromap of the counties 
#      of the state of New York state using the border group 
#      "NewYorkDF".  The pop/inc data is shown using two dot glyphs, 
#      an arrow and bar glyph (2010 Population, an arrow showing the 
#      change in population from 2000 to 2010, Population in 2000, 
#      and a bar showing the amount of the change.)
#      
###

# Load example data from package.
utils::data(nyPopData,envir=environment())

nyPopData$Dif00_10 <- nyPopData$Pop_2010 - nyPopData$Pop_2000

# set up 6 column page layout with colSize

panelDesc13 <- data.frame(
  type=c("map","id","dot",           "arrow",        "dot",     "bar")    
  ,lab1=c("",  "",  "Population in", "Increase from","Pop 2005","Incre")  
  ,lab2=c("",  "",  "2010",          "2000",         "",        "2000to2010") 
  ,lab3=c("",  "",  "",              "",             "",        "") 
  ,col1=c(NA,  NA,  "Pop_2010",      "Pop_2000",     "Pop_2000","Dif00_10")
  ,col2=c(NA,  NA,  "",              "Pop_2010",     "",        "")
  ,colSize=c(NA,NA, 15,              20,             5,        20)
  ) 
  
ExTitle   <- c("Ex13-New York Population data",
                "2010 Pop and since 2000-colSize,sn,stag")

grDevices::pdf(file=paste0(TDir,"Ex13-New York Pop 2010 and Change-sn colSize.pdf"), 
     width=7.5, height=10.5) 

micromapST(nyPopData, panelDesc13, 
        sortVar="Pop_2000", ascend=FALSE,
        title=ExTitle,
        rowNames="full",rowNamesCol="Area", 
        axisScale="sn", staggerLab=TRUE,
        bordGrp="NewYorkBG"
      )

x <- grDevices::dev.off()        
#
#### End Example 13

###
#
#   micromapST - Example 14 - A linked micromap of the counties in the 
#      state of Utah.  The UtahPopData data is shown using two dot glypics
#      - current population and average increase per area.
#
###

# Load example data from package.
utils::data(UtahPopData,envir=environment())

#
#  Get population differences from 2011 to 2001 and 1991.
#   Data contains ",".  The comma's must be removed and values are 
#     converted to numbers.
#   If data is factors, need to add "as.character()" function 
#     to the formula below.

UtahPopData2 <- as.data.frame(sapply(UtahPopData, 
                        function(x) gsub(",","",x)),stringsAsFactors=FALSE)

# Calculate the differenct between 2001 and 2011 population.
UtahPopData2$Del1101 <- as.numeric(UtahPopData2$X2011) 
                         - as.numeric(UtahPopData2$X2001)
                         
# Calculate the difference between 1991 and 2001 population.
UtahPopData2$Del0191 <- as.numeric(UtahPopData2$X2001) 
                         - as.numeric(UtahPopData2$X1991)

# set up 5 column page layout

panelDesc14 = data.frame(
  type=c("map","id","dot","arrow","arrow")    
  ,lab1=c("","","Population", "2001-2011","Chg 1991-2001")  
  ,lab2=c("","","in 2011","pop change","pop change") 
  ,lab3=c("","","","","") 
  ,col1=c(NA,NA,"X2011","X2011","X2001")
  ,col2=c(NA,NA,NA,"X2001","X1991")
  ) 

ExTitle    <- c("Ex14 - Utah county population 2011",
                  " and changes last two decades,sn")

grDevices::pdf(file=paste0(TDir,"Ex14-Utah Population.pdf"),
     width=7.5,height=10.5) 

micromapST(UtahPopData, panelDesc14, 
        sortVar="X2011", ascend=FALSE,
        title=ExTitle,
        rowNames="full",rowNamesCol='County', 
        axisScale="sn",
        bordGrp="UtahBG",
        plotNames="ab"
      )

x <- grDevices::dev.off()        
#
### End Example 14

###
#
#   micromapST - Example 15 - A linked micromap of the provinces, 
#      municipalities, autonomous regions and special administrative 
#      regions of China using the border group of "ChinaDF". 
#      The ChinaPopInc data is shown using two dot glypics - current 
#      population and average increase per area.
#
###

utils::data(cnPopData,envir=environment())

# set up 4 column page layout

panelDesc15 = data.frame(
  type=c("map","id","dot","bar")    
  ,lab1=c("","","Population", "Population")  
  ,lab2=c("","","in 2013","in 2013") 
  ,lab3=c("","","","") 
  ,col1=c(NA,NA,"pop2013","pop2013")
  ) 

ExTitle       <- c("Ex14-China Population",
                     "in 2013 by area")

grDevices::pdf(file=paste0(TDir,"Ex15-China 2013 Population.pdf"), 
    width=7.5, height=10.5) 

micromapST(cnPopData, panelDesc15, 
        sortVar="pop2013", ascend=FALSE, 
        title=ExTitle,
        rowNames="full", rowNamesCol='area',
        bordGrp="ChinaBG",
        plotNames="full")

x <- grDevices::dev.off()        
#
### End Example 15

###
#
#   micromapST - Example 16 - A linked micromap of the districts 
#      of the city Seoul South Korea, using the border group of 
#      "SeoulSKoreaBG". The included SeoulPopData dataset provides 
#      population and district area statistics for 2012.  
#      The micromapST generates two glyphics, a sorted dot
#      glyphic based on the population and a bar graph based on 
#      the area.
#
###

# Load example data from package.
utils::data(SeoulPopData,envir=environment())

# set up 4 column page layout

panelDesc16 = data.frame(
  type=c("map","id","dot","bar")    
  ,lab1=c("","","Population", "Area")  
  ,lab2=c("","","in 2012","in 2012") 
  ,lab3=c("","","","sqkm") 
  ,col1=c(NA,NA,"Pop.2012","Area")
  ) 

ExTitle       <- c("Ex16-Seoul Population",
                     "in 2012 by district")

grDevices::pdf(file=paste0(TDir,"Ex16-Seoul 2012 Population.pdf"), 
     width=7.5, height=10.5) 

micromapST(SeoulPopData,panelDesc16,
        sortVar=3, ascend=FALSE,    # sort based on the population
        title=ExTitle,
        rowNames="full", rowNamesCol='District',
        bordGrp="SeoulSKoreaBG",
        plotNames="full"
      )

x <- grDevices::dev.off()        
#
### End Example 16

###
#
#  Example 17 - use of Africa population data as data source
#      Demonstrates support for vertical oriented geographical 
#       areas.
#
###

# Load example data from package.
utils::data(AfricaPopData,envir=environment())

panelDesc17 <- data.frame(
              type = c("map", "id", "dot",      "dot",          "dot"),
              lab1 = c("",      "","Population","Percentage Of","Est x2 Time"),
              lab3 = c("",      "","People",    "Total",        "Years"),
              col1 = c(NA,      NA,"Projection","PercOf",       "Est2Time")
            )
                          
ExTitle <- c("Ex17-Africa Population Data",
               "Sorted by Population on 11x17")

grDevices::pdf(file = paste0(TDir,"Ex17-Africa Micromap-11x17.pdf"), 
     width = 11, height = 17) 

micromapST(AfricaPopData, panelDesc17, 
           sortVar = "Projection", ascend = TRUE,
           title = ExTitle, 
           rowNames = "ab", rowNamesCol = "Abbr",
           bordGrp = "AfricaBG" )

x <- grDevices::dev.off()
#
### End  Example 17



###

unlink(TDir)

}

