\name{panelDesc}
\alias{panelDesc}
\docType{data}
\title{micromapST panel description data.frame structure }
\description{
  The panelDesc data.frame provides the \var{micromapST} function with 
  the information required to process the \var{statsDFrame} data and panelData data.frames
  and to generate the required linked micromap plot.
  \cr
  It specifies which columns in the \var{statsDFrame} data.frame contain the data
  for each glyph column, the column types, labels, reference values and text, and when 
  more complex data is needed by a glyph (boxplot and time series) what the name of the data 
  structure..
   \preformatted{
  Example
    panelDesc = data.frame(
        type=c("mapcum","id","dotconf","dotconf"),
        lab1=c("","","White Males","White Females"),
        lab2=c("","","Rate and 95\% CI","Rate and 95\% CI"),
        lab3=c("","","Deaths per 100,000","Deaths per 100,000"),
        col1=c(NA,NA,"Rate",9), 
        col2=c(NA,NA,4,11),
        col3=c(NA,NA,5,12),
        colSize=c(NA,NA,5,5),
        refVals=c(NA,NA,NA,wflungUS[,1]),
        refTexts=c(NA,NA,NA,"US Rate"),
        panelData=c("","","","")
     }
  The panelDesc data.frame (which does not have to be named "panelDesc", any name will do) 
  provides the means of defining how many columns to create, the type of glyph per column, 
  where the data required by the glyph is located in the \var{statsDFrame} (column number or name) 
  or the name of a supplemental data structure when the glyph is boxplots or time series 
  (via the \var{panelData} list entry), the column titles, and the column's reference value and label
  for the link micromap generation.
  
  In the following description the term "AREA" represents the geographic unit being mapped 
  and associated with data in the \var{statsDFrame}.  The naming used must match the border group
  specified.  If the border group of "USStatesDF" is used, the areas are U.S. States and DC and 
  51 data rows must be present.
  If the border group of "USSeerDF" is used, the areas are U.S. Seer areas as defined by NCI and 
  the number of data rows can be 9, 11, 13, 17 or 18.  In all cases, the abbreviations and names
  defined in the border group dataset must be used in preparing the statsDFrame and panelData structures.
  
  \bold{Glyph Types}
  
  The \var{type} vector defines the type of glyph to be used for each column.
  The available glyphs are:
      \describe{
        \item{Map types:}{"map", "mapcum","maptail","mapmedian"}
        \item{State or Area ID and/or Name:}{"id"} 
        \item{Ranking:}{"rank"}
        \item{Graphical Type:}{"dot", "dotse","dotconf", "dotsignf", "bar", "arrow", "ts", 
        "tsconf","scatdot", "segbar", "normbar", "ctrbar", "boxplot"}
      }
   The following provides a description of each panel type:
     \describe{
         \item{map}{ - US map with active areas colored}
         \item{mapcum}{ - US map with active areas colored and previously active area highlighted 
         generating an accumulation from top to bottom}
         \item{maptail}{ - US map with active areas colored and previously active area highlighted 
         until the median area, then the reverse to the end (areas that have not been active 
         are highlighted.)}
         \item{mapmedian}{ - US map with active areas colored. Maps above the median area 
         have areas with values above the median highlighted. Maps below the median area 
         have areas with values below the median highlighted. This helps define the above 
         and below median area groups.} 
         
         \item{id}{ - generates a column with a colored identifier (a square) 
         and the area or area name or abbreviation.}
         \item{rank}{ - number the area in rank order, sequentially.} 
         
         \item{arrow}{ - an arrow between two values with a head.}
         
         \item{bar}{ - a single bar chart.}
         
         \item{boxplot}{ - a boxplot per area with box, upper and lower whiskers and outliers.}
          
         \item{dot}{ - a dot for a single value.}
         \item{dotse}{ - a dot for a single value and its standard error.}
         \item{dotconf}{ - a dot for a single value and its confidence interval.}
         \item{dotsignf}{ - a dot for a single value with an indicator of its significants.}
         
         \item{ts}{ - a time series line for up to 30 sets "x" and "y" values for 
         each area. The TS glyph can have X-Axis labels formated as numbers or dates.} 
         \item{tsconf}{ - a time series line for a up to 30 sets of "x", "y" and upper "y" 
         and lower "y" values as a confidence interval band for each area. The TSConf glyph 
         can have X-Axis labels formated as numbers or dates.}  
         
         \item{segbar}{ - a horizontal stacked (segmented) bar plot 
          starting at 0 for 2 to 9 bars.}
         \item{normbar}{ - a stacked bar plot where the data is normalized for 
          each area by dividing the bar segment values by the sum of the 
          values for all of the bars. Up to 9 bars are supported.}
         \item{ctrbar}{ - a stacked bar plot where the bar segments are 
          centered around the 0.Up to 9 bars are supported.}
          \cr
         \item{scatdot}{ - a set of points for each area with an "x" and "y" value.}
     }
  \bold{Labels (Column Headers and Footers)}
       
    \var{micromapST} supports up to 3 column labels or titles: \var{lab1}, 
    \var{lab2} and \var{lab3}, where  \var{lab1} and \var{lab2} are header titles for the column.
    \var{lab3} is the footer title for the column. All titles are optional.  
    \var{lab3} is used to indicate the unit of measure at the bottom of the columns, 
    but is not limited to this use.  For example:
 
    \preformatted{
     lab1=c("Col1-Title", "Col2=Title", "Col3-Title" ) # 1st title for columns
     lab2=c("Col1-Sub",   "Col2-Sub",   "Col3-Sub"   ) # 2nd title for columns
     lab3=c("Col1-Footer","Col2-Footer","Col3-Footer") # Footer title for columns
    }
    \var{lab4} is used only when time series or scatter dot glyphs are used 
    to provide a Y axis title for the column.
    All label-title vectors are optional and only required when an title or label is needed.  
  
  \bold{Data References}
    
  Depending on the type of glyph selected for the column, 1 to 3 data 
  values for each area may be required:  The \var{col1}, \var{col2} and 
  \var{col3} vectors serve as indexes to columns in the \var{statsDFrame} 
  data.frame passed in the arguments of the \var{micromapST} function call.  
  The values can be either the numeric number of the row in 
  \var{statsDFrame} data.frame or the column name.  
  If no index is required, the entry should be set to \var{NA}.
  
  If the glyph requires one value, then only the \var{col1} index is 
  used and the \var{col2} and \var{col3} indexes are set to \var{NA} if present  .
  If 2 values are required, then \var{col1} and \var{col2} indexes are used 
  and the \var{col3} index is set to \var{NA}, if present.
  If 3 values are required, then \var{col1}, \var{col2}, and \var{col3} indexes are used.
  
  The \var{statsDFrame} column indexes can be provided as an integer 
  or the column name.  If the integer value is less than 1 or greater than  
  the number of columns in \var{statsDFrame} or a column name is used that does 
  not exist in statsDFrame, the \var{micromapST} function will stop and 
  generate an error message.

  \tabular{llllll}{
  Glyph     \tab Meaning   \tab col1          \tab col2          \tab col3 \tab panelData\cr
  Name      \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  arrow     \tab Arrow     \tab Beginning     \tab Ending Values \tab NA   \tab NA       \cr
            \tab           \tab Values        \tab (arrow head)  \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  bar       \tab Horizontal\tab Bar end       \tab NA            \tab NA   \tab NA       \cr
            \tab bar       \tab values        \tab               \tab      \tab          \cr
            \tab           \tab (length)      \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  segbar    \tab Horizontal\tab Values for    \tab Values for    \tab NA   \tab NA       \cr
            \tab stacked   \tab first (left   \tab the last      \tab      \tab          \cr
            \tab bar       \tab -most) segment\tab (right-most)  \tab      \tab          \cr
            \tab           \tab (length)      \tab bar segment   \tab      \tab          \cr
            \tab           \tab               \tab (length)      \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  normbar   \tab Horizontal\tab Values for    \tab Values for    \tab NA   \tab NA       \cr
            \tab stacked   \tab first (left-  \tab last (right-  \tab      \tab          \cr
            \tab bar, nor- \tab most) bar     \tab most,bar      \tab      \tab          \cr
            \tab malized to\tab segment       \tab segment       \tab      \tab          \cr
            \tab total 100\%\tab (length)     \tab (length)      \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  ctrbar    \tab Horizontal\tab Values for    \tab Values for    \tab NA   \tab NA       \cr
            \tab stacked   \tab first (left-  \tab last (right-  \tab      \tab          \cr
            \tab bar, cen- \tab most) bar     \tab most,bar      \tab      \tab          \cr
            \tab tered on  \tab segment       \tab segment       \tab      \tab          \cr
            \tab the middle\tab (length)      \tab (length)      \tab      \tab          \cr
            \tab bar       \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  boxplot   \tab Horizontal\tab NA            \tab NA            \tab NA   \tab Name of  \cr
            \tab box plot  \tab               \tab               \tab      \tab output   \cr
            \tab           \tab               \tab               \tab      \tab list from \cr
            \tab           \tab               \tab               \tab      \tab call to          \cr
            \tab           \tab               \tab               \tab      \tab boxplot(...plot=F)         \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  dot       \tab Dot       \tab Values for    \tab NA            \tab NA   \tab NA       \cr
            \tab           \tab dots          \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  dotconf   \tab Dot with  \tab Values        \tab Values of     \tab Values for  \tab NA    \cr
            \tab confidence\tab for dots      \tab lower limits  \tab upper limits tab       \cr
            \tab interval  \tab               \tab               \tab      \tab          \cr
            \tab line      \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  dotse     \tab Dot with    \tab Values for  \tab Standard      \tab NA   \tab NA       \cr
            \tab line length \tab dots        \tab errors        \tab      \tab          \cr
            \tab +/- standard\tab             \tab               \tab      \tab          \cr
            \tab error     \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  dotsignf  \tab Dot       \tab Values for    \tab P value       \tab NA   \tab NA       \cr
            \tab overprinted \tab dots        \tab associated    \tab      \tab          \cr
            \tab if not    \tab               \tab with dot      \tab      \tab          \cr
            \tab significant \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  scatdot   \tab Scater plot \tab Values on   \tab Values on     \tab NA   \tab NA       \cr
            \tab of dots   \tab horizontal    \tab vertical      \tab      \tab          \cr
            \tab           \tab (x) axis      \tab (y) axis      \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
            \tab           \tab               \tab               \tab      \tab          \cr
  ts        \tab Time Series\tab NA           \tab NA            \tab NA   \tab Name of array       \cr
            \tab (line) plot\tab              \tab               \tab      \tab with dimensions         \cr
            \tab           \tab               \tab               \tab      \tab of c(51,t,2),         \cr
            \tab           \tab               \tab               \tab      \tab where t = #         \cr
            \tab           \tab               \tab               \tab      \tab of time points         \cr
            \tab           \tab               \tab               \tab      \tab (max 15), x values \cr
            \tab           \tab               \tab               \tab      \tab in [,,1], y values \cr
            \tab           \tab               \tab               \tab      \tab in [,,2]   \cr
            \tab           \tab               \tab               \tab      \tab            \cr
  tsconf    \tab Time Series\tab NA           \tab NA            \tab NA   \tab Name of array       \cr
            \tab (line) plot\tab              \tab               \tab      \tab with dimensions         \cr
            \tab with confidence\tab          \tab               \tab      \tab of c(51,t,4), as ts         \cr
            \tab limits    \tab               \tab               \tab      \tab lower limit is \cr
            \tab           \tab               \tab               \tab      \tab [,,3] amd the         \cr
            \tab           \tab               \tab               \tab      \tab upper limit is \cr
            \tab           \tab               \tab               \tab      \tab [,,4]     \cr
            \tab           \tab               \tab               \tab      \tab            \cr
  }
  
  The panelData data.frame is only used when a glyph requires more data per area than can be
  provided by the \var{statsDFrame} columns. Only glyphs using this vector are boxplots and time series.
  
  In the case of the \option{boxplot} glyph, the boxplot function with plot=F is used to generate 
  the boxplot statistical details for each area. The name of the resulting list of 51 sets of boxplot 
  statistics (one for each area) is placed in the panelData data.frame element for the boxplot column.
  
  For the time series and time series with confidence interval, the glyphs require a 3 dimensional 
  array of data.  The first dimension ([\var{area},,]) represents the areas.  
  The second dimension ([,\var{t},]) ranges from 2 to \var{n}.  
  There is no upper limit, but 200-250 samples is a practical limit.
  One for each data point. The third dimension ([,,\var{v}]) provides the values at 
  data point \var{t} for area \var{st}.  [,,\var{1}] is the \var{x} axis value. 
  For time series, is usually 
  just the value 1 to \var{n} to order the \var{y} values. [,,\var{2}] is the median 
  \var{y} value. For time series with confidence intervals: [,,\var{3}] is 
  the lower value \var{y} and [,,\var{4}] is the upper value \var{y}.
  
  \bold{Reference Lines}
  
  Reference lines can be created in arror, bar, dot, dotconf, dotse, and segbar glyphs by specifying the 
  reference values in the \var{RefVal=} vector.  A label appearing at the bottom of the column can be 
  specified using the  \var{RefTxt=} vector in the \var{panelDesc} data.frame.
}
\format{
  The parameters in the panelDesc data.frame structure are:
  \describe{
    \item{type=}{The types of graphics for each column of panels can 
      be specified by the following keywords in the "type variable": 
      
      The following are the type of glyphs that can be specified in the type vector:
      
      \describe{
        \item{Map types:}{"map", "mapcum","maptail","mapmedian"}
        \item{State ID and/or Name:}{"id"}
        \item{Glyph Type:}{"dot", "dotse","dotconf", "dotsignf", "bar", "arrow", "ts", 
        "tsconf","scatdot", "segbar", "normbar", "ctrbar", "boxplot"}
      }
      
      The following provides a description of each panel type:
      \describe{
        \item{map}{ - a non-highlighted map}
        \item{mapcum}{ - maps show the accumulated areas top to bottom}
        \item{maptail}{ - maps show the accumulated areas from the top and bottom  
         toward median area.}
        \item{mapmedian}{ - the maps above the median highlight the areas above the 
         median area and maps below the median highlight areas below the median area based on 
         the sorting variable.
        }
         
        \item{id}{ - generates a column with a color identifier (a filled in square) 
         and the area abbreviation or  name. The plotNames parameter in the 
         \var{micromapSEER} call controls 
         whether the area's full name or 2 character abbreviation is displayed.
        }
        \item{rank}{ - sequentially number areas from 1 (highest rank) to "n" (lowest rank)
        }
        
        \item{arrow}{ - an arrow from value 1 to value 2 with value 2 
         the head of the arrow.
        }
        
        \item{bar}{ - a bar for a single set of values,  The values can be 
         positive or negative.
        }
        
        \item{boxplot}{ - a boxplot for each area using a data.frame 
         generated by the boxplot function with plot=F.  The name of the boxplot data.frame is passed 
         to \var{micromapSEER} using the \var{panelData} vector.
        }
          
        \item{dot}{ - a dot for a single value using one set of values.
        }
        \item{dotse}{ - a dot for a single value and its standard error using two values.
        }
        \item{dotconf}{ - a dot for a single value and its confidence interval using 
         three values.
        }
        \item{dotsignf}{ - a dot for a single value overlaid if value is not significant using 
         two values: value for dot and P value.
        }
       
        \item{ts}{ - a time series line plot for each area. 
         The glyph use the \var{panelData} vector to get the name of a three (3) dimensional array 
         the data for the plot. The array contains one entry per area, 1 to 30+ data points 
         and the \var{x} and \var{y} values.
         See section on \var{panelData} below for more details.  A reasonable upper limit to the
         number of points is between 200-300.  Only a few will be selected to be used as X-Axis labels.
         The format of the X-Axis label is controled by the "xIsDate" attribute on the array being set to 
         TRUE. If the "xIsDate" attribute is not set to TRUE, the X-Axis will be formated as numeric and axisScaling 
         can be preformed.  If the "xIsDate" attribute is TRUE, the default date format 
         of "%Y-%m" (YYYY-MM) will be used. If the number of days in the series is equal to 
         or less than 90, a short date format will be used of "%b-%d" (MMM-DD).  
         The x-axis date feature will override the specification of the axisScale call parameters
         on time series glyph columns.
        }
        \item{tsconf}{ - a time series line and confidence interval 
         band for each area.   
         The glyph use the \var{panelData} vector to get the name of a three (3) dimensional array 
         the data for the plot. The array contains one entry per area, 1 to \var{n} data points 
         and the \var{x}, \var{y}, \var{lower y} and \var{upper y} values.
         See section on \var{panelData} below for more details.  A reasonable upper limit to the
         number of points is between 200-300.
         The format of the X-Axis label is controled by the "xIsDate" attribute on the array.  
         If the "xIsDate" is set to TRUE, the X-Axis values will be format using the default date format 
         of "%Y-%m" (YYYY-MM). If the number of days in the series is equal to or less than 90, a short 
         date format of "%b-%d" (MMM-DD) will be used. If the "xIsDate" attribute is missing or not equal to 
         TRUE, the X-Axis will be formated as numeric and axisScaling can be preformed. 
         The x-axis date feature will override the specification of the axisScale call parameters
         on time series glyph columns.
        }
        
        \item{segbar}{ - a horizontal stacked (segmented) bar plot 
         starting at 0 using data in the statsDFrame data.frame.  The \var{col1} and 
         \var{col2} columns are used to indicate the first and last columns in the 
         statsDFrame data.frame that contain the contiguous bar segment values (lengths).
         For example:  The data for a 5 segment bar glyph is in columns 4 through 8 in 
         the \var{statsDFrame} (5 columns). \var{col1} is set to 4 to identify the first column 
         and \var{col2} is set to 8 to identify the last column in the sequence.  Column names 
         may be used, but the column identified in \var{col1} must preceed the column 
         identified in \var{col2}.
        }
        \item{normbar}{ - a stacked bar plot where the data is normalized for 
         each area by dividing the bar segment values by the sum of the 
         values for all of the bars.  The stacked bar plot for each area
         then ranges from 0 to 100\% (edge to edge).  The \var{col1} and \var{col2} columns 
         are used to identify the first and last columns for bar data in the \var{statsDFrame} 
         in the same way as for the "segbar" glyph (see above.) 
        }
        \item{ctrbar}{ - a stacked bar plot where the bar segments are 
         centered around the middle of the data. If there is an even number of 
         segments, the 0 point is between the lower half and the upper half of 
         the segments.  If there is an odd number of segments, the center is 
         the midpoint of the middle segment. The other segments are plotted 
         to the left and right of the center point. The \var{col1} and \var{col2} columns 
         are used to indicate the first and last columns in the \var{statsDFrame} 
         data.frame that contain the contiguous bar segment values. (See "segbar" type above for 
         more information.)
        }
        
        \item{scatdot}{ - a set of 51 points with an \var{x} and \var{y} value per area. All points 
         are plotted in each panel with the key areas in the panel highlighted. 
         \var{col1} indicates \var{statsDFrame} column containing the \var{x} values and 
         \var{col2} indicates the column containing the \var{y} values.
        }
      }
     Example: \code{type=c("id","map","rank", "boxplot")}  
     To specify a \var{micromapSEER} with three columns, left to right,
     containing the area label, a map and a boxplot.
    }
    
    \item{\var{col1=}, \var{col2=}, \var{col3=}}{Vectors of index numbers 
     or names of columns in \var{statsDFrame} data.frame to be used as data for graphics. 
     The uses of these three vectors are defined below:
     \describe{
          \item{any "map" type, id, boxplots, ts, and tsconf}{glyphs do 
            not use the \var{col1}, \var{col2}, or \var{col3} vectors to locate data 
            in the \var{statsDFrame} data.frame. If these vectors are present, the 
            corresponding entires should be \code{NA} for the respective columns.
            }
           
           \item{dot}{uses \var{col1} to specify a single data column in 
            \var{statsDFrame} data.frame to be ploted.
            }
           \item{bar}{uses \var{col1} to specify the data column in 
            \var{statsDFrame} data.frame for the length of the bar.  
            The data value can be positive or negative.
            }
           
           \item{dotse}{uses \var{col1} and \var{col2} to 
            specify the data columns in \var{statsDFrame} data.frame 
            to be used as the estimate and standard error values, respectively.
            }
           \item{dotsignf}{uses \var{col1} and \var{col2} to specify the data columns
            in \var{statsDFrame} data.frame to be used as the value for the dot and
            its associated P value.
            }
           \item{arrow}{uses \var{col1} and \var{col2} to specify 
            the data columns in \var{statsDFrame} data.frame 
            for the beginning and end values of the arrow.
            }
           
           \item{segbar, normbar, ctrbar}{uses \var{col1} and \var{col2} to specify 
            the first and last columns in the \var{statsDFrame} data.frame.
            The \var{statsDFrame} data.frame columns from \var{col1} to \var{col2} 
            are used for the length values of each bar in the glyph. \var{col1} must 
            preceed \var{col2} in the \var{statsDFrame} data.frame.
            The minimum number of data columns is 2 columns with a maximum of 9 columns.
            }
           \item{scatdot}{uses \var{col1} and \var{col2} to 
            specify the \var{x} and \var{y} values respectivefully for a
            dot for each of the 51 areas and DC in a scatter dot plot.
            }
          
           \item{dotconf}{uses \var{col1}, \var{col2}, and 
            \var{col3} to specify the data columns in \var{statsDFrame} data.frame 
            for the estimate value, lower confidence interval, and upper confidence 
            interval values.
            }
        }
        See the table above. 
      }
    \item{\var{colSize=}}{
      A numeric vector used to specify the proportional width size of 
      a glyph column in relation to all other glyph columns.  
      If used, values must be included for all glyph columns except for 
      the map and id glyphs, which are fixed width columns. The width of a 
      glyph column is determined by summing all of the colSize values and 
      dividing the sum into the value for each glyph column to yield 
      a percentage of the available width to be allocated to each column.  
      For example:  colSize=c(NA,NA,10,10,5,15), does not 
      affect columns 1 and 2.  The percentages for columns 
      3 through 6 are 25\%, 25\%, 12.5\% and 37.5\%.  If 4 inches of 
      space is available, the columns will be allocated: 1, 1, 0.5, and 1.5 inches.  
      The column widths are still regulated by the minimum and maximum column 
      widths set in the package.  If a value is missing for non-map 
      or id glyph, the package will a value equal to the average of the 
      provided values. 
     }
     
    \item{\var{lab1=}, \var{lab2=}}{
      Character vectors provide the two column labels (titles) lines at the 
      top of each column. If no label is required, use \code{""} for a blank line. 
     }
    \item{\var{lab3=}}{Character vector used as a label at the bottom of 
      each column. This is typically used to show units of measure.  If no label 
      is required, use \code{""} for a blank line. 
     }
     
    \item{\var{lab4=}}{Character vector used as the vertical (y) axis label 
      for \var{ts}, \var{tsconf}, and \var{scatdot} glyphs.  
      If no label is required, use \code{""} for a blank line.
     }
     
    \item{\var{refVals=}}{Is a list of object names providing the reference 
      values for each graphic column. The reference value is displayed 
      as a dashed vertical line for each panel in the specified column.
      }
    \item{\var{refTexts=}}{Is a list of 1 or 2 labels to be displayed at the 
      bottom of each column to identify the reference value.
      }
      
    \item{\var{panelData=}}{List of object names containing the boxplot data list and/or an array of time series 
      data for each area.  If boxplot and time series data are not used in a column, 
      then associated object names should be \code{NA}.
      
      For \option{boxplot} data, each row name in the boxplot list must be the area 
      abbreviation (2 character) for the area associated with the data. There must 
      be the same number of rows as in the name table and statsDFrame table.
      Each row must be data produced by the \option{boxplot} function.  
      The area location identifier used in the statsDFrame data and must be 
      placed in the boxplot$names (\var{names}) attribute for that set of boxplot data 
      to be able to associate the individual boxplots to each area.  
      
      For the time series glyph (ts), the data must be a three (3) dimensional array.  
      The first dimension \code{[\var{st},,]} represent one entry for each area (1 to 51).  
      The second dimension \code{[,\var{t},]} indexes up to 30+ data points for the area.  
      The third dimension \code{[,,\var{v}]} are the data point values at each data point.  
      \code{[,,var{1}]} is 
      the \var{x} value and \code{[,,\var{2}]} is the median \var{y} value for the data point.  
      The rownames associated with the first dimension must be the area location ids used
      in the statsDFrame table to link the elements of this structure the presentation 
      order of the areas.
      
      For the time series with confidence intervals glyph (tsconf), the array is extended to include:  
      \code{[,,\var{3}]} and \code{[,,\var{4}]} for the \var{lower y} and \var{upper y} values.
      
      For time series data, the order of the first dimension of the array must match the 
      area order in the \var{statsDFrame}.  For example, the data in \code{dataArray[\var{1},,]} is the 
      the area identified in \code{statsDFrame[\var{1},]}
     
      The Date feature allows the caller to request the TS X-Axis labels be formated at dates.
      This requires the data in the TS array has valid date data as the X data.  These are numbers
      based on 1970-1-1 being day zero in the computer calendar.  There are many functions in R
      to convert to and from characters and date variable.  In the past, before this feature, users 
      had to do work-a-rounds by using year numbers or year and faction numbers.  Once you have inserted
      the date X values into the array [,,1], modify the class of the array to add the "Date" class.
      micrpmapST will inspect the array and find the "Date" class, flag it for internal operations and remove it.
      The date format of "%Y-%m" (YYYY-MM) will be setup as the default format.  If the time series contains <= 90 days,
      the date format will be changed to "%b-%d" (MMM-DD) to provide a more useful label.==-
      The date feature is only available on the Time Series Glyphs.
      
      If axisScale is set to "s" or "sn", they will be ignored for any TS glyph using the date feature.
      }
     
  } 
}
\note{A descriptor may be omitted if none of the panel plots need it.} 
\details{
   The panelDesc data.frame is used to describe the content of the 
   micromapST plot to the function. It contains the index of the data 
   in the \code{statsDFrame} data.frame, the types of graphics to be used in
   each column, titles, column headers, reference values and labels, etc.
}
\author{Daniel B. Carr, George Mason University, Fairfax VA, with contributions from Jim Pearson and Linda Pickle of StatNet Consulting, LLC, Gaithersburg, MD}
\keyword{data structure}
\keyword{panelDesc}
\seealso{\link{micromapST}}
