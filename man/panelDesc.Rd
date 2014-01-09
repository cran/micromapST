\name{panelDesc}
\alias{panelDesc}
\docType{data}
\title{micromapST panel description data.frame structure }
\description{
  The \var{panelDesc} data.frame provides the \var{micromapST} function with 
  the information required to process the \var{stateFrame} data and panelData data.frames
  and to generate the required linked micromap plot.
  
  It specifies which columns in the \var{stateFrame} data.frame contain the data
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
        col1=c(NA,NA,'Rate',9), 
        col2=c(NA,NA,4,11),
        col3=c(NA,NA,5,12),
        refVals=c(NA,NA,NA,wflungUS[,1]),
        refTexts=c(NA,NA,NA,"US Rate"),
        panelData=c("","","","")
     }

  The \var{panelDesc} data.frame (which does not have to be named 'panelDesc', any name will do) 
  provides the means of defining how many columns to create, the type of glyph per column, 
  where the data required by the glyph is located in the \var{stateFrame} (column number or name) 
  or the name of a supplimental data structure when the glyph is boxplots or time series 
  (via the \var{panelData=} list entry), the column titles, and the column''s reference value and label
  for the link micromap generation.

  \bold{Glyph Types}
 
  The \var{type} vector defines the type of glyph to be used for each column.
  The available glyphs are:
      \describe{
        \item{Map types:}{'map', 'mapcum','maptail','mapmedian',}
        \item{State ID and/or Name:}{'id',} 
        \item{Graphical Type:}{'dot', 'dotse','dotconf', 'bar', 'arrow', 'ts', 
        'tsconf','scatdot', 'segbar', 'normbar', 'ctrbar', 'boxplot'}
      }
 
   The following provides a description of each panel type:
     \describe{
         \item{map}{ - US map with active states colored}
         \item{mapcum}{ - US map with active states colored and previously active state highlighted 
         generating an accumulation from top to bottom}
         \item{maptail}{ - US map with active states colored and previously active state highlighted 
         until the median state, then the reverse to the end (states that have not been active 
         are highlighted.)}
         \item{mapmedian}{ - US map with active states colored. Maps above the median state 
         have states with values above the median highlighted. Maps below the median state 
         have states with values below the median highlighted. This helps define the above 
         and below median state groups.} 
         \cr 
         \item{id}{ - generates a column with a colored identifier (a square) 
         and the state name or abbreviation.}
         \cr
         \item{arrow}{ - an arrow between two values with a head.}
         \cr
         \item{bar}{ - a single bar chart.}
         \cr
         \item{boxplot}{ - a boxplot per state with box, upper and lower whiskers and outliers.}
         \cr  
         \item{dot}{ - a dot for a single value.}
         \item{dotse}{ - a dot for a single value and its standard error.}
         \item{dotconf}{ - a dot for a single value and its confidence interval.}
         \cr
         \item{ts}{ - a time series line for up to 15 sets 'x' and 'y' values for 
         each state.} 
         \item{tsconf}{ - a time series line for a up to 15 sets of 'x', 'y' and upper 'y' 
         and lower 'y' values as a confidence interval band for each state.}  
         \cr
         \item{segbar}{ - a horizontal stacked (segmented) bar plot 
          starting at 0 for 2 to 9 bars.}
         \item{normbar}{ - a stacked bar plot where the data is normalized for 
          each state by dividing the bar segment values by the sum of the 
          values for all of the bars. Up to 9 bars are supported.}
         \item{ctrbar}{ - a stacked bar plot where the bar segments are 
          centered around the 0.Up to 9 bars are supported.}
          \cr
         \item{scatdot}{ - a set of 51 points with an 'x' and 'y' value per state.}
     }
  
  
  \bold{Labels (Column Headers and Footers)}
       
    \var{micromapST} supports up to 3 column labels or titles: \var{lab1}, 
    \var{lab2} and \var{lab3}, where  \var{lab1} and \var{lab2} are header titles for the column.
    \var{lab3} is the footer title for the column. All titles are optional.  
    \var{lab3} is used to indicate the unit of measure at the bottom of the columns, 
    but is not limited to this use.  For example:
 
    \preformatted{
       lab1=c('Col1-Title', 'Col2=Title', 'Col3-Title' ) # 1st title for each column
       lab2=c('Col1-Sub',   'Col2-Sub',   'Col3-Sub'   ).# 2nd title for each column
       lab3=c('Col1-Footer','Col2-Footer','Col3-Footer') # Footer title for each column
    }
 
    \var{lab4} is used only when time series or scatter dot glyphs are used to provide a "Y" axis
    title for the column.
    All label/title vectors are optional and only required when an title or label is needed.  

  
  \bold{Data References}
    
  Depending on the type of glyphic selected for the column, 1 to 3 data 
  values for each state may be required:  The \var{col1}, \var{col2} and 
  \var{col3} vectors serve as indexes to columns in the \var{stateFrame} 
  data.frame passed in the arguments of the \var{micromapST} function call.  
  The values can be either the numeric number of the row in 
  \var{stateFrame} data.frame or the column name.  
  If no index is required, the entry should be set to \code{"NA"}.
  
  If the glyph requires one value, then only the \var{col1} index is 
  used and the \var{col2} and \var{col3} indexes are set to \code{"NA"} if present  .
  If 2 values are required, then \var{col1} and \var{col2} indexes are used 
  and the \var{col3} index is set to "NA", if present.
  If 3 values are required, then \var{col1}, \var{col2}, and \var{col3} indexes are used.
  
  The \var{stateFrame} column indexes can be provided as an integer 
  or the column name.  If the integer value is less than 1 or greater than  
  the number of columns in \var{stateFrame} or a column name is used that does 
  not exist in stateFrame, the \var{micromapST} function will stop and 
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
  
  The \var{panelData} vector is only used when a glyph requires more data per state than can be
  provided by the \var{stateFrame} columns. Only glyphs using this vector are boxplots and time series.
  
  In the case of the \option{boxplot} glyph, the boxplot function with plot=F is used to generate 
  the boxplot statistical details for each state. The name of the resulting list of 51 sets of boxplot 
  statistics (one for each state) is placed in the panelData vector element for the boxplot column.
  
  For the time series and time series with confidence interval, the glyphs require a 3 dimensional 
  array of data.  The first dimension ([st,,]) represents the 51 states.  The second dimension ([,t,]) 
  ranges from 2 to 15. One for each data point. The third dimension ([,,v]) provides the values at 
  data point "t" for state "st".  [,,1] is the "x" axis value. For time series, is usually 
  just the value 1 to 15 to order the "y" values.  [,,2] is the median "y" value.  For time series 
  with confidence intervals: [,,3] is the lower value "y" and [,,4] is the upper value "y".
  
  \bold{Reference Lines}
  
  Reference lines can be created in arror, bar, dot, dotconf, dotse, and segbar glyphs by specifying the 
  reference values in the \var{RefVal=} vector.  A label appearing at the bottom of the column can be 
  specified using the  \var{RefTxt=} vector in the \var{panelDesc} data.frame.



}
\usage{panelDesc}
\format{
  The parameters in the panelDesc data.frame structure are:
  \describe{
    \item{type=}{The types of graphics for each column of panels can 
      be specified by the following keywords in the 'type variable': 
      \cr
      The following are the type of glyphics that can be specified in the type vector:
      \cr
      \describe{
        \item{Map types:}{'map', 'mapcum','maptail','mapmedian',}
        \item{State ID and/or Name:}{'id',} 
        \item{Glyph Type:}{'dot', 'dotse','dotconf', 'bar', 'arrow', 'ts', 
        'tsconf','scatdot', 'segbar', 'normbar', 'ctrbar', 'boxplot'}
      }
      /cr
      The following provides a description of each panel type:
      \describe{
        \item{map}{ - a non-highlighted map}
        \item{mapcum}{ - maps show the accumulated states top to bottom}
        \item{maptail}{ - maps show the accumulated states from the top and bottom  
         toward median state.}
        \item{mapmedian}{ - the maps above the median highlight the states above the 
         median state and maps below the median highlight states below the median state based on 
         the sorting variable.}
        \cr 
        \item{id}{ - generates a column with a color identifier (a filled in square) 
         and the state name. The plotNames parameter in the \var{micromapST} call controls 
         whether the state''s full name or 2 character abbreviation is displayed.}
        \cr
        \item{arrow}{ - an arrow from value 1 to value 2 with value 2 
         the head of the arrow.}
        \cr
        \item{bar}{ - a bar for a single set of values,  The values can be 
         positive or negative.}
        \cr
        \item{boxplot}{ - a boxplot for each state using a data.frame 
         generated by the boxplot function with plot=F.  The name of the data.frame is passed 
         to \var{micromapST} using the \var{panelData} vector.}
         \cr  
        \item{dot}{ - a dot for a single value using one set of values.}
        \item{dotse}{ - a dot for a single value and its standard error using two values.}
        \item{dotconf}{ - a dot for a single value and its confidence interval using 
         three values.}
         \cr
        \item{ts}{ - a time series line plot for each state. 
         The glyph use the panelData vector to get the name of a three (3) dimensional array 
         the data for the plot. The array contains one entry per state, 1 to 15 data points 
         and the "x" and "y" values.
         See section on \var{panelData} below for more details.}  
        \item{tsconf}{ - a time series line and confidence interval 
         band for each state.   
         The glyph use the panelData vector to get the name of a three (3) dimensional array 
         the data for the plot. The array contains one entry per state, 1 to 15 data points 
         and the "x", "y", lower "y" and upper "y" values.
         See section on \var{panelData} below for more details.}  
        \cr
        \item{segbar}{ - a horizontal stacked (segmented) bar plot 
         starting at 0 using data in the stateFrame data.frame.  The \var{col1} and 
         \var{col2} columns are used to indicate the first and last columns in the 
         stateFrame data.frame that contain the contiguous bar segment values (lengths).
         For example:  the data for a 5 segment bar glyph is in columns 4 through 8 in 
         the \var{stateFrame} (5 columns). \var{col1} is set to 4 to identify the first column 
         and \var{col2} is set to 8 to identify the last column in the sequence.  Column names 
         may be used, but the column identified in \var{col1} must preceed the column 
         identified in \var{col2}.}
        \item{normbar}{ - a stacked bar plot where the data is normalized for 
         each state by dividing the bar segment values by the sum of the 
         values for all of the bars.  The stacked bar plot for each state
         then ranges from 0 to 100\% (edge to edge).  The \var{col1} and \var{col2} columns 
         are used to identify the first and last columns for bar data in the \var{stateFrame} 
         in the same way as for the 'segbar' glyph (see above.) }
        \item{ctrbar}{ - a stacked bar plot where the bar segments are 
         centered around the middle of the data. If there is an even number of 
         segments, the 0 point is between the lower half and the upper half of 
         the segments.  If there is an odd number of segments, the center is 
         the midpoint of the middle segment. The other segments are plotted 
         to the left and right of the center point. The \var{col1} and \var{col2} columns 
         are used to indicate the first and last columns in the \var{stateFrame} 
         data.frame that contain the contiguous bar segment values. (See 'segbar' above for 
         more information.)}
        \cr
        \item{scatdot}{ - a set of 51 points with an 'x' and 'y' value per state..All points 
         are plotted in each panel with the key states in the panel highlighted. 
         \var{col1} indicates \var{stateFrame} column containing the 'x' values and 
         \var{col2} indicates the column containing the 'y' values.}
      }
     Example: \code{type=c('id','map','boxplot')}  
     To specify a \var{micromapST} with three columns, left to right,
     containing the state label, a map and a boxplot.
    }\cr
    \item{\var{col1=}, \var{col2=}, \var{col3=}}{Vectors of index numbers 
     or names of columns in \var{stateFrame} data.frame to be used as data for graphics. 
     The uses of these three vectors are defined below:
     \describe{
          \item{any 'map' type, id, boxplots, ts, and tsconf}{glyphs do 
            not use the \var{col1}, \var{col2}, or \var{col3} vectors to locate data 
            in the \var{stateFrame} data.frame. If these vectors are present, the 
            corresponding entires should be \code{'NA'} for the respective columns.}
           \cr
           \item{dot}{uses \var{col1} to specify a single data column in 
            \var{stateFrame} data.frame to be ploted. }
           \item{bar}{uses \var{col1} to specify the data column in 
            \var{stateFrame} data.frame for the length of the bar.  
            The data value can be positive or negative.}
            \cr
           \item{dotse}{uses \var{col1} and \var{col2} to 
            specify the data columns in \var{stateFrame} data.frame 
            to be used as the estimate and standard error values, respectively.}
            \item{arrow}{uses \var{col1} and \var{col2} to specify 
            the data columns in stateFrame data.frame 
            for the beginning and end values of the arrow.}
            \cr
           \item{segbar, normbar, ctrbar}{uses \var{col1} and \var{col2} to specify 
            the first and last columns in the \var{stateFrame} data.frame.
            The \var{stateFrame} data.frame columns from \var{col1} to \var{col2} 
            are used for the length values of each bar in the glyph. \var{col1} must 
            preceed \var{col2} in the \var{stateFrame} data.frame.
            The minimum number of data columns is 2 columns with a maximum of 9 columns.}
           \item{scatdot}{uses \var{col1} and \var{col2} to 
            specify the x and y values respectivefully for a
            dot for each of the 51 states and DC in a scatter dot plot.}
            \cr
           \item{dotconf}{uses \var{col1}, \var{col2}, and 
            \var{col3} to specify the data columns in \var{stateFrame} data.frame 
            for the estimate value, lower confidence interval, and upper confidence 
            interval values.}
        }
        See the table above 
      }      
    \item{\var{lab1=}, \var{lab2=}}{
      Character vectors provide the two column labels (titles) lines at the top of each column. 
      If no label is required, use \code{''} for a blank line. }
    \item{\var{lab3=}}{Character vector used as a label at the bottom of each column. This is typically used 
      to show units of measure.  If no label is required, use \code{''} for a blank line. }
      \cr
    \item{\var{lab4=}}{Character vector used as the vertical (y) axis label for 'ts', 'tsconf', and 
      'scatdot' glyphics.  If no label is required, use \code{''} for a blank line.}
      \cr
    \item{\var{refVals=}}{Is a list of object names providing the reference values for each graphic column. 
      The reference value is displayed as a dashed vertical line for each panel in the specified column.}\
    \item{\var{refTexts=}}{Is a list of 1 or 2 labels to be displayed at the 
      bottom of each column to identify the reference value.}
      \cr
    \item{\var{panelData=}}{List of object names containing the boxplot data list and/or an array of time series 
      data for each state.  If boxplot and time series data are not used in a column, 
      then associated object names should be \code{'NA'}.
      
      For \option{boxplot} data, each row name in the boxplot list must be the state 
      abbreviation (2 character) for the state associated with the data. There must 
      be 51 rows in the data.frame. Each row must be data produced by the 
      \option{boxplot} function.  The state abbreviation must be used as the boxplot$names 
      to be able to associate the individual boxplots to each state.
      
      For the time series glyph (ts), the data must be a three (3) dimensional array.  
      The first dimension \code{[st,,]} represent one entry for each state (1 to 51).  
      The second dimension \code{[,t,]} indexes up to 15 data points for the state.  
      The third dimension \code{[,,v]} are the data point values.  \code{[,,1]} is 
      the "x" value and \code{[,,2]} is the median "y" value for the data point.  
      
      For the time series with confidence intervals glyph (tsconf), the array is extended to include:  
      \code{[,,3]} and \code{[,,4]} for the lower and upper "y" values.
     
      For time series data, the order of the first dimension of the array must match the 
      state order in the \var{stateFrame}.  For example, the data in \code{dataArray[1,,]} is the 
      the state identified in \code{stateFrame[1,]}   }
      \cr
  } 
}
 
\note{A descriptor may be omitted if none of the panel plots need it.} 

\details{
   The \var{panelDesc} data.frame is used to describe the content of the 
   micromapST plot to the function. It contains the index of the data 
   in the \code{stateFrame} data.frame, the types of graphics to be used in
   each column, titles, column headers, reference values and labels, etc.
}

\author{Daniel B. Carr, George Mason University, Fairfax VA, with contributions from Jim Pearson and Linda Pickle of StatNet Consulting, LLC, Gaithersburg, MD}

\keyword{data structure}

\keyword{panelDesc}

\seealso{\code{\link{micromapST}}}
