#
# Updated Package Version 120828
# Updated Package Version 130426
# Updated Package Version 130506 - V0.94
# Updated Package Version 130510 - V0.95 - fixes.
# Updated Package Version 130511 - V0.96 - attempt to complete - 
# Updated Package Version 130511 - V0.97 (8pm) - fixes 
# Updated Package Version 130513 - V0.98 (8:00am) - fixes and testing
# Updated Package Version 130517 - V0.99 - fixes and work with BW.
#                                        - correct ref line color and minor updates.
#                                        - corrected micromapSTDefaults and Arrows errors.
#                                        - label adjustment and fix parameter checking for boxplots
# Updated Package Version 130604 - V1.0  - Final Edit and fixes for release.
#                                        - Dynamically defined variables must be globalVariables add.
#                                        - Formal Release of package.
# Updated Package Version 131127 - V1.01 - Correct segmented and centered  bars to handle only two data columns
#
#  Update Log by Jim Pearson
#    May 31, 2009 - corrected dates on three column micromap
#        1990-2000 to 2001-5   --> 1996-2000 to 2001-5
#    June 7, 2009 - Added VerStr as a parameter to be able to determine
#        which output files are from this version.
#        - Updated book Micromap-Dot-Arrow-Box plot to use new 
#        data files:
#           WFAgeAdjLungMort2000-4CountyAgeAdj2000.csv
#           WFLungMort19951999AgeAdj2000State.csv
#           WFLungMort20002004AgeAdj2000State.csv
#        and change the titles for the columns in the output to match.
#        - Updated sections to use labels instead of column numbers.
#        - Updated Book micromap to merge two files instead of using
#        one file.  This also changed the column number by +1.
#        Note: future update should look at using column names instead of 
#        numbers.
#        - Updated ARROW chart to plot DOT when difference is zero.
#        - Reduce white space between columns (just a little, cannot be eliminate to
#        maintain readibility.
#    July 22, 2010 - Correct reference value (refVals) code.
#        - add variable for reference value label text (refTexts) per column.
#             panelDesc$refTexts -> vector, one per column.
#        - add variable to color the reference value label test 
#             details$colRefTxt
#        - No reference label (legend) is printed if no refTexts for the
#             column is provided.
#    January 30, 2011 - Determine running directory and load
#             panelFunctions.r, panelLayout.Rdata, and micromapST.Rdata 
#             from directory.
#    August 28, 2012 - Cleaned up code and re-packaged it with .onLoad
#        - duplicate variable cleaned up, and unused code removed.
#        - integrated the test/demo code correctly.
#        - made adjustments to handle the micromapST namespace.
#        - changed refVals and refTexts to local variables (lRefVals and lRefTexts) to add clarity.
#        - changed parameter for BoxPlots colMedian to colBpMedian to kill duplication with the colMedian 
#          used on the general graphic
#        - Modified "Details" and "Colors" variable to be unique and
#          re-ordered by subroutine usage.
#    October 5, 2012 - update documentation for review.
#        - deleted second version of panelGroupOutline- in panelFunctions.r
#        - Changed rlStateRefText function to build a legend with a line followed by 
#          the reference text.  Problem was line was on both sides of the label and 
#          in some cases overlaid the text.  This way the line is on the left of the text.
#        - changed default value for reference text from black to mid green to match the line 
#          color.
#    April 26, 2013 - add new panel graphic function - TS and TSConf
#        - added Time Series where each state has a strip within the panel for the line graph.
#        - changed boxPlot argument to panelData to represent more types of auxilary data for the program.
#    May 1-2, 2013  - add new panel graphic functions - ScatDot, StackedBar, and Normalized Bar
#        - add graduated colors to stacked bars and normalized stacked bars.
#        - changed normalized axis labels to percentages.
#        - add Time Series with all plots in one panels (one x-y graph)
#        - change TS confidence band to lighter shade = 10% transparency.
#        - attempted to fix order issues.  On TS series of panels, assume order of the panelData is the 
#          same as the original col1, col2, col3, stateId orders.  When they are re-ordered, Save the 
#          index change to remap back to the old order.  Use this to re-order panelData.
#        - On scatdot and segbar panels, the panelData contains a stateId.  Reordering is 
#          done by using the sorted stateId column in the stateFrame to re-order the panelData frames.
#        - added programing feature to permit adjustments to colsize, left and right margins of a 
#          panel based on the type of panel to be created.  Needed to allow space for the 
#          left axis labels for the time series panels (4).
#    May 4, 2013 - remove prototype strip time series - did not work, code deleted.
#        - Added centered stacked bars.
#        - changed circle size on Scatdot of non-colored dots to 75 smaller.
#        - Changed source of data for "scatdot", "segbar", "normbar", and "ctrbar" from 
#           an extra panelData structure to using columns in the stateFrame call parameters data.frame.
#           Now the col1 and col2 parameters in the panelDesc data.frame indicate which columns or
#           range of columns in the startFrame data.frame to use for the X,Y coordinates or the 
#           set of bar segment values per state.
#    May 6, 2013 - change package name from stateMicromap to micromapST.
#        - updated documentation and added new examples to micromapST.Rd
#    May 8, 2013 - Fixes - change colData to panelData to avoid confusion.
#        - Add parameter value checks to Arrow, Bar, dot, dotSE, dotconf, TS, ScatDot, segbar, normbar, and ctrbar functions.
#        - fix examples 
#    May 9, 2013 - switch the TS Array to be 1=x, 2=y, 3=low-y, 4=high-y.
#    May 10, 2013 - add support for rownames on the time series arrays.
#        - added validation of state ids in boxplots and time series.
#        - added new time series dataset to package.
#        - added panelInBound to generating x and y axis labels.
#    May 11, 2013 - reduced Y axis labels size to get more detail
#        - replaced wflung00cnty data file.
#        - created segbardata data file.
#        - fixed problem with saving new time series file - needed names on all dimensions.
#        - fixed problem with at and labels argments on mtext calls.
#        - saved original tests in init/tests directory and replace them 
#          in the micromapST.Rd with the master 6 examples.
#        - cleaned up examples.
#        - added code to try and ensure the min and max values on the y axis 
#          are always printed for the median area (middle).
#        - add code to do Dan's color mixing to get opaque colors in bars.
#    May 17, 2013 - make adjustment for publishing package
#        - adjust grey colors to allow a grey scale color pattern to be used. (based on 
#          ColorBrewer "Greys" for 5 colors.
#        - fixed grey/gray colors issues with dots, etc.  using outline colors.
#        - added circles around dots to make the grey standout more.
#    May 20, 2013 - Added "grays" as an equivalent palette name.
#    May 21, 2013 - Fix ref line color to mid-green, change reftext to black.
#        - check fill color for scat dot, fixed.
#        - changed scat dot median symbol from triangle to dot and filled with blakc.
#        - adjusted box positions on maptail, mapcum, and mapmedian titles.
#        - fixed grays to work with ref lines.
#    May 24, 2013 - finish clean up - fix micromapSTDefaults error during loading.
#        - Final Testing.
#    May 25, 2013 - fixed micromapSTDefaults error on initial load
#        - fixed arror warning by using > .005 as zero.
#        - moved up titles printing to let INTERRUPTED pdf build have titles.
#    May 28, 2013 - fix parameter checking for boxplot list. 
#        - Added names check for box plot,
#        - Added "list" type check for box plot.
#        - Reorganized test to not cause a secondary error.
#        - Added idty parameter to details and rlStateID to adjust text alignment.
#    June 2, 2013 - fix DotSE missing X1 variable - should be x.
#        - Added code to do proper capitalization of state abbreviations and full state names.
#        - Added code to intercept common names for Washington, D. C. and convert to "D.C."
#    June 3, 2013 - Released to CRAN.
#    June 4, 2013 - cran check does not handle automatic variable assignments (around line 3100.)
#          register them with R via globalVariable function to add them to the list for rcmd check.
#          During testing, the variables do not show up as globals and are protected within the 
#          micromapST namespace.  - re-released.
#    Nov. 27, 2013 - Correct the parameter check for segmented and centered bars to permit a 
#          minimum of 2 data columns.
#       
#        .
########

########
#
# Copyrighted 2013 - by: Dan Carr, GMU and Linda Pickle and Jim Pearson of StatNet Consulting, LLC.
#
########

########
#
#  functions used from RColorBrewer:   brewer.pal
#
#  functions used from graphics:   plot, lines, arrows, polygon, axis, text, mtext, boxplot,
#                                  points, legend, plot.new, plot.default, plot.design, plot.function,
#                                  plot.xy, plot.windows, abline, axTicks, barplot, matplot,
#                                  matpoints, title
#
#  functions used from stats:      qnorm
#
#  functions used from grDevices:  rgb, col2rgb
#
########
#
#  Initial Variables that require setting before running this file:
#
#   current directory <-  location of the three Micromap files
#                   micromapST.r
#                   panelFunctions.r
#                   micromapST.Rbata
#
#   
#
#  The following datasets must be included in the package to provide the boundaries.
#
#   stateNamesFips
#   stateVisBorders
#   stateNationVisBorders
#
#   ONLOAD - micromapST moves these structures into local variables:
#
#    rlStateNamesFips, rlStateVisBorders, rlStateNationVisBorder
#
#   also created is the rlMicromapSTDefaults data.frame with colors and details.
#
#   in the micromapST namespace.
#
######

######
#
#

globalVariables(c("ne","ng","ib","ie","topMar","botMar","botMarLegend","botMardif",
                "leftMarAxis","rowSep","rowSize","groupedRowSize","groupedRowSep",
                "mapWidth","idWidth","sc","pad","padex","padMinus","line1","line2",
                "lineTiclab","=.NULL","line3","line4","line5","colGrid","lwdGrid",
                "mgpTop","mgpBottom","padjBottom","mgpLeft","colPanelFill",
                "colOutline","cexText","cexTitle","ltyRefVal","lwdRefVal",
                "colRefVal","colGRefVal","colRefTxt","colGRefTxt","lengthArrow",
                "lwdArrow","lwdArrowShadow","colArrowShadow","cexArrow","barht",
                "colBarOutline","lwdBarOutline","colBarZero","lwdBarZero","ltyBarZero",
                "colSBarOutline","lwdSBarOutline","thinBox","thickBox","useBlack",
                "medianLine","colBpDotMedian","pchMedian","cexMedian","lwdMedian",
                "colBpMedian","colBpOutline","lwdOutlier","cexOutlier",
                "colBpGreyOutliner","idDotPch","pchDot","cexDot","conf","lwdConf","cexConf",
                "OutlineDot","colDotOutline","lwdDotOutline","lwdTs","axisCexTs",
                "hGridTs","lwdSCD","bgFillSCD","medPchSCD","medColSCD","axisCexSCD",
                "sizePchSCD","xscSCD","yscSCD","hGridSCD","colIdOutline","cexId",
                "cexIdDot","idty","colMapBackgr","colLineBackgr","colLineForegr",
                "colLineNation","cexState","lwdBackGr","lwdForeGr","lwdNation"),
                "micromapST",add=TRUE)

#
#   Would rather have these variable in the local "micromapST" environment.
#
######

######
#
# Functions 
#
# groupPanelOutline 
#

groupPanelOutline = function (panelGroup, j )
   ## used in micromapST function  - assumes 3 rows in the panels..
{

  for (i in 1:3){
     panelSelect(panelGroup,i,j)  # select a space
     panelScale()               # scale it
     panelOutline()             # outline it.
  }
}   

#
# simpleCap - capitalize each word in a phrase and removes "."s and extra blanks.
#     Not good on vectors - must apply
#

simpleCap <- function (x)
   {
      s <- strsplit(x,"[ ._]")[[1]]
      s1 <- s[s != ""]
      paste(toupper(substring(s1,1,1)),tolower(substring(s1,2)),sep="",collapse=" ")
   }
      
#
# Alternative:
#   gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
#

###
#
#  micromapST
#
#  In the "micromapST.Rdata", the micromapST and the 
#  micromapSTSetDefaults functions have been replaced by the following code.
#

micromapST = function(
    stateFrame,
    panelDesc,
    rowNames=c("ab","fips","full")[1],   # default = "ab"
    sortVar=NULL, 
    ascend=TRUE,     
    title=c("",""),
    plotNames=c("ab","full")[2],         # default = "full"
    colors = micromapSTDefaults$colors,
    details= micromapSTDefaults$details)
{
#
#  Routine:   micromapST
#
#  Created by:  Dr. Dan Carr
#  Updated and Extended by:  Jim Pearson, April 20, 2009
#  Updated and Extended by:  Jim Pearson, August 28, 2012
#  Updated and Extended by:  Jim Pearson, May and June, 2013
#  Updated and Extended by:  Jim Pearson, Nov, 2013
#
#  Packaged by: Jim Pearson
#
#  Dependencies:   micromapSTSetDefaults
#  DataSets:
#                  stateNamesFips
#                  stateVisBorders
#                  stateNationVisBorders
#
#           Files: panelFunctions.r
#
#  Call Parameters:
#
#####
#
# stateFrame  data.frame           # data.frame of state ID and data for micromaps.
#             rownames must be state abbreviations, names, or fips codes
#
#             Used with Dot, DotConf, DotSE, arrows, bars, segbar, ctrbar, and normbar column panels. 
#
#             Not used for boxplots or time series column panels.
#
#             The stateFrame must have the state abbr, state name or fips code as 
#             the rownames of the data.frame.
#     
#             The data.frame must be at least 2 columns for some of the functions
#             in R.  To compensate for possible 1 column data.frames, a column of zero 
#             is appended to the right side of the data.frame to ensure there is always 
#             2 columns.
#
#             An example of the problem:
#               When the structure is ordered xxx[ord,] and then assigned to the working 
#               variable "dat", the dimensions are preserved. 
#               If the data.frame has only one column, the ordering and assigned, 
#               strips the rownames and leaves the dim(dat) = NULL.
#
######
#
# panelDesc   data.frame        # data frame for panel descriptions/definitions               
#             Example
#             panelDesc = data.frame(
#                type=c('mapcum','id','dotconf','dotconf'),                  # manditory column
#                lab1=c('','','White Males','White Females'),                # recommended
#                lab2=c('','','Rate and 95% CI','Rate and 95% CI'),          # optional
#                lab3=c('','','Deaths per 100,000','Deaths per 100,000'),    # optional
#                lab4=c('','','',''),
#                col1=c(NA,NA,2,9),                                          # dependent on "type"
#                col2=c(NA,NA,4,11),                                         # dependent on "type" 
#                col3=c(NA,NA,5,12),                                         # dependent on "type"
#                refVals=c(NA,NA,NA,wflungbUS[,1]),                          # optional
#                refTexts=c(NA,NA,NA,'US Rate'),                             # optional
#                panelData=c('','','','')                                    # required if boxplot or time series used.
#                )
#
#             The first description row describes the first column of panels
#             an so on.  This is a candidate for change since each column
#             describing a column avoids a mental transposition.  
#  
# The type parameter must be present for each panel column.  The other parameters are optionals.
# However, if a parameter is required for any column, it is present for all columns.  
# If not used by a column, the parameter's value for that column should be set to "NA".
#
#  type refers the graphic panel type to be used. The valid types are  
#          "map", "mapcum","maptail","mapmedian",       for maps
#          "id",                                        for state ids
#          "dot", "dotse","dotconf",                    for dot plots
#          "arrow",                                     for arrow plots
#          "bar",                                       for simple bar plots
#          "ts", "tsconf",                              for time series plots
#          "scatdot",                                   for scatter dot plots
#          "normbar","segbar","ctrbar",                 for stacked bar plots
#          "boxplot"                                    for box plot 
#                   
#         For non-highlighted contours:
#             map accumulates states top to bottom
#             maptail accumulates states outside in
#             mapMedian feature above median state above the median and vis versa
#
#         bar  will accept negative values and plot from 0 in that direction.
#
#  col1, col2, col3
#    These values idenfity the column numbers in stateFrame to be used as data for most
#       of the panel types.  They are used by:
#            "dot", "bar", "dotse", "dotconf", "scatdot", "segbar", "ctrbar", "normbar"
#      
#     Panel types using only one column parameter:
#
#       Dot and bar plots require column 1 = value  (height of bar)
#
#     Panel types using two column parameters:
#  
#       Dotse and arrow dots require columns 1 and 2
#           dotse needs  col1=estimates         and col2=standard errors 
#                  Plus and minus the SE is draw around the estimates
#
#           arrows needs col1=beginning (older) and col2=ending (newer) values. The arrow head
#                  is on the col2 end of the arrow.
#                  
#       ScatDat requires 2 columns.  
#                col1 = x value (horizontal axis), col2 = y value (vertical axis).
#
#       SegBar, CtrBar, and NormBar requires 2 columns. col1 is the column number in  
#           the stateFrame for the first bar segment.  col2 is the column number of the 
#           last bar segments.  The columns must be contiguous between the col1 column and 
#           the col2 column. E.g. col1 = 3, col2 = 9, indicates columns 3 through 9 in the 
#           stateFrame are to be used for the segment bar values.  The number of data columns
#           can range from 2 to 9 columns.
#
#     Panel types using three column parameters:
#     
#       Dotconf requires 3 columns values
#           col1=estimate, col2=lower and col3=upper bounds
#
#     Panel types not requiring column parameters:
#
#       Boxplots require 0 columns - uses "panelData" vector in panelDesc
#           The boxplot vector from the boxplot(...,plot=F) function.
#
#       ts and tsconf require 0 columns - uses "panelData" vector in the panelDesc.
#           The name of a array(51,"x",4) is passed to provide the time series data.
#
#  lab1, lab2
#     Two label lines at the top of columns. Use "" for blank
#
#  lab3
#     One label line at the bottom of a each column,
#     typically measurement units
#
#  lab4
#     One label line for used with the Y axis on each panel.  Only used
#     with time series panels.
#
#  refVals           # P-2010/07/23  changed variable from refvals to refVals 
#                    #    to be consistant.
#     name of objects providing a reference values shown
#     as a line down the column 
#
#  refTexts          # JP-2010/07/23 - New 
#     texts to be used as the legend for the reference values.
#     If refTexts for column is NA, then no legend is added.
#
#  colSize           
#     If value > 0 then the calculated column size is overridden by this value.
#     The value is in inches.  If specified, the widths of all columns must be
#     specified. 
#
#  panelData           # (old boxplot column)
#      names a list object with a boxplot data or time series data (x/y or x/yl/ym/yh data for each state.
#
#      The boxplot list the xxxx$names list must be the abbreviated state id
#      for the entry and the related data in the structure. 
#.
#      Used to link graphic to additional data beyond the 3 data elements 
#      provided in col1, col2, col3 indexes into the stateFrame..
#
#      For boxplot graphics, a list of "boxplot" function values for each state and DC
#        with the names (2 characters) used as the row.names. 
#
#      For time series graphics, the object must be an array(51,"x",4), 
#         where the 1st index is the states (1 to 51), the second index is the number 
#         of time periods ("x") with a minimum of 2 and maximum of 30, and 
#         the third index is the type of variable. The rownames of array must
#         be the associate state id (a 2 character abbreviation).  This 
#         is required so the time series array can be properly associated 
#         with the data in the stateFrame when it's sorted.
#         For time series with no confidence band, column 1 is the x value and column 2 is the y value.  
#         For time series with a confidence band, column 1 is the x value, column 2 is the y-low value, 
#         column 3 is the y-median value, and column 4 is the  y-high value.
#                
#      Note:  Some descriptors may be omitted if none of the panel plots need them.
#             often refValues and boxplots can be omitted 
#
#
#####
#
# Individual Parameters:
#
# rowNames: Type of state id used as row.names in stateFrame. The default is "ab" for abbreviation, 
#           Acceptable values are: "ab", "full", "fips".
#
# plotNames: State label use in in the plot. The default is the "full" for full name
#           Acceptable values are: "ab", "full"
#
# sortVar   The column number in the stateFrame to be used as the variable in sorting.  
#           Can be a vector of column subscripts to break ties.
#           Warning: The sortVar parameter cannot be used to sort a boxplot or time series.
#
# ascend    TRUE default sorts in ascending order.  FALSE indicated descending order.
#
# title     A vector with one or two character strings to use the title. 
#      
#####
#
# List/Control Parameters:  (package default data.frames are used if the colors and 
#      details parameters do not specify an alternate data.frame.  
#      It is strongly recommended to use the default data.frame)
#
# colors   a color palette as a vectors of strings (character-vectors)
#              5 colors for states in a group of 5
#              1 color for the median state
#              1 foreground color for non-highlighted states in the map
#          and 7 matching colors with 20% transparency for time series.
#
#          If a color vector is provided, it's length must = 14.
#
#          If the value of colors is "bw" or "greys", a grey scale is used instead 
#          of the default or user provided colors vector.
#
#      see rlMicromapSTDefaults$colors for more details
#
#
# details   spacing, line widths and other details
#      see rlMicromapSTDefaults$details
#
#      The r1MicromapSTDefaults$details contains the line spacing and text size, 
#              group spacing, etc. instead of the panel tables.  
#              Yet, panels and the default must be in sync.
#
#####

#______________________Argument Checks______________________

# Check input data format

# stateFrame - data frame
if(!is.data.frame(stateFrame)) stop("First argument (stateFrame) must be a data.frame")

nr = nrow(stateFrame)
if(nr!=51) stop(paste("The first argument (stateFrame) must have 51 rows (states plus DC). It only has",nr,".",sep=""))

#
#   JP - Make sure the input data.frame is at least two columns - add one.
#   JP - Dot code (at least) has problems with single column stateFrame structures.
#   To protect code and any other areas that may have problems,
#   quick fix is to append "0" column to the right of the provided data.frame.
#   This forces the data.frame to be at least 2 columns.
#

Ex = rep(0,nr)
SFrame = cbind(stateFrame,Ex)   # move to SFrame and add Zero column.

#
#  headers or US rate rows should not be included in data.format.
#

# Check panel description format

if(!is.data.frame(panelDesc))
    stop("Panel descriptor argument (2nd argument) must be a data.frame")

# Check for panelDesc$type validity

valid = c("map","mapcum","maptail","mapmedian",
          "id","arrow","bar",
          "dot","dotse","dotconf",
          "ts","tsconf",
          "scatdot",
          "segbar","normbar","ctrbar",
          "boxplot")        # idDot and rank are not currently implemented

type = as.character(panelDesc$type)
subs = match(type,valid)

if(any(is.na(subs)))
    stop(paste("The panelDesc data.frame has an invalid panel type. ",type[is.na(subs)], " was found.",sep=""))


ncol = nrow(panelDesc)
blank = rep('',ncol)

if(is.null(panelDesc$lab1)) lab1 = blank else
              lab1 = as.character(panelDesc$lab1)

if(is.null(panelDesc$lab2)) lab2 = blank else
              lab2 = as.character(panelDesc$lab2)

if(is.null(panelDesc$lab3)) lab3 = blank else
              lab3 = as.character(panelDesc$lab3)

if(is.null(panelDesc$lab4)) lab4 = blank else
              lab4 = as.character(panelDesc$lab4)



# Column width defaults

###  Add check of column type to table of miniumal or statics column widths.

plotWidth = par("pin")[1]

if(is.null(panelDesc$colSize)){
     #  no colSize provided by User - create the default version.
     
     colSize = rep(0,length(type))             # set vector to zeros. Length equal the number of columns requested.

     # check for "map..." type panel columns
     loc = substring(type,1,3)=='map'
     # was "map" the start of the type name for column?
     if(any(loc))  colSize[loc] = details$mapWidth  # set size for map for columns doing maps.

     # check for "id" type panel columns
     loc = type=='id'   # is column type = "id"
     if(any(loc))
       {
         sub = ifelse(plotNames=="full",1,2)  # yes, set size for ID (ab or full)
         colSize[loc] = details$idWidth[sub]
       }
     # Get plot width and calculate size of each remaining column.
     #   Assume equal width for each non-id or non-map column.
     #
     equalWidth= (plotWidth-sum(colSize))/sum(colSize==0)
     colSize = ifelse(colSize==0,equalWidth,colSize)
  } else {
     colSize = panelDesc$colSize
  }

#
#if (sum(colSize) >= plotWidth)
#  {
#     warning("The sum of colSize vector provided in the panelDesc argument is greater then the plotting area width.")
#  }
# did not work - try again later..
#

#  Setup for stateId checks
sortedStateId = sort(stateNamesFips$ab)

# more panelDesc checks and setups after the function definitions.

#
#  Verify "colors=" argument
#
#  Second purpose is to set the graphics colors not in the "colors" vector to grays or colors.
#  
#

colFull = TRUE                  # control logical = TRUE doing Color, FALSE doing Greys
cGood = FALSE
doDotOutline = details$OutlineDot

if (length(colors) == 14)
  {
    cGood = TRUE             # colors must a vector of 14 colors  or "bw"
  } else {
    if (length(colors) == 1)
      {
        if (colors == "bw" || colors == "greys" || colors == "grays")
          {

            xbw <- brewer.pal(name="Greys",8)
            greyColors <- c(xbw[c(3:7)],"#000000","#E8E8E8")
            TransRgb <- t(col2rgb(greyColors))/255
            TransColors <- rgb(TransRgb[,1],TransRgb[,2],TransRgb[,3],.2)
            colors <- c(greyColors,TransColors)
            colFull = FALSE
            cGood = TRUE
            doDotOutline = TRUE  # outline dots in dot glyphs.
          }
      }
  }

if (!cGood)
  {
    warning('The colors argument is not valid. Must equal "bw", "greys" or a vector of 14 colors') 
  }


####
#
#  add checks to make sure colors and details are data.frames.
#
####

#####
#
# Define panel functions=====================================
#    All of these functions are internal to the micromapST function.
#

#####
#
# type = 'arrow' =========================================================
#
# rlStateArrow
#
# JP - fixed error when difference is zero.

rlStateArrow = function(j){
  # j = current panel column number
  #  
  #  col1[j] points to the stateFrame column holding the first arrow end point.value
  #  col2[j] points to the startFrame column holding the second arrow end point value
  #
  wdim <- dim(dat)
  ErrFnd = FALSE
  if (col1[j] > wdim[2])
    { 
       warning(paste("ARROW-01 Specified column number in col1 for the first end point is out of range.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col1[j] < 1)
    {
       warning(paste("ARROW-02 Specified column number in col1 for the first end point is <= 0.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col2[j] > wdim[2])
     { 
        warning(paste("ARROW-03 Specified column number in col2 for the second end point is out of range.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
  if (col2[j] < 1)
     {
        warning(paste("ARROW-04 Specified column number in col2 for the second end point is <= 0.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
  
  if (ErrFnd) return()    # Error warning noted, return from function.
  
  x1 = dat[,col1[j]]      # Arrow uses two columns from the state.frame (col1 = arrow start points
  x2 = dat[,col2[j]]      #              col2 = arrow end points.)
  refval = lRefVals[j]    # change to lRefVals - JP-2010/07/23   Reference value for column
  reftxt = lRefTexts[j]   # added - JP-2010/07/23                Reference test for column
  
  good = !is.na(x1+x2)                   # test to see if both values are present.
  
  rx = range(x1,x2,na.rm=T)              # range on of all x1 and x2 values for all states.
  
  rx = sc*diff(rx)*c(-.5,.5)+mean(rx)    # 
                                   #  x-scale extention (sc) = 1.08 *
                                   #  diff of min and max of all * 1/2 + or - to get bracket around mean
                                   #  if range 1 to 25, mean is 13, diff(rx) = 24, --> 0.04 to 25.96 (almost + and - 1)
  ry = c(0,1)                            # Y axis range = 0 to 1.. 

  # ____________labeling and axes_______________

  panelSelect(panels,1,j)               # Select the first panel in the column
  panelScale(rx,ry)                     # scale panels for all states, based on above calculations.  rx and ry.
  
  mtext(lab1[j],side=3,line=line1,cex=cexText)                 # top labels (2)  (above panel # 1)
  mtext(lab2[j],side=3,line=line2,cex=cexText)
 
  atRx <- panelInbounds(rx)
  axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))              # tick labels. 

  panelSelect(panels,ng,j)              # Select the last panel in the column
  panelScale(rx,ry)                     # temp set scale to 0 to 1.
  
  # padj in axis needed to make grid line label close
  axis(side=1,mgp=mgpBottom,padj=padjBottom,tck=0,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # bottom pad
  mtext(side=1,lab3[j],line=line3,cex=cexText)                      # bottom labels.


  #_________________drawing loop__________________
  #  Draw all of the elements - one per state.

  for (i in 1:ng){
     # loop to generate each panel in column
     gsubs = ib[i]:ie[i]       # get range ib to ie (state indexes for this panel) ----  gsubs vector of the indexes for this panel.
     ke = length(gsubs)        # get length  (length = 1 or 5)
     laby = ke:1               # labels 1:1 or 5:1 in most the US state cases.
     
     pen = if(i==6) 6 else 1:ke # if index=6 (?) then pen = 6, else 1:ke (length of line)
     
     panelSelect(panels,i,j)          # select current panel
     panelScale(rx,c(1-pad,ke+pad))   # scale to rx by 1,ke (pad)  (ry = effectively 0.33 to 5.67 (pad = 0.67)
                                      #   Scale = rx by 0.33 to 5.67 with arrows at 1,2,3,4,5...
     panelFill(col=colPanelFill) 
  
     arrLim = max(diff(rx)/par("pin")/1000) * 1.05
  
     axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid lines in panel

     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
      
     panelOutline(col=colOutline)     # outline panel 
  
     oldpar = par(lend="butt")        # save old 
  
     for (k in 1:ke){
        # loop through each item in panel (5 or 1)
        m = gsubs[k]     # get index into data array
        if(good[m]){              #  if good values
          # print(paste(k,m,x1[m],x2[m],abs(x1[m]-x2[m])))
          # Getting warning for NON-ZERO length arrows - must be rounding error <> 0.
          #  So, taking liberties to say 0 is .002 and below.  Arrow works in inches??
          #  Alternative is to suppressWarnings...
          if(abs(x1[m]-x2[m])> arrLim){         #  If arrow length is > 1.05/1000 inch do line draw...
             arrows(x1[m],laby[k],x2[m],laby[k],col=colors[pen[k]],
                    length=lengthArrow,lwd=lwdArrow)
          } else {
             # length of arrow is zero, so plot a dot..
             points(x1[m],laby[k],pch=20,cex=cexDot,col=colors[pen[k]])
          }
        }  
     }   
     #  y is from 0 to 6, so the enter line for each arrow is 1,2,3,4,5, etc.
     par(oldpar)
   
   }

  # ____________________________PanelOutline____________________

  groupPanelOutline(panelGroup,j)      # outline full group (column)
  
  # Column done check for reference line.
  
  if(!is.na(refval)) 
             rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

#####
#
#  type = 'bar' =========================================================
#
#  rlStateBar
#

rlStateBar = function(j){
  # j = current panel column number
  #  
  #  col1[j] points to the stateFrame column holding the bar height from zero.
  #
  wdim <- dim(dat)
  ErrFnd = FALSE
  if (col1[j] > wdim[2])
    { 
       warning(paste("SGLBAR-01 Specified column number in col1 for the bar height value is out of range.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col1[j] < 1)
    {
       warning(paste("SGLBAR-02 Specified column number in col1 for the bar height value is <= 0.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
   
  if (ErrFnd) return ()    # error warning found - return.

  py =  barht*c(-.5,-.5,.5,.5,NA)     #  barht = 2/3 (0.6667)
  ry = c(0,1)
  refval = lRefVals[j]    # changed to lRefVals - JP-2010/07/23
  reftxt = lRefTexts[j]   # new - JP-2010/07/23

  # ________scale x axis________________________

  x = dat[,col1[j]]             # one column
  
  good = !is.na(x)
  
  rx = range(x,na.rm=T)         # get range of values (min-1, max-2)
  if(rx[2]<=0){                 
      # max < 0..
    rx[2]= 0           # set max to zero
    rx[1] = mean(1,sc)*rx[1]   # adjust min.
  } else if (rx[1] >=0 ) {
       #  min > 0 
       rx[1]= 0      # set min to zero
       rx[2] = rx[2]*(1+sc)/2  # adjust max
    } else {
       # min and max are both > 0 
       rx = sc*diff(rx)*c(-.5,.5)+mean(rx)
    }

  # ____________label axis_______________

  panelSelect(panels,1,j)                         # first panel
  panelScale(rx,ry)                               # scale to match data.
  mtext(lab1[j],side=3,line=line1,cex=cexText)    # two column top column titles
  mtext(lab2[j],side=3,line=line2,cex=cexText)
  
  atRx = panelInbounds(rx)
  axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # top of column axis labels

  panelSelect(panels,ng,j)                        # last panel
  panelScale(rx,ry)
  # padj in axis needed to make grid line label close
  
  axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))  # both labels
  mtext(side=1,lab3[j],line=line3,cex=cexText)    # bottom column title

  # _______________drawing loop___________________

  for (i in 1:ng){                       
     gsubs = ib[i]:ie[i]                         # index of elements in panel
     ke = length(gsubs)
     pen = if(i==6)6 else 1:ke                   # Pen indexes.
     laby = ke:1                                 # laby (1 or 1:5)
     
     panelSelect(panels,i,j)                     # select current panel
     panelScale(rx,c(1-pad,ke+pad))              # scale to 1 or 5 entries         
     panelFill(col=colPanelFill)
     
     axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grids
     
     # if a refval is provided then add line.
        if(!is.na(refval))
           {
             lines(rep(refval,2),c(1-padMinus,ke+padMinus), lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
           }          
     panelOutline(col=colOutline)                # outline full panel
     
     for (k in 1:ke){
        m = gsubs[k]                             # draw each entry (1 to ke), get index from gsubs
        val = x[m]                               # get value for bar height
        if(good[m]){
           # good value - draw bars are polygons.  (why to polygon)
           polygon(c(0,val,val,0,NA),rep(laby[k],5)+py,col=colors[pen[k]]) 
           polygon(c(0,val,val,0,NA),rep(laby[k],5)+py,
               col=colBarOutline,lwd=lwdBarOutline,density=0)
        }
        lines(c(0,0),c(1-.5*barht,ke+.5*barht),col=1) # bar base line  
     }   
  }

  # ____________________________PanelOutline____________________

  groupPanelOutline(panelGroup,j)
 
  # _______Reference Value Legend

  if(!is.na(refval)) 
             rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

#####
#
#  type = 'boxplot' ======================================================
#
#  rlStateBoxplot
#

rlStateBoxplot = function(j,boxnam){
   
   ErrFnd = FALSE
   boxlist = tryCatch(get(boxnam, pos=1),error=function(e) e)
   if (inherits(boxlist,"error"))
     {
        warning(paste("BOXP-00 List ",boxnam," does not exist or is bad.",sep=""))
        ErrFnd = TRUE
     
     } else {
        if (!is.list(boxlist))
          {
             warning("BOXP-01 Data structure for Boxplots must be a list.")
             ErrFnd = TRUE

          } else {
      
            lnam = names(boxlist)
            if (length(lnam) < 1)
              {    
                # must have at least 1 element and name.
	        warning("BOXP-07 No names exist on list of boxplot data.")
	        ErrFnd = TRUE

              } else {
                nbox = c("stats","n","conf","out","group","names")  # correct list of names for boxplot data.
           
                if (any(is.na(match(lnam,nbox))))
                  {
                     # at least one of the list names does not match or is missing.
                     warning("BOXP-06 The list names do not match the standard boxplot function output list names.")
                     ErrFnd = TRUE

                  } else {
                    nc = dim(boxlist$stat)[2]                # number of columns in boxplot stats data list.
                    if (nc != 51)
                      {
                         warning("BOXP-02 The $stats matrix in the boxplot data must have 51 elements - one for each state and DC.")
                         ErrFnd = TRUE
                      }
   
                    nr = dim(boxlist$stat)[1]
                    if (nr != 5)
                      {
                         warning("BOXP-05 The $stats matrix in the boxplot data does not have 5 values per state/DC.") 
                         ErrFnd = TRUE
                      }
   
                    nn = sort(unique(boxlist$names))          # get list of unique state ids used 
                    if (length(nn) != 51)
                      {
                         warning("BOXP-03 The boxplot list does not contain 51 unique state ids.")
                         ErrFnd = TRUE
                      }
   
                    tnn = is.na(match(nn,sortedStateId))
                    if (any(tnn))   # test to see if any did NOT match
                      {
                         lnn = paste(nn[tnn],collapse=" ")
                         warning(paste("BOXP-04 The abbreviated state ids found in the boxplot list $names list contain invalid values: ",lnn,sep=""))
                         ErrFnd = TRUE
                      }
                  }
              }
          }   
     }
   
   if (ErrFnd) return ()
   
   refval = lRefVals[j]              # get referrence to object, changed 
                                     #    to lRefVals - JP-2010/07/23
   reftxt = lRefTexts[j]             # new - JP-2010/07/23

   #_______________Scaling____________
   
   # y boxplot scaling               # standard - horizontal box - no vertical 
                                     #     (y) dimensions
   py = c(-.5,-.5,.5,.5)
   thiny = thinBox*py
   thicky = thickBox*py 
   medy = medianLine*c(-.5,.5)
  
   ry = c(0,1)                       # used in y scaling for grid lines
  
   #_______________Gather stats and put in State Order______________
  
   # For the moment match on names
   #                     Boxlist = names, stats, out, group, 
   #
   # Boxplot function generates a list value containing:
   #     stats  = matrix - each column is lower, lower hinge, median, upper hinge, upper wicker for plot/group
   #     n      = vector of number of observ in each group
   #     conf   = a matrix which each col contins the low/upper extremes
   #     out    = valies of any data points which lie extremes of whiskers
   #     group  = vector (same length as out) whose elements indicate to which group
   #     names  = vector of names for the groups  (must be 2 char state names)
   #              There must be 51 unique names that match the state abbreviation list.
   #
   
   stats = boxlist$stats       # statistics: 1-low,2-25%,3-median,4-75%,5-high 
                               #   - 5 variables for each state.
   #  indexes to boxplot values.   (pull values into thin and thick)  (set up for "boxes")
   thin = stats[c(1,5,5,1),]   # a column for each state - thin line - outliers (Lower, upper wickers)
                               #   - columns in boxlist (1,5,5,1)
   thick = stats[c(2,4,4,2),]  # a column for each state - thick line - 25% to 75% (lower and upper hinge)
                               #   - columns in boxlist(2,4,4,2)
   med = stats[3,]             # a single value for each state (median)
  
   nam = boxlist$names         # state name.
  
   # conf = boxlist$conf       # matrix of extremes - not used.
   
   outlier = rep(F,length(med))   # build vector of all outliers - set to False
   if(!is.null(boxlist$out))
     {                               # if outliers exist
       out = boxlist$out
       group = boxlist$group
       outlier[unique(group)] = T
       # set to True if we have an outlier to graph.
     }


   #### Need to put in order
   ord = match(stateId,nam)   # ord based on match between boxplot$names and StateIDs.  (Convert XX to index.

   # what about missing values  -  if NA do not plot on that line

   # What about name type inconsistency  
   # I will require use of state name abbreviation
   
   # Fips codes be useful
   #    split() based on first two digits of county fips  
   #    I could stash state fips in stateFrame sorted order

   # For Boxplot median sorting    
   #   Currently the user would need to sort the 
   #   medians in the state frame making sure
   #   the row.names were correct.
   #
   #   JP-no data in col1, col2, or col3 to sort like the other columns... All of the data is in these structures.
   #   
   #   boxlist$stats[3,]   # the median.
   #
   #   at present no re-ordering of the boxplots like the other plots.
   #   JP-if other column is sorted, boxplots will follow that order via the indexes.
   #

   # ___________ scale x axis_______________

   if(is.null(out)) rx = range(stats,na.rm=TRUE) else    # if no outliers - range only on stats
              rx = range(stats,out,na.rm=TRUE)           # if outliers - range on stats and outliers
   
   rx = sc*diff(rx)*c(-.5,.5)+mean(rx)        # min to max range with expansion factors.
   # are these used.
   dx = diff(rx)/200                          # difference / 200 (??? use)
   px= c(-dx,-dx,dx,dx)                       # is this used???

   # ____________titles and labeling axes_______________

   # _____________top of column______
   panelSelect(panels,1,j)   # top panel - add title.
   panelScale(rx,ry)
   mtext(lab1[j],side=3,line=line1,cex=cexText)             # top column titles
   mtext(lab2[j],side=3,line=line2,cex=cexText)
   
   atRx = panelInbounds(rx)
   axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # top axis labels

   # ____________bottom of column____
   panelSelect(panels,ng,j)  # bottom panel - add sub title and refvals.
   panelScale(rx,ry)
   # padj in axis needed to make grid line label close
   axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # bottom axis labels
   mtext(side=1,lab3[j],line=line3,cex=cexText)             # bottom column titles

   # _______________drawing loop___________________

   oldpar = par(lend="butt")

   for (i in 1:ng){

      # Cycle through the Row/Groups in the micromap column
      
      gsubs = ib[i]:ie[i]    # get beginning to end row number in group  
      ke = length(gsubs)     # get number of rows in group  
      
      pen = if(i==6) 6 else 1:ke  # if middle group (6), then pen=6, otherwise pen = c(1...x)   
      
      laby = ke:1            # laby = reverse order list for row index.         
      
      panelSelect(panels,i,j)   # select panel for group i in column j)
      panelScale(rx,c(1-pad,ke+pad))   # set scale for panel
      panelFill(col=colPanelFill)           # set fill for panel
      
      axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid lines
  
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
      
      panelOutline(col=colOutline)     # outline panel

      for (k in 1:ke){
         # cycle through row-groups and build each box plot
         
         m = ord[gsubs[k]]   # m is the location of the state in boxlist
         if(is.na(m)) next   #   if no location - skip box plot for state
         kp = pen[k]         # color number
         ht = laby[k]
         
         if(outlier[m]){
            #   plot points for outliers
            vals = out[group==m]
            if (colFull)
              {  # full color do the correct color
                 points(vals,rep(ht,length(vals)),pch=1,
                    col=ifelse(useBlack,"black",colors[kp]),
                    cex=cexOutlier,lwd=lwdOutlier)
              } else {
                 # Greys - do the a grey.
                 points(vals,rep(ht,length(vals)),pch=1,
                    col=colBpGreyOutliner,
                    cex=cexOutlier,lwd=lwdOutlier)
              }
         }  
 
         # draw thin lower to upper box.
         polygon(thin[,m],rep(ht,4)+ thiny,col=colors[kp],border=NA)
#        polygon(thin[,m],rep(ht,4)+ thiny,col=colBpOutline,density=0) # don't outline boxes
         # draw middle think box
         polygon(thick[,m],rep(ht,4)+ thicky,col=colors[kp],border=NA)
#        polygon(thick[,m],rep(ht,4)+ thicky,col=colBpOutline,density=0) # don't outline boxes

#        points(med[m],ht,col=colBpDotMedian,pch=pchMedian,cex=cexMedian)  # don't put a dot in.
#        points(med[m],ht,col="black",pch=1,cex=cexMedian)

#        polygon(med[m]+px,ht+medianLine*dy,lwd=1,density=0)   # median line?

#        Lines looked crooked
         segments(med[m],ht+medy[1],med[m],ht+medy[2],         # use segment line.
               col=colBpMedian,lwd=lwdMedian)
#        lines(rep(med[m],2),ht+medy,col=colBpMedian,lwd=lwdMedian)
      }   
   }
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
         rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23


}


#####
#
# type = 'dot'   =====================================================
#
# rlStateDot
#

rlStateDot = function(j){

  # Single Dot, no extra line or interval
  #
  # j = current panel column number
  #  
  #  col1[j] points to the stateFrame column holding the first arrow end point.value
  #
  wdim <- dim(dat)
  ErrFnd = FALSE
  if (col1[j] > wdim[2])
    { 
       warning(paste("SGLDOT-01 Specified column number in col1 for the dot value is out of range.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col1[j] < 1)
    {
       warning(paste("SGLDOT-02 Specified column number in col1 for the dot value is <= 0.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  
  if (ErrFnd)  return ()    # error warning found - return

  #  JB - add "as.double(as.vector(" to handle variation in how objects are converted.
  
  x = as.double(as.vector(dat[,col1[j]]))   # one value - the dot.
 
  good = !is.na(x)
 
  refval = lRefVals[j]    # get reference value for this column, changed 
                         #   to lRefVals - JP-2010/07/23
  reftxt = lRefTexts[j]   # new - JP-2010/07/23

  ry = c(0,1)

  #____________scale x axis______________________
  rx = range(x,na.rm=TRUE)
  rx = sc*diff(rx)*c(-.5,.5)+mean(rx)

  # ____________labeling axis_______________
  panelSelect(panels,1,j)
  panelScale(rx,ry)
  mtext(lab1[j],side=3,line=line1,cex=cexText)   # top column titles
  mtext(lab2[j],side=3,line=line2,cex=cexText)

  atRx = panelInbounds(rx)
  axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))  # top axis labels
 
  panelSelect(panels,ng,j)
  panelScale(rx,ry)
  # padj in axis needed to make grid line label close
  axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))  # bottom axis labels
  mtext(side=1,lab3[j],line=line3,cex=cexText)   # bottom column titles

  # _______________drawing loop___________________
  for (i in 1:ng){
     gsubs = ib[i]:ie[i]
     ke = length(gsubs)
     pen = if(i==6) 6 else 1:ke
     laby = ke:1 
     panelSelect(panels,i,j)
     panelScale(rx,c(1-pad,ke+pad))
     panelFill(col=colPanelFill)
     axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid
     
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
     
     panelOutline(colOutline) 
     for (k in 1:ke){
        # step through values for this panel
        m=gsubs[k]
        if(good[m]){    # if good - plot dot.
           if (doDotOutline) 
             {
               points(x[m],laby[k],pch=pchDot,cex=cexDot,lwd=lwdDotOutline, col=colDotOutline,bg=colors[pen[k]])         
             } else {
               points(x[m],laby[k],pch=pchDot,cex=cexDot,col=NA, bg=colors[pen[k]])
             }
        }
        
     }
  }

  # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
             rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

#####
#
#  type = 'dotconf' ====================================================
#
#  flStateDotConf
#

rlStateDotConf = function(j){
  #
  #  j is the current panel column index
  #
  #   col1 indicates the column number for the dot value in the stateFrame.
  #   col2 indicates the column number for the lower confidence value in the stateFrame.
  #   col3 indicates the column number for the upper confidence value in the stateFrame.
  
  wdim <- dim(dat)
  ErrFnd = FALSE
  if (col1[j] > wdim[2])
    { 
       warning(paste("DOTCONF-01 Specified column number in col1 for dot values is out of range.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col1[j] < 1)
    {
       warning(paste("DOTCONF-02 Specified column number in col1 for dot values is <= 0.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col2[j] > wdim[2])
    { 
       warning(paste("DOTCONF-03 Specified column number in col2 for lower confidence values is out of range.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col2[j] < 1)
    {
       warning(paste("DOTCONF-04 Specified column number in col2 for lower confidence values is <= 0.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col3[j] > wdim[2])
    { 
       warning(paste("DOTCONF-03 Specified column number in col2 for upper confidence values is out of range.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col3[j] < 1)
    {
       warning(paste("DOTCONF-04 Specified column number in col2 for upper confidence values is <= 0.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }

  if (ErrFnd) return ()        # error warning found - return
 
  x = dat[,col1[j]]              # Col 1 = DOT - median/mean
  lower = dat[,col2[j]]          # Col 2 = lower
  upper = dat[,col3[j]]          # Col 3 = upper
 
  good1 = !is.na(x)              # Good Col1 values (dot value)
  good2 = !is.na(upper+lower)    # Good col2,3 values (upper and lower)
 
  if(!all(good2))warning("Missing Value in Confidence Intervals") 
 
  refval = lRefVals[j]           # changed to lRefVals, JP-2010/07/23
  reftxt = lRefTexts[j]          # new - JP-2010/07/23

  ry = c(0,1)

  #_____________scale x axis________________
  rx = range(upper,lower,x,na.rm=TRUE)
                        #  x may not be needed???
  rx = sc*diff(rx)*c(-.5,.5)+mean(rx)

  # ____________labeling axes_______________
  
  # panel 1 has line 1 and line 2, top Axis + later image.

  panelSelect(panels,1,j)     # labels (line 1, line 2 and top axis)
  panelScale(rx,ry)
  mtext(lab1[j],side=3,line=line1,cex=cexText)    # top column titles
  mtext(lab2[j],side=3,line=line2,cex=cexText)
  
  atRx = panelInbounds(rx)
  axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))  # top axis labels
  
  # panel ng has bottom axis and line 3 + later image.
  panelSelect(panels,ng,j)    # labels (bottom -> axis and line 3)
  panelScale(rx,ry)
  # padj in axis needed to make grid line label close
  axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # bottom axis labels
  mtext(side=1,lab3[j],line=line3,cex=cexText)  # bottom column title
 
  # line 4 is added if refval is present

  #_____________drawing loop___________________
  
  for (i in 1:ng){
     gsubs = ib[i]:ie[i]
     ke = length(gsubs)
     pen = if(i==6) 6 else 1:ke
     laby = ke:1
     panelSelect(panels,i,j)   
     panelScale(rx,c(1-pad,ke+pad))   # Adjusted scale for interior
     panelFill(col=colPanelFill)
     axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # vertical grid lines
     
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
     
     panelOutline(col=colOutline)     # outline scaled image.
     for (k in 1:ke){ 
        m = gsubs[k]
        
        #
        #  Not checking good1 - change code and test.
        #

        if(good2[m])  # if valid upper value.
          {
             lines(c(lower[m],upper[m]),rep(laby[k],2),
                   col=colors[pen[k]],lwd=lwdConf)
          }
        if (doDotOutline) 
          {
             points(x[m],laby[k],pch=pchDot,cex=cexDot,lwd=lwdDotOutline,col=colDotOutline,bg=colors[pen[k]])         
          } else {
             points(x[m],laby[k],pch=pchDot,cex=cexDot,col=NA, bg=colors[pen[k]])
          }
     }   

     #   segments(lower[gsubs],laby,upper[gsubs],laby,col=color[pen],lwd=lwdConf)
     #   points(x[gsubs],laby,pch=pch,cex=dotCex,col=colors[pen])
     #   points(x[gsubs],laby,pch=1,cex=cexDot,col=colDotOutline,
     #         lwd=lwdDotOutline)
   }

  # ____________________________PanelOutline____________________

  groupPanelOutline(panelGroup,j)
  
  #  Put legend at end of column for reference value.
  if(!is.na(refval)) 
             rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

#####
#
# type = 'dotse' =======================================================
#
# rlStateDotSe
#

rlStateDotSe = function(j){
  #   j = current panel column
  #
  #   col1 indicates the column number for the dot value in the stateFrame.
  #   col2 indicates the column number for the SE value in the stateFrame.
  
  wdim <- dim(dat)
  ErrFnd = FALSE
  
  if (col1[j] > wdim[2])
    { 
       warning(paste("DOTSE-01 Specified column number in col1 for dot values is out of range.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col1[j] < 1)
    {
       warning(paste("DOTSE-02 Specified column number in col1 for dot values is <= 0.",col1[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col2[j] > wdim[2])
    { 
       warning(paste("DOTSE-03 Specified column number in col2 for SE values is out of range.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }
  if (col2[j] < 1)
    {
       warning(paste("DOTSE-04 Specified column number in col2 for SE values is <= 0.",col2[j],"Not in stateFrame."))
       ErrFnd = TRUE
    }

  if (ErrFnd) return ()   # error warning found - return
  
  x = dat[,col1[j]]
  zval = qnorm(.5+conf/200)
  inc = zval*dat[,col2[j]]
  upper = x+inc
  lower = x-inc
 
  good1 = !is.na(x)
  good2 = !is.na(upper)
 
  if(any(is.na(inc)))warning("Missing Value in Standard Errors")
  refval = lRefVals[j]          # changed to lRefVals, JP-2010/07/23
  reftxt = lRefTexts[j]         # new - JP-2010/07/23

  ry=c(0,1)

#_______________scale x axis__________________
  rx = range(upper,lower,x,na.rm=TRUE)  # use upper, lower and x to find "range" of x
          # x may not be needed at all. But best to leave.
  rx = sc*diff(rx)*c(-.5,.5)+mean(rx)

# ____________labeling axes_______________

  panelSelect(panels,1,j)
  panelScale(rx,ry)
  mtext(lab1[j],side=3,line=line1,cex=cexText)   # top column titles
  mtext(lab2[j],side=3,line=line2,cex=cexText)
  
  atRx = panelInbounds(rx)
  axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))  # top axis labels

  panelSelect(panels,ng,j)
  panelScale(rx,ry)
  # padj in axis needed to make grid line label close
  axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # bottom axis labels
  mtext(side=1,lab3[j],line=line3,cex=cexText)   # bottom column titles

#__________________drawing loop________________

  for (i in 1:ng){
     gsubs = ib[i]:ie[i]
     ke = length(gsubs)
     pen = if(i==6)6 else 1:ke
     laby = ke:1 
     panelSelect(panels,i,j)
     panelScale(rx,c(1-pad,ke+pad))
     panelFill(col=colPanelFill)
     axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid
     
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
     
     panelOutline(colOutline)
     for (k in 1:ke){
        m = gsubs[k]
        if(good2[m])  # if upper value good.
          {
             lines(c(lower[m],upper[m]),rep(laby[k],2),
                   col=colors[pen[k]],lwd=lwdConf)
          }
        if(good1[m])
          {
            if (doDotOutline) 
             {
               points(x[m],laby[k],pch=pchDot,cex=cexDot,lwd=lwdDotOutline,col=colDotOutline,bg=colors[pen[k]])         
             } else {
               points(x[m],laby[k],pch=pchDot,cex=cexDot,col=NA, bg=colors[pen[k]])
             }
          }  
     }   
   }

# ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
             rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

 }

#####
#
# type = 'id' =======================================================
#
# rlStateId
#

rlStateId = function(j){
  #  j = panel column number
  
  py = barht*c(-.5,-.5,.5,.5,NA)
  px = c(.04,.095,.095,.04,NA)
  idstart = .137

#_____________ Scaling ______________________ 
 
  rx = c(0,diff(panels$coltabs[j+1,])) # width in inches
  ry = c(0,1)

#______________________panel labels_____________

  panelSelect(panels,1,j)      # start at I = 1, but j= is the current column.
  panelScale(rx,ry)
  mtext('U.S.',side=3,line=line1,cex=cexText)
  mtext('States',side=3,line=line2,cex=cexText)

# Cycle thought the GROUPS (ng)
  for (i in 1:ng){
     gsubs = ib[i]:ie[i]           # first element of group to last element of group.
     ke = length(gsubs)            # number of elements.
     laby = ke:1
     pen = if(i==6)6 else 1:ke
     panelSelect(panels,i,j)
     npad = ifelse(i==6,.57,pad)
     panelScale(rx,c(1-npad,ke+npad))
     gnams = stateNames[gsubs]
     polygon(rep(px,ke),rep(laby,rep(5,ke)) + py,col=colors[pen])
     polygon(rep(px,ke),rep(laby,rep(5,ke)) + py,col=colIdOutline,density=0)
     text(rep(idstart,ke), laby-idty, gnams, adj=c(0,0), cex=cexText,xpd=T)
  }

  # No reference values for this type of column
}

#####
#
# Id Dot================================================================
#
#  rlStateIdDot      ##  NOT USED  ##
#      Plot the state name and a dot.
#

rlStateIdDot = function(j){
  #  j = panel column number

  #________________ Scaling _______________

  #  py = barht*c(-.5,-.5,.5,.5,NA)  # form the box... (not used its a PCH)
  rx = c(0,1)
  ry = c(0,1)
  idstart = .137
  dotstart = .0675

  #______________________panel labels_____________

  panelSelect(panels,1,j)
  panelScale(rx,ry)
  mtext('U.S.',side=3,line=line1,cex=cexText)
  mtext('States',side=3,line=line2,cex=cexText)
 
  for (i in 1:ng){
     gsubs = ib[i]:ie[i]
     ke = length(gsubs)
     laby = ke:1
     pen = if(i==6)6 else 1:ke
     panelSelect(panels,i,j)
     panelScale(rx,c(1-pad,ke+pad))
     gnams = stateNames[gsubs]
     points(dotstart,laby,pch=idDotPch,col=colors[pen],cex=cexDot)
     #points(dotstart,laby,pch=1,col=colDotOutline,cex=cexDot)
     text(rep(idstart,ke),laby+.1,gnams,adj=0,cex=cexText)
  }

  #  No reference values for this type of column.
}



#####
#
# type = 'map'  =========================================================
#
# rlStateMap
#

rlStateMap = function(j){

  # Works using state abbreviations
  # bnd.ord gives abbreviations in the
  #           the boundary are stored.
  # stateId give the abbreviations in the order plotted 

  bnd.ord = rlStateVisBorders$st[is.na(rlStateVisBorders$x)] # State abbrev
  rxpoly = range(rlStateVisBorders$x,na.rm=TRUE)
  rypoly = range(rlStateVisBorders$y,na.rm=TRUE)

  # ____________labeling and axes_______________
  
  panelSelect(panels,1,j)
  panelScale()
  par(xpd=T)
  
  mtext("Highlighted",side=3,line=line1,cex=cexText)
  mtext("States",side=3,line=line2,cex=cexText)

  # Drawing Loop

  for (i in 1:ng){

    if(i==6){                   # line break in maps.   Group 6 - middle group of 11.
      panelSelect(panels,6,j)
      panelScale()
      panelFill (col=colPanelFill)
      
      panelOutline()
      text (.5,.55,'Median For Sorted Panel',cex= cexText*0.8)
      next  # skip to next FOR item
    }
    
    panelSelect(panels,i,j)     # Do map in - Panels by group...
    panelScale(rxpoly,rypoly)
    gsubs = ib[i]:ie[i]

    if(i==5) gsubs = c(gsubs,26)  # slot 5 - add 26 to this group
    if(i==7) gsubs = c(gsubs,26)  # slot 7 - add 26 to this group
  
    gnams = stateId[gsubs]
  
    # now find the state regions to plot
    
    back = is.na(match(rlStateVisBorders$st,gnams))
    
    if(any(back)){
      polygon(rlStateVisBorders$x[back], rlStateVisBorders$y[back],
          density=-1, col=colMapBackgr, border=FALSE)         # fill in states
      polygon(rlStateVisBorders$x[back], rlStateVisBorders$y[back],
          col=colLineBackgr, density=0, lwd=lwdBackGr)     # outline states
    }

    fore = !back
    pen = match(bnd.ord,gnams,nomatch=0)
    pen = pen[pen>0]
    
    polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
          density=-1, col=colors[pen], border=FALSE)        # fill in states
    
    polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
          density=0,  col=colLineForegr, lwd=lwdForeGr) # outline states
  
    polygon(rlStateNationVisBorders$x, rlStateNationVisBorders$y,
          density=0, col=colLineNation, lwd=lwdNation)  # outside US boundary
  
    # might be made a function
    if(i==1){
       text(135,31,'DC',cex=cexState,adj=.5, col=1)
       text(22, 17,'AK',cex=cexState,adj=.5, col=1)
       text(47, 8, 'HI',cex=cexState,adj=.5, col=1)
    }
  
  }  # i loop
  
  # no reference values for this type of column. If present - ignor.
}

#####
#
# type = 'mapcum'   ========================================================
#
# rlStateMapCum
#

rlStateMapCum = function(j){

  # Works using state abbreviations
  # bnd.ord gives abbreviations in the
  #           the boundary are stored.
  # stateId give the abbreviations in the order plotted 

  bnd.ord = rlStateVisBorders$st[is.na(rlStateVisBorders$x)] # State abbrev
  rxpoly = range(rlStateVisBorders$x,na.rm=TRUE)
  rypoly = range(rlStateVisBorders$y,na.rm=TRUE)

  # ____________labeling and axes_______________

  panelSelect(panels,1,j)
  panelScale()
  
  
  box.x = rep(c(.14,.14,.208,.208,NA),2)-.04     
  par(xpd=T)
  y.ht = c(.05,.172)
  y.sep = .19*legfactor + 0.05            #  .185
  box.y = 1.025*legfactor +c(y.ht,rev(y.ht),NA) + 0.07 - 0.06  ## down 0.06

  polygon(box.x,c(box.y,box.y+y.sep),col=c(colMapBackgr,colors[7]))
  polygon(box.x,c(box.y,box.y+y.sep),col=1,density=0)
  
  mtext("Cumulative Maps",side=3,line=line1,cex=cexText)
  mtext('States Featured Above',side=3,line=line2,at=.20,cex=cexText,adj=0)
  mtext('States Featured Below',side=3,line=lineTiclab,at=.20,cex=cexText,adj=0)

  # Drawing Loop

  for (i in 1:ng){

     if(i==6){
        panelSelect(panels,6,j)
        panelScale()
        panelFill (col=colPanelFill)
        panelOutline()
        text (.5,.55,'Median For Sorted Panel',cex=cexText)
        next
     }
     panelSelect(panels,i,j)
     panelScale(rxpoly,rypoly)
     gsubs = ib[i]:ie[i]

     if(i < 5)  cont = stateId[1:(5*i)] else cont = stateId[1:(5*i-4)]
     if(i == 5) {gsubs = c(gsubs,26); cont = stateId[1:26]}
     if(i == 7) gsubs = c(gsubs,26) 

     gnams = stateId[gsubs]

     # now find the state regions to plot
     back = is.na(match(rlStateVisBorders$st,cont))
     if(any(back)){
           polygon(rlStateVisBorders$x[back],rlStateVisBorders$y[back],
               density=-1, col=colMapBackgr,border=F)         # fill in states
           polygon(rlStateVisBorders$x[back], rlStateVisBorders$y[back],
               density=0,  col=colLineBackgr,lwd=lwdBackGr) # outline states
     }

     fore = !back
     pen = match(bnd.ord,gnams,nomatch=0)
     pen = ifelse(pen==0 & match(bnd.ord,cont,nomatch=0)>0,7, pen)
     pen = pen[pen>0]
     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=-1,col=colors[pen], border=F)           # fill in states
     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=0, col=colLineForegr, lwd=lwdForeGr)    # outline states

     polygon(rlStateNationVisBorders$x,rlStateNationVisBorders$y,
        col=colLineNation,density=0,lwd=lwdNation)      # US outside boundary

     # might be made a function
     if(i==1){
        text(135,31,'DC',cex=cexState,adj=.5, col=1)
        text(22,17,'AK',cex=cexState,adj=.5, col=1)
        text(47,8,'HI',cex=cexState,adj=.5, col=1)
     }

   }  # i loop

  # no reference values for this type of column. If present - ignor.

}

#####
#
# type = 'mapmedian'  =================================================
#
# rlStateMapMedian
#

rlStateMapMedian = function(j){

  # Works using state abbreviations
  # bnd.ord gives abbreviations in the
  #           the boundary are stored.
  # stateId give the abbreviations in the order plotted
  # This MapMedian cream colors all states above and below the median state. 

  bnd.ord = rlStateVisBorders$st[is.na(rlStateVisBorders$x)] # State abbrev
  rxpoly = range(rlStateVisBorders$x,na.rm=TRUE)
  rypoly = range(rlStateVisBorders$y,na.rm=TRUE)

  # ____________labeling and axes_______________

  panelSelect(panels,1,j)
  panelScale()
  box.x = rep(c(.14,.14,.208,.208,NA),2)+.02   
  par(xpd=T)
  y.ht = c(.05,.172)
  y.sep = .19*legfactor + 0.05    # .185
  box.y = 1.025*legfactor +c(y.ht,rev(y.ht),NA) + 0.07 - 0.03
  
  polygon(box.x,c(box.y,box.y+y.sep),col=c(colMapBackgr,colors[7]))
  polygon(box.x,c(box.y,box.y+y.sep),col=1,density=0)

  mtext("Median Based Contours",side=3,line=line1,cex=cexText)
  mtext('States In This Half',side=3,line=line2,at=.26,cex=cexText,adj=0)
  mtext('States In Other Half',side=3,line=lineTiclab,at=.26,cex=cexText,adj=0)

  # Drawing Loop

  for (i in 1:ng){

     if(i==6){
        panelSelect(panels,6,j)
        panelScale()
        panelFill (col=colPanelFill)
        panelOutline()
        text (.5,.58,'Median For Sorted Panel',cex=cexText)
        next  
     }
     panelSelect(panels,i,j)
     panelScale(rxpoly,rypoly)
     gsubs = ib[i]:ie[i]
     if(i <= 5) cont = stateId[1:26] else cont = stateId[26:51]
     if(i == 5) gsubs = c(gsubs,26)
     if(i == 7) gsubs = c(gsubs,26) 
     #  gsubs = current state list
     #  cont  = state list to be colored cream.

     gnams = stateId[gsubs]

     # now find the state regions to plot
     back = is.na(match(rlStateVisBorders$st,cont))
     if(any(back)){
          polygon(rlStateVisBorders$x[back],rlStateVisBorders$y[back],
             density=-1,col=colMapBackgr,border=F)          # fill in states
          polygon(rlStateVisBorders$x[back], rlStateVisBorders$y[back],
             density=0, col=colLineBackgr,lwd=lwdBackGr) # outline states

     }

     fore = !back     # 
     pen = match(bnd.ord,gnams,nomatch=0)
     pen = ifelse(pen==0 & match(bnd.ord,cont,nomatch=0)>0,7, pen)
     pen = pen[pen>0]

     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=-1,col=colors[pen], border=F)       # fill in states
     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=0, col=colLineForegr, lwd=lwdForeGr) # outline states

     polygon(rlStateNationVisBorders$x,rlStateNationVisBorders$y,
        density=0, col=colLineNation,lwd=lwdNation)  # outside US boundary

     if(i==1){
        text(135,31,'DC',cex=cexState,adj=.5, col=1)
        text(22,17,'AK',cex=cexState,adj=.5, col=1)
        text(47,8,'HI',cex=cexState,adj=.5, col=1)
     }

   }   # i loop

  # no reference values for this type of column. If present - ignor.

}

#####
#
# type = 'maptail' ====================================================
#
# rlStateMapTail
#

rlStateMapTail = function(j){

  # Works using state abbreviations
  # bnd.ord gives abbreviations in the
  #           the boundary are stored.
  # stateId give the abbreviations in the order plotted
  # MapTail shows current states in a group as colored and
  # a tail of states (in cream color) from the outside inward.  
  # 

  bnd.ord = rlStateVisBorders$st[is.na(rlStateVisBorders$x)] # State abbrev
  rxpoly = range(rlStateVisBorders$x,na.rm=TRUE)
  rypoly = range(rlStateVisBorders$y,na.rm=TRUE)

  # ____________labeling and axes_______________

  # column header titles and "box"
  panelSelect(panels,1,j)    #  Line 1 and Line 2 - panel 1
  panelScale()
  # JP - as column labels move around or are repositioned,
  #    the associated BOX below does not follow it.
  box.x = c(.14,.14,.208,.208,NA)-.00 
  par(xpd=T)
  y.ht = c(.05,.172)
  y.sep = .185*legfactor
  
  box.y = 1.025*legfactor +c(y.ht,rev(y.ht),NA) + .07 - 0.01  # down 0.01
  polygon(box.x,box.y+y.sep,col=colors[7])
  polygon(box.x,box.y+y.sep,col=1,density=0)

  mtext("Two Ended Cumulative Maps",side=3,line=line1,cex=cexText)
  mtext('States Highlighted',side=3,line=line2,at=.25,cex=cexText,adj=0)
  
  #  JP - removed - temp
  #  mtext('Further From Median',side=3,line=lineTiclab,at=.15,cex=cexText,adj=0)

  # Drawing Loop

  for (i in 1:ng){

     if(i==6){
        panelSelect(panels,6,j)
        panelScale()
        panelFill (col=colPanelFill)
        panelOutline()
        text (.5,.58,'Median For Sorted Panel',cex=cexText)
        next
     }
     panelSelect(panels,i,j)  
     panelScale(rxpoly,rypoly)
     # get list of states in this group.
     gsubs = ib[i]:ie[i]
     if(i < 5) cont = stateId[1:(5*i)]
     if(i==5)  {gsubs = c(gsubs,26); cont = stateId[1:26]}
     if(i==7)  {gsubs = c(gsubs,26); cont = stateId[26:51]} 
     if(i > 7) cont = stateId[(5*i-8):51]
     # get list of group state names 
     gnams = stateId[gsubs]

     # now find the state regions to plot
     #   plot states with cream filling (reported states)
     back = is.na(match(rlStateVisBorders$st,cont))
     if(any(back)){
         # paint fill
         polygon(rlStateVisBorders$x[back],rlStateVisBorders$y[back],
              density=-1,col=colMapBackgr,border=F)          # fill in states
         # paint lines
         polygon(rlStateVisBorders$x[back], rlStateVisBorders$y[back],
              density=0, col=colLineBackgr,lwd=lwdBackGr) # outline states
     }

     fore = !back
     #  current 5 states with colors.
     pen = match(bnd.ord,gnams,nomatch=0)
     pen = ifelse(pen==0 & match(bnd.ord,cont,nomatch=0)>0,7, pen)
     pen = pen[pen>0]

     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=-1,col=colors[pen], border=F)        # fill in states
     polygon(rlStateVisBorders$x[fore], rlStateVisBorders$y[fore],
        density=0, col=colLineForegr, lwd=lwdForeGr) # outline states

     #  The US border.
     polygon(rlStateNationVisBorders$x,rlStateNationVisBorders$y,
        density=0, col=colLineNation, lwd=lwdNation) # outside US boundary

     if(i==1){
        text(135,31,'DC',cex=cexState,adj=.5, col=1)
        text(22,17,'AK',cex=cexState,adj=.5, col=1)
        text(47,8,'HI',cex=cexState,adj=.5, col=1)
     }

   }   #  i loop

  # no reference values for this type of column. If present - ignor.

}  

#
#

###################################################
#
#  For TS, and TSConf I could not find a way to  use to have stateIDs as the names of 
#  each state matrix, in list or data.frame.   So, the out at this time is
#  to assume the original panelData array is in the order of the original stateFrame data.frame.
#  When stateFrame is re-ordered, I have captured the re-ordering. Using the "order" index
#  the raw panelData is used via the order index to associate the line on the micromap to the data.
#   
#  Boxplot uses $names to look up to find out the record and link the Boxplot list to the 
#  stateFrame data.
#
#
#####


#####
#
# type = TS and TSConf   =====================================================
#
# rlStateTSConf  (Time Series with and without confidence interval in panel groups)
#
#     Plot all data for panel's states as one graph in panel.
#


rlStateTSConf = function(j,dataNam,conf=TRUE){
   #
   #  j = panel column number
   #  dataNam = Name of large data array containing the x, y (or y low, med and high) values 
   #     for each time period and state.
   #  conf = logical.  
   #    If TRUE, do the confidence band using y-low, y-med, and y-high values (columns 2, 3, 4)
   #    If FALSE, only plot the Y value (column 2)
   #
   
   ErrFnd = FALSE
   MsgLabel <- "TS"
   if (conf) MsgLabel <- "TSCONF"
     
   
   DataList = tryCatch(get(dataNam,pos=1),error=function(e) e)      # get name of array data object list.
   
   if (inherits(DataList,"error"))
     {
     
        warning(paste(MsgLabel,"-00 data.frame ",dataNam," does not exist or is not valid.",sep=""))
        ErrFnd = TRUE
        
     } else {
     
        # data.frame exists - can do other checks
        workDArr <- DataList
        wDArrNames <- rownames(workDArr)  # get rownames
  
        if (!is.array(workDArr))
          {
            warning(paste(MsgLabel,"-09 The data structured passed in the panelData field is not an array. Structure name = ",dataNam,sep=""))
            ErrFnd = TRUE
          }
   
        dimDArr <- dim(workDArr)
   
        if (dimDArr[1] != 51)
          {
            warning(paste(MsgLabel,"-10 The time serial array\'s 1st dimension is not 51 (states and DC). It is ",dimDArr[1],".",sep=""))
            ErrFnd = TRUE
          }
        if (dimDArr[2] < 2 || dimDArr[2] > 31)
          {
            warning(paste(MsgLabel,"-12 The time serial array\'s 2nd dimension is not between 2 and 30 (number of time periods).  It is ",dimDArr[2],".",sep=""))
            ErrFnd = TRUE
          }
  
        if (conf)   # TSCONF option.  
          {
            if (dimDArr[3] !=4)
              {
                warning(paste(MsgLabel,"-11 The time serial array\'s 3rd dimension is not 4.  It is ",dimDArr[3],",",sep=""))
                ErrFnd = TRUE
              }
          } else {
     
            if (dimDArr[3] !=2)
              {
                warning(paste(MsgLabel,"-13 The time serial array\'s 3rd dimension is not 2.  It is ",dimDArr[3],".",sep=""))
                ErrFnd = TRUE
              }
          }
  
        tnn <- is.na(match(wDArrNames,stateId))
        if (any(tnn))    # non-match found.
          {
             lnn <- paste(wDArrNames[tnn],collapse=" ")
             warning(paste(MsgLabel,"-14 Rownames on array do not match state ID list. The bad state IDs are:",lnn,sep=""))
             ErrFnd = TRUE
          }
        
     }

   if (ErrFnd) return ()
  
   refval = lRefVals[j]              # get referrence to object, changed 
                                     #    to lRefVals - JP-2010/07/23
   reftxt = lRefTexts[j]             # new - JP-2010/07/23

   # structure of dataArr
   #     dataList is a 3 dim array :
   #          a * b * c, where: 
   #          a is the state index number (1 to 51)
   #          b is the time period index (2 to 30 range)
   #          c is the type of value (1=x, 2=low, 3=mid, 4=high) or (1=x, 2=y)
   #
   
   
   
   #_______________Scaling____________
   
   # x scaling
   rx <- range(workDArr[,,1],na.rm=TRUE)           # x range from all values in vector
   rx = sc*diff(rx)*c(-.5,.5)+mean(rx)        # min to max range with expansion factors.
   
   # y scaling                  
   if (conf)
     {
        ry <- range(workDArr[,,c(-1)],na.rm=TRUE)       # range of all Y values
     } else {
        ry <- range(workDArr[,,2],na.rm=TRUE)           # range for the one Y value
     }
   ry = sc*diff(ry)*c(-.5,.5)+mean(ry)        # min to max range with expansion factors.
   
   #_______________Gather stats and put in State Order______________
  
   #
   #   JP-no data in col1, col2, or col3 to sort like the other columns... 
   #     All of the data is in these structures.
   #   
   #   at present no re-ordering of the time series like the other plots.
   #   JP-if other column is sorted, time series will follow that order via the indexes.
   #

   # ____________titles and labeling axes_______________

   # _____________top of column______
   
   panelSelect(panels,1,j)   # top panel - add title.
   panelScale(rx,ry)
   mtext(lab1[j],side=3,line=line1,cex=cexText)   # top column titles
   mtext(lab2[j],side=3,line=line2,cex=cexText)
  
   atRx = panelInbounds(rx)
   axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # top axis labels

   # ____________bottom of column____
   
   panelSelect(panels,ng,j)  # bottom panel - add sub title and refvals.
   panelScale(rx,ry)
   
   # padj in axis needed to make grid line label close
   
   axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx)) # bottom axis labels
   mtext(side=1,lab3[j],line=line3,cex=cexText)  # bottom column titles

   # _______________drawing loop___________________

   oldpar = par(lend="butt")

   for (i in 1:ng)
    {

      # Cycle through the Row/Groups in the micromap column
      
      gsubs = ib[i]:ie[i]               # get beginning to end index row number in group  
      ke = length(gsubs)                # get number of rows in group  (5 or 1)  

      gnams = stateId[gsubs]            # get list of group state ids.

      # adjust if middle group      
      pen = if(i==6) 6 else 1:ke        # if middle group (6), then pen=6, otherwise pen = c(1...x)   
      
      panelSelect(panels,i,j)           # select panel for group i in column j)
      panelScale(rx,ry)                 # set scale for panel  (should this be ry * 5 or 1?)
      panelFill(col=colPanelFill)            # set fill for panel
      
      axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid lines
      
      if (i==6)  # median panel
        {
          atRy = c(saveAtRy[1],saveAtRy[length(saveAtRy)])   
        } else {
      
          atRy = panelInbounds(ry)
        }
      if (hGridTs)
        {
           axis(side=2,tck=1,labels=F,col=colGrid,lwd=lwdGrid, at=atRy) # Grid lines
        }
      axis(side=2,tick=F,mgp=mgpLeft,cex.axis= axisCexTs*.75 , at=atRy, labels=as.character(atRy)) # Y axis labels
      mtext(lab4[j],side=2,line=line5,cex=axisCexTs)  # Y axis title
      
      #if(!is.na(refval))
      #   # add vertical line for reference.
      #   lines(rep(refval,2),c(1-padMinus,ke+padMinus), 
      #         lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
      
      panelOutline(col=colOutline)     # outline panel
   

      for (k in 1:ke)                  # Process each slot of panel - step 1 to 5 or 1 to 1

       {
         # cycle through row-groups and build each time series
         
         
         #m = datOrder[gsubs[k]]

         #m = ord[gsubs[k]]   # m is the location of the state in DataList$DArr matrix

         #if(is.na(m)) next   #     if no location - skip box plot for state

         kp = pen[k]         # color number
  
         
         #wDArr <- workDArr[m,,]
         wDArr <- workDArr[gnams[k],,]
         
         wX   <- wDArr[,1]    # get X values for line and polygon plots
	 wLine = wDArr[,2]    #  Get Y values for mid line 
         
         if (conf)
           {
             #  build polygon of confidence band to fill (y-low to y-high)
            
             wArr <- wDArr[,c(3,4)]  # adjust data for plotting in the right slot.
             wPoly <- cbind(c(wX[1],wX,rev(wX)),c(wArr[1,1],wArr[,2],rev(wArr[,1])))
            
             polygon(wPoly,col=colors[kp+7],border=NA)
        
           }
    
         #  Plot mid Line
         lines(wX,wLine,col=colors[kp],lwd=lwdTs)
       }   
      saveAtRy = atRy
    }
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
         rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

####
#
# type = 'segbar' and 'normbar'  ====================================
#
#  rlStateSegBar   (Segmented Bar chart)
#
#  Segmented bars is actually a stacked bar chart. Each segment is the length of one value.
#  The total length is the sum of the lengths of all segments.
#  The x scale of the column panels will be set to the "max" length of any bar.
#
#  In the normalized mode, the total for the segments is divided into value of each 
#  segment to get a percentage (0 to 100%).  The segments are then plotted as stacked
#  bars using the percentage.  The complete bar will be drawn from the left to right edge of 
#  the panel.
#
#  The data structure can have between 2 to 9 values per state.
#  Each state must have the same number of values. This limitation may be removed in the future.
#
#  panelData => data.frame where each row is a state with the stateId as the row.name.
#     The columns are the bar segment values.

#

rlStateSegBar = function(j, SBnorm=FALSE) {
   #  j = the panel column number
   #  SBnorm  (FALSE = stacked,  TRUE = normalized)

   #   col1 indicates the starting or first column in the stateFrame data for bar segment values.
   #   col2 indicates the ending or last column in the stateFrame data.
   #
   #   The bar segment values are in the stateFrame for each state in columns "col1" to "col2".
   #
   wdim <- dim(dat)
   ErrFnd = FALSE
   
   if (col1[j] > wdim[2])
     { 
        warning(paste("SEGBAR-01 Specified column number in col1 for the first segment bar values is out of range.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col1[j] < 1)
     {
        warning(paste("SEGBAR-02 Specified column number in col1 for the first segment bar values is <= 0.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col2[j] > wdim[2])
      { 
         warning(paste("SEGBAR-03 Specified column number in col2 for the last segment bar values is out of range.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }
   if (col2[j] < 1)
      {
         warning(paste("SEGBAR-04 Specified column number in col2 for the last segment bar values is <= 0.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }
   
   if (col1[j] >= col2[j])
      {
         warning(paste("SEGBAR-05 The first column number of bar values cannot be => the last column number.",col1[j],col2[j]))
        ErrFnd = TRUE
      }
   wD =  ( col2[j] - col1[j] + 1 )   # corrected to calculate the number of data columns
   if (wD < 2 || wD > 9)
      {
         warning(paste("SEGBAR-06 The number of bar values for segmented/normalized bar plots must be between 2 and 9.",wD))
        ErrFnd = TRUE
      }

   if (ErrFnd) return ()    # error warning found - return
 
   workSB = dat[,c(col1[j]:col2[j])]  # bar segment values
   
   refval = lRefVals[j]              # get referrence to object, changed 
                                     #    to lRefVals - JP-2010/07/23
   reftxt = lRefTexts[j]             # new - JP-2010/07/23

   good = !is.na(rowSums(workSB))    # good values.
   
   #
   #
   # Colors - added transparency from x in steps of number of Segments up to 100%
   #   so 2 step = 50, 100
   #      3 step = 33.3, 66.6, 100
   #      4 step = 25, 50, 75, 100
   #      5 step = 20, 40, 60, 80, 100
   #      6 step = 16.6, 33.3, 50, 66,6, 83.3, 100
   #    etc.
   #    1/(NumSegs)*step = transparency
   #
   #   Dan's addition ==> 
   #    as the colors are generated from the base color
   #
   #    pInc = 1 / NumSegs
   #
   #    cSteps = cumsum(rep(pInc,NumSegs))^1.35
   #
   
   #_______________Gather stats and put in State Order______________
  
   #  Sorting has already been done - by stateId or value.
   #  The stateId list has therefore been re-ordered accordingly.  
   #  Reorder the DataList to match.  The assumption was that the input data order for the panelData 
   #  matched the order of the original data in the stateFrame.
   #
   
   workMatSB <- as.matrix(workSB)
   
   SBLen    = apply(workMatSB,1,length)  # get length of each row.
   SBLRange = range(SBLen,na.rm=TRUE)

   NumSegs  = SBLRange[2]                # number of segments (Max Length)
 
   SBBarPt <- cbind(rep(0,51),workMatSB)
   SBBarPt <- t(apply(SBBarPt,1,cumsum))
 
   #_______________Scaling____________
   
   # x scaling
 
   rMax  <- max(SBBarPt)
   if (SBnorm)
     {
       rx <- c(0,100)
     } else {
       rx <- c(0,rMax*1.02)
     }
   ry = c(0,1)
   py =  barht*c(-.5,-.5,.5,.5,NA)     #  barht = 2/3 (0.6667)
   
   # _____________ Color Patterns _______________
   
   #  Build color patterns for all bar charts
   baseColors <- t(col2rgb(colors[1:7]))
   #bgColors   <- t(col2rgb(colPanelFill))
   bgColors   <- t(col2rgb("white"))
   
   #  Old Way with Transparency
   #transDelta <- 1/NumSegs
   #vTrans <- cumsum(rep(transDelta,NumSegs))*255
   #baseColRgb <- sapply(vTrans, function (x) { rgb(baseColors[,1],baseColors[,2],baseColors[,3],x,maxColorValue=255) } )
   #  baseColRgb[colors,segment]
   
   
   #  New Way with lighter colors - but opaque 
   #pInc <- cumsum(rep(1/NumSegs,NumSegs))^1.35   # length = NumSegs
   x1 = cumsum(rep(1/NumSegs,NumSegs))
   #x2 = (( x1 - x1[1] ) ^ 1.35 ) + x1[1]
   #pInc = x2 / x2[NumSegs]
   x2 = x1 ^ 1.9
   pInc = (x2 * .6) + .4
   
   # baseColors -- base 255...
   baseCol2 <- baseColors/255
   # baseCol2[Colors,RGB]
   baseCol3 <- sapply(pInc,function(x) baseCol2 * x)  # colors(1-7),  segment(1-5) for (Rgb=RED)
                                                      # colors(8-14), segment(1-5) for (Rgb=GREEN)
                                                      # colors(15-21),segment(1-5) for (Rbg=BLUE)
   # baseCol3[(Colors-Red,Colors-Grn,Colors-Blu),Segments]
   
   baseColMod <- array(baseCol3,c(7,3,NumSegs))
                         #   [x,,]   x = color (1-7)
                         #   [,,y]   y = segment (1-5)
                         #   [,z,]   z = RGB 1=RED, 2=GREEN, 3=BLUE
                         #
                         #   [1,2,3]   1 fills first, 2 fills next, 3 fills last.
                         #  
   # baseColMod[Colors,RGB,Segments]
   # baseColSave <- baseColMod
   
   pIncM <- 1-pInc
   bgCol2 <- bgColors/255
   bgCol3 <- sapply(pIncM,function(x) bgCol2 * x)   # [rgb,segment]
   bgColMod <- t(bgCol3)                            # [segment, rgb]
   #  bgColMod[Segments,RGB]   (Segment =5 ==> 0)p
   #   NumSegs, RGB value
   
   baseColRgb <- matrix(rep(0,7*NumSegs),nrow=7,ncol=NumSegs)
   #  baseColRgb[Colors, Segment]
      
   for (isg in 1:NumSegs)  # [,,isg]   Level
      {
        for (icl in 1:7)   # colors   [icl,,]
          {
            wC <- baseColMod[icl,,isg] + bgColMod[isg,]          
            baseColRgb[icl,isg] <- rgb(wC[1],wC[2],wC[3])
        }
      }
   #
   #  Resulting colors are in baseColRgb[color,segment]
   #


   #   rows - color ID
   #   columns - segment 1:x
   
   # ____________titles and labeling axes_______________
   
   # _____________top of column______
   
   panelSelect(panels,1,j)   # top panel - add title.
   panelScale(rx,ry)
   mtext(lab1[j],side=3,line=line1,cex=cexText)
   mtext(lab2[j],side=3,line=line2,cex=cexText)
   
   atRx = panelInbounds(rx)
   axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))
   
   # ____________bottom of column____
   
   panelSelect(panels,ng,j)  # bottom panel - add sub title and refvals.
   panelScale(rx,ry)
   
   # padj in axis needed to make grid line label close
   axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))
  
   mtext(side=1,lab3[j],line=line3,cex=cexText) 

   # ___________________drawing loop_____________________

   oldpar = par(lend="butt")
 
   #  build each panel for each stacked bar set.
   
     for (i in 1:ng)
      {
        gsubs = ib[i]:ie[i]               # get beginning to end index row number in this group  
        ke = length(gsubs)                # get number of rows in group  (5 or 1)  
        # adjust if median group      
        pen = if(i==6) 6 else 1:ke        # if median group (6)(black), then pen=6, otherwise pen = c(1...x)   
        laby = ke:1 
   
        panelSelect(panels,i,j)
        panelScale(rx,c(1-pad,ke+pad)) #   1 to 5 are the y values for each bar.
        panelFill(col=colPanelFill)
 
        axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid
        
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
        
        #
        #  Not checking "good" values provided.
        #
        
        for (k in 1:ke)
         {
           # cycle through row-groups and assign colors to associated states dots.
           m = gsubs[k]
           wX = SBBarPt[m,]            # Get Row of data.
           if (SBnorm) 
             {
                wX = wX / wX[NumSegs+1] * 100   # last segment value is in NumSegs + 1 to get last column (end point)
             }
           wYP <- rep(laby[k],5)+py
      
           for (ik in 1:NumSegs)
            {
              val0 = wX[ik]     # start
              val1 = wX[ik+1]   # end position
              wXP <- c(val0,val1,val1,val0,NA)
              # good value - draw bars are polygons.  (why to polygon)
              polygon(wXP,wYP,col=baseColRgb[pen[k],ik],lwd=lwdSBarOutline,border=colSBarOutline) 
              #polygon(wXP,wYP,col=colBarOutline,density=0)
            }
       
         }   
        panelOutline(colOutline)
 
      }

  
  par(oldpar)
  # ____________________________PanelOutline____________________

  groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
         rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}


####
#
# type = 'ctrbar'  ====================================
#
#  rlStateCtrBar   (Centered Bar chart)
#
#  The centered bars is a stacked bar chart with the middle segment centered on the "0" value
#  of the chart and extending 1/2 it's value in both directions (plus and minus).
#  The other bar segments are plotted to it's left and right as appropriate.
#
#
#  The data structure can have between 2 to 9 data values per state.
#  Each state must have the same number of values. This limitation may be removed in the future.
#
#  panelData => data.frame where each row is a state with the stateId as the row.name.
#     The columns are the bar segment values.

#

rlStateCtrBar = function(j) {
   #  j = the panel column number
   #  
   #   col1 and col2 indentify the starting column and ending column number in the stateFrame
   #   that contains the bar values for each state.
   #
   wdim <- dim(dat)
   ErrFnd = FALSE
   
   if (col1[j] > wdim[2])
     { 
        warning(paste("CTRBAR-01 Specified column number in col1 for the first segment bar values is out of range.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col1[j] < 1)
     {
        warning(paste("CTRBAR-02 Specified column number in col1 for the first segment bar values is <= 0.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col2[j] > wdim[2])
      { 
         warning(paste("CTRBAR-03 Specified column number in col2 for the last segment bar values is out of range.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }
   if (col2[j] < 1)
      {
         warning(paste("CTRBAR-04 Specified column number in col2 for the last segment bar values is <= 0.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }
  
   if (col1[j] >= col2[j])
      {
         warning(paste("CTRBAR-05 The first column number of bar values cannot be => the last column number.",col1[j],col2[j]))
        ErrFnd = TRUE
      }
   wD = ( col2[j] - col1[j] + 1 )  # corrected to properly calculate the number of data columns.
   if (wD < 2 || wD > 9)
      {
         warning(paste("CTRBAR-06 The number of bar values for centered bar plots must be between 2 and 9.",wD))
        ErrFnd = TRUE
      }

   if (ErrFnd) return ()

   workCB = dat[,c(col1[j]:col2[j])]  # get bar segment data from the stateFrame.
   
   refval = lRefVals[j]              # get referrence to object, changed 
                                     #    to lRefVals - JP-2010/07/23
   reftxt = lRefTexts[j]             # new - JP-2010/07/23

   #
   # Colors - series of lighter colors of the base colors for each bar.
   #   Use an adjusted list of percentages based on the Number of Segments.
   #      2 step = 50, 100
   #      3 step = 33.3, 66.6, 100
   #      4 step = 25, 50, 75, 100
   #      5 step = 20, 40, 60, 80, 100
   #      6 step = 16.6, 33.3, 50, 66,6, 83.3, 100
   #    etc.
   #    1/(NumSegs)*step = transparency or lightness level  (100% full)
   
   #_______________Gather stats and put in State Order______________
  
   #  Sorting has already been done - by stateId or value.
   #  The stateId list has therefore been re-ordered accordingly.  
   #  Reorder the DataList to match.  The assumption was that the input data order for the panelData 
   #  matched the order of the original data in the stateFrame.
   #
   
   workMatCB = as.matrix(workCB)

   CBLen    = apply(workMatCB,1,length)  # get length of each row.
   CBLRange = range(CBLen,na.rm=TRUE)

   NumSegs  = CBLRange[2]                # number of segments
   CtrSeg   = as.integer(NumSegs/2) + 1  # center segment

   CBBarPt  = cbind(rep(0,51),workMatCB)
   CBBarPt  = t(apply(CBBarPt,1,cumsum))

   if ((NumSegs %% 2) != 0)
     {   # old number of segments
        CtrPt = workMatCB[,CtrSeg]/2 + CBBarPt[,CtrSeg]
    
     } else {
         # even number of segments
  
        CtrPt = CBBarPt[,CtrSeg]
     }

   CBPlotPts = CBBarPt - CtrPt

   #_______________Scaling____________
   
   # x scaling
   
   rx = range(CBPlotPts,na.rm=TRUE)
   rx = sc*diff(rx)*c(-.5,.5)+mean(rx)
 
   ry = c(0,1)
   py =  barht*c(-.5,-.5,.5,.5,NA)           #  barht = 2/3 (0.6667)
   
   # _____________ Color Patterns _______________
   
   #  Build color patterns for all bar charts
   baseColors <- t(col2rgb(colors[1:7]))
   #bgColors   <- t(col2rgb(colPanelFill))
   bgColors   <- t(col2rgb("white"))
   
   #  Old Way with Transparency
   #transDelta <- 1/NumSegs
   #vTrans <- cumsum(rep(transDelta,NumSegs))*255
   #baseColRgb <- sapply(vTrans, function (x) { rgb(baseColors[,1],baseColors[,2],baseColors[,3],x,maxColorValue=255) } )
   #  baseColRgb[colors,segment]
   
   
   #  New Way with lighter colors - but opaque 
   #pInc <- cumsum(rep(1/NumSegs,NumSegs))^1.35   # length = NumSegs
   x1 = cumsum(rep(1/NumSegs,NumSegs))
   #x2 = (( x1 - x1[1] ) ^ 1.35 ) + x1[1]
   #pInc = x2 / x2[NumSegs]
   x2 = x1 ^ 1.9
   pInc = (x2 * .6) + .4

   # baseColors -- base 255...
   baseCol2 <- baseColors/255
   # baseCol2[Colors,RGB]
   baseCol3 <- sapply(pInc,function(x) baseCol2 * x)  # colors(1-7),  segment(1-5) for (Rgb=RED)
                                                      # colors(8-14), segment(1-5) for (Rgb=GREEN)
                                                      # colors(15-21),segment(1-5) for (Rbg=BLUE)
   # baseCol3[(Colors-Red,Colors-Grn,Colors-Blu),Segments]
   
   baseColMod <- array(baseCol3,c(7,3,NumSegs))
                         #   [x,,]   x = color (1-7)
                         #   [,,y]   y = segment (1-5)
                         #   [,z,]   z = RGB 1=RED, 2=GREEN, 3=BLUE
                         #
                         #   [1,2,3]   1 fills first, 2 fills next, 3 fills last.
                         #  
   # baseColMod[Colors,RGB,Segments]
   # baseColSave <- baseColMod  (???)
   
   pIncM <- 1-pInc
   bgCol2 <- bgColors/255
   bgCol3 <- sapply(pIncM,function(x) bgCol2 * x)   # [rgb,segment]
   bgColMod <- t(bgCol3)                            # [segment, rgb]
   #  bgColMod[Segments,RGB]   (Segment =5 ==> 0)p
   #   NumSegs, RGB value
   
   baseColRgb <- matrix(rep(0,7*NumSegs),nrow=7,ncol=NumSegs)
   #  baseColRgb[Colors, Segment]
      
   for (isg in 1:NumSegs)  # [,,isg]   Level
      {
        for (icl in 1:7)   # colors   [icl,,]
          {
            wC <- baseColMod[icl,,isg] + bgColMod[isg,]          
            baseColRgb[icl,isg] <- rgb(wC[1],wC[2],wC[3])
        }
      }
   #
   #  Resulting colors are in baseColRgb[color,segment]
   #
   #  Now I have a matrix of colors - [x,y] where
   #   x is the color base - 1 to 7 (we use 1 to 6).
   #   y is the level based on the number of segments = 1 : NumSegs
   #
   
   # ____________titles and labeling axes_______________
   
   # _____________top of column______
   
   panelSelect(panels,1,j)   # top panel - add title.
   panelScale(rx,ry)
   
   mtext(lab1[j],side=3,line=line1,cex=cexText)
   mtext(lab2[j],side=3,line=line2,cex=cexText)
 
   atRx = panelInbounds(rx)
   atRxAbs = abs(atRx)
   #
   # Special Note - for the centered segmented bars, the values to the left and right are offsets
   #   and listed as positive -  NOT negative to left and positive to right.
   #
   axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRxAbs))
   
   # ____________bottom of column____
   
   panelSelect(panels,ng,j)  # bottom panel - add sub title and refvals.
   panelScale(rx,ry)
   
   # padj in axis needed to make grid line label close
   axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRxAbs))
  
   mtext(side=1,lab3[j],line=line3,cex=cexText) 

   # ___________________drawing loop_____________________

   oldpar = par(lend="butt")
 
   #  build each panel for each stacked bar set.
   
     for (i in 1:ng)
      {
        gsubs = ib[i]:ie[i]               # get beginning to end index row number in this group  
        ke = length(gsubs)                # get number of rows in group  (5 or 1)  
        # adjust if median group      
        pen = if(i==6) 6 else 1:ke        # if median group (6)(black), then pen=6, otherwise pen = c(1...x)   
        laby = ke:1 
   
        panelSelect(panels,i,j)
        panelScale(rx,c(1-pad,ke+pad)) #   1 to 5 are the y values for each bar.
        panelFill(col=colPanelFill)
 
        axis(side=1,at=atRx, tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid
        
     # if a refval is provided then add line.
     if(!is.na(refval))
        {
          lines(rep(refval,2),c(1-padMinus,ke+padMinus),lty=ltyRefVal,lwd=lwdRefVal,col=icolRefVal)
        }
        
        
        for (k in 1:ke)
         {
           # cycle through row-groups and assign colors to associated states dots.
           m = gsubs[k]
           wX = CBPlotPts[m,]            # Get Row of data.
      
           #xcolLine = colors[pen[k]]
           #xcolFill = colors[pen[k]+7]
           wYP <- rep(laby[k],5)+py
      
           for (ik in 1:NumSegs)
            {
              val0 = wX[ik]     # start
              val1 = wX[ik+1]   # end position
              wXP <- c(val0,val1,val1,val0,NA)
              # good value - draw bars are polygons.  (why to polygon)
              
              polygon(wXP,wYP,col=baseColRgb[pen[k],ik],lwd=lwdSBarOutline,border=colSBarOutline) 
              #polygon(wXP,wYP,col=colBarOutline,density=0)

            }
       
         }   
       # draw vertical line at zero.
       lines(rep(0,2),c(1-padMinus,ke+padMinus),
               lty=ltyBarZero,lwd=lwdBarZero,col=colBarZero)
       panelOutline(colOutline)
 
      }

  
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
         rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

   
}

#####
#
# type = 'ScatDot'   =====================================================
#
# rlStateScatDot  (Scattered Plot Dots)
#

rlStateScatDot = function(j){
  #
  #  j = panel column number
  #
  #  col1 and col2 point to the X and Y data values in the stateFrame data.frame (known here as "dat").
  # 
  #
  wdim <- dim(dat)
   ErrFnd = FALSE
   if (col1[j] > wdim[2])
     { 
        warning(paste("SCATDOT-01 Specified column number in col1 for X values is out of range.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col1[j] < 1)
     {
        warning(paste("SCATDOT-02 Specified column number in col1 for X values is <= 0.",col1[j],"Not in stateFrame."))
        ErrFnd = TRUE
     }
   if (col2[j] > wdim[2])
      { 
         warning(paste("SCATDOT-03 Specified column number in col2 for Y values is out of range.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }
   if (col2[j] < 1)
      {
         warning(paste("SCATDOT-04 Specified column number in col2 for Y values is <= 0.",col2[j],"Not in stateFrame."))
        ErrFnd = TRUE
      }

   if (ErrFnd) return ()
  
   workSCD = dat[,c(col1[j],col2[j])]     # get x and y data from the stateFrame.
   colnames(workSCD) <- c("x","y")
   
   refval = lRefVals[j]              # get referrence to object, changed 
                                     #    to lRefVals - JP-2010/07/23
   reftxt = lRefTexts[j]             # new - JP-2010/07/23

   #_______________Gather stats and put in State Order______________
  
   #  Sorting has already been done of the stateFrame (dat) by stateId or value 
   #     in the function startup.
   
   #_______________Scaling____________
   
   # x scaling
   rx = range(workSCD$x,na.rm=TRUE)       # range of X values
   rx = xscSCD*diff(rx)*c(-.5,.5)+mean(rx)     # min to max range with expansion factors.
   
   # y scaling                  
   ry = range(workSCD$y,na.rm=TRUE)       # range of Y values
   ry = yscSCD*diff(ry)*c(-.5,.5)+mean(ry)
   
   # ____________titles and labeling axes_______________

 
   # _____________top of column______
   
   panelSelect(panels,1,j)   # top panel - add title - above.
   panelScale(rx,ry)         # scaled for data.
   
   mtext(lab1[j],side=3,line=line1,cex=cexText)
   mtext(lab2[j],side=3,line=line2,cex=cexText)
 
   atRx = panelInbounds(rx)
   axis(side=3,mgp=mgpTop,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))

   # ____________bottom of column____
   
   panelSelect(panels,ng,j)  # bottom panel - add sub title and refvals.
   panelScale(rx,ry)
   
   # padj in axis needed to make grid line label close
   axis(side=1,mgp=mgpBottom,padj=padjBottom,tick=F,cex.axis=cexText,at=atRx,labels=as.character(atRx))
   
   mtext(side=1,lab3[j],line=line3,cex=cexText) 

   # ___________________drawing loop_____________________

   # set the symbols for the dots and median.
   workSCD$bg     <- ""
   workSCD$cex     <- 0.75
   workSCD$pch     <- 21            # filled circle - default shape
   workSCD$pch[26] <- medPchSCD     # filled triangle for median dot.
   
         # in the ordered list, the median should be 26 of 51 items.
     
   oldpar = par(lend="butt")

   #  build each panel for scatter plot dots
   
   for (i in 1:ng)   # groups from 1 to 5, 6, 7 to 11   ##  6 is the median group.
    {
      # Cycle through the Row/Groups in the micromap column

      # Set defaults for dots for this panel
     
      workSCD$bg <- NA                  # default color   - was bgFillSCD
      workSCD$cex <- .75                # default size, except median.
      workSCD$bg[26] = medColSCD
      workSCD$cex[26] <- 1
       
      gsubs = ib[i]:ie[i]               # get beginning to end index row number in this group  
      ke = length(gsubs)                # get number of rows in group  (5 or 1)  

      # adjust if median group      
      pen = if(i==6) 6 else 1:ke        # if median group (6)(black), then pen=6, otherwise pen = c(1...x)   
      
      panelSelect(panels,i,j)           # select panel for group i in column j)
      panelScale(rx,ry)                 # set scale for panel  (should this be ry * 5 or 1?)
      panelFill(col=colPanelFill)            # set fill for panel
      
      # vertical grid lines.
      axis(side=1,tck=1,labels=F,col=colGrid,lwd=lwdGrid) # grid lines
      # y axis labels
   
      if (i==6)  # median panel
        {
          atRy = c(saveAtRy[1],saveAtRy[length(saveAtRy)])   
        } else {
      
          atRy = panelInbounds(ry)
        }
      if (hGridSCD)
        {
           axis(side=2,tck=1,labels=F,col=colGrid,lwd=lwdGrid, at=atRy) # Grid lines
        }
      axis(side=2,tick=F,cex.axis=axisCexTs*.75,mgp=mgpLeft,at=atRy,labels=as.character(atRy))
      mtext(lab4[j],side=2,line=line5,cex=axisCexTs)
      
      panelOutline(col=colOutline)     # outline panel

      # dv <- c(gsubs[1:ke],26)
      
      for (k in 1:ke)                  # Process each slot of panel - step 1 to 5 or 1 to 1
        {
          # cycle through row-groups and assign colors to associated state's dots.
          m = gsubs[k]
          workSCD$bg[m] <- colors[pen[k]]       # set approvate color to circle fill.
          workSCD$cex[m] <- 1
        }
      if (ke == 1)                     # median line
        {
          wS <- workSCD[gsubs[1],]      # get one entry - the median.
        } else {
          wS <- workSCD[order(workSCD$cex,decreasing=FALSE),]
          # plot all points by size, others first, colored and median last.   
        
        }
       points(wS$x,wS$y,pch=wS$pch, col="black",bg=wS$bg, cex=wS$cex, lwd=0.6)  # removed 
       saveAtRy <- atRy  # save for possible use on median panel.
    }
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

  if(!is.na(refval)) 
         rlStateRefText(j,reftxt)  # added reftxt field - JP-2010/07/23

}

############################################


####
#
# ============================================================
# rlStateRefText  function inserts text lines at bottom of column as
#   legend.   Reference line has already been placed in each panel.
#
#   Changed-10/05/12- legend line overprints the texts.
# 

rlStateRefText= function(j,reftext){
   par(xpd=T)
   panelSelect(panels,ng,j)  # select one beyond the last panel (legend)
   panelScale()
   
   if (!is.null(reftext))
       if (!is.na(reftext)) {
   
          xt = (strwidth(reftext,cex=cexText) + .2)/2  # length of line and text
          xl = par("pin")[2]/2  # x axis
          xp = xl - xt
          # graphic space is 0 to 1 from left to right.
          # reference line is from 0.24->.37 and 0.63->.76 (x) at line4 = y; 
          #   text inserted from .37->,63 (???)
        
          # way to find graphic length of string --> sw <- strwidth(reftext,cex=cexText)

          # print reference line for legend.
          #lines(c(.24,.37,NA,.63,.76),rep(line4,5),
          #      lty=ltyRefVal,col=icolRefVal,lwd=lwdRefVal)
          #  10/10/12-changed above to print section of line on left of reference legend label.
          
          #text(.50,line4+.01,reftext,cex=cexText,col=icolRefTxt,adj=.5)
          #  10/10/12-changed above to print reference label to right of dash line. Not centered.   
      
          # add text definition for legend.   (5/21/13 - added color to line)
    
          lines(c(xp, xp+.15),rep(line4,2), lty=ltyRefVal, lwd=lwdRefVal, col=icolRefVal)
          text(xp + .2,line4+.01,reftext,cex=cexText,col=icolRefTxt,adj=0)
       }
   # later add code to center line and text dependent on length of text (pixels)
}

#### end of micromap functions

#####  end of functions  #####

#####
#
# Bring out parameter==============================================
#
# JP - copy global variables setup from micromapST.R base into 
# the local variables this function expects.
#
   rlStateNamesFips = stateNamesFips
   rlStateVisBorders = stateVisBorders
   rlStateNationVisBorders = stateNationVisBorders

   assign('lab1',lab1)
   assign('lab2',lab2)
   assign('lab3',lab3)
   assign('lab4',lab4)

# Save panelDesc for function use Set

# number of columns based on the presence of Descriptions for Column

   if(!is.null(panelDesc$col1))
      assign("col1",panelDesc$col1)
   if(!is.null(panelDesc$col2))
      assign("col2",panelDesc$col2)
   if(!is.null(panelDesc$col3))
      assign("col3",panelDesc$col3)

   if(is.null(panelDesc$refVals))
      assign('lRefVals',rep(NA,nrow(panelDesc))) else
      assign('lRefVals',panelDesc$refVals)

   if(is.null(panelDesc$refTexts))
      assign('lRefTexts',rep(NA,nrow(panelDesc))) else
      assign('lRefTexts',panelDesc$refTexts)
      
# Check to see if columns are available for
# the type of plot and values are plausible

   if(!is.null(panelDesc$panelData)) 
       {
         wPanelData = panelDesc#panelData
       } else {
         wPanelData = NA
       }
   assign('panelData',wPanelData)
   rm(wPanelData)
       
# Get state abbreviation as polygon link
   #  

   fullNames = row.names(rlStateNamesFips)    # List of full state names.

   curnam = row.names(SFrame)                 # Get list of current names in row.names.
            # get proper capitalization of state ab or full names.
   curnam2 = as.vector(sapply(curnam,function(x) simpleCap(x)))
   
   #  Compare against common "DC" names and replace with "D.C."
   DCnames = c("Washington, D. C.",   "Washington D. C.",
               "Washington, D C",   "Washington D C",
               "Washington, Dc",    "Washington Dc",
               "District Columbia", "District Of Columbia",
               "DC","Dc","D C","D. C.")
   curnam2[!is.na(match(curnam2,DCnames))] = "D.C."            

   #  
   stateId = switch(rowNames,
      # if "ab", use current name
      "ab"=  rlStateNamesFips$ab[match(toupper(curnam),
                                  rlStateNamesFips$ab)],
      # if "fips", convert to abrv name      
      "fips"= rlStateNamesFips$ab[match(as.integer(curnam),
                                  rlStateNamesFips$fips)],
      # if "full" state name, convert abrv name
      "full"= rlStateNamesFips$ab[match(curnam2,fullNames)],
      #  No match..
      warning("check rownames type")
   )

   if (any(is.na(stateId)))
     {  # one of the state abrv or full names are not valid
       BadList = paste(curnam[is.na(stateId)],collapse=" ")  # create a list of bad names.
       stop(paste("The following row names in the stateFrame data.frame are invalid: ",BadList,sep=""))
     }

# Get statenames or abbreviations to plot_______________________
   stateNames = switch(plotNames,
          "ab"=stateId,
          "full"= fullNames[match(stateId,rlStateNamesFips$ab)],
          warning("check plotNames type")
   )

# sort and store stateFrame, stateid, and stateNames____________
   if(is.null(sortVar) || is.character(sortVar))
      ord = order(stateNames) else             # if field omitted (null) or a character, sort by state name
      ord = order(SFrame[,sortVar])            # if field a numeric and present, sort by specified SFrame column.
  
   if(!ascend)ord = rev(ord)


   assign("dat",SFrame[ord,])                       # data fields    "dat" has sorted data frame of the stateFrame
   assign("stateId",stateId[ord])                   # StateID        "stateId" in order of the dat
   assign("stateNames",stateNames[ord])             # StateNames
   assign("datOrder",ord)                           # data order for use with panelData.
   
# ________________Detail defaults_______________________________

   assign("colors",colors)
   
   #
   # dynamic assignment of detail data.frame to individual variables in the 
   #  "micromapST' namespace..
   #
   nam = names(details)                            # parse th details list into variable that can be
   for (i in 1:length(details)){                   #  referenced using the list's name.
      assign(nam[i],details[[i]])
   }
   
   
   # 
   # This is the code the rcmd check could not detect the scope of the detail$ variables.
   #


   dy = details[["dy"]]                      # for debugging
   cexTitle = details[["cexTitle"]]          # Used in this function

#  
#  Make adjustments for color or grays
#

if (colFull)
  {  
    # set color values to work variables
    icolRefVal = colRefVal
    icolRefTxt = colRefTxt
    
  } else {
  
    # set gray values to work variables
    icolRefVal = colGRefVal
    icolRefTxt = colGRefTxt
  }
  
# __________________________layout

  cparm <- data.frame(cSize=numeric(0),lSep=numeric(0),rSep=numeric(0))
  
  ncol = length(type)
  
  for (j in 1:ncol)
    {
      # Test type of column to be built and call build routine.
    cparm2 =  switch(type[j],
                  #  colSize, col left sep, col right sep
         "map"=      c(1.4,0,0),
         "mapcum"=   c(1.4,0,0),
         "maptail"=  c(1.4,0,0),
         "mapmedian"=c(1.4,0,0),
         "id"=       c(0,0,0),
         "dot"=      c(0,0,0),
         "dotse"=    c(0,0,0),
         "dotconf"=  c(0,0,0),
         "arrow"=    c(0,0,0),
         "bar"=      c(0,0,0),
         "segbar" =  c(0,0,0),
         "boxplot"=  c(0,0,0),
         "ts" =      c(0,.2,0),
         "tsconf" =  c(0,.2,0),
         "scatdot" = c(0,.2,0),
         "segbar"  = c(0,0,0),
         "normbar" = c(0,0,0),
         "ctrbar"  = c(0,0,0),
         "nomatch" = c(0,0,0)
      )
     cparm <- rbind(cparm,cparm2)
    }
   colnames(cparm) <- c("cSize","lSep","rSep")

   topMar = details[["topMar"]]
   botMar = details[["botMar"]]
   legfactor=1

   # add space if reference values provided.
   # JP-2010/07/23 0 change to refVals to be consistent.
   if(!is.null(panelDesc$refVals)){
      # if not null field.
      if(any(!is.na(panelDesc$refVals))){
         # value provided - not NA
         botMar = details[["botMarLegend"]]
         # revisit calculation below to be more precise
         legfactor= 9/(9-details[['botMardif']])
      }      
   }
   assign('legfactor',legfactor,sys.frame(which = -1))  # set legfactor in environment -1 (caller's space.)

   ncol   = length(type)
   
   ladj    = c(0,cparm[2:ncol,2],0)
   radj    = c(0,cparm[1:ncol-1,3],0)
   
   colSep = c(0,rep(.1,ncol-1),0)  + ladj + radj  # column separators = 0.1 
                                        # in 4 columns minimum -> c(0,.1,.1,.1,0) Side don't get sep.
                                        
   
   # build panels from panelLayout and pieces of rlStateDefaults$Details
       # nrow = 11 -> 5,5,5,5,5,1,5,5,5,5,5 states = 11 groups
   # individual panels (rows(11) and columns)
   assign("panels",panelLayout(nrow=11,ncol=ncol,
                        topMargin=topMar,                   # 0.95
                        leftMargin=0,                      
                        bottomMargin=botMar,                # 0.5
                        rowSep=details[["rowSep"]],         # c(0,0,0,0,0,.1,.1,0,0,0,0,0)
                        rowSize=details[["rowSize"]],       # c(7,7,7,7,7,1.65,7,7,7,7,7)
                        colSize=colSize,                    # calculated colsizes (???)
                        colSep=colSep))                     # c(.1,.1,.1) for 3

   grounpedRowSize = details[["groupedRowSize"]]            # c(35,1.65,35)
   groupedRowSep   = details[["groupedRowSep"]]             # c(0,.1,.1,0)

   # Major panel group  title-top, panels, title-bottom  by columns (overlays panels)
   # section of panels (top(25), median(1), bottom(25) and "N" columns wide.
   assign("panelGroup",panelLayout(nrow=3,ncol=ncol,
                        topMargin=topMar,
                        leftMargin=0,
                        bottomMargin=botMar,
                        rowSize=groupedRowSize,
                        rowSep=groupedRowSep,
                        colSize=colSize,
                        colSep=colSep))

   # Page panel - top, middle, bottom - 1 across.                   (overlays panels)
   #   One column wide for each group (top, median, bottom)
   assign("panelOne",panelLayout(nrow=3,ncol=1,
                        topMargin=topMar,
                        leftMargin=0,
                        bottomMargin=botMar,
                        rowSize=groupedRowSize,
                        rowSep=groupedRowSep))

#####
# ____________________Main loop______________________________
#####

#  Build images of each column

   for (j in 1:ncol)
    {
      # Test type of column to be built and call build routine.
      switch(type[j],
         "map"=      rlStateMap(j),
         "mapcum"=   rlStateMapCum(j),
         "maptail"=  rlStateMapTail(j),
         "mapmedian"=rlStateMapMedian(j),
         "id"=       rlStateId(j),
         "dot"=      rlStateDot(j),
         "dotse"=    rlStateDotSe(j),
         "dotconf"=  rlStateDotConf(j),
         "arrow"=    rlStateArrow(j),
         "bar"=      rlStateBar(j),
         "boxplot"=  rlStateBoxplot(j,  as.character(panelDesc$panelData[j]) ),
         "ts" =      rlStateTSConf(j,   as.character(panelDesc$panelData[j]),conf=FALSE),
         "tsconf" =  rlStateTSConf(j,   as.character(panelDesc$panelData[j]),conf=TRUE),
         "scatdot" = rlStateScatDot(j),
         "segbar"  = rlStateSegBar(j),
         "normbar" = rlStateSegBar(j,SBnorm=TRUE),
         "ctrbar"  = rlStateCtrBar(j),
         "nomatch"
      )
    }

 
      # All columns are built and sitting in the panel.
      panelSelect(panelOne,margin="top")    # full page top label area.
      panelScale()
 
      if(length(title)==1){
         text(.5,.77,title,cex=cexTitle)
      } else {
         text(.5, .9,title[1],cex=cexTitle)
         text(.5,.65,title[2],cex=cexTitle)
   }
} # end of micromapST Function

###  End of micromapST

###
#
#  micromapSTSetDefaults function
#
#  Must be run once to generate the default lists.
#  If you customize - then make a copy and change the copy.
#
#  Call by .onload at package load.  Reference is exported to globlal space for user's access.
#
###


micromapSTSetDefaults = function()
   {
      return(micromapSTDefaults)
   }

#
#  build micromapSTDefaults data.frame so it can be exported.
#

# Candidate colors________________________________________
colorsRefRgb = matrix(c(
 1.00,1.00,1.00,  # white
  .92, .92, .92,  # lighter gray            # changed from .90
  .78, .78, .78,  # light gray              # changed from .80
  .50, .50, .50,  # middle gray
  .30, .30, .30,  # dark gray
  .00, .00, .00,  # black
 
  .93,1.00, .93,  # light green
  .00, .50, .00,  # mid green
 1.00,1.00, .84,  # light yellow foreground  
  .90, .80,1.00,  # bright yellow foreground 
  .80, .90,1.00,  # light green blue
  .60, .70, .85), # mid green blue
  ncol=3,byrow=TRUE)

colorsRef = grDevices::rgb(colorsRefRgb[,1],colorsRefRgb[,2],colorsRefRgb[,3])
names(colorsRef) = c("white","lighter gray","light gray",
                     "mid gray","dark gray", "black",

                     "light green","mid green",
                     "light yellow","bright yellow",
                     "light green blue","mid green blue")           

# Region colors________________________________________________
colorsRgb = matrix(c(
 1.00, .15, .15,  #region 1: red
  .90, .55, .00,  #region 2: orange
  .00, .65, .00,  #region 3: green
  .20, .50,1.00,  #region 4: greenish blue
  .50, .20, .70,  #region 5: lavendar 
  .00, .00, .00,  #region 6: black for median
 1.00,1.00, .80), #non-highlighted foreground
  ncol=3,byrow=TRUE)

colors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3]),
            grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],.2))
names(colors) =c("red","orange","green","greenish blue", "purple","black","light yellow",
                 "l_red","l_orange","l_green","l_greenish blue", "l_purple","l_black","l_light yellow")       

## JP added temp variables so function would read in in R 2.7
#      cannot use values within the details list since it's not really built yet.

tempne      = 5                           # number of states per panel
tempcolGrid = colorsRef["white"]          # grid line color

tempcolFill = colorsRef["lighter gray"]   # panel and default fill color
tempCexText = .7

details = list(

# panel layout grouping 
    ne = tempne,                   # number of item per group
    ng = ceiling(51/tempne),       # number of groups of states 
    ib =  c(1, 6,11,16,21,26,27,32,37,42,47), #group lower index
    ie =  c(5,10,15,20,25,26,31,36,41,46,51), #group upper index

# panel layout margin allocation
    # JP - changed median row size to 1.5.
    topMar       = .95,       # margin panel height (inches)
    botMar       = .5,        # no legend bottom margin
    botMarLegend = .5,
    botMardif    = .2,        # maybe not needed
    leftMarAxis  = .2,        # left margin adjustment when Y axis is printed.
    #                1 2 3 4 5   6   7 8 9 10 11
    rowSep       = c(0,0,0,0,0,.1,.1,0,0,0,0,0),
    #                1 2 3 4 5 6 7 8 9 10 11
    rowSize      = c(7,7,7,7,7,1.65,7,7,7,7,7),  # JP change 1.5 to 1.65 on Median strip.
    #                 1-5 6 7-11
    groupedRowSize = c(35,1.65,35),             # JP changed 1.5 to 1.65 to give median a little more room 
    #               1-5 5-6 6-7 7-11  
    groupedRowSep  = c(0,.1,.1,0),

# panel column width allocation
             ## JP changed map width to 1.4
    mapWidth    = 1.4,        # map width should be set portionally to the height of the panel
    idWidth     = c(.9,.30),  # full and ab
   
# panel scaling
    sc       = 1.08,             # x and y axis scale expansion factor
    pad      = .67,              # y axis padding for integer plotting locates
                                 # ry = c(1-pad,ke+pad),ke = no. items in panel
    padex    = .34,              # total panel padding
                                 # (.67-.5)=.17 padding at top and bottom of panel
    padMinus = .63,              # .67 - .04 # keep reference line off panel edge

# mtext line placement (Titles)
    ##  JP adjusted placement of lines (titles)
    line1        = 1.75,         # top panel 1st line placement    
    line2        = 1.05,         # top panel 2nd line placement
    lineTiclab   =.2,            # lowest line for map legend text 
    line3        = .65,          # bottom panel line placement
    line4        = -.7,          # reference line (below panel)
    line5        = .40,          # Y axis titles for ScatDot and TS.

# grid line parameters
    colGrid      = tempcolGrid,  # grid line color
    lwdGrid      = 1,            # weight of grid line
    mgpTop       = c(2,.1,0),    # gridline (tick) placement
    mgpBottom    = c(2,0,0),     # gridline (tick) placement
    padjBottom   = -.7,          # gridline (tick  placement
    mgpLeft      = c(.75,0.1,0),   # left axis labels.

# panels
    colPanelFill  = tempcolFill, # panel fill color
    colOutline    = "black",     # panel outline color

# Title and Text - cex for character size
    cexText       = tempCexText, ## JP decreased text size.  Used almost everywhere.
    cexTitle      = 1.0,

# refVals parameters

    # see padMinus above for other parameters 
    ltyRefVal     = "dashed",    # dash line
    lwdRefVal     = 1.5,
    colRefVal     = colorsRef["mid green"],
    colGRefVal    = colorsRef["black"],

# refText parameters
    colRefTxt     = colorsRef["black"],   # JP 10/10/12-changed from black to mid green.
                                          #  5/21/13 - changed back to black.
    colGRefTxt    = colorsRef["black"],

#__________________________________________________________ 
# working parameters for each panel graphing subfunction within micromapST

# arrow plot parameters
    lengthArrow   = .08,
    lwdArrow      = 2.5,                  ## JP decrease arrow width.
    lwdArrowShadow = 4.0,                 # Arrows shadow when border needed.
    colArrowShadow = colorsRef["black"],  # Not Used.
    cexArrow      = .08,                  # Not Used

# bar plot parameters
    barht         = 2/3,           # fraction of line spacing
    colBarOutline = colorsRef["black"],
    lwdBarOutline = .5,
    
    colBarZero    = colorsRef["white"],
    lwdBarZero    = 1,
    ltyBarZero    = "dotted",

# segmented bar parameters
    colSBarOutline = colorsRef["black"],
    lwdSBarOutline = .75,
    
# box plot parameters
    thinBox       =.2,     # was .29     ## JP decreased line width
    thickBox      =.60,    # was .58
   
    useBlack      = FALSE, # FALSE = Use the Color for outliners;  TRUE = use black
  
    medianLine    = .88,
    colBpDotMedian= colorsRef["white"],      ## JP changed from colDotMedian for clarity

    pchMedian     = 19,
    cexMedian     =.95, 
    lwdMedian     = 2,
    colBpMedian   = colorsRef["black"],     ## JP changed to colBpMedian from colMedian - was duplicate - set to black.
    colBpOutline  = colorsRef["dark gray"], 
  
    lwdOutlier    = .4,               ## JP decreased dot line width
    cexOutlier    = .7,               # see cexDot  ## JP decreased dot size  (was .6)
    colBpGreyOutliner = colorsRef["dark gray"], 
  
# id State Dot parameters
    idDotPch      = 21,               # rlStateIdDot 

# dot plot parameters

    pchDot        = 21,              # plotting character  (1 circle, 16 dot, 21 filled circle)
    cexDot        = .9,              # dot size            ## JP adjusted dot size.

    conf          = 95,              # % confidence interval
    lwdConf       = 2,
    cexConf       = .55,             # Not Used

    OutlineDot    = FALSE,           ## JP added option to control Dot outline.
    colDotOutline = colorsRef["black"],
    lwdDotOutline = .5,
 
# ts and tsconf parameters
    lwdTs         = 1.1,             # TS Line weight
    axisCexTs     = tempCexText * .7,
    hGridTs       = FALSE,
    
# scatdot parameters
    lwdSCD        = .4,              # Scat Dot symbol border line size.
    bgFillSCD     = tempcolFill,     # not selected symbol fill (bg) color
    medPchSCD     = 21,              # median symbol PCH value (21 = filled circle)
    medColSCD     = colorsRef["black"],  # color for median symbol fill.
    axisCexSCD    = tempCexText * .7,
    sizePchSCD    = tempCexText * .7,
    xscSCD        = 1.08,            # fudge for margins to try and not clip circles.
    yscSCD        = 1.12,             # fudge for margins to try and not clip circles.
    hGridSCD      = FALSE, 

# id link panel (state lab and dot)
    colIdOutline  = colorsRef["dark gray"],
    cexId         = .9,        ## JP decreased ID text size.
    cexIdDot      = .6,        # Not Used.
    idty          = 1/3,       # subtract to lower text baseline for state names.

# map parameters
    colMapBackgr  = tempcolFill,        # map/state background fill color
    colLineBackgr = tempcolGrid,
    colLineForegr = colorsRef["black"], 
    colLineNation = colorsRef["black"],
    cexState      = .32,                    # label size for AK, HI, DC in top map.  
    lwdBackGr     = 1, 
    lwdForeGr     = 1,
    lwdNation     = 1

  )

#
#  Set up variable in the micromapST namespace - used by micromapST and micromapSTSetDefaults functions.
#

micromapSTDefaults = list(colors=colors,details=details)

####
#
#   .onLoad function - executed when the package is loaded initially.
#      builds a non-changeable micromapSTDefault data.frame for use
#      as the default when colors and/or details are not specified.
#
#    Added by JP - Oct, 2012 - Setup permanent micromapSTDefault data.frame for 
#          use as the default values on the call.
#
####

.onLoad = function (libraryName, pkgName)

   { 
     #packageStartupMessage(".onLoad")
     #packageStartupMessage(libraryName)
     #packageStartupMessage(pkgName)
     # generate default data.frame for micromapST.
     rlMicromapSTDefaults <- micromapSTSetDefaults()
     micromapSTDefaults <<- rlMicromapSTDefaults
  
    }

#  
####  
#
# End of load and variable initialization
#
####

