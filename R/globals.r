#
# Create key global variable before referenced - These variables are referencable by all subroutines and functions 
# in this package.
#

utils::globalVariables(c( 
          # Call Parameters
                "sDFName",            "pDName",
                "wSFName",
                "callVarList",
                
          # Working Variables
                "areaDatIDNames",     "ReadFlag",
                
          # panel variables and parameters      
                "numRows",            "numGrps",         
                "rowSep",             "rowSepGap",
	        "rowSize",            "rowSizeMaj",         "rowSizeMin",
	        "rowSizeMx",          "rowSizeMn",

                "colSepGap",
                "colSizeMax",         "colSizeMin", 
                
                "rcRatioMin",         "rcRatioMax",
                
                "groupedRowSize",     "groupedRowSep",
         
                "medGrp",             "medGrpSize",         
                "medRow",             "medRowAbv",         "medRowBlw",

                "ib",                 "ie",         
  
                "sc",                 "pad",                "padex",              "padMinus",
           
                "topMar",             "botMar",             "botMarLegend",       "botMardif",
                
                "borderSize",
                
          # System
                "detailsVariables",   "varName",            "mstColorNames",      "colorsRef",
                "mcolors",            "colorsRgb",          "colorsRefRgb",       
               
            
          # Axis adjustments
                "mgpTop",             "mgpBottom",          "padjBottom",         "mgpLeft",

                "leftMarAxis",        "leftMar",            "rightMar",

          # Axis Lab variables

                "staggered",          
		"lastLab2Space",      "lastLab3Space",
                
          # Call Parameters 
                "ignoreNoMatch",      
           
                "bordGrp",            "bordDir",           "grpPattern",
          
          # Counter functions 
                "warnCnt",            "stopCnt",
          
          # glyphs variables
            # General
                "Title.Line.1.pos",   "Title.Line.2.pos",   "Title.Line.2x.pos",  
                "Title.Line.3.pos",   "Title.Line.4.pos",
                "Title.Line.5.pos",
                "Title.cex",
                
                "Grid.Line.col",      "Grid.Line.lwd",
                
                "Panel.Fill.col",     "Panel.Outline.col",
                
                "Text.cex",
                
                "XAxis.L.mcex",       "XAxis.M.mcex",       "XAxis.S.mcex",       
                "XAxis.Sp.mcex",
                "XAxis.offset",       "XAxis.indent",       "XAxis.nGridpIn",          
                "XAxis.staggered",    "XAxis.gapPC",
                
                "YAxis.cex",          "YAxis.offset",       "YAxis.nGridpIn",
                "YAxis.width",        
          
            # Arrow
                "Arrow.Head.length",  "Arrow.lwd",            "Arrow.cex",
                "Arrow.Shadow.col",   "Arrow.Shadow.lwd",   
                "Arrow.Dot.pch",      "Arrow.Dot.pch.size",   "Arrow.Dot.pch.lwd",       
                "Arrow.Dot.Outline",  "Arrow.Dot.Outline.col","Arrow.Dot.Outline.lwd",
           
            # Bar
                "Bar.barht",          
                "Bar.Outline.col",    "Bar.Outline.lwd",   "Bar.Outline.lty",
           
            # Boxplot
                "BoxP.thin",          "BoxP.thick",         "BoxP.Use.Black",
                "BoxP.Median.Line",   "BoxP.Median.col",
                "BoxP.Median.Dot.col","BoxP.Median.Dot.pch","BoxP.Median.Dot.cex","BoxP.Median.Dot.lwd",
                "BoxP.Outline.col",   "BoxP.Outlier.BW.col","BoxP.Outlier.lwd",   "BoxP.Outlier.cex",   
           
            # Center Stacked Bars
                "CBar.varht",         "CBar.two.ended",
                "CBar.Zero.Line.col", "CBar.Zero.Line.lwd", "CBar.Zero.Line.lty",
           
            # Center, Segmented, and Normalized Stacked Bars
                "CSNBar.barht",
                "CSNBar.Outline.col", "CSNBar.Outline.lwd", "CSNBar.Outline.lty",
                "CSNBar.First.barht", "CSNBar.Last.barht",
           
            # Dot, Dotsignif, Dotconf, Dotse
                "Dot.pch",            "Dot.pch.size",       "Dot.pch.lwd",       
                "Dot.Outline",        "Dot.Outline.col",    "Dot.Outline.lwd",
                
                "Dot.Conf.pch",       "Dot.Conf.pch.size",  "Dot.Conf.pch.lwd",
                "Dot.Conf.lwd",
                "Dot.Conf.Outline",   "Dot.Conf.Outline.lwd","Dot.Conf.Outline.col",
                
                "Dot.SE",             
                "Dot.SE.pch",         "Dot.SE.pch.size",     "Dot.SE.pch.lwd",             
                "Dot.SE.lwd",      
                "Dot.SE.Outline",     "Dot.SE.Outline.lwd", "Dot.SE.Outline.col",
                
                                      
            # Dotsignif
                "Dot.Signif.pch",     "Dot.Signif.pch.size","Dot.Signif.pch.col","Dot.Signif.pch.lwd",
                
                "Dot.Signif.Outline", "Dot.Signif.Outline.col","Dot.Signif.Outline.lwd",
                
                "Dot.Signif.pvalue",
                "Dot.Signif.range",
           
            # Dotconf, Dotse
                "Dot.conf.pch",       "Dot.conf.pch.size",
                "Dot.conf",           "Dot.conf.lwd",       "Dot.conf.size",
           
            # Id
                "Id.Hdr1",            "Id.Hdr2",            
                "Id.Title.1.pos",     "Id.Title.2.pos",
                "Id.Start",           "Id.Space",           "Id.Cex.mod",
                "Id.Text.cex",        "Id.Text.adj",
                "Id.Dot.pch",         "Id.Dot.lwd",         "Id.Dot.cexm",     "Id.Dot.width",
                "Id.Dot.Outline.col", "Id.Dot.Outline.lwd",
           
            # map, mapcum, mapmedian, maptail
                "Map.Min.width",      # will become dynamic
                "Map.Max.width",      #
                
                "Map.Aspect",         # from areaParms
                "Map.L2Borders",      "Map.RegBorders",    "Map.L3Borders",
                "Map.MinH",           "Map.MaxH",
                "Map.Lab.Box.Width",
                "Map.Median.text",    "Map.Median.cex",
                                
                "Map.Bg.col",
                "Map.Bg.Line.col",    "Map.Bg.Line.lwd",
                "Map.Fg.Line.col",    "Map.Fg.Line.lwd",
                "Map.L2.Fill.col",    "Map.L2.Line.col",   "Map.L2.Line.lwd",
                "Map.L3.Fill.col",    "Map.L3.Line.col",   "Map.L3.Line.lwd",
                
                "Map.Area.Spec.cex",  "Map.Hdr1",          "Map.Hdr2",

            # rank
                "Rank.width",
           
            # Support - refVal, refText
                "Ref.Val.col",        "Ref.Val.BW.col",     "Ref.Val.lwd",        "Ref.Val.lty",        
                "Ref.Text.col",       "Ref.Text.BW.col",    "Ref.Text.cex",

            # ScatDot
                "SCD.Bg.pch",         "SCD.Bg.pch.size",    "SCD.Bg.pch.fill",
                "SCD.Bg.pch.col",     "SCD.Bg.pch.lwd",
                "SCD.Fg.pch",         "SCD.Fg.pch.size",
                "SCD.Fg.pch.col",     "SCD.Fg.pch.lwd",     
                "SCD.Median.pch",     "SCD.Median.pch.size","SCD.Median.pch.fill",
                "SCD.Median.pch.col", "SCD.Median.pch.lwd", 
                "SCD.Axis.cex",
                "SCD.xsc",            "SCD.ysc",            "SCD.hGrid",
                "SCD.DiagLine",       "SCD.DiagLine.col",   "SCD.DiagLine.lwd",   "SCD.DiagLine.lty",

            # Normalized and Segmented stacked bar                
                "SNBar.varht",        "SNBar.two.ended",                
                "SNBar.Middle.Dot",   
                "SNBar.MDot.pch",     "SNBar.MDot.pch.fill","SNBar.MDot.pch.lwd", "SNBar.MDot.pch.size",
                "SNBar.MDot.pch.border.col",
                "SNBar.MDot.pch.border.lwd",

            # TS and TSConf                
                "TS.lwd",             "TS.Axis.cex",        "TS.hGrid",

            # debug            
                "MST.Debug",
                
            # functions
                "GetMColors",         "mchr",               "masc",               "NewCounter",
                "PlotVis",            "Plotsf",             "is.between.r",       "is.between",
                "errCntMsg",          "stopCntMsg",         "TS_Date",            
                "ClnStr",             "ClnStr2"
                
           
                ), add=TRUE)

#
#   Would rather have these variable in the local "micromapST" environment.
#
######


########
#
#  Global Functions called by BuildBorderGroup and micromapST
#
####
#
#  counter function definition in Global Environment to be accessible from all functions.
#
NewCounter <- function() {
    i <- 0
    function() {
       i <<- i + 1
    }
}
#
#####


#####
#
#  mchr(x) returns character value for "x".   
#      if x is a character, x is returned.
#      if x is numeric, it is converted to character value
#
mchr <- function(x) {
        if (methods::is(x,"character")) {
           return(x)
        } else {
           if (methods::is(x,"numeric")) {
              as.character(rawToChar(as.raw(x)))     
           } else {
              return("\025")
           }
        }
     }
#
#
#####


#####
#
#  masc - returns the numerical value for the character "wX"
#    
#
masc <- function (wX) {
         wax <- wX
         if (methods::is(wX,"numeric"))   {
            # numeric - turn into character
            wax <- as.character(wX)
         }
         if (methods::is(wX,"character")) {
            if (nchar(wX) > 1) {  wax <- substr(wX,1,1)  }   # get only one character
            
            strtoi(charToRaw(wX),16L)   # convert character to numericstrtoi(charToRaw(wX),16L) 
         } else {
            NA
         }
      }
#
#
#####

#####
#
# function to test if "x" is between or equal to a and b.
#
is.between <- function(x,a,b) {
        # function checks x to make sure it's is between a and b
        #  This version supports vectors.
        if (a>b) {
          (x >= b & x <= a)
        } else {
          (x >= a & x <= b)
        }
      }
 
 #
 #####
 
 #####
 #
 # function to test if "x" is within or equal to the range of "r".
 #    "r" must be a vector of length 2 to be evaluated.
 #
 is.between.r <- function(x,r) {
    # the x must be within or equal to the range spacified in R
    #
    if (length(r) != 2) {
         errCntMsg("***0491 INB is.between.r The r range value is not a vector with length of 2. FALSE returned.\n")
         return(rep(FALSE,length(x)))   # not valid range
    } else {
         return(is.between(x,r[1],r[2]))
    }
  }

#
#####
 

####
#
#  odd - check if number is odd (TRUE) or even (FALSE)
#

odd <- function(x) {
    x%%2 == 1
}

#
####

####
#
#  CleanString - clean up character string - remove extra spaces, all punctuation, control characters and 
#     make all caps.   Replaces all "space", "control", "punctuation" with "space".
#
CleanString <- function(wstr) {
   nstr <- stringr::str_to_upper(stringr::str_trim(stringr::str_replace_all(wstr,"[[:space:][:cntrl:][:punct:]]+"," ")))
   return(nstr)
}

#
####

####
#
#  Clean up strings - remove 
#    1) special single and double quotes (open and closed)
#    2) tick mark
#    3) general punctuation (periods, etc.)
#    4) control characters
#    5) Make upper case.
#    6) remove multiple blanks.
#
#    Designed to allow strings that may have different times of quotes, apos. to be compared.
#
ClnStr <- function(x) {

    z <- gsub("[[:punct:][:cntrl:]\u2018-\u201F]", "", x, perl=TRUE)
    z <- stringr::str_to_upper(z)
    z <- stringr::str_squish(z)
    return(z)
}

#
####

####
#
#  Clean up strings - remove 
#    1) special single and double quotes (open and closed)
#    2) tick mark
#    3) control characters
#    4) Make upper case.
#    5) remove multiple blanks.
#
#    Designed to allow strings that may have different times of quotes, apos. to be compared.
#
ClnStr2 <- function(x) {

    z <- gsub("[[:cntrl:]\u2018-\u201F]", "", x, perl=TRUE)
    z <- stringr::str_to_upper(z)
    z <- stringr::str_squish(z)
    return(z)
}

#
####

####
#
# simpleCap - capitalize each word in a phrase and removes "."s, "_"s, and extra blanks.
#     Not good on vectors - must apply
#

simpleCap <- function (x)
   {
      s  <- stringr::str_split(x,"[ ._]")[[1]]    # split on boundaries " ", "." or "_".
      s1 <- s[s != ""]                 # skip empty strings
      
      paste0(stringr::str_to_upper(stringr::str_sub(s1,1,1)),stringr::str_to_lower(stringr::str_sub(s1,2)),collapse=" ")
   }
      
#
# Alternative:
#   gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
#
####


####
#
#  Color string to hex string conversion (handles vectors of values)
#
col2hex <- function(cname) {

   res <- try(colMat <- grDevices::col2rgb(cname), silent=TRUE)  # us the color name valid?
   if (!inherits(res,c("try-error")) ) {
       # result is no-error - res has a good answer.
       grDevices::rgb(red=colMat[1,]/255, green=colMat[2,]/255, blue=colMat[3,]/255)
   } else {
       # result is with error - return the error vectors
       res  # recheck on outside.
   }
 }
  
#
####


####
#
#   Common to BuildBorderGroup and micromapST
#

GetMColors <- function() {
   #####
   # Candidate colors________________________________________
   colorsRefRgb = matrix(c(
    1.00,1.00,1.00,  # white            "#FFFFFF"               # borders
     .95, .95, .95,  # lightest gray    "#F2F2F2" or "gray95"   # L2 area background
     .92, .92, .92,  # lighter gray     "#EBEBEB" or "gray92"   # changed from .90  # inactive area background
     .78, .78, .78,  # light gray       "#C7C7C7" or "gray78"   # changed from .80
     .50, .50, .50,  # middle gray      "#7F7F7F" or "gray50"
     .30, .30, .30,  # dark gray        "#4D4D4D" or "gray30"  
     .00, .00, .00,  # black            "#000000" or "black"    # borders
    
     .93,1.00, .93,  # light green              #EDFFED
     .00, .50, .00,  # mid green                #007F00
    1.00,1.00, .84,  # light yellow foreground  #FFFFD6
     .90, .80,1.00,  # bright yellow foreground #E5CCFF  
     .80, .90,1.00,  # light green blue         #CCE5FF
     .60, .70, .85), # mid green blue           #99B2D8

     ncol=3,byrow=TRUE)
   
   colorsRef = grDevices::rgb(colorsRefRgb[,1],colorsRefRgb[,2],colorsRefRgb[,3])
   names(colorsRef) = c("white","lightest gray","lighter gray","light gray",
                        "mid gray","dark gray", "black",
                     "light green","mid green",
                     "light yellow","bright yellow",
                     "light green blue","mid green blue")  
                     
   #print(colorsRef)                  

   #
   # colors copies from the micromapST defaults. Copies from micromapST.

   colorsRgb = matrix(c(                              # the basic 7 (9) colors.
    1.00, .15, .15,     #region 1: red	               1  #D53E4F - Red
     .90, .55, .00,     #region 2: orange	       2  #FC8D59 - Brn/Org
     .00, .65, .00,     #region 3: green	       3  #FEE08B - Pale Brn
     .20, .50,1.00,     #region 4: greenish blue       4  #99D594 - Pale Green
     .50, .20, .70,     #region 5: lavendar            5  #3288BD - Blue
     .88, .20, .59,     #region 6: magenta             6            (Added)
     .00, .00, .00,     #region 7: black for median    7  #000000 - Black
    1.00,1.00, .80,     #non-highlighted foreground    8  #FFFFCC  ?#E6F598 - Pale Yellow
#    1.00, .9875,0.95,   #upper shade - very pale red   9  #FFFFB2   (Added)
   0.7843137,1,0.8784314,  #lower shade - very Ylw/Grn  9  #C8FFE0  (highlighted above median)  #c8ffe0
#     .955,.98,1.00,     #lower shade - very pale blue 10  #F4FAFF   (Added)
   1.00,0.7843134,0.8784314,   #upper shade - very Ylw/rd 10 #FFC8D0  (highlighted below median)  #ffc8e0
     .95, .95, .95,     #lightest gray - not referenced sub-area        11 
     .92, .92, .92),    #lighter gray  - non-active backgroup sub-area  12
     ncol=3,byrow=TRUE)



   #
   #   mcolors  1 - 6     are sub-area active colors           (unused items = NA)
   #   mcolor   7         is black for median row accent       (7th, item in gsubs list)
   #   mcolors  8,  9, 10 are fill colors for median features. (8-pale yellow, 9-pale red, 10-pale blue)
   #   mcolors 11, 12     are background colors for not-referenced and not active.
   #
   #   Need to figure out how to add and what to add as 6th sub-area color.
   #

   mcolors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3]),           # solid mcolors (12)        format =>  "#FFFFFFF"
               grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],.2))         # translucent mcolors (12) - 20%.
   # adjust colors 9 and 10
   mcolors[9] <- paste0(str_sub(mcolors[9],1,7),"60")
   mcolors[10] <- paste0(str_sub(mcolors[10],1,7),"60")
   
   names(mcolors) =c("red","orange","green","greenish blue", "purple","magenta",
                    "black","light yellow","light red","light blue",
                    "lightest gray","lighter gray",
                    "l_red","l_orange","l_green","l_greenish blue", "l_purple","l_magenta",
                    "l_black","l_light yellow","l_light red","l_light blue",
                    "l_lightest gray", "l_lighter gray"
                   )
                   
   #print(mcolors)
   
   res <- list(mcolors=mcolors,colorsRgb=colorsRgb,colorsRefRgb=colorsRefRgb,colorsRef=colorsRef)
   return(res)
 }
#
#####

#####
#
#   PlotVis ( VisB, VisCol, xTitle=NULL, xAxes=FALSE, xLwd=0.5)
#   Function to plot border data in VisBorder data structure format.  
#   The color of the areas can be specified as a vector in VisCol.
#
#     VisB  = VisBorder data .frame structure.
#           se = sequence number
#           x   = x coordinates value (in meters) (or X)
#           y   = y coordinates value (in meters) (or Y)
#           areaName = name or id of the area 
#           hole = T/F indicating this polygon is a hole in the area.
#                  Hole must be drawn last when drawing an area.
#                  Areas with hole must be drawn first, to allow other 
#                  areas to possibly fill the hole.
#           Key = the areas key the point belongs to.  All sets of points
#                  for an area with multiple polygons, must be in the same
#                  series under the same Key in the data .frame. Each polygon
#                  is ended with a X and Y value of NA. This stops the drawing
#                  and allows the drawing of another polygon at a different X, Y
#                  coordinate.
#     VisCol = standard color definition used for the color of the areas.
#        It should be a vector of color names with the same length as the number of 
#        areas in the VisBorder structure.  If the length of the color vector is shorter
#        than the number of areas, the mcolors in the vector will be re-used.
#        If the vectors between the NA rows is a point or a line, R's polygon function
#        will not advance to the next color, nothing to fill.  This will affect which 
#        colors shade each polygon.  If multiple polygons represent a single area,
#        the function caller must compensate for this in the VisCol vector.
#        The usual strategy is to assign a color to each row in the Name Table, then
#        compare the keys in the Name Table and the keys in the VisBorder and assign 
#        the color for that Key/Polygon.
#
#     xTitle - character string to be used as the title for the graphics.
#
#     xAxes  -  TRUE or FALSE - include an axis line and numbers (def=FALSE, don't)
#
#     xLwd   - line width for the map drawing.
#
PlotVis <- function(VisBrd, VisCol, xTitle=NULL, xAxes=FALSE, xLwd=0.05) {

    # If VisCol is missing, set the VisCol to NULL.
    if (missing(VisCol) || is.null(VisCol) || length(VisCol) == 0) 
        VisCol = NULL
   
    # Set the x and y Limits based on the range of the x and y coordinates in the VisBorder.       
    xLim         <- (range(VisBrd$x,na.rm=TRUE))
    yLim         <- (range(VisBrd$y,na.rm=TRUE))
    #cat('xLim:',xLim,'  yLim:',yLim,"\n")
    #print(par("plt"))    # plot area 
    #print(par("pin"))    # plot in inches
    #print(par("usr"))    # plot in user scale
     
    # polygon function steps to the next color on each "NA" ending
    # a polygon in the data.frame.  Since multtiple polygons 
    # can make up an area, logic must be added to get 
    # a table of KEYs to NAs in the VisBorder, map the mcolors provided
    # to KEYs, then call polygon with the adjusted color list.
      
    # Start an empty plot to get the limits and graphic space set.
    plot(0,0,type="n", xlim=xLim, ylim=yLim, asp=1,
                       axes=xAxes, lwd=xLwd,
                       xlab="", ylab="", 
                       main=xTitle)
     
    # Draw the VisBorder using the polygon function for the group/row.
    # The entire VisBorder data.frame can be drawn with 
    # one polygon function call.
    
    graphics::polygon(VisBrd$x, VisBrd$y ,border="black", col=VisCol, lwd=xLwd)  
    
               # drawn using one call because of its structure.
}
#
#####



#####
#
#   Plotsf (xsf, xCol ,xTitle=NULL, xAxes=FALSE, xLwd=0.5)
#   Function plots an sf data structure.  
#   The color of the areas can be specified.  This is the same
#   type of function as the PlotVis function.  It corrects for 
#   the aspect ratio of the map and graphic plot space, and 
#   draws the map from the spatial structure.
#
#    xsf - standard sf structure structure.
#         The sf has a row.names set as the areas' key
#    xCol - standard color definition used for the color of the areas.
#         It should be a vector of color names with the same length 
#         as the number of areas in the sp.  If the length of the color 
#         vector is shorter than the number of areas, the mcolors in the 
#         vector will be re-used.
#
#    xTitle - character string to be used as the title for the graphics.
#
#    xAxes -  TRUE or FALSE - include an axis line and numbers (def=FALSE, don't)
#
#    xLwd - line width for the map drawing.
#
Plotsf <- function(xsf, xCol, xTitle=NULL, xAxes=FALSE, xLwd=0.5) {

    # If xCol is missing, set the xCol to NULL.
    if (missing(xCol) || is.null(xCol) || length(xCol)==0) 
                  xCol = rep(NA,length(st_geometry(xsf)))
    #                  
    # Set the x and y Limits based on the range of the X and Y coordinates in the VisBorder.       
    xBBox      <- as.numeric(sf::st_bbox(xsf))             # get box - xLim and yLim
    xLim       <- xBBox[1,]
    yLim       <- xBBox[2,]
    #print(xLim)          # X axis limits
    #print(yLim)          # Y axis limits
     
    # polygon function steps to the next color on each "NA" ending
    # a polygon in the data.frame.  Since multtiple polygons 
    # can make up an area, logic must be added to get 
    # a table of KEYs to NAs in the VisBorder, map the mcolors provided
    # to KEYs, then call polygon with the adjusted color list.
      
    ## Once the par(pin) is set, draw the map.
    xsfG  <- st_geometry(xsf)
    plot(xsfG,col=xCol, xlim=xLim,ylim=yLim,axes=xAxes,lwd=xLwd, asp=1, 
                       xlab="",ylab="",main=xTitle)
 }
#
#
#
#### Global functions

