###
#
#   micromapGSetPanelDef function to set the panel vectors up for the function execution.
#
#

micromapGSetPanelDef <- function(nRows,rSizeMaj,rSizeMin,rSepGap,MaxRows,UGrpPattern) {

   #cat("Set Panel Def: nRows:",nRows,"  rSizeMaj:",rSizeMaj," rSizeMin:",rSizeMin," rSepGap:",rSepGap," MaxRows:",MaxRows,"\n")
   
  
   #  $detail variables now in memory.  Use references up one caller level.
   
   #
   #  MaxRows is the switch to do 5 rows per group or 6 rows per group.
   # 

   ### Generalize settings.
   
   # build panels from panelLayout and pieces of rlAreaDefaults$Details
   
   # nrow = 11 -> 5,5,5,5,5,1,5,5,5,5,5 states = 11 groups
   # individual panels (rows(11) and columns)
   
   # changes to generalize number of rows.
   
   # vnumRows = number of data areas to be represented
   # vnumGrps = number of logical groups or panels to represent numRows
   
   #
   #  Some of this code was in the micromapGSetDefaults function.  Moved here since
   #  it appears code needs to have the variables setup prior.  Look at return it later.
   #

####  Algorithm around the core elements structure that adds 5 on the sides as needed. 
#       special pattern      ( 1 to 2)  - initial patterns and specials to override the calculated pattern
#       core pattern         (3 to 12 and repeated onward)  - calculation base patterns
#
#       alternate table for special cases  (has numbers of rows in column to match on. 
#       Put starting patterns in this group.   If no match - go to the core pattern.

#  These are list of vectors

 cGrpTabSpecial <- data.frame(r = c(1,2), 
                                p=I(list(
    c(1),                     #  1 row
    c(2)                      #  2 rows
    ))
   )
   
 cGrpTabCore5 <- data.frame(p=I(list(
     c(3),                 #  3-13 rows
     c(4),                 #  4-14 rows
     c(5),                 #  5-15 rows -  3 groups
     c(3,3),               #  6-16 rows + 2"x" times 5  (0,10,20,30,40,50,60,...,300,310,...)
     c(3,1,3),             #  7-17 rows
     c(4,4),               #  8-18 rows
     c(4,1,4),             #  9-19 rows
     c(5,5),               #  10-20 rows
     c(5,1,5),             #  11-21 rows 
     c(4,4,4),             #  12-22 rows
     c(5,3,5),             #  13    rows   # used only in the initial round
     c(5,4,5)              #  14    rows
    ))
   )                 # (nr - 3) \ 10 => n5   then (nr - ((n5 * 10) + 2))

 cGrpTabCore6 <- data.frame(p=I(list(
     c(1),                 #  0- 1,13 rows
     c(2),                 #  1- 2,14 rows
     c(3),                 #  2- 3,15 rows
     c(4),                 #  3- 4,16 rows
     c(5),                 #  4- 5,17 rows
     c(6),                 #  5- 6.18 rows 
     c(3,1,3),             #  6- 7,19 rows + 2"x" times 5  (0,10,20,30,40,50,60,...,300,310,...)
     c(4,4),               #  7- 8,20 rows
     c(4,1,4),             #  8- 9,21 rows    alternate = c(3,3,3)
     c(5,5),               #  9-10,22 rows                c(4,2,4) 
     c(5,1,5),             # 10-11,23 rows                c(4,3,4) 
     c(6,6)                # 11-12,24 rows           c(4,4,4) or c(5,2,5)
    ))
   )


#
#  Process - Scan xGrpTabSpecial first for a match on the number of rows.
#          - If no match, calculate the pattern by adding 5 to the sides of the remaining
#            patterns
#
#  Formula is   Number of 5 to add <- as.integer((rows - 3) / 10) 
#               TabCore index <- numRows - (2 x number of 5)      #  ranges from 0 to 9) 
#

##### functions
#
#  GrpCal5 - function to calculate the number off 5's and index into table
#
GrpCal5 <- function(nr) {
    #   nr - number of rows
    n5 <- as.integer((nr-3)/10)           # number of 5's to each side
    
    idx <- (nr) - ((  n5 * 10 ) + 3)      # index into cGrpTabCore list.
    
    return(c(n5,idx+1))
  
  }
#
#  GrpCal6 - function to calculate the number off 5's and index into table
#
GrpCal6 <- function(nr) {
    #   nr - number of rows
    n6 <- as.integer((nr-1)/12)           # number of 5's to each side
    
    idx <- (nr-1) - ((  n6 * 12 ))      # index into cGrpTabCore list.
    
    return(c(n6,idx+1))
  
  }
#
#
##### end of functions

#######  Main Code ######
#
#

#
#  On entry:   nRows    - number of rows
#              rSizeMaj - number of units in Major group/row (7) - 2 to 6 rows.
#              rSizeMin - number of units in Minor group/row (1) - single row
#              rSepGap  - row separator gap (inches)
#              uGrpPattern - vector of number of rows per group.
#

vnumRows = nRows        # number of rows in areaDFrame = Still need to check for duplicutes and validity.
   

wGCn <- match(vnumRows,cGrpTabSpecial$r)  # look for unique pattern in special table.

if (is.na(wGCn)) {
   # not in special table
   wy <- GrpCal5(nRows)    # calculate parameter to generate entry
   GrpPattern <- c(rep(5,wy[1]),unlist(cGrpTabCore5[wy[2],]),rep(5,wy[1]))   # create GrpPattern
} else {
   GrpPattern <- cGrpTabSpecial$p[[wGCn]]    # matched special table - pick up pattern  
}

#cat("Cal_GrpPattern:",paste0(GrpPattern,collapse=", "),"\n")
#cat("\n")

#  with the above code, GC is not the vector with the number of rows per group.

#
# Key variables from this setup:
#
#  numRows       - number of areas
#  maxRows       - maximum number of rows per group (5 or 6) - not implemented.
#  medRow        - number of the median sub-area  or 0 - no exact median (even number of groups)
#  GrpPattern    - rows per glyph group - pattern
#  numGrps       - number of groups (sets of rows 1 to 5/6)
#  medGrp        - number of the median group or 0 - no median group (even number of groups)
#  medGrpSize    - number of rows in median group
#  ib and ie     - starting and ending row numbers (relative) for each group
#  numRowBlwMed  - number of the row below the median point or row
#  numRowAbvMed  - number of the row above the median point or row
#

#######
   # if user provided GrpPattern 
   if (!(is.null(UGrpPattern) || length(UGrpPattern) < 1 )) {
      # User provided grpPattern - yes
      UGrpPattern <- as.numeric(UGrpPattern)
      if (any(is.na(UGrpPattern))) {
         # one or more of the values in the GrpPattern is not a number or is an NA.
         # should have been caught eariler
         xmsg <- ("***0182 CARG-GP User provided GrpPattern is invalid, contain non-numeric and/or NA values. The GrpPattern will be ignored.")
         cat(xmsg,"\n")
      } else {
         if (sum(UGrpPattern,na.rm=TRUE) != sum(GrpPattern,na.rm=TRUE)) {
            stop("***0181 CARG-GP User provided grpPattern is invalid. The total of the rows per group must equal the number of rows as the data.")
         } else {   
            # replace generated pattern with user provided pattern
            GrpPattern <- UGrpPattern   # Save user pattern.
         }
      }   
   }
   numGrps <- length(GrpPattern)
 
   if (sum(GrpPattern,na.rm=TRUE) != vnumRows) stop("Programming problem. GrpPattern has different number of rows then system needs.")
   
   if (odd(numGrps)) {
      medGrp     <- as.integer(numGrps/2) + 1
      medGrpSize <- GrpPattern[medGrp]
   } else {
      medGrp     <- 0
      medGrpSize <- 0
   }

   if (odd(vnumRows)) {
      medRow     <- as.integer(vnumRows/2) + 1
      medRowAbv  <- medRow - 1
      medRowBlw  <- medRow + 1
   } else {
      medRow     <- 0
      medRowAbv  <- as.integer(vnumRows/2)
      medRowBlw  <- medRowAbv + 1
   }

   #cat("numGrps:",numGrps,"  medGrp:",medGrp,"  medGrpSize:",medGrpSize,"\n",
   #    "vnumRows:",vnumRows,"  medRow:",medRow,"  medRowAbv:",medRowAbv,"  medRowBlw:",medRowBlw,"\n")
   #cat("GrpPattern:",paste0(GrpPattern,collapse=", ",sep=""),"\n")    
   
  iw = cumsum(c(1,GrpPattern))      # number of groups + 1  (starting row 1, 6, 11, 16, 17, 22, 27, 32)
  wj <- as.integer(numGrps/2)       # 1/2 numGrps - signal of median group
  wi <- wj*2                        # get rounded value for number of groups (see if odd or even)
   
  #cat(" Set Defs Panel - num Grps-wj:",wj,"  rounded-num Grps-wi:",wi,"\n")

  vrowSep         <- rep(0,numGrps+1)
  vgroupedRowSep  <- rep(0,numGrps+1)
  vrowSize        <- rep(rSizeMaj,numGrps) 
  vgroupedRowSize <- rep(rSizeMaj,numGrps)
  #cat("vrowSep:",vrowSep," grpRowSep:",vgroupedRowSep,"  vrowSize:",vrowSize,"  grpRowSize:",vgroupedRowSize,"\n")
  
   # generate rowSep and rowSize vectors
   # vrowSep
   if (odd(numGrps)) {
      # odd number of groups - we have a median group - must check to see if the number of groups are > 1.
      if (numGrps>1) {     # odd so 3, 5, 7, etc group/rows.
         # Row Information - Must be 3 or greater, so handle the median group.
         vrowSep[wj+1] = 1                    # set to 1 around the median group.
         vrowSep[wj+2] = 1                    # above and below.
         
         vgroupedRowSize <- c(rSizeMaj*wj, rSizeMaj, rSizeMaj*wj)  # median group same height as others. due to map.
         vgroupedRowSep  <- c(0,1,1,0)                             # Upper Block, Median, Lower Block   
         if (medGrpSize == 1) {                # if median group with 1 row -> no map - special height       
                       # this can't be if the number of areas is less than 5.
            # set middle group's size to the default 1.65 for "median of sorted data" group
            vrowSize[medGrp] = rSizeMin   
               
            # group Row size impacted - Large, one row, Large.
            vgroupedRowSize[2] <- rSizeMin  # units (1.65)  - center group (odd and > 1 group.)
         }
      } else {
         # numGrps is 1 - slim it down.
         # NO Reason to put 1 around median..  Only the center group.
         # vgpRowSep should be c(0,0)
         # vgpRowSize should be rSizeMaj - c(rSizeMaj).
         x <- NULL
      }
   } else {
   
      # EVEN number - only middle separator  (2, 4, 6, 8 groups instead of three)
                                           
      # Row information                    $ vrowSep has numGrps + 1 entries.  The middle is 1/2 numGrps + 1
      vrowSep[wj+1] = 1                    # to the center row(grp)  - multiple by sep size later.
      
      vgroupedRowSize <- c(rSizeMaj*wj, rSizeMaj*wj)  # upper half and lower half
      vgroupedRowSep  <- c(0,1,0)               # top Sep, middle Sep, bot Sep  for 2, 4, 6 groups.
   }
   
   #
   #  So,  numRowMed > 0 when a single row is the median group/row
   #       numGrpMed > 0 when 1 or more rows is the median group/row
   #
   
   #  Convert the separators from markers to inch values.
   vgroupedRowSep <- vgroupedRowSep * rSepGap
   vrowSep        <- vrowSep * rSepGap          
  
   DetailsPanel <- list(
   
      numRows        = vnumRows,        #* number of rows (areas)
      maxRows        = MaxRows,         #* maximum number of rows per group (5 or 6)
      rowSize        = vrowSize,        #* RowSize vector for the row panel
      rowSep         = vrowSep,         #* RowSep vector for row panel
    
      numGrps        = numGrps,         #* number of groups
      GrpPattern     = GrpPattern,      #* pattern for number of rows in each group
      
      medGrp         = medGrp,          #* number of the median group or 0, if even number of groups
      medRow         = medRow,          #* number of the median row or 0, if even number of groups,
      medGrpSize     = medGrpSize,      #* number of rows in the median group, if it exists otherwise 0
      medRowBlw      = medRowBlw,       #* the number of the row before the median row (if exists)
      medRowAbv      = medRowAbv,       #* the number of the row above the median row (if exists)
      
      groupedRowSize = vgroupedRowSize, #* RowSize vector for the group panel
      groupedRowSep  = vgroupedRowSep,  #* RowSep vector for the group panel
      
      ib = iw[1:length(iw)-1],          #* beginning row for groups
      ie = iw[2:length(iw)]-1           #* ending row for groups.
    
    )
 
    #print("DetailsPanel:")
    #str(DetailsPanel)
    
    return(DetailsPanel)

}
#
#
###


###############################################################
###
#
#  micromapGSetDefaults function
#
#  Must be run once to generate the default lists.
#  If you customize - then make a copy and change the copy.
#
#  Call by .onload at package load.  Reference is exported to globlal space for user's access.
#
###

micromapGSetDefaults = function()
   {

 
#
#  build micromapGDefaults data.frame so it can be exported.
#

## Candidate colors________________________________________
#colorsRefRgb = matrix(c(
# 1.00,1.00,1.00,  # white            "#FFFFFF"               # borders
#  .95, .95, .95,  # lightest gray    "#F2F2F2" or "gray95"   # L2 area background
#  .92, .92, .92,  # lighter gray     "#EBEBEB" or "gray92"   # changed from .90  # inactive area background
#  .78, .78, .78,  # light gray       "#C7C7C7" or "gray78"   # changed from .80
#  .50, .50, .50,  # middle gray      "#7F7F7F" or "gray50"
#  .30, .30, .30,  # dark gray        "#4D4D4D" or "gray30"  
#  .00, .00, .00,  # black            "#000000" or "black"    # borders
# 
#  .93,1.00, .93,  # light green
#  .00, .50, .00,  # mid green
# 1.00,1.00, .84,  # light yellow foreground  
#  .90, .80,1.00,  # bright yellow foreground 
#  .80, .90,1.00,  # light green blue 
#  .60, .70, .85), # mid green blue
#  ncol=3,byrow=TRUE)
#
#colorsRef = grDevices::rgb(colorsRefRgb[,1],colorsRefRgb[,2],colorsRefRgb[,3])
#names(colorsRef) = c("white","lightest gray","lighter gray","light gray",
#                     "mid gray","dark gray", "black",
#                     "light green","mid green",
#                     "light yellow","bright yellow",
#                     "light green blue","mid green blue")           
#
## row color table________________________________________________
#
#colorsRgb = matrix(c(                                # the basic 7 (9) colors.
# 1.00, .15, .15,     #region 1: red	              1  #D53E4F - Red
#  .90, .55, .00,     #region 2: orange	              2  #FC8D59 - Brn/Org
#  .00, .65, .00,     #region 3: green	              3  #FEE08B - Pale Brn
#  .20, .50,1.00,     #region 4: greenish blue        4  #99D594 - Pale Green
#  .50, .20, .70,     #region 5: lavendar             5  #3288BD - Blue
#  .88, .20, .59,     #region 6: magenta              6            (Added)
#  .00, .00, .00,     #region 7: black for median     7  #000000 - Black
# 1.00,1.00, .80,     #highlighted foreground (present) 8  #E6F598 - Pale Yellow
# 1.00,0.7843134,0.8784314,   #upper shade - very pale red   9  #FFFCF2   (highlighted above median)  #ffc8e0
# 0.7843137,1,0.8784314,     #lower shade - very pale green 10  #F4FAFF   (highlighted below median)  #c8ffe0
#  .95, .95, .95,     #lightest gray - not referenced area        11 
#  .94, .94, .94),    #lighter gray  - non-active backgroup area  12
#  ncol=3,byrow=TRUE)
#
##
##   mcolors  1 - 6     are sub-area active colors           (unused items = NA)
##   mcolor   7         is black for median row accent       (7th, item in gsubs list)
##   mcolors  8,  9, 10 are fill colors for median features. (8-pale yellow, 9-pale red, 10-pale blue)
##   mcolors 11, 12     are background colors for not-referenced and not active.
##
##   Need to figure out how to add and what to add as 6th sub-area color.
##
#
#mcolors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3]),           # solid colors (12)        format =>  "#FFFFFFF"
#            grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],.2))         # translucent colors (12) - 20%.
#
#names(mcolors) =c("red","orange","green","greenish blue", "purple","magenta",
#                 "black","light yellow","light red","light blue",
#                 "lightest gray","lighter gray",
#                 "l_red","l_orange","l_green","l_greenish blue", "l_purple","l_magenta",
#                 "l_black","l_light yellow","l_light red","l_light blue",
#                 "l_lightest gray", "l_lighter gray"
#                 )       

XColors      <- GetMColors()    # get colors.
mcolors      <- XColors$mcolors
colorsRgb    <- XColors$colorRgb
colorsRefRgb <- XColors$colorsRefRgb
colorsRef    <- XColors$colorsRef

#str(XColors)
#print(colorsRef)

# Details variable list _________________________________________

#
# It appears no matter what point size is set, the margins spacing is still based on 0.2" per line.
# cin, csi, etc. do not chan
# Real height to line spacing =  0.15/0.2 = 75%   or ps-height * 1.333 = space. (1.476925 actual)
#  To work with line spacing in margins - line height = 0.2 all of the time.
#                       working line height = strheight of font * 1.476925 = height of line.
#
#                       Point Size Height (PSH)      = strheight()  (inches)
#                       Point Size Line Space (PSLS) = PSH * 1.476925  (inches)
#                       Point Size Line Value (PSLV) = PSLS(inch per line) * package values(lines)  (inches)
#                       
#                       Margin Line value            = MLV
#          At Last minute - convert to Margin Line Values at 0.2" per line
#                       MLV  = PSLV / 0.2  (lines)
#
#          If font size (point size) changes, must 
#
#                  MLV(lines) = ( ( PSH(inchs) * 1.476935 )(inch per line) * PackageLine(lines) ) / 0.2
#
#          To estimate graphic width and height must work within pointsize (width, height).
#          Since all original estimates are in point size = 12.  should be able to do a ratio to 
#          find point size that will work.  
#    
#          At the current time it appears if the par(ps=) is set and the par(mex=) is scaled appropriately
#          the spacing and line orientation in the margins are therefore scaled.  If mex= is not set,
#          if the ps is reduced, They still get drawn at the same places.  Would have to scale
#          back the line positions, to tighten it up.  However, the size of the margins would stay 
#          the same.   Using mex=  solves this problem.
#
#          If using mex=, then just do the lines as if ps=12.  If need space, change ps and mex.
#

## JP added temp variables so function would read in in R 2.7
#      cannot use values within the details list since it's not really built yet.

tempOutline.Line.col <- colorsRef["white"]            # grid and outline line color

tempcolFill       <- colorsRef["lighter gray"]        # panel and default fill color
tempcolSubFill    <- colorsRef["lightest gray"]

tempText.cex      <- 0.75                       # 12 pt default -> 9 pt.

#cat("colors for graphs: ",tempOutline.Line.col,"  tempcolFill:",tempcolFill,
#        "  tempcolSubFill:",tempcolSubFill,"\n")


details = list(

    ### dynamic layout based on number of entries...
   
    ### Assumption of 10" space.  use pattern, then scale to real space.

    ### Start of panel sizing and layout - see micromapGSetPanelDef()
    
# Call variables - save slot for them later.
    
    callVarList                = list(statsDFrame="", panelDesc=""),    # to be replaced by the match.call results.
    pkgBGList                  = c("USStatesBG"                         # List of border groups included in package
                                   ,"USSeerBG"
                                   ,"KansasBG"
                                   ,"MarylandBG"
                                   ,"NewYorkBG"
                                   ,"UtahBG"
                                   ,"AfricaBG"
                                   ,"ChinaBG"
                                   ,"UKIrelandBG"
                                   ,"SeuolSKoreaBG"
                                  ),
                                             
    sDFColNames                = c("1","2","3","4"),                    # to be replaced by the column names in the statsDFrame data.frame
   
# panel layout grouping   (moved to micromapGSetPanelDef function)

    #  See micromapGSetPanelDef output 

    #ne                        = tempne,              # number of item per group      ## Built/Cal  (delete-global remove)
    #ng                        = ceiling(51/tempne),  # number of groups of areas     ## Built/Cal  (delete)

    # see SetPanelDef

    #numRows                   = vnumRows                                             ## Built/Cal
    #numGrps                   = vnumGrps                                             ## Built/Cal
    
    #ib                        =  c( 1, 6,11,16,21,26,27,32,37,42,47), #group lower index   ## Built/Cal  (replace)
    #ie                        =  c( 5,10,15,20,25,26,31,36,41,46,51), #group upper index   ## Built/Cal  (replace)

# panel layout margin allocation
    # JP - changed median row size to 1.5.
    topMar                     = 1.1,                 # margin panel height (inches) #  1
                  # must have enough space for titles above the linked micromap.
                  # The column titles/headers and axis ticks are handled by the gryphics
                  
    botMar                     = 0.5,                 # no legend bottom margin (inches)                           #  2
    botMarLegend               = 0.75,                #                                                            #  3
    botMardif                  = 0.2,                 # calulate legfacgor      #  4   
                  # ? room for the bottom "what", no titles, axis tick should be in columns.

    leftMar                    = 0,                   #                                                            #  5 
    leftMarAxis                = 0.2,                 # left margin adjustment when Y axis is printed  #  6
                  # no left margin, but the left axis is in the column?
                  
    rightMar                   = 0,                   #                                                            #  7

    borderSize                 = 0.5,                 # margin border - at least 0.5 inches.                       #  8
                  # why a general border spacing?
    #  height constraints
    rowSepGap                  = 0.075,                 # Size of the rowSep  (in inches)                          #  9
    
    rowSizeMn                  = 0.5,                 # Minimum Row Size in inches                                 # 10   # overriden by data from BG areaParms (add validation check)  Map.MinH
    rowSizeMx                  = 1.25,                # Maximum Row Size in inches.                                # 11   # overriden by data from BG areaParms     Map.MaxH
 
    rowSizeMin                 = 1.65,                # row Size for small-median minor group (no map) (in units)  # 12
    rowSizeMaj                 = 7,                   # row Size for standard - major group (in units)             # 13
                                                      # value of 7 for 5 rows per group - 5 rows + 2
                                                      # change to 8 when 6 rows per group is used.
                                                      # 
    #  width constraints
    colSepGap                  = 0.075,               # gap between columsn in inches.                             # 14

    colSizeMin                 = 0.25,                # minimum column size in inches.                             # 15
    colSizeMax                 = 2.5,                 # maximum column size in inches.                             # 16
 
    rcRatioMin                 = 0.25,                # row to col size ratio minimum.                             # 17
    rcRatioMax                 = 2,                   # row to col size ratio maximum.                             # 18
 
    #  See micromapGSetPanelDef -> micromapGPanelDefaults
    
    ### dynamic layout based on number of rows - see micromapGSetPanelDef()
    #
    #  Original settings for 51 areas - U. S. States and DC.
    #
    #  if number of rows = "n"
    #
    #                              1  2  3  4  5    6    7  8  9 10 11 12
    #rowSep                    = c(0, 0, 0, 0, 0, 0.1, 0.1, 0, 0, 0, 0, 0), # spaces - gaps - over 10 (inches)     # Built/Cal   
    #                              1  2  3  4  5    6    7  8  9 10 11 12   # row seperators (inches)              # Built/Cal
    #rowSep                    = vrowSep
    
    #                              1  2  3  4  5     6  7  8  9 10 11
    #rowSize                   = c(7, 7, 7, 7, 7, 1.65, 7, 7, 7, 7, 7),  # JP change 1.5 to 1.65 on Median strip   # Built/Cal.
    #rowSize                   = vrowSize                                                                          # Built/Cal.
    
    #                 1-5 6 7-11                        # working units
    #groupedRowSize            = c(35, 1.65, 35),       # JP changed 1.5 to 1.65 to give median a little more room # Built/Cal  
    #groupedRowSize            = vgroupedRowSize                                                                   # Built/Cal
    
    #               1-5 5-6 6-7 7-11                    # working units.
    #groupedRowSep             = c(0,0.1,0.1,0),        # rowGroup separators (inches)                             # Built/Cal
    #groupedRowSep             = vgroupedRowSep,                                                                   # Built/Cal
    
    #
    #medGroupID                = vnumGrpMed,                                                                       # Built/Cal
    #medRowID                  = vnumRowMed,                                                                       # Built/Cal
    
    #
    #   This block of variables are now calculated based on the number of areas in the linked micromap.
    #
    ####
    
    
# panel scaling
    
    sc                         = 1.08,                   # x and y axis scale expansion factor                     # 19
    pad                        = 0.67,                   # y axis padding for integer plotting locates (units)     # 20
                                       # ry = c(1-pad,ke+pad),ke = no. items in panel (units)  
    padex                      = 0.34,                   # total panel padding                                     # 21
                                       # (.67-.5)=.17 padding at top and bottom of panel         
    padMinus                   = 0.63,                   # .67 - .04 # keep reference line off panel edge          # 22

# mtext line placement (Titles)  # modifed Aug 2022 - align title with ID and glyphs.

    ##  JP adjusted placement of lines (titles) (units are "lines")
    Title.Line.0.pos	       = 0.01+(0.7*1)*3,       # top panel 0th line placement (lines=2.11 + .7 < 3)        # 23
    Title.Line.1.pos           = 0.01+(0.7*1)*2,       # top panel 1st line placement (lines=1.41) (delta 0.70)    # 24  
    Title.Line.2.pos           = 0.01+(0.7*1)*1,       # top panel 2nd line placement (lines=0.71)                 # 25  
    Title.Line.2x.pos          = 0.01,                 # (USED) top panel 3rd line placement (lines) also Tick Labs placement # 26
 
    Title.Line.3x.pos          = 0.01,                 # bottom panel-line 3x-Axis 1 placement (axis or 2)         # 27
    Title.Line.3.pos           = 0.01+(0.7*1)*1,       # bottom panel-line 3-Axis 2 or header (3 line) (title)     # 28  
    Title.Line.4.pos           = 0.01+(0.7*1)*2,       # bottom panel-line 4-lab 4 header   (3 lines) (refText and line) # 29 
    Title.Line.4x.pos          = 0.01+(0.7*1)*3,       # bottom panel-line 4x-reference line (4 line)              # 30
    Title.Line.5.pos           = 0.35,                 # Y axis titles for ScatDot and TS. (lines)                 # 31

    Title.cex                  = 1.0,                  #                                                           # 32  (not used)
    
    # Title.Line.2x.pos is only used in glyphs that do not have x-axis (the maps and id).
    # The X-axis line is independent.  However, we now want Title.line.1.pos to 
    # match across the entire page.
  
# grid line parameters
    Grid.Line.col              = colorsRef["white"],     # grid line color                                         # 33
    Grid.Line.lwd              = 1,                      # weight of grid line  # 34
 
# margin lines (in mex units) for axis title, axis labels, and axis line.  
    mgpTop                     = c(3.2,    0.1,  0),     # label & gridline (tick) placement (changed from 2,0.1,0)# 35  (not used)(margin spacing for title and axis = (title=2 lines, tick 0.1 lines, and 0 lines)
    mgpBottom                  = c(3.2,    0.1,  0),     # label & gridline (tick) placement (changed from 2,0,0)  # 36  (not used)
    padjBottom                 = -0.35,                  # gridline (tick  placement)                               # 37  (not used) Was -0.7 adjusted 11/14
    mgpLeft                    = c(0.75,   0.1,  0),     # left axis labels (when present)                                       # 38  (used TS and SCD)

    ###  End of sizing of areas...
    
# axis parameters.

    XAxis.L.mcex               = 0.8888889,              # NA font size multiplier for X axis large labels (8 pt)    # 39
    XAxis.M.mcex               = 0.7777778,              # NA font size multiplier for X axis medium                 # 40
    XAxis.S.mcex               = 0.6666667,              # NA font size multiplier for X axis small staggered and scaled (6 pt) # 41 
    XAxis.T.mcex               = 0.3,                    # Tiny font for axis                                        # 42
    XAxis.Sp.mcex              = 0.2,                    # Labels to Axis spacing.                                   # 43

    XAxis.offset               = 0.0,                    # offset for X Axis above plotting area.                    # 44
    XAxis.indent               = 10,                     # indent outside labels by x/1000 of width                  # 45
    XAxis.nGridpIn             = 3.4,                    # X Axis - Grid lines per Inch max.                         # 46
    XAxis.gapPC                = 0.75,                   # percentage of character width between columns (buffer)    # 47
    XAxis.staggered            = TRUE,                   # NA straight or stagger labels.                            # 48

    YAxis.cex                  = 0.333333,               # NA font size (relative) for Y axis labels (4 pt)          # 49
    YAxis.offset               = 0.0,                    # NA offset for Y Axis above plotting area.                 # 50
    YAxis.nGridpIn             = 5,                      # NA X Axis - Grid lines per Inch max.                      # 51
    YAxis.staggered            = TRUE,                   # NA straight or stagger labels.                            # 52

# panel column width allocation
    ### no change - scale from 7.5 inches by 10 inches
    
    YAxis.width                = 0.2,                    # width for Y axis labels. (X axis-about inches-working units) # 53

# axis labeling

    staggered                  = FALSE,                  # intra column flags - staggered state of previous column # 54
    
# panels
    Panel.Fill.col             = colorsRef["lighter gray"],            # panel fill color                                        # 55
    Panel.Outline.col          = colorsRef["black"],     # panel outline color                                     # 56

# Title and Text - cex for character size
    Text.cex                   = 0.75,               ## JP decreased text size.  Used almost everywhere.                # 57

# refVals parameters

    # see padMinus above for other parameters 
    Ref.Val.lty                = "dashed",               # line type for Ref Value (dashed)                        # 58
    Ref.Val.lwd                = 1.5,                    # line weight for Ref Value                               # 59
    Ref.Val.col                = colorsRef["mid green"], # line color                                              # 60
    Ref.Val.BW.col             = colorsRef["black"],     # line color when "grays"                                 # 61

# refText parameters
    Ref.Text.cex               = 0.75,                   # NA Ref Text Size                                        # 63
    Ref.Text.col               = colorsRef["black"],     # JP 10/10/12-changed from black to mid green.            # 64
                                          #  5/21/13 - changed back to black.
    Ref.Text.BW.col            = colorsRef["black"],     # Ref Text color when "grays"                             # 65

#__________________________________________________________ 
# working parameters for each panel graphing subfunction within micromapXXXX

# arrow plot parameters
    Arrow.cex                  = 0.08,                   # Size of Arrow.                                          # 66
    Arrow.lwd                  = 2.5,                    ## JP decrease arrow width.                               # 67
    Arrow.Head.length          = 0.08,                   #  Length of arrow head in inches.                        # 68
    Arrow.Shadow.col           = colorsRef["black"],     # NA Not Used.                                            # 69
    Arrow.Shadow.lwd           = 4.0,                    # NA Arrows shadow when border needed. (Not used)         # 70

    Arrow.Dot.pch              = 21,                     # plotting character  (1 circle, 16 dot, 21 filled circle)# 70
    Arrow.Dot.pch.size         = 0.9,                    # dot size            ## JP adjusted dot size.            # 71
    Arrow.Dot.pch.lwd          = 0.5,                    # 0:18 line weight                                        # 72
    
    Arrow.Dot.Outline          = FALSE,                  ## JP added option to control Dot outline.                # 73
    Arrow.Dot.Outline.col      = colorsRef["black"],                                                               # 74
    Arrow.Dot.Outline.lwd      = 0.5,                                                                              # 75

# bar plot parameters
    Bar.barht                  = 2/3,                    # fraction of line spacing                                # 76
    Bar.Outline.col            = colorsRef["black"],     #                                                         # 77
    Bar.Outline.lty            = "solid",                #                                                         # 78
    Bar.Outline.lwd            = 0.5,                    #                                                         # 79

# box plot parameters
    BoxP.thin                  =0.2,                     # was .29     ## JP decreased line width                  # 80
    BoxP.thick                 =0.60,                    # was .58                                                 # 81
   
    BoxP.Use.Black             = FALSE,                  # FALSE = Use the Color for outliners;  TRUE = use black  # 82
  
    BoxP.Median.col            = colorsRef["black"],     ## JP changed to BoxP.Median.col from colMedian-was duplicate-set to black # 83
    BoxP.Median.Line           = 0.80,                   # lwd                                                     # 84

    BoxP.Median.Dot.cex        = 0.95,                   # NA                                                      # 85
    BoxP.Median.Dot.col        = colorsRef["white"],     # NA # JP changed from colDotMedian for clarity           # 86
    BoxP.Median.Dot.lwd        = 2,                      #                                                         # 87
    BoxP.Median.Dot.pch        = 19,                     # NA                                                      # 88
  
    BoxP.Outlier.lwd           = 0.5,                    ## JP decreased dot border line width                     # 89
    BoxP.Outlier.cex           = 0.7,                    # see Dot.pch.size  ## JP decreased dot size  (was .6)    # 90
    BoxP.Outlier.BW.col        = colorsRef["dark gray"], # color for outline when using BW mode                    # 91
    BoxP.Outlier.pch           = 20,                     # NA Outlier symbol                                       # 92
  
    BoxP.Outline.col           = colorsRef["dark gray"], # NA color for outline when using colors                  # 93
  
# segmented bar parameters - centered bar only
    CBar.two.ended             = FALSE,                  #  NA (not implemented)                                   # 94
    CBar.varht                 = FALSE,                  #  (default = fixed height)                               # 95  
    CBar.Center.Line.enable    = FALSE,                  #  NA (not implemented)                                   # 96
    CBar.Center.value          = 0,                      #  Center Bar - center value (def = 0)                    # 97
    CBar.Zero.Line.col         = colorsRef["white"],     #  Center Bar - Zero vertical line color                  # 98
    CBar.Zero.Line.lty         = "dotted",               #  Center Bar - Zero vertical line type                   # 99
    CBar.Zero.Line.lwd         = 1,                      #  Center Bar - Zero vertical line weight                 # 100

# segmented bar parameters for all (segbar, normbar and ctrbar)
    # common parameters for center, segmented and normalized stacked bars.
    CSNBar.barht               =  2/3,                   #  bar heights (percentage of row)                        # 101
    CSNBar.First.barht         = 0.3333,                 # Segmented Bars (Ctr, Seg, Norm) height of first bar in variable height  # 102                                       # 111
    CSNBar.Last.barht          = 0.80,                   # Segmented Bars (Ctr, Seg, Norm) height of last bar in variable height   # 103

    CSNBar.Outline.col         = colorsRef["black"],     #  bar outline border color                               # 104
    CSNBar.Outline.lty         = "solid",                #  bar outline border type                                # 105
    CSNBar.Outline.lwd         = .75,                    #  bar outline border width                               # 106
                                                         # parameters when variable height is requested.
    
# dot plot parameters (dot, dotconf, dotse, dotsignif)
    Dot.pch                    = 21,                     # plotting character  (1 circle, 16 dot, 21 filled circle)# 107
    Dot.pch.size               = 0.9,                    # dot size            ## JP adjusted dot size.            # 108
    Dot.pch.lwd                = 0.5,                    # 0:18 line weight                                        # 109
                                                           
                                                         # 19:25 border parameters
    Dot.Outline                = FALSE,                  ## JP added option to control Dot outline.                # 110
    Dot.Outline.col            = colorsRef["black"],                                                               # 111
    Dot.Outline.lwd            = 0.5,                                                                              # 112

# dot conf parameters
    Dot.Conf.pch               = 21,                     # plotting character for dot confidence                   # 113
    Dot.Conf.pch.size          = 0.9,                    # symbol size                                             # 114
    Dot.Conf.pch.lwd           = 0.5,                    # 0:18 - line weight                                      # 115
    
                                                         # 19:25 - border parameters.
    Dot.Conf.Outline           = FALSE,                  ## JP added option to control Dot outline.                # 116
    Dot.Conf.Outline.col       = colorsRef["black"],                                                               # 117
    Dot.Conf.Outline.lwd       = 0.5,                    # for characters 0:18 - line lwd.                         # 118
                                                         # Confidence line parameters.
    Dot.Conf.lwd               = 2,                                                                                # 119
    Dot.Conf.size              = 0.55,                   # NA Not Used                                             # 120

# dot SE parameters
    Dot.SE.pch                 = 21,                     # plotting character for dot confidence                   # 121
    Dot.SE.pch.size            = 0.9,                    # symbol size                                             # 122
    Dot.SE.pch.lwd             = 0.5,                    # 0:18 line weight                                        # 123
    
                                                         # 19:25 symbol border parameters
    Dot.SE.Outline             = FALSE,                  ## JP added option to control Dot outline.                # 124
    Dot.SE.Outline.col         = colorsRef["black"],     # border line color                                       # 125
    Dot.SE.Outline.lwd         = 0.5,                    # border line weight                                      # 126

                                                         # DotSE confidence line.
    Dot.SE                     = 95,                     # % confidence interval                                   # 127
    Dot.SE.lwd                 = 2,                                                                                # 128
    Dot.SE.size                = 0.55,                   # NA Not Used                                             # 129
                                                         # use default lty, and border color (.col = black)           
# dot signif parameters
    Dot.Signif.pch             = 4,                      # Over print character "x"                                # 130
    Dot.Signif.pch.size        = 0.9*1.2,                # size of over print                                      # 131
    Dot.Signif.pch.lwd         = 0.5,                    # 0:18 line weight                                        # 132
    Dot.Signif.pch.col         = colorsRef["black"],     # color (NA -> follow row color.                          # 133
    
    Dot.Signif.Outline         = FALSE,                  # enable 19:25 border outline                             # 134
    Dot.Signif.Outline.lwd     = 0.5,                    # border line weight                                      # 135
    Dot.Signif.Outline.col     = colorsRef["black"],     # border color                                            # 136
    
    Dot.Signif.pvalue          = 0.05,                   # default p-value test point.                             # 137
    Dot.Signif.range           = c(0,1),                 # p_value range 0 to 1 inclusive                          # 138,139
    
# id area Dot parameters (link - area Lab and Dot)
    Id.Cex.mod                 = 0.75,                   # Fudge adjustment for Id text size. Default = 1          # 140
    
    Id.Title.1.pos             = 0.71,                   # ID column title line # 1 (lines)                        # 141
    Id.Title.2.pos             = 0.01,                   # ID column title line # 2 (lines)                        # 142
                                                 # changed to match up with map titles and X Axis.

    Id.Text.cex                = 0.75,                   ## JP decreased ID text size.                             # 143

    Id.Dot.pch                 = 22,                     # ID Symbol - pch values (19:25) solid and filled symbols, default = filled square  # 144
    Id.Dot.cexm                = 2,                      # multiplier to the Id.Text.cex value for the symbol. Max should be about 3         # 145
    Id.Dot.lwd                 = 0.8,                    # line width applied to solid symbols                     # 146
    Id.Dot.width               = 0.1,                    # inches.   (Size of Dot/Square or Symbol)                # 147

    Id.Dot.Outline.col         = colorsRef["dark gray"], # NA line (border) color on filled symbols (21:25)        # 148
    Id.Dot.Outline.lwd         = 0.8,                    # NA line width of outline on filled symbols (21.25)      # 149

    Id.Space                   = 0.03125,                # width of a space.(inches)                               # 150
    Id.Start                   = 0.055,                  # offset from left edge of column for the center of symbol. (inches) # 151
    
# map parameters 
    Map.Area.Spec.cex          = 0.25,                   # label size for AK, HI, DC in top map.    (.32->.4->.25)      # 152
    Map.Bg.col                 = grey(.88),              # map/state/sub-area background fill color                # 153
    Map.Bg.Line.col            = colorsRef["white"],     # map/state/sub-area background line color (white)        # 154
    Map.Bg.Line.lty            = "solid",                # NA map/state/sub-area background line type              # 155
    Map.Bg.Line.lwd            = 0.3,                    # map/state/sub-area background line weight               # 156
    Map.Fg.Line.col            = colorsRef["black"],     # sub-area foreground line color                          # 157
    Map.Fg.Line.lty            = "solid",                # NA sub-area foreground line type                        # 158
    Map.Fg.Line.lwd            = 0.28,                   # sub-area foreground line weight                         # 159
    Map.L2.Fill.col            = colorsRef["lightest gray"], # L2 region fill color                                # 160
    Map.L2.Line.col            = colorsRef["white"],     # L2 region line color                     (white)        # 161
    Map.L2.Line.lty            = "solid",                # NA L2 region line type                                  # 162
    Map.L2.Line.lwd            = 0.31,                   # L2 region line weight                                   # 163
    Map.L3.Line.col            = colorsRef["black"],     # L3 area outline line color                              # 164
    Map.L3.Line.lty            = "solid",                # NA L3 area outline line type                            # 165
    Map.L3.Line.lwd            = 0.35,                   # L3 area outline line weight                             # 166
    Map.Lab.Box.Width          = 0.09,                   # width and height of the title "boxes" (updated from 0.075 to 0.09 - aug, 2015)  # 167
    Map.Max.width              = 2.5,                    # map max width                                           # 168
    Map.Min.width              = 1.5,                    # map min width should be set portionally to the height of the panel (x axis - about inches)  # 169
    Map.Median.text            = "Median for Sorted Panel", # text for the median single row box.                  # 170

    Map.Panel.col              = "white",                # NA map panel fill color                                 # 171
    Map.Unu.col                = colorsRef["lightest gray"],  # NA map unused sub-area fill color                  # 172

# Rank area parameters
    Rank.width                 = 0.25,                   # rank width of column   (x axis - about inches - working units) # 173
    Rank.method                = 1,                      # rank method                                             * 174

# scatdot parameters 
    SCD.Axis.cex               = 0.75 * .7,              # not used                                                # 175

    SCD.Bg.pch                 =  21,                    # Background symbol pch                                   # 176
    SCD.Bg.pch.fill            =  'transparent',         # Background symbol fill (bg) color (19:25)               # 177
    SCD.Bg.pch.col             =  "black",               # Background symbol border color                          # 178
    SCD.Bg.pch.lwd             =  0.6,                   # Background symbol border line weight                    # 179
    SCD.Bg.pch.size            =  0.75,                  # Background symbol size                                  # 180
    SCD.Fg.pch                 =  21,                    # Foreground symbol pch                                   # 181
    SCD.Fg.pch.col             =  "black",               # Foreground symbol border color                          # 182
    SCD.Fg.pch.lwd             =  0.6,                   # Foreground symbol border line weight                    # 183
    SCD.Fg.pch.size            =  1,                     # Foreground symbol size                                  # 184
    SCD.Median.pch             =  21,                    # median symbol PCH value (21 = filled circle)            # 185
    SCD.Median.pch.fill        = colorsRef["black"],     # median median symbol fill color.                        # 186
    SCD.Median.pch.col         =  "black",               # median symbol border color                              # 187
    SCD.Median.pch.lwd         =  0.6,                   # median symbol border line weight                        # 188
    SCD.Median.pch.size        =  1,                     # median symbol border size (cex)                         # 189
    SCD.hGrid                  = FALSE,                  # draw horizontal grid.                                   # 190
 
    SCD.DiagLine               = TRUE,                   # TRUE, draw a diagonal line in scatter plot at x=y       # 191
    SCD.DiagLine.col           = colorsRef["white"],     # color of diagonal line                                  # 192
    SCD.DiagLine.lwd           = 1.25,                   # width of diagonal line                                  # 193
    SCD.DiagLine.lty           = "solid",                # type of diagnoal line (see "R" line function)           # 194

    SCD.xsc                    = 1.08,                   # NA fudge for margins to try and not clip circles.(not used)# 195
    SCD.ysc                    = 1.12,                   # NA fudge for margins to try and not clip circles.(not used)# 196

# segmented bar parameters - segbar and normbar only
    SNBar.MDot.pch             = 21,                     #  middle point symbol                                    # 197
    SNBar.MDot.pch.border.col  = 'black',                # middle point symbol.border.col with using filled symbols# 198
    SNBar.MDot.pch.border.lwd  = 0.6,                    # middle point symbol border lwd                          # 199
    SNBar.MDot.pch.fill        = colorsRef["white"],     # middle point symbol fill/color                          # 200
    SNBar.MDot.pch.size        = 0.6,                    # middle point symbol size                                # 201
    SNBar.Middle.Dot           = FALSE,                  #  draw dot in middle point of segmented bars (default - no mid-poing dot) # 202

    SNBar.two.ended            = FALSE,                  #  (not implemented)                                      # 203
    SNBar.varht                = FALSE,                  #  (default fixed height)                                 # 204

# ts and tsconf parameters
    TS.Axis.cex                = 0.75 * 0.7,                                                                       # 205
    TS.hGrid                   = FALSE,                                                                            # 206
    TS.lwd                     = 1.1,                    # TS Line weight                                          # 207

# debug parameter
    MST.Debug                  = 0                       # debug switch - for use by developers only. (default=0)  # 208

  )

#
# When something is added or deleted from this structure, change the 
# globalVariables call at the start of this module.
#
#
#  Set up variable in the micromapXXXX namespace - used by micromapXXXX and micromapGSetDefaults functions.
#

     micromapGDefaults = list(colors=mcolors,details=details)

     return(micromapGDefaults)
   }
