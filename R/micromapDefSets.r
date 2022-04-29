###
#
#   micromapGSetPanelDef function to set the panel vectors up for the function execution.
#
#

micromapGSetPanelDef <- function(nRows,rSizeMaj,rSizeMin,rSepGap,MaxRows,UGrpPattern) {

  
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
#  numRows       - number of sub-areas
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
   if (!(is.null(UGrpPattern) || is.na(UGrpPattern))) {
      # User provided grpPattern
      if (sum(UGrpPattern) != sum(GrpPattern)) {
         stop("***0181 CARG-GP User provided grpPattern is invalid. The total of the rows per group must equal the number of rows as the data.")
      } else {   
         # replace generated pattern with user provided pattern
         GrpPattern <- UGrpPattern   
      }
   }
   numGrps <- length(GrpPattern)
   
   
   if (sum(GrpPattern) != vnumRows) stop("Programming problem. GrpPattern has different number of rows then system needs.")
   
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
       
   
  iw = cumsum(c(1,GrpPattern))
  wj <- as.integer(numGrps/2)       # 1/2 numGrps
  wi <- wj*2                        # get rounded value for number of groups (see if odd or even)
   
  #print(paste0("num Grps-wj:",wj,"  rounded-num Grps-wi:",wi))

  vrowSep  <- rep(0,numGrps+1)
  vrowSize <- rep(rSizeMaj,numGrps) 
  
   # generate rowSep and rowSize vectors
   vrowSep
   if (odd(numGrps)) {
      # odd number of groups - we have a median group
    
      # Row Information 
      vrowSep[wj+1] = 1                    # set to 1 around the median group.
      vrowSep[wj+2] = 1                    # above and below.
         
      vgroupedRowSize <- c(rSizeMaj*wj, rSizeMaj, rSizeMaj*wj)  # median group same height as others. due to map.
      vgroupedRowSep  <- c(0,1,1,0)                             # Upper Block, Median, Lower Block   
    
      if (medGrpSize == 1) {                # if median group with 1 row -> no map - special height       
         
         # set middle group's size to the default 1.65 for "median of sorted data" group
         vrowSize[medGrp] = rSizeMin   
            
         # group Row size impacted - Large, one row, Large.
         vgroupedRowSize[2] <- rSizeMin  # units (1.65)
         
      }
         
   } else {
   
      # EVEN number - only middle separator  (2, 4, 6, 8 groups instead of three)
                                           
      # Row information                    $ vrowSep has numGrps + 1 entries.  The middle is 1/2 numGrps + 1
      vrowSep[wj+1] = 1                    # to the center row(grp)  - multiple by sep size later.
      
      vgroupedRowSize <- c(rSizeMaj*wj, rSizeMaj*wj)  # upper half and lower half
      vgroupedRowSep  <- c(0,1,0)               # top Sep, middle Sep, bot Sep
   
   }
   
   #
   #  So,  numRowMed > 0 when a single row is the median group/row
   #       numGrpMed > 0 when 1 or more rows is the median group/row
   #
   
   #  Convert the separators from markers to inch values.
   vgroupedRowSep <- vgroupedRowSep * rSepGap
   vrowSep        <- vrowSep * rSepGap          

   
   DetailsPanel <- list(
   
      numRows        = vnumRows,        #* number of rows (sub-areas)
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
      
      ib = iw[1:length(iw)-1],          #*
      ie = iw[2:length(iw)]-1           #*
    
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

# Candidate colors________________________________________
colorsRefRgb = matrix(c(
 1.00,1.00,1.00,  # white            "#FFFFFF"               # borders
  .95, .95, .95,  # lightest gray    "#F2F2F2" or "gray95"   # L2 area background
  .92, .92, .92,  # lighter gray     "#EBEBEB" or "gray92"   # changed from .90  # inactive area background
  .78, .78, .78,  # light gray       "#C7C7C7" or "gray78"   # changed from .80
  .50, .50, .50,  # middle gray      "#7F7F7F" or "gray50"
  .30, .30, .30,  # dark gray        "#4D4D4D" or "gray30"  
  .00, .00, .00,  # black            "#000000" or "black"    # borders
 
  .93,1.00, .93,  # light green
  .00, .50, .00,  # mid green
 1.00,1.00, .84,  # light yellow foreground  
  .90, .80,1.00,  # bright yellow foreground 
  .80, .90,1.00,  # light green blue 
  .60, .70, .85), # mid green blue
  ncol=3,byrow=TRUE)

colorsRef = grDevices::rgb(colorsRefRgb[,1],colorsRefRgb[,2],colorsRefRgb[,3])
names(colorsRef) = c("white","lightest gray","lighter gray","light gray",
                     "mid gray","dark gray", "black",
                     "light green","mid green",
                     "light yellow","bright yellow",
                     "light green blue","mid green blue")           

# row color table________________________________________________

colorsRgb = matrix(c(                              # the basic 7 (9) colors.
 1.00, .15, .15,     #region 1: red	            1  #D53E4F - Red
  .90, .55, .00,     #region 2: orange	            2  #FC8D59 - Brn/Org
  .00, .65, .00,     #region 3: green	            3  #FEE08B - Pale Brn
  .20, .50,1.00,     #region 4: greenish blue       4  #99D594 - Pale Green
  .50, .20, .70,     #region 5: lavendar            5  #3288BD - Blue
  .88, .20, .59,     #region 6: magenta             6            (Added)
  .00, .00, .00,     #region 7: black for median    7  #000000 - Black
 1.00,1.00, .80,     #non-highlighted foreground    8  #E6F598 - Pale Yellow
 1.00, .9875,0.95,   #upper shade - very pale red   9  #FFFCF2   (Added)
  .955,.98,1.00,     #lower shade - very pale blue 10  #F4FAFF   (Added)
  .95, .95, .95,     #lightest gray - not referenced sub-area        11 
  .92, .92, .92),    #lighter gray  - non-active backgroup sub-area  12
  ncol=3,byrow=TRUE)

#
#   colors  1 - 6     are sub-area active colors           (unused items = NA)
#   color   7         is black for median row accent       (7th, item in gsubs list)
#   colors  8,  9, 10 are fill colors for median features. (8-pale yellow, 9-pale red, 10-pale blue)
#   colors 11, 12     are background colors for not-referenced and not active.
#
#   Need to figure out how to add and what to add as 6th sub-area color.
#

colors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3]),            # solid colors (12)        format =>  "#FFFFFFF"
            grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],.2))         # translucent colors (12) - 20%.

names(colors) =c("red","orange","green","greenish blue", "purple","magenta",
                 "black","light yellow","light red","light blue",
                 "lightest gray","lighter gray",
                 "l_red","l_orange","l_green","l_greenish blue", "l_purple","l_magenta",
                 "l_black","l_light yellow","l_light red","l_light blue",
                 "l_lightest gray", "l_lighter gray"
                 )       


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

#tempne           <- 5                                # number of areas per panel   ## delete
tempOutline.Line.col <- colorsRef["white"]            # grid and outline line color

tempcolFill       <- colorsRef["lighter gray"]        # panel and default fill color
tempcolSubFill    <- colorsRef["lightest gray"]

tempText.cex      <- 0.75                       # 12 pt default -> 9 pt.


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
    topMar                     = 1.1,                 # margin panel height (inches)                               #  1

    botMar                     = 0.5,                 # no legend bottom margin (inches)                           #  2
    botMarLegend               = 0.75,                #                                                            #  3
    botMardif                  = 0.2,                 # maybe not needed                                           #  4   

    leftMar                    = 0,                   #                                                            #  5 
    leftMarAxis                = 0.2,                 # left margin adjustment when Y axis is printed              #  6
  
    rightMar                   = 0,                   #                                                            #  7

    borderSize                 = 0.5,                 # margin border - at least 0.5 inches.                       #  8
    
    #  height constraints
    rowSepGap                  = 0.1,                 # Size of the rowSep  (in inches)                            #  9
    
    rowSizeMn                  = 0.5,                 # Minimum Row Size in inches                                 # 11   # overriden by data from BG areaParms (add validation check)  Map.MinH
    rowSizeMx                  = 1.25,                # Maximum Row Size in inches.                                # 10   # overriden by data from BG areaParms     Map.MaxH
 
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

# panel scaling
    
    sc                         = 1.08,                   # x and y axis scale expansion factor                     # 19
    pad                        = 0.67,                   # y axis padding for integer plotting locates (units)     # 20
                                       # ry = c(1-pad,ke+pad),ke = no. items in panel (units)  
    padex                      = 0.34,                   # total panel padding                                     # 21
                                       # (.67-.5)=.17 padding at top and bottom of panel         
    padMinus                   = 0.63,                   # .67 - .04 # keep reference line off panel edge          # 22

# mtext line placement (Titles)

    ##  JP adjusted placement of lines (titles) (units are "lines")
    Title.Line.1.pos           = 0.01+(0.7*0.9)*2,       # top panel 1st line placement (lines=1.27) (delta 0.80)  # 23  (not used)
    Title.Line.2.pos           = 0.01+(0.7*0.9)*1,       # top panel 2nd line placement (lines=0.64)               # 24  (not used)
    Title.Line.2x.pos          = 0.01,                   # (USED) top panel 3rd line placement (lines) also Tick Labs placement # 25
 
    Title.Line.3x.pos          = 0.01,                   # ??? bottom panel 3x line placement (axis)               # 26
    Title.Line.3.pos           = 0.01+(0.7*0.9)*1,       # ??? bottom panel 3 line placement  (lines) (title)      # 27  (not used)
    Title.Line.4.pos           = 0.01+(0.7*0.9)*2,       # ??? bottom panel-reference line  (lines) (refText and line) # 28 (not used)
   
    Title.Line.5.pos           = 0.35,                   # (USED) Y axis titles for ScatDot and TS. (lines)        # 29

    Title.cex                  = 1.0,                    #                                                         # 30  (not used)
    
    # Title.Line.2x.pos is only used in glyphs that do not have x-axis (the maps and id).
    # The X-axis line is independent.  However, we now want Title.line.1.pos to 
    # match across the entire page.
  
# grid line parameters
    Grid.Line.col              = tempOutline.Line.col,   # grid line color                                         # 31
    Grid.Line.lwd              = 1,                      # weight of grid line                                     # 32
 
    mgpTop                     = c(3.2,    0.1,  0),     # label & gridline (tick) placement (changed from 2,0.1,0)# 33  (not used)(margin spacing for title and axis = (title=2 lines, tick 0.1 lines, and 0 lines)
    mgpBottom                  = c(3.2,    0.1,  0),     # label & gridline (tick) placement (changed from 2,0,0)  # 34  (not used)
    padjBottom                 = -0.35,                  # gridline (tick  placement                               # 35  (not used) Was -0.7 adjusted 11/14
    mgpLeft                    = c(0.75, 0.1,  0),       # left axis labels                                        # 36  (used TS and SCD)

    ###  End of sizing of areas...
    
# axis parameters.

    XAxis.L.mcex               = 0.8888889,              # font size multiplier for X axis large labels (8 pt)     # 38
    XAxis.M.mcex               = 0.7777778,
    XAxis.S.mcex               = 0.6666667,              # font size multiplier for X axis small staggered and scaled (6 pt) # 37 
    XAxis.Sp.mcex              = 0.2,                    # Labels to Axis spacing.

    XAxis.offset               = 0.0,                    # offset for X Axis above plotting area.                  # 39
    XAxis.indent               = 10,                     # indent outside labels by x/1000 of width                # 40
    XAxis.nGridpIn             = 3.4,                    # X Axis - Grid lines per Inch max.                       # 41
    XAxis.gapPC                = 0.75,                   # percentage of character width between columns (buffer)
    XAxis.staggered            = TRUE,                   # straight or stagger labels.                             # 42

    YAxis.cex                  = 0.333333,               # font size (relative) for Y axis labels (4 pt)           # 43
    YAxis.offset               = 0.0,                    # offset for Y Axis above plotting area.                  # 44
    YAxis.nGridpIn             = 5,                      # X Axis - Grid lines per Inch max.                       # 45
    YAxis.staggered            = TRUE,                   # straight or stagger labels.                             # 46

# panel column width allocation
    ### no change - scale from 7.5 inches by 10 inches
    
    YAxis.width                = 0.2,                    # width for Y axis labels. (X axis-about inches-working units) # 47

# axis labeling

    staggered                  = FALSE,                  # intra column flags - staggered state of previous column
    
# panels
    Panel.Fill.col             = tempcolFill,            # panel fill color                                        # 48
    Panel.Outline.col          = colorsRef["black"],     # panel outline color                                     # 49

# Title and Text - cex for character size
    Text.cex                   = tempText.cex,  ## JP decreased text size.  Used almost everywhere.                # 50

# refVals parameters

    # see padMinus above for other parameters 
    Ref.Val.lty                = "dashed",               # line type for Ref Value (dashed)                        # 51
    Ref.Val.lwd                = 1.5,                    # line weight for Ref Value                               # 52
    Ref.Val.col                = colorsRef["mid green"], # line color                                              # 53
    Ref.Val.BW.col             = colorsRef["black"],     # line color when "grays"                                 # 54

# refText parameters
    Ref.Text.cex               = tempText.cex,           # Ref Text Size                                           # 55
    Ref.Text.col               = colorsRef["black"],     # JP 10/10/12-changed from black to mid green.            # 56
                                          #  5/21/13 - changed back to black.
    Ref.Text.BW.col            = colorsRef["black"],     # Ref Text color when "grays"                             # 57

#__________________________________________________________ 
# working parameters for each panel graphing subfunction within micromapXXXX

# arrow plot parameters
    Arrow.cex                  = 0.08,                   # Not Used                                                # 58
    Arrow.Head.length          = 0.08,                   #  Length of arrow head in inches.                        # 59
    Arrow.lwd                  = 2.5,                    ## JP decrease arrow width.                               # 60
    Arrow.Shadow.col           = colorsRef["black"],     # Not Used.                                               # 61
    Arrow.Shadow.lwd           = 4.0,                    # Arrows shadow when border needed. (Not used)            # 62

    Arrow.Dot.pch              = 21,                     # plotting character  (1 circle, 16 dot, 21 filled circle)# 94
    Arrow.Dot.pch.size         = 0.9,                    # dot size            ## JP adjusted dot size.            # 95
    Arrow.Dot.pch.lwd          = 0.5,                    # 0:18 line weight                                        # 96
    
    Arrow.Dot.Outline          = FALSE,                  ## JP added option to control Dot outline.                # 97
    Arrow.Dot.Outline.col      = colorsRef["black"],                                                               # 98
    Arrow.Dot.Outline.lwd      = 0.5,                                                                              # 99

# bar plot parameters
    Bar.barht                  = 2/3,                    # fraction of line spacing                                # 63
    Bar.Outline.col            = colorsRef["black"],     #                                                         # 64
    Bar.Outline.lty            = "solid",                #                                                         # 65
    Bar.Outline.lwd            = 0.5,                    #                                                         # 66

# box plot parameters
    BoxP.thin                  =0.2,                     # was .29     ## JP decreased line width                  # 67
    BoxP.thick                 =0.60,                    # was .58                                                 # 68
   
    BoxP.Use.Black             = FALSE,                  # FALSE = Use the Color for outliners;  TRUE = use black  # 69
  
    BoxP.Median.col            = colorsRef["black"],     ## JP changed to BoxP.Median.col from colMedian-was duplicate-set to black # 70
    BoxP.Median.Line           = 0.80,                   # lwd                                                     # 75

    BoxP.Median.Dot.cex        = 0.95,                   #                                                         # 71
    BoxP.Median.Dot.col        = colorsRef["white"],     ## JP changed from colDotMedian for clarity               # 72
    BoxP.Median.Dot.lwd        = 2,                      #                                                         # 73
    BoxP.Median.Dot.pch        = 19,                     #                                                         # 74
  
    BoxP.Outlier.BW.col        = colorsRef["dark gray"], # color for outline when using BW mode                    # 76
    BoxP.Outlier.cex           = 0.7,                    # see Dot.pch.size  ## JP decreased dot size  (was .6)    # 77
    BoxP.Outlier.lwd           = 0.4,                    ## JP decreased dot border line width                     # 78
    BoxP.Outlier.pch           = 20,                     # Outlier symbol                                          # 79
  
    BoxP.Outline.col           = colorsRef["dark gray"], # color for outline when using colors                     # 80
  
# segmented bar parameters - centered bar only
    CBar.two.ended             = FALSE,                  #  (not implemented)                                      # 81
    CBar.varht                 = FALSE,                  #  (default = fixed height)                               # 82  
    CBar.Center.Line.enable    = FALSE,                  #  (not implemented)                                      # 83
    CBar.Center.value          = 0,                      #  Center Bar - center value (def = 0)                    # 84
    CBar.Zero.Line.col         = colorsRef["white"],     #  Center Bar - Zero vertical line color                  # 85
    CBar.Zero.Line.lty         = "dotted",               #  Center Bar - Zero vertical line type                   # 86
    CBar.Zero.Line.lwd         = 1,                      #  Center Bar - Zero vertical line weight                 # 87

# segmented bar parameters for all (segbar, normbar and ctrbar)
    # common parameters for center, segmented and normalized stacked bars.
    CSNBar.barht               =  2/3,                   #  bar heights (percentage of row)                        # 88
    CSNBar.First.barht         = 0.3333,                 # Segmented Bars (Ctr, Seg, Norm) height of first bar in variable height  # 89                                       # 111
    CSNBar.Last.barht          = 0.80,                   # Segmented Bars (Ctr, Seg, Norm) height of last bar in variable height   # 90

    CSNBar.Outline.col         = colorsRef["black"],     #  bar outline border color                               # 91
    CSNBar.Outline.lty         = "solid",                #  bar outline border type                                # 92
    CSNBar.Outline.lwd         = .75,                    #  bar outline border width                               # 93
                                                         # parameters when variable height is requested.
    
# dot plot parameters (dot, dotconf, dotse, dotsignif)
    Dot.pch                    = 21,                     # plotting character  (1 circle, 16 dot, 21 filled circle)# 94
    Dot.pch.size               = 0.9,                    # dot size            ## JP adjusted dot size.            # 95

    Dot.pch.lwd                = 0.5,                    # 0:18 line weight                                        # 96
                                                           
                                                         # 19:25 border parameters
    Dot.Outline                = FALSE,                  ## JP added option to control Dot outline.                # 97
    Dot.Outline.col            = colorsRef["black"],                                                               # 98
    Dot.Outline.lwd            = 0.5,                                                                              # 99

# dot conf parameters
    Dot.Conf.pch               = 21,                     # plotting character for dot confidence                   #100
    Dot.Conf.pch.size          = 0.9,                    # symbol size                                             #101

    Dot.Conf.pch.lwd           = 0.5,                    # 0:18 - line weight                                      #102
    
                                                         # 19:25 - border parameters.
    Dot.Conf.Outline           = FALSE,                  ## JP added option to control Dot outline.                #103
    Dot.Conf.Outline.col       = colorsRef["black"],                                                               #104
    Dot.Conf.Outline.lwd       = 0.5,                    # for characters 0:18 - line lwd.                         #105                             # 85

                                                         # Confidence line parameters.
    Dot.Conf.lwd               = 2,                                                                                #106
    Dot.Conf.size              = 0.55,                   # Not Used                                                #107

# dot SE parameters
    Dot.SE.pch                 = 21,                     # plotting character for dot confidence                   #108
    Dot.SE.pch.size            = 0.9,                    # symbol size                                             #109
 
    Dot.SE.pch.lwd             = 0.5,                    # 0:18 line weight                                        #110
    
                                                         # 19:25 symbol border parameters
    Dot.SE.Outline             = FALSE,                  ## JP added option to control Dot outline.                #111
    Dot.SE.Outline.col         = colorsRef["black"],     # border line color                                       #112
    Dot.SE.Outline.lwd         = 0.5,                    # border line weight                                      #113

                                                         # DotSE confidence line.
    Dot.SE                     = 95,                     # % confidence interval                                   #114
    Dot.SE.lwd                 = 2,                                                                                #115
    Dot.SE.size                = 0.55,                   # Not Used                                                #116
                                                         # use default lty, and border color (.col = black)           
# dot signif parameters
    Dot.Signif.pch             = 4,                      # Over print character "x"                                #117
    Dot.Signif.pch.size        = 0.9*1.2,                # size of over print                                      #118
    Dot.Signif.pch.lwd         = 0.5,                    # 0:18 line weight                                        #119
    Dot.Signif.pch.col         = colorsRef["black"],     # color (NA -> follow row color.                          #120
    
    Dot.Signif.Outline         = FALSE,                  # enable 19:25 border outline                             #121
    Dot.Signif.Outline.lwd     = 0.5,                    # border line weight                                      #122
    Dot.Signif.Outline.col     = colorsRef["black"],     # border color                                            #123
    
    Dot.Signif.pvalue          = 0.05,                   # default p-value test point.                             #124
    Dot.Signif.range           = c(0,1),                 # p_value range 0 to 1 inclusive                          #125,126
    
# id area Dot parameters (link - area Lab and Dot)
    Id.Cex.mod                 = 1,                      # Fudge adjustment for Id text size. Default = 1          #127

    Id.Dot.pch                 = 22,                     # ID Symbol - pch values (19:25) solid and filled symbols, default = filled square  #128
    Id.Dot.cexm                = 1.5,                    # multiplier to the Id.Text.cex value for the symbol. Max should be about 3         #129
    Id.Dot.lwd                 = 0.8,                    # line width applied to solid symbols                     #130
    Id.Dot.Outline.col         = colorsRef["dark gray"], # line (border) color on filled symbols (21:25)           #131
    Id.Dot.Outline.lwd         = 0.8,                    # line width of outline on filled symbols (21.25)         #132
    Id.Dot.width               = 0.1,                    # inches.   (Size of Dot/Square or Symbol)                #133

    Id.Space                   = 0.03125,                # width of a space.(inches)                               #134
    Id.Start                   = 0.055,                  # offset from left edge of column for the center of symbol. (inches) #135

    Id.Text.cex                = 0.65,                   ## JP decreased ID text size.                             #136
    
    Id.Title.1.pos             = 0.9,                    # ID column title line # 1 (lines)                        #137
    Id.Title.2.pos             = 0.1,                    # ID column title line # 2 (lines)                        #138
    
# map parameters  
    Map.Area.Spec.cex          = 0.32,                   # label size for AK, HI, DC in top map.                   #139
    Map.Bg.col                 = grey(.88),              # map/state/sub-area background fill color                #140
    Map.Bg.Line.col            = tempOutline.Line.col,   # map/state/sub-area background line color (white)        #141
    Map.Bg.Line.lty            = "solid",                # map/state/sub-area background line type                 #142
    Map.Bg.Line.lwd            = 0.5,                    # map/state/sub-area background line weight               #143
    Map.Fg.Line.col            = colorsRef["black"],     # sub-area foreground line color                          #144
    Map.Fg.Line.lty            = "solid",                # sub-area foreground line type                           #145
    Map.Fg.Line.lwd            = 0.5,                    # sub-area foreground line weight                         #146
    Map.L2.Fill.col            = tempcolSubFill,         # L2 region fill color                                    #147
    Map.L2.Line.col            = tempOutline.Line.col,   # L2 region line color                     (white)        #148
    Map.L2.Line.lty            = "solid",                # L2 region line type                                     #149
    Map.L2.Line.lwd            = 0.5,                    # L2 region line weight                                   #150
    Map.L3.Line.col            = colorsRef["black"],     # L3 area outline line color                              #151
    Map.L3.Line.lty            = "solid",                # L3 area outline line type                               #152
    Map.L3.Line.lwd            = 0.5,                    # L3 area outline line weight                             #153
    Map.Lab.Box.Width          = 0.09,                   # width and height of the title "boxes" (updated from 0.075 to 0.09 - aug, 2015)  #154
    Map.Max.width              = 2.5,                    # map max width                                           #155
    Map.Median.text            = "Median for Sorted Panel", # text for the median single row box.                  #156
    Map.Min.width              = 1.5,                    # map min width should be set portionally to the height of the panel (x axis - about inches)  #157
    Map.Panel.col              = "white",                # map panel fill color                                    #158
    
    Map.Unu.col                = colorsRef["lightest gray"],  # map unused sub-area fill color                     #159

# Rank area parameters
    Rank.width                 = 0.25,                   # rank width of column   (x axis - about inches - working units) #160
    Rank.method                = 1,                      # rank method                                             *161

# scatdot parameters 
    SCD.Axis.cex               = tempText.cex * .7,      # not used                                                #162

    SCD.Bg.pch                 =  21,                    # Background symbol pch                                   #163
    SCD.Bg.pch.fill            =  'transparent',         # Background symbol fill (bg) color (19:25)               #164
    SCD.Bg.pch.col             =  "black",               # Background symbol border color                          #165
    SCD.Bg.pch.lwd             =  0.6,                   # Background symbol border line weight                    #166
    SCD.Bg.pch.size            =  0.75,                  # Background symbol size                                  #167
    SCD.Fg.pch                 =  21,                    # Foreground symbol pch                                   #168
    SCD.Fg.pch.col             =  "black",               # Foreground symbol border color                          #169
    SCD.Fg.pch.lwd             =  0.6,                   # Foreground symbol border line weight                    #170
    SCD.Fg.pch.size            =  1,                     # Foreground symbol size                                  #171
    SCD.Median.pch             =  21,                    # median symbol PCH value (21 = filled circle)            #172
    SCD.Median.pch.fill        = colorsRef["black"],     # median median symbol fill color.                        #173
    SCD.Median.pch.col         =  "black",               # median symbol border color                              #174
    SCD.Median.pch.lwd         =  0.6,                   # median symbol border line weight                        #175
    SCD.Median.pch.size        =  1,                     # median symbol border size (cex)                         #176
    SCD.hGrid                  = FALSE,                  # draw horizontal grid.                                   #177
 
    SCD.DiagLine               = TRUE,                   # TRUE, draw a diagonal line in scatter plot at x=y       #178
    SCD.DiagLine.col           = tempOutline.Line.col,   # color of diagonal line                                  #179
    SCD.DiagLine.lwd           = 1.25,                   # width of diagonal line                                  #180
    SCD.DiagLine.lty           = "solid",                # type of diagnoal line (see "R" line function)           #181

    SCD.xsc                    = 1.08,                   # fudge for margins to try and not clip circles.(not used)#182
    SCD.ysc                    = 1.12,                   # fudge for margins to try and not clip circles.(not used)#183

# segmented bar parameters - segbar and normbar only
    SNBar.MDot.pch             = 21,                     #  middle point symbol                                    #184
    SNBar.MDot.pch.border.col  = 'black',                # middle point symbol.border.col with using filled symbols#185
    SNBar.MDot.pch.border.lwd  = 0.6,                    # middle point symbol border lwd                          #186
    SNBar.MDot.pch.fill        = colorsRef["white"],     # middle point symbol fill/color                          #187
    SNBar.MDot.pch.size        = 0.6,                    # middle point symbol size                                #188
    SNBar.Middle.Dot           = FALSE,                  #  draw dot in middle point of segmented bars (default - no mid-poing dot) #189

    SNBar.two.ended            = FALSE,                  #  (not implemented)                                      #190
    SNBar.varht                = FALSE,                  #  (default fixed height)                                 #191

# ts and tsconf parameters
    TS.Axis.cex                = tempText.cex * 0.7,                                                               #192
    TS.hGrid                   = FALSE,                                                                            #193
    TS.lwd                     = 1.1,                    # TS Line weight                                          #194

# debug parameter
    MST.Debug                  = 0                       # debug switch - for use by developers only. (default=0)  #195

  )

#
# When something is added or deleted from this structure, change the 
# globalVariables call at the start of this module.
#
#
#  Set up variable in the micromapXXXX namespace - used by micromapXXXX and micromapGSetDefaults functions.
#

     micromapGDefaults = list(colors=colors,details=details)

     return(micromapGDefaults)
   }
