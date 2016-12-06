#
# Release Version 130602 build - V1.0 (micromapST)
# Release Version 141107 build - V1.1 (micromapSEER)
# Release Version 150127 build - V1.1 (micromapST (super version))
#     Adjustments for dynamic panel setup.
#     Updated logic to enforce min and max panel sizes.
#     Allow legal paper sideways.
#
# Release Version 150726 build - V1.2 (update for variable columns and maps)
#


panelFill <- function(col="#D0D0D0",border=NA,...)

{
   # fill a panel specified by "usr" with fill color of "col" and and outline color of "border="
    xy <- par("usr")               # get usr data with polygon points.  Fil with "col"
    polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)],col=col,border=border,xpd=TRUE,...)
}


panelGrid <- function(x = NULL, y = NULL, col = 2, lwd = 1, lty = 1)
{
  # place grids in panel.  If x present = vertical grids
  #                        if y present = horizontal grids  
  #                        if x and y present = both grids
     if(!is.null(x))
		abline(v = x, lwd = lwd, lty = lty, col = col)
     if(!is.null(y))
		abline(h = y, lwd = lwd, lty = lty, col = col)
}


panelInbounds <- function(bnds) {
   #  bnds = min and max of panel boundaries.
   #  times potential pretty to panel limits.
      grid = pretty(bnds)    # range + defaults of n=5  (number of intervals) 
                             #                     min.n = n %/% 3, minimal number of intervals. (multiple of 3) 
                             #                     shrink.sml = 0.75,  
                             #                     high.u.bias = 1.5, (>1) interval units
                             #                     u5.bias = .5 + 1.5*high.u.bias,   multipler favoring 5 over 2.
                             #                     eps.correct = 0   (0,1,2) 
   return(grid[bnds[1] < grid & grid < bnds[2]])  # grid values must be internal to the range 
}


panelLengthen <- function(x, n=1) {

#   x = original vector to lengthen 
#   n = number of entries in new vector. 
#  expand number of columns or rows to "n", but replicated data may not be right..  WATCH!.
#    data in original vector is to be replicated to new elements. 
#  If no original data, zeroes are provided.
 
   if(n<1) {
      stopCnt()
      StopFnd <- TRUE
      xmsg <- "***0450 PANEL panelLengthen - invalid vector length. < 1"
      stop(xmsg, call.=FALSE)      
   }
   if(length(x)==0) return(rep(0,n))    # original vector length = 0,  return vector with "n" zeros.
   newx = rep(x,ceiling(n/length(x)))   # repeat x into new space. (multiple length of original)
   length(newx) = n                     # set results to length of n  (trim to "n" length)
   return(newx)
}


panelOutline <- function(col = "black", lwd = 1, lty = 1) {

   # Outline panel in "col". Current panel = "usr"
	xy <- par("usr")            # get window size (save to reuse)
	polygon(xy[c(1, 2, 2, 1)], xy[c(3, 3, 4, 4)], density=0, col=col, xpd=TRUE)
}

panelScale <-  function(rx = c(0, 1), ry = c(0, 1), inches = FALSE)  {
   #  Set scale of panel.
   #  If inches - set rx and ry to current inches values and return.
   #  Do "new=TRUE" plot to set the scale.  (could use plot.window?)
   #  firstp does not appear to be used.

	if(inches) {
	   pin = par("pin")
	   rx = c(0, pin[1])
	   ry = c(0, pin[2])
	}
	warn = unlist(options('warn'))
        options(warn=-1)   # turn off warnings
        
        par(new=TRUE)
        options(warn=warn) # turn back on warnings
        
	plot(rx, ry, type = "n", axes = FALSE, xaxs = "i", yaxs = "i", 
		xlab = "", ylab = "", main = "")
	return(list(rx = rx, ry = ry))
}


panelSelect <- function(layout, i = 1, j = 1, margin = NULL) {
   #
   # Panel Select
   #
   #    Layout = panel structure
   #       dim = dimensions of panel = c(i,j).  If i or j > respective dimension - fatal.
   #      If no margin specified---
   #       datfig = par(fig <- data)
   #       pad = par(mai = layout$lpad[c(4,1,3,2)] # reorder
   #      if margin specified---
   #       labfig = par(fig = layout$labfig[ind,]) # based on margin 
   #       brd = par(mai = layout$brd[c(4,1,3,2)] # reorder 
   #         par(fig = c(a,b,c,d)...  NDC coordinates of figure region in the display.  
   #                                  new plot, unless new=TRUE?
   #         par(mai = c(b,l,t,r)...  numerical vector margin size (bot, left, top, right) in inches.
   #    i =     row index
   #    j =     column index
   #
   #    margin = left, right, top, bottom, bot,...
   #
   #    returned values = none
   #

	dim = layout$dim
	if(i > dim[1] || j > dim[2])    # is i and j within range.
	    stop("***0451 PANEL panelSelect - Dimension error. Program error - index 'i' or 'j' is out of bounds.", call.=FALSE)
	
	if(is.null(margin)) {               # "margin" is missing. (normal call for panelGroup or panels)
            k = dim[2] * (i - 1) + j
	    par(fig = layout$datfig[k,  ], 
	        mai = layout$lpad[c(4, 1, 3, 2)] )
	} else {
	    # panelOne call.
	    vec = 1:4
	    nam = c("left", "right", "top", "bottom", "bot")
	    ind = match(margin, nam)
	    if(is.na(ind))
		stop("***0452 PANEL panelSelect - Bad label region name.  Must be left, right, top or bottom.", call.=FALSE)
	    if(ind == 5)   ind = 4      # "bot" -> "bottom"
	    par(fig = layout$labfig[ind,  ], 
	        mai = layout$brd[c(4, 1, 3, 2)] )
	}
#	"done"
}


panelLayout <-
function(vnrow = 1, 
         vncol = 1, 
         leftMargin  = 0,                     # inches
         rightMargin = 0,                     # inches
         topMargin   = 1,                     # inches, leave room for page titles. 
         bottomMargin = 0,                    # inches
         borders     = rep(.5, 4),            # inches 
         # The figure borders are left(1), right, top, bottom(4)
         colSize     = 1, 
         rowSize     = 1, 
         colSep      = 0, 
         rowSep      = 0,
         rSizeMx     = 2,
         rSizeMn     = 0.5,
         rSizeMaj    = 7,
         rMapCol     = NULL,
         lpad        = NULL,
         disErr      = FALSE,
         rDebug      = NULL
         )
{
  #
  #  Function to Generate a panel layout structure based on column and row parameters
  #
  #    Margins
  #    borders
  #    colSize  = vector - one entry per column   (inches)
  #    rowSize  = vector - one entry per panel/row (group) (units, either 1.67 or 7)
  #    colSep   = vector or single - spacing between columns (inches)
  #    rowSep   = vector - spacing between panel/rows (groups) (? inches)
  #    rSizeMx  = Maximum Size of panel/row - inches
  #    rSizeMn  = Minimum Size of panel/row - inches
  #    rSizeMaj = Size of major panel/rows - units
  #    rMapCol  = vector of column numbers containing "maps" or "id"
  #    lpad     = padding
  #    disErr   = T/F  - disable error checking and messages.
  #    rDebug   = debug flag
  #cat("panelLayout parameters:\n")
  #cat("  colSize :",colSize,"\n")
  #cat("  rowSize :",rowSize,"\n")
  #cat("  colSep  :",colSep,"\n")
  #cat("  rowSep  :",rowSep,"\n")
  #cat("  rSizeMx :",rSizeMx,"\n")
  #cat("  rSizeMn :",rSizeMn,"\n")
  #cat("  rSizeMaj:",rSizeMaj,"\n")
  #cat("  rMapCol :",rMapCol,"\n")
  #cat("  lpad    :",lpad,"\n")
  #cat("  rDebug  :",rDebug,"\n\n")
  
  #
  #  Used for panels, panelGroup, panelOne
  #   panels have multiple columns and panel/rows for each glyphic.  the individual panels are single glyphics
  #   panelGroups have two or three panels per column representing the above median areas, the 
  #     below median areas, and the single area at the median (if odd number of areas).
  #   panelOne has one column covering all columns, and one row, covering all rows.
  #
  enableErr <- !disErr
  
  #cat("borders    :",borders,"\n")
  #cat("leftMargin :",leftMargin,"\n")
  #cat("rightMargin:",rightMargin,"\n")
  #cat("topMargin  :",topMargin,"\n")
  #cat("bottomMargin:",bottomMargin,"\n")
  
  # Note fig matrices rounded to 6 places in an effort of avoid a R problem with fig when
  #  values appear in exponential notation.

    	oldpar = par()                       # save original par values.
	din = oldpar$din                     # get device dimensions (inches)
	din.x = din[1]                       #  x = width
	din.y = din[2]                       #  y = height
	#cat("pL din:",din,"\n")

        # page >>  b[1] | leftM | plotX | rightM | b[2] = din.x
	plotX = din.x - borders[1] - borders[2] - leftMargin - rightMargin  # usable width inches
        #cat("plotX:",plotX,"\n")

        # page >>  b[4] | topM  | plotY | bottomM | b[3] = din.y
	plotY = din.y - borders[3] - borders[4] - bottomMargin - topMargin  # usable height inches
        #cat("plotY:",plotY,"\n")	

	# bounds (x1, x2, y1, y2)
	
	#   plotY and plotX is the plotting sapce after margins and borders are removed.

        #
        #  inches of box within "borders"  (include margins)	
        #
	#   bounds (edge left, margin left, margin right, edge right) shifted right by "borders[1]"
	
	xbnds = c(0, leftMargin, leftMargin + plotX, leftMargin + plotX + 
		rightMargin) + borders[1]  #shift all by the left borders
        #cat("xbnds:",xbnds,"\n")
	
	#  [1] >>   |
	#  [2] >>   |   LM    |
	#  [3] >>   |   LM    |   plotX   |
	#  [4] >>   |   LM    |   plotX   |   RM   |
	#  
	#   All adjusted border[1] +
	#      0--------LM---------------------RM---------Rpt  + borders[1]
	#           1         2           3         4
	# 
	#  [1] >>   | b[1]  |
	#  [2] >>   | b[1]  |   LM    |
	#  [3] >>   | b[1]  |   LM    |   plotX   |
	#  [4] >>   | b[1]  |   LM    |   plotX   |   RM   |
	#           | b[1]  |   LM    |   plotX   |   RM   |  b[2]  }
	#
	#      
	#   bounds (edge bottom, margin bottom, margin top, edge top) shifted up by "borders[4]"

	ybnds = c(0, bottomMargin, bottomMargin + plotY, bottomMargin + 
		plotY   + topMargin) + borders[4] # shift all by bottom border
        #cat("ybnds:",ybnds,"\n")
    
	#   Note: on y axis, 0 is bottom of area.
	#
	#      0--------BM----------------------------TM---------Tpt  + borders[4]
	
	#
	#           |  b[3]  |  BM    |  ploty    |    TM  |   b[4]  | 
 
	#
	#
	# the right and top borders are handled in the first calculation.
	
	#  fig.scale = inches coordinates of device space.
	
	fig.scale = c(din.x, din.x, din.y, din.y)    # inches of current device/space	
        #cat("fig.scale:",fig.scale,"\n")
	
        #  for entire page including borders and margins
                
        # left figure is in the left margin space of the plot area from top to bottom
	#   from left edge (0) to leftmargin + borders, 
	#   from bottom edge to top edge 
	#   So, outlines the left margin space (left to plot and bottom to top) with 
	#   borders around the area.
	#
	leftfig = c(xbnds[1] - borders[1], xbnds[2] + borders[2], ybnds[1] - 
		borders[4], ybnds[4] + borders[3])   
		    # x   >>  left edge to B1+LM+B2 from left edge, 
		    # y   >>  bot edge              to   top edge 
	
	# right figure is in the right margin space of the plot area from top to bottom
	rightfig = c(xbnds[3] - borders[1], xbnds[4] + borders[2], ybnds[1] - 
		borders[4], ybnds[4] + borders[3])   
		    # x   >>  B1+RM+B2 from right   to   right edge
		    # y   >>  bot edge              to   top edge
	
	# top figure is in the top margin space from from left to right
	topfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2], ybnds[3] - 
		borders[4], ybnds[4] + borders[3])
		    # x   >>  left edge             to   right edge, 
	            # y   >>  B3+TM+B4 from top edge to  top edge  
	
	# bottom figure is in the bottom margin space from left to right
	botfig = c(xbnds[1] - borders[1], xbnds[4] + borders[2], ybnds[1] - 
		borders[4], ybnds[2] + borders[3])
		    # x   >>  left edge             to   right edge, 
		    # y   >>  bot edge              to   B3+BM+B4 from bot edge  
	
        #cat("leftfig :",leftfig,"\n")
        #cat("rightfig:",rightfig,"\n")
        #cat("topfig  :",topfig,"\n")
        #cat("botfig  :",botfig,"\n")
        
	#
	#  All of the above are in inches..
	#
	# these figure areas are from the devices left to right and top to bottom limits.
	# 
	
	# both vectors should be +1 the column or row count
	#   if "n" columns, there are "n+1" separator (includes one on left and right of
	#   columns.  Same with panel/rows
	#
	
	colSep = panelLengthen(colSep, vncol + 1)      # initially 1 element of zero - now "ncol" elements
	rowSep = panelLengthen(rowSep, vnrow + 1)      # nrow elements.
	
	#cat("vncol:",vncol,"  adj colSep:",colSep,"\n")
	#cat("vnrow:",vnrow,"  adj rowSep:",rowSep,"\n")
	
	if(is.null(lpad)) {    # now sure this is used.
	     # no lpad, initialize - lpad is space round columns and rows.
	     #   includes borders, margins and left and right separators
	     lpad = c(borders[1] + leftMargin + colSep[1],            # left
		      colSep[vncol + 1] + rightMargin + borders[2],   # right
		      borders[3] + topMargin + rowSep[1],             # top
		      rowSep[vnrow + 1] + bottomMargin + borders[4])  # bottom	
	}
	#cat("lpad:",lpad,"\n")
	
	#  lpad[1] = borders[1] + leftmargin + colSep[1] = left edge
	#  lpad[2] = borders[2] + rightmargin + (farright colSep) = right edge
	#  lpad[3] = borders[3] + topmargin + rowsep[1]  = top edge
	#  lpad[4] = borders[4] + bottommargin + (lowest rowsep (at bottom)) + bottom edge
	
	#  everyone assumea col or rowsep around the entire graphic.
	
	# The borders should align around the edge.
	
	# accumulative offset from left    - last value = total.
	
	#
	#  Check to see if Header and Trailer of page is accounted for and subtracted.
	#
	
	colCumSep = cumsum(colSep)              # columns - inches -convert individual spaces to cumulative sums.           
	rowCumSep = cumsum(rowSep)              # inches.
	
	#cat("colCumSep:",colCumSep,"\n")
	#cat("rowCumSep:",rowCumSep,"\n")
	
	wPlotXAvail = round(plotX - colCumSep[vncol + 1], digits=3)    # subtract total separate gaps. inches  (space available for columns in inches)
	wPlotYAvail = round(plotY - rowCumSep[vnrow + 1], digits=3)    # inches remaining.                     (space available for rows in inches
	
	#cat("wPlotXAvail:",wPlotXAvail,"\n")
	#cat("wPlotYAvail:",wPlotYAvail,"\n")
	
        # the colSize and rowSize values are relative to a projected sum of units.
	#      colSize = inches
	#      rowSize = user units
	
	# sum(colSize) =  inches needed minus separators.  
	# sum(rowSize) = 71.65 as coded for 10 full groups and the median group
	
	#  Assumption is we use all of the space?  But need to account for 
	#  aspect ratios and min and max sizes.
	
	# max column space available = plotX inches
	# max row space available = plotY   //  now convert to units --->>>  rowMax = din.y*7 (7 units per inch)
	
	#####  Row size scaling.
	
	#cat("rSizeMn:", rSizeMn,"   rSizeMx:", rSizeMx,"\n")
	
	if (rDebug == 2) {
   	   print(par("din"))
	   print(paste0("plotX Avail=",wPlotXAvail,"  plotY Avail=",wPlotYAvail))
	}
	#   Calculate and verify the row sizing first.  May need to adjust.
	
	# Number of "units" in row size vector
	
	sumRowSize <- sum(rowSize)
	
	# Check size of group/row (inches) (min -> max)
	rInPerUnit = wPlotYAvail / sumRowSize
	
	curGRowInch <- rInPerUnit * rSizeMaj    # inches for 7 units  -> a group/row.
	
	#cat(" rInPerUnit Orig:",rInPerUnit,"  curGRowInch:",curGRowInch,"  sumRowSize:",sumRowSize,"\n",
	#   "  rowSize:",paste0(rowSize,collapse=", "),"\n")
	
        # check min and max range
	if (curGRowInch < rSizeMn) {
	   if (enableErr) {
	      warnCnt()
	      #warnNum <<- warnNum + 1
	      xmsg    <- paste0("***0432 PANEL panelLayout - The calculated GrpRow Height is: ",curGRowInch," inches.  The mininum size limit is:",rSizeMn,"\n")
	      warning(xmsg, call.=FALSE)
	   }
	   if (curGRowInch < (rSizeMn/2)) {
	      stopCnt()
	      #stopNum <<- stopNum + 1
	      xmsg <- paste0("***0431 PANEL panelLayout - The calculated GrpRow Height is too small to be used.")
	      stop(xmsg, call.=FALSE)
	   }
	} else {
	
	   if (curGRowInch > rSizeMx) { curGRowInch <- rSizeMx }
	
	}
	
	# recalculate Inches per unit (recalibrated.)
	rInPerUnit   <- curGRowInch / rSizeMaj               # get inches per unit for full convert
	
	# convert rowSize from units into inches.
	rowSizeIn    <- rowSize * rInPerUnit                  # convert rowSize to inches
	sumrowSizeIn <- sum(rowSizeIn)
	
	#cat(" Inch/Unit:",rInPerUnit,"  sumrowSizeIn:",sumrowSizeIn,"  rowSizeIn:",paste0(rowSizeIn,collapse=", "),"\n")
		
	#  why???
	#rowSizeTotMaxInches = plotY
	#rowSizeTotMaxUnits  = plotY * yUnitsPerInch       # 7 units per 2 inches -> max height we could use in units, so units / inch * space = units.
	#rowSizeTotMaxUnits  = round(rowSizeTotMaxUnits, digits = 3)   # in UNITS.
	
	#cat("rowSizeTotMax - Inches:",rowSizeTotMaxInches,"  Units:",rowSizeTotMaxUnits,"\n")
	
	# size colSize and rowSize if not long enough - we have space to fill.
	
	relyIn      <- panelLengthen(rowSizeIn, vnrow)       # size inches - required == "n" * 7 + 1.65 (opt)   (expand to the number of needed rows.  
        
	relyInSum   <- round(sum(relyIn),digits=3)           # total row size in units
        
        ######    column sizes
		
	relxIn      <- panelLengthen(colSize, vncol)         # size (inches) required == 3 * 1    relx is 3 elements of 1
	
	relxInSum   <- round(sum(relxIn),digits=3)   
	
	
	xRatio      <- 1
	
	#cat("size compare  - relxInSum:",relxInSum,"  to wPlotXAvail:", wPlotXAvail,"\n")
	
	if (relxInSum > (wPlotXAvail+0.0001)) {
	   if (enableErr) {
              warnCnt()
              xmsg      <- paste0("***0430 PANEL panelLayout - The calculated width of ",relxInSum," is too large for the available space of ",wPlotXAvail)
              warning(xmsg, call.=FALSE)
              #stop
           }
	   xRatio    <- wPlotXAvail/relxInSum

	   #cat("xRatio required:",xRatio,"\n")
	}
	
	######

        
        if (rDebug == 2) { 
	   cat("relxInSum:",relxInSum,"  relxIn:",paste0(relxIn,collapse=", "),"\n")
	   cat("colSize:",paste0(colSize,collapse=", "),"\n")
	   cat("xRatio:",xRatio,"\n")
	}
	
	yRatio    <- 1
	
	#cat("size compare  - relyInSum:",relyInSum,"  to wPlotYAvail:", wPlotYAvail,"\n")
	
	if (relyInSum > (wPlotYAvail + 0.0001)) {
           #  If requirements to large, find the ratio to cut them all back.
            
           yRatio = wPlotYAvail / relyInSum   #  Have / Needed  ---  inches/units = inches per unit.
           
	   if (rDebug == 2) {
	     cat("rowSize array - in inches:",relyInSum,"\n")
	     print(paste0("   rowSize in inches reduced by ",yRatio*100," percent."))
	   }
	}

        if (rDebug == 2) { 
	   cat("relyInSum:",relyInSum,"  relyIn:",paste0(relyIn,collapse=", "),"\n")
	   cat("rowSize:",paste0(rowSize,collapse=", "),"\n")
	   cat("yRatio:",yRatio,"\n")
	}
	
	
	comRatio <- min(xRatio, yRatio)
        #cat("comRatio:", comRatio,"\n")
        
	if (comRatio < 0.999) {
	   # reduce scaling.
	   #rowSizeTotMaxUnits =  relySum
	   #cat("reduction enforced - map columns - REALLY, comRatio:",comRatio,"\n")
	
	   colSize   <- colSize  * xRatio   #  adjust column width (but doesn's work right - to small)
	   #cat("reduced colSize:",paste0(colSize,collapse=", "),"\n")
	   
	   rowSizeIn <- rowSizeIn * yRatio
	   #cat("reduced rowSizeIn:", paste0(rowSizeIn,collapse=", "),"\n")
	   
	   #stop()  # don't want this logic to be working yet.
	}
	
	if (rDebug == 2) {
	   cat("xRatio :", xRatio,"  yRatio=",yRatio, "  map cols=",paste0(rMapCol,collapse=" "),"\n")
	   cat("colSize:", paste0(colSize,collapse=" "),"\n")
	   cat("rowSizeIn:",rowSizeIn,"\n")
	}
	
	######
	
	relxPerc <- relxIn/wPlotXAvail     # percentage of "inches" allowed (setup of colSize totals width)
	relyPerc <- relyIn/wPlotYAvail     # percentage of "inches" allowed (for Setup)
	
	relxP    <- cumsum(c(0,relxPerc))
	relyP    <- cumsum(c(0,relyPerc))
	
        #cat(" relxPerc    :", paste0(relxPerc,collapse=", "),"\n")
        #cat(" relyPerc    :", paste0(relyPerc,collapse=", "),"\n")
	#cat(" cumsum relxP:", paste0(relxP,collapse=", "),"\n")
	#cat(" cumsum relyP:", paste0(relyP,collapse=", "),"\n")
	#cat("\n")
	
	# increment table from left and bottom.
	
	xinc    <- wPlotXAvail * relxP     # ignoring fixed fields  (positions from 0 to n 
	yinc    <- wPlotYAvail * relyP     # all floating (may not need all of this room.) from 0 to n
	
	#cat("xinc:",xinc,"  yinc:",yinc,"\n")
	
	#   inc is the space for each element
	
	#  init fig matrix to all zeros
	fig = matrix(0, nrow = vnrow * vncol, ncol = 4)  # for values for each panel (empty)
	
	k = 0   # index to zero
	
	#  pads are equal to left separators, borders and margins on top, bot, left, right.
	#  The inc vectors are offsets from left and bottom..
	
	#  All this in inches.
	#  We step through the i rows and j columns, and fill in one row in fig for each. 
	
	#  one row per panel (i,j),   1 = left x, 2 = right x, 3 = bottom y, 4 = top y
	#                                 x1      x2           y1            y2
	
	for (i in 1:vnrow) {   # y rows
           for (j in 1:vncol) {  # x columns
               k    <- k + 1
              
               #  x axis
               #            left     + left x  + left sep - lpad(1)       (Start of x in i,j panel)
	       fig[k, 1] <- xbnds[2] + xinc[j] + colCumSep[j] - lpad[1]             # x.pos k<=1:- start (x left)
	       
	       #            left     + right x + left sep - lpad(2)       (End of x in i.j panel)
	       fig[k, 2] <- xbnds[2] + xinc[j + 1] + colCumSep[j] + lpad[2]         # x pos      - end   (x right)
	      
	       #            top      - top y   - top sep  + lpad(3)       (Top of y in i,j panel)
	       fig[k, 4] <- ybnds[3] - yinc[i] - rowCumSep[i] + lpad[3]             # y pos - start      (y top)
	       
	       #            top      - bottom y - top sep + lpad(4)       (end of y in i.y panel)
	       fig[k, 3] <- ybnds[3] - yinc[i + 1] - rowCumSep[i] - lpad[4]         # y pos - end        (y bottom) 
	   }
	}
	
	#cat("Initial build of fig:\n")
	#print(fig)
	
	#
	#  fig structure is one long vector.
	#    k incremented each new column with row.
	#    so 4 column micromap = 1r,1c = 1(k),  1r,2c = 2(k), 2r,1c = 5(k)
	#    each k contains 4 values: x1(left), x2(right), y1(bot), y2(top) - The opposite points in a rectangle
	#    
	#    built from xbnds[2]
	#               xinc[j]
	#               colCumSep[j]
	#               lpad[1], lpad[2]
	#
	#               ybnds[3]
	#               yinc[i]
	#               rowCumSep[i]
	#               lpad[4], lpad[3]
	#
	#
	
	# fig now has the x1, x2, y1, y2 physical position of each panel on the page.

        #  Scale the "inch" coordinates to coordinates 0 to 1 (relative) in fig space.
	fig      <- abs(t(t(fig)/fig.scale))  # change to device 0 to 1 NDC
	
	#cat("Modified fig:\n")
	#print(fig)
	
	labfig = rbind(leftfig, rightfig, topfig, botfig)
	# lab figure has four rows - one for each margin/border space around the plot area. 
	
	#  Scale the "inch" coordinates to coordinates 0 to 1 (relative) in fig space
	labfig   <- t(t(labfig)/fig.scale)  # change to device 0 to 1 NDC
	#cat("labfig:",labfig,"\n")

	# coltabs are in inches and start inside the left border
	#     elements= left, points (CumSep+XInc+leftmar)(nCol), leftMar+CumSep + xinc + rightMarf
	coltabs  <- cbind(c(0, colCumSep + xinc + leftMargin), leftMargin + c(0,colCumSep) + 
	                  c(xinc, xinc[length(xinc)] + rightMargin )
	               )	
	#cat("coltabs:",coltabs,"\n")
	
	
	# rowtabs are in inches and start below the lower border
       
        # yinc is padded with a leading 0.  
	rowtabs  <- cbind(c(ybnds[3], ybnds[3] - rowCumSep - c(yinc[-1], yinc[vnrow + 1] + bottomMargin)), 
	                c(ybnds[4], ybnds[3] - rowCumSep - yinc)
	               ) - borders[4]
	#cat("rowtabs:",rowtabs,"\n")
	
	# The tabs provide the physical points for each panel.
		
	if (rDebug == 2) {
	   print(fig)
           print(paste0("coltabs=",paste0(coltabs,collapse=" ")))
           print(paste0("rowtabs=",paste0(rowtabs,collapse=" ")))
	} 

	list(dim = c(vnrow, vncol), 
	     datfig = round(fig,6), 
	     labfig = round(labfig,6), 
	     brd = borders,
             lpad = lpad, 
             coltabs = coltabs, 
             rowtabs = rowtabs, 
	     figsize = c(din.x, din.y)
	    )
	    
}
