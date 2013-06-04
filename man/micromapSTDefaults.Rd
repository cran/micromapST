\name{micromapSTDefaults}
\alias{micromapSTDefaults}
\docType{data}
\title{micromapSTDefaults data.frame structure }
\description{
The \code{micromapSTDefaults} data.frame provides all of the detailed structure,
colors, sizing, font sizes, separation distances, line weights and types, spacing, etc.
required to physically construct the requested micromapST graphic for an 8.5" by 11" page
in portrait format.
}

\details{
The \code{micromapSTDefaults} data.frame is built when the micromapST package is 
loaded via the library() function.  Once built it cannot be changed.  
To do customization, copy the \code{micromapSTDefaults} to a new data.frame and 
modify the copy.   
}

\author{Daniel B. Carr, George Mason University, Fairfax VA, 
with contributions from Jim Pearson and Linda Pickle of 
StatNet Consulting, LLC, Gaithersburg, MD
}

\format{The data.frame contains two lists: colors and details.

  \describe{
    \item{colors}{
      The colors list contains the definition for the specific graphic device for the colors (14):
     "red", "orange", "green", "greenish blue", "lavender", "black", "light yellow" plus 7 
     corresponding translucent colors to time series bands. These colors were chosen to improve
     the readability of the graphics, provide consistant color usage, and minimize visual color perception issues.
    }
    \item{details}{The details structure and field names:
       \describe{
        \item{ne}{= 5 items (rows) per group.}
        \item{ng}{= 11 groups of states}
        \item{ib}{= index for lower items in each of the 11 groups (1,6,11,16,21,26,27,32,37,42,47)}
        \item{ie}{= index for upper items in each of the 11 groups (5,10,15,20,25,26,31,36,41,46,51)}
        \item{topMar}{= 0.95, top margin in inches}
        \item{botMar}{= 0.5, bottom margin in inches}
        \item{botMarLegend}{= 0.5, bottom margin for legend}
        \item{botMardif}{= 0.2,  bottom margin difference}
        \item{leftMarAxis}{= 0.2, left margin when Y axis labels and title are required}
        \item{rowSep}{= separation between rows in each panel (0.1 around the median row, 0 for others)}
        \item{rowSize}{= size of rows in each panel (1.5 pixels for median row, 7 pixels for others)}
        \item{groupedRowSize}{= size of panels (1.5 pixels for median panel, 35 pixels for others)}
        \item{groupedRowSep}{= separation between panels (0.1 inches for median panel, 0 for others)}
        \item{mapWidth}{= 1.4 inches, width of each map}
        \item{idWidth}{= 0.9, 0.3  width of id column (full name and abbreviated)}
        \item{sc}{= 1.08, x axis scale expansion factor}
        \item{pad}{= 0.67, y axis padding for integer plotting locations}
        \item{padex}{= 0.34 inches, total panel padding (i.e., 0.17 at top and bottom of panel)}
        \item{padMinus}{= 0.63 inches, spacing to keep reference line off panel edge}
        \item{line1}{= 1.75 inches, top panel 1st line placement}
        \item{line2}{= 1.05 inches, top panel 2nd line placement}
        \item{lineTiclab}{= 0.2 inches, lowest line for map legend text}
        \item{line3}{= 0.65 inches, bottom panel line placement}
        \item{line4}{= -0.7 inches, reference line (below panel)}
        \item{line5}{= .45 inches, Y axis label placement (left of panel)}
        \item{colGrid}{= tempcolGrid, defaults to white}
        \item{lwdGrid}{= 1, line width of grid}
        \item{mgpTop}{= c(2,0.1,0), gridline tick placement}
        \item{mgpBottom}{= c(2,0,0), gridline tick placement}
        \item{padjBottom}{= -0.7, gridline tick placement}
        \item{mgpLeft}{= c(.75, .05, 0) left axis labels}
        \item{colPanelFill}{= tempcolFill, defaults to light gray}
        \item{colOutline}{= "black", color of panel outlines}
        \item{cexText}{= 0.7, text size}		
        \item{cexTitle}{= 1, text size of title}
        \item{ltyRefVal}{= 2, set reference value line to dashed}
        \item{lwdRefVal}{= 2, line width of reference line}
        \item{colRefVal}{= "mid green", color of reference line}
        \item{colGRefVal}{= "black", gray color of reference line}
        \item{colRefTxt}{= "black", color of reference line text}
        \item{colGRefTxt}{= "black" gray color of reference line text}  
        \item{lengthArrow}{= 0.08, length of arrow in arrow plots}
        \item{lwdArrow}{= 2.5, line width of arrow in arrow plots}
        \item{lwdArrowShadow}{= 5, line width of arrow shadow to create outline in arrow plots}
        \item{colArrowShadow}{= "black", arrow shadow color}
        \item{cexArrow}{= 0.08, size of arrow }
        \item{barht}{= 2/3, fraction of line height for bar in bar plots}
        \item{colBarOutline}{= "mid gray", color of bar outline in bar plots}
        \item{lwdBarOutline}{= 1, line width for bar outline }
        \item{colBarZero}{= "white", centered bar "zero" vertical line color}
        \item{lwdBarZero}{= 1, line width for centered bar "zero" vertical line}
        \item{ltyBarZero}{= "dotted", type of centered bar "zero" line}
        \item{colSBarOutline}{= rgb(.15,.15,.15) - dark gray, color of bar segment outline in segmented bar plots}
        \item{lwdSBarOutline}{= .75, line weight for bar segment outline in segmented bar plots}
        \item{thinBox}{= 0.2, line width of box in box plots}
        \item{thickBox}{= 0.6, thick line width in box plots }
        \item{useBlack}{= FALSE, whether to outline the outlier points in box plots}
        \item{medianLine}{= 0.88, line width of median line in box plots}
        \item{colBpDotMedian}{= "white", color of median dot in box plots}
        \item{pchMedian}{= 19, solid circle symbol in box plots }
        \item{cexMedian}{= 0.95, size of circles in box plots}
        \item{lwdMedian}{= 2, line width for median in box plots} 
        \item{colBpMedian}{= "black", color of median box}
        \item{colBpOutline}{= "light gray", boxplot outline color}
        \item{lwdOutlier}{= 0.4, line width of outlier outlines in box plots}
        \item{cexOutlier}{= 0.7, size of outlier dots in box plots}
        \item{colBpGreyOutlier}{= colorsRef["dark grey"] color of outliner lines when greys used.}
        \item{idDotPch}{= 26, pch symbol value to plot next to state name/abbrev.}
        \item{pchDot}{= 26, solid circle (S compatible) used in dot plots}
        \item{cexDot}{= 0.9, size of dot in dot plots}
        \item{conf}{= 95, percent confidence interval used in dot plots}
        \item{lwdConf}{= 2, line width of confidence interval lines in dot plots}
        \item{cexConf}{= 0.55, size of confidence interval}
        \item{OutlineDot}{= TRUE, whether to outline the dots in dot plots}
        \item{colDotOutline}{= "black", color of dot outline in dot plots}
        \item{lwdDotOutline}{= 1, line width of dot outline in dot plots}
        \item{lwdTs}{= 1, time series line weight}
        \item{axisCexTs}{=.49, font size for Y axis labels}
        \item{hGridTs}{= FALSE, whether or not to include horizontal grid lines in panel}
        \item{lwsSCD}{= .4, Scatter dot symbol border line size}
        \item{bgFillSCD}{= colorsRefrgb["lighter grey"], fill color for not selected state's dots in scatter dot panel}
        \item{medPchSCD}{= 21, shape of filled symbol for median value - scatter dots}
        \item{medColSCD}{= "black", color of filled symbol for median vlaue - scatter dots}
        \item{axisCesSCD}{= .49, font size for Y axis labels for scatter dots}
        \item{sizePchSCD}{= .49, size of symbol scatter dot}
        \item{xscSCD}{= 1.1,  x range multiplier to keep dots from being clipped}
        \item{yscSCD}{= 1.08, y range multiplier to keep dots from being clipped}
        \item{hGridSCD}{= FALSE, whether or not to include horizontal grid lines in panel}
        \item{colIdOutline}{= "dark gray", outlines of ID}
        \item{cexId}{= 0.9, text side of ID column}
        \item{cexIDDot}{= .6, size of dot symbol for state ID}
        \item{idty}{= .1/3, offset to align state names}
        \item{colMapBackgr}{= colorsRefRgb["lighter grey"] color of state fill in maps}
        \item{colLineBackgr}{= "white", background of maps}
        \item{colLineForegr}{= "black", foreground color of maps}
        \item{colLineNation}{= "black", color of national outline in maps}
        \item{cexState}{=.32, font size for state labels}
        \item{lwdBackGr}{= 1, line weight for map background boundaries}
        \item{lwdForeGr}{= 1, line weight for map foreground boundaries}
        \item{lwdNation}{= 1. line weight for national boundaries}
       }
    }
    
  }
}
\keyword{data structure}
\seealso{
  \code{\link{micromapSTSetDefaults}},
  \code{\link{micromapST}}
}

