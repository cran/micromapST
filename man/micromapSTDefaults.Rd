\name{micromapSTDefaults}
\alias{micromapSTDefaults}
\docType{data}
\title{micromapSTDefaults data.frame structure }
\description{
The micromapSTDefaults data.frame provides all of the detailed structure,
colors, sizing, font sizes, separation distances, line weights and types, spacing, etc.
required to physically construct the requested micromapST graphic for an 8.5" by 11" page
in portrait format with 1/2" margines.  The data.frame is mainly used internal to micromapST, 
but a copy can 
be obtained by a user when a large number of changes are required.  This is not recommended.
The primary purpose of this section is to provide a list and description of 
many of the variables in the details list that can be used to enable or disable
functions of micromapST and it's glyphics.  This variable are identified by a "*" after
the variable name.  These are the only variables that generally safe to be modified by 
the user. }
\format{The data.frame contains two lists: colors and details.
 \describe{
  \item{colors}{The colors vector is the name of a color palette or a vector of 
  7 or 14 color names or values ('#xxxxxx'). The vector defines the colors used for:
    \itemize{
      \item{The 5 colors in each group for the states and symbols in the glyphcs. 
      One color per row (state).}
      \item{1 color for the median state and glyphics,}
      \item{1 foreground color for highlighted states in the map.  This is used to 
      highlight states already referenced previously or have meaning depend on 
      the type of map requested. The usage is as follows:
       \preformatted{
        'map'       - not used.
        'mapcum'    - highlight states previously referenced
                      above.
        'maptail'   - highlight states previously referenced
                      above the median row and highlight 
                      remaining states below the median row.
        'mapmedian' - highlight all states above the median 
                      in maps above the median row and highlight 
                      all states below the median in maps 
                      below the median row.
        }                  
      }
      \item{When 7 additional colors are specified, they are used as the translucent colors in
      the time series glyphics bands.  If only 7 colors are provided, the additional
      7 translucent colors are generated from the original colors as a 20\% transparent version
      of the original color e.g., adjustcolors(colors,0.2)
      }
     }
     If colors = 'greys' or 'grays' or 'bw', the entire plot will be done using 
     the packages standard black/white/gray shades. 
     Additional color palettes may be supported in future releases.
       
     If a colors vector is not provided, the package default colors will be used:
     \itemize{
       \item 5 state colors: 'red', 'orange', 'green', 'greenish blue', 'lavender'
       \item 1 median state color: 'black'
       \item 1 highlighed states: 'light yellow' and 
       \item 7 translucent colors using the above colors at 20\%.
     }
     It is strongly recommended to use the default.
    }
    \item{details}{is a list structure that contains the variables and values for the 
       detailed layout and operation of micromapST.  The list of variables is:
       \describe{
        \item{ne}{= 5, items (rows) per group.}
        \item{ng}{= 11, groups of states. The one state median row counts as 1.}
        \cr
        \item{ib}{= c(1,6,11,16,21,26,27,32,37,42,47), index for first items in each of the 11 groups }
        \item{ie}{= c(5,10,15,20,25,26,31,36,41,46,51), index for last items in each of the 11 groups }
        \cr
        The \var{ib} and \var{ie} variables are used to index into the sorted list of states to pull out the states 
        for a particular group/row.
        \cr
        \item{topMar}{= 0.95, top margin in inches}
        \item{botMar}{= 0.5, bottom margin in inches}
        \item{botMarLegend}{= 0.5, bottom margin for legend}
        \item{botMardif}{= 0.2,  bottom margin difference}
        \item{leftMarAxis}{= 0.2, left margin when Y axis labels and title are required}
        \cr    
        \item{rowSep}{= c(0,0,0,0,0,0.1,0.1,0.0.0.0.0), location of row separations in 
        each column of panels (0.1 around the median row, 0 for others). (inches)}
        \item{rowSize}{= c(7,7,7,7,7,1.6,7,7,7,7,7), size of rows in each panel 
        (1.6 row units for median row, 7 row units for the 5 item groups)}
        \item{groupedRowSize}{= c(35, 1.65, 35),  size of panels (1.65 row units for median panel, 
        35 row units for the 5 item groups.)}
        \item{groupedRowSep}{= c(0,0.1,0.1,0), separation between panels 
        (0.1 inches for median panel, 0 for others)}
        \item{Map.Width}{= 1.4 inches, width of each map}
        \item{Id.Width}{= c(0.9, 0.3),  width of id columns (full name (0.9) and abbreviated (0.2)) inches}
        \cr  
        \item{sc}{= 1.08, x axis scale expansion factor.  Applied to the data range to calculate the graph's range.}
        \item{pad}{= 0.67, y axis padding for integer plotting locations}
        \item{padex}{= 0.34 inches, total panel padding (i.e., 0.17 at top and bottom of panel)}
        \item{padMinus}{= 0.63 inches, spacing to keep reference line off panel edge}
        \cr
        \item{Title.Line1}{= 1.75 inches, top panel 1st line placement above the first panel, used with lab1}
        \item{Title.Line2}{= 1.05 inches, top panel 2nd line placement above the first panel, used with lab2}
        \item{Title.Line3}{= 0.65 inches, bottom panel line placement below the last panel, used with lab3}
        \item{Title.Line4}{= -0.7 inches, reference line legend below last panel, used with reftext}
        \item{Title.Line5}{= .40 inches, Y axis label placement (to the left of panel), used with lab4}
        \item{Title.cex}{= 1.0, text size of title, used with title}
        \item{lineTiclab}{= 0.2 inches, lowest line for map legend text, used with map glyph for footer label}
        \cr 
        \item{Grid.col}{= 'white' * Grid line color}
        \item{Grid.lwd}{= 1, Grid line width}
        \cr
        \item{mgpTop}{= c(2,0.1,0), Top margin line for 'X' axis labels and axis line.}
        \item{mgpBottom}{= c(2,0,0), Bottom margin line for 'X' axis labels and axis line.}
        \item{padjBottom}{= -0.7, Axis tick label placement adjustment}
        \item{mgpLeft}{= c(0.75, 0.05, 0), Left 'Y' axis margin line for axis labels and axis line.}
        \cr 
        \item{Panel.Fill.col}{= "#EBEBEB", * defaults to light gray}
        \item{Panel.Outline.col}{= "black", * color of panel outlines}
        \cr
        \item{Text.cex}{= 0.7, general text size}		
       }
       The following variable relate to the reference text and line features:
       \describe{
        \item{Ref.Val.lty}{= 'dashed', * set reference value line to dashed}
        \item{Ref.Val.lwd}{= 1.5, * line width of reference line}
        \item{Ref.Val.col}{=  "mid green", * color of reference line when color is used.}
        \item{Ref.Val.BW.col}{= "black", * color of reference line when grays are used.}
        \item{Ref.Text.cex}{= 0.7, * size of reference line text}
        \item{Ref.Text.col}{= "black", * color of reference line text when color is used.}
        \item{Ref.Text.BW.col}{= "black", * color of reference line text when grays are used.}  
       }
       The following variables are used by the \option{arrow} glyph:
       \describe{
        \item{Arrow.lwd}{= 2.5, line width of arrow.}
        \item{Arrow.cex}{= 0.08, * size of arrow  (*** not implemented ***)}
        \item{Arrow.Head.length}{= 0.08, length of arrow.}
        \item{Arrow.Shadow.lwd}{= 4.0, * line width of arrow shadow to create 
        outline (*** not implemented ***)}
        \item{Arrow;Shadow.col}{= "black", * arrow shadow color (*** not implemented ***)}
       }
       The following variables are used by the \option{bar} glyph:
       \describe{
        \item{Bar.barht}{= 2/3,  * fraction of line height for. 
        Should never be > .90. Usable range is 0.333 to 0.90}
        \item{Bar.Outline.col}{= "black", * color of bar outline.}
        \item{Bar.Outline.lwd}{= 0.5, * line width for bar outline }
        \item{Bar.Outline.lty}{= 'solid', * line type for bar outline }
       }
       The following variables are used by the \option{boxplot} glyph:
       \describe{
        \item{BoxP.thin}{= 0.2, line width of box.}
        \item{BoxP.thick}{= 0.6, thick line width.}
        \item{BoxP.Use.Black}{= FALSE, * whether to outline the outlier points.}
        \cr
        \item{BoxP.Median.Line}{= 0.80, * line width of median line.}
        \item{BoxP.Median.Dot.col}{= "white", * color of median dot.}
        \item{BoxP.Median.Dot.pch}{= 19,  * solid circle symbol.}
        \item{BoxP.Median.Dot.cex}{= 0.95, * size of circles.}
        \item{BoxP.Median.Dot.lwd}{= 2, * line width for median.} 
        \item{BoxP.Median.col}{= "black", * color of median box}
        \cr
        \item{BoxP.Outline.col}{= '#262626FF", * boxplot outline color}
        \cr 
        \item{BoxP.Outlier.lwd}{= 0.4, * line width of outlier outlines.}
        \item{BoxP.Outlier.cex}{= 0.7, * size of outlier dots.}
        \item{BoxP.Outlier.BW.col}{= "#4c4c4cFF" * color of outliner lines when greys used.}
       }
       The following variables are used by the \option{dot}, \option{dotconf}, and \option{dotse} glyphs:
       \describe{
        \item{Dot.pch}{= 21, * solid circle (S compatible).}
        \item{Dot.pch.cex}{= 0.9, * size of dot.}
        \item{Dot.Outline}{=FALSE, * whether to outline the dots.}
        \item{Dot.Outline.col}{= "black", * color of dot outline.}
        \item{Dot.Outline.lwd}{= 0.5, * line width of dot outline.}
       }
       The following variables are used by the \option{dotconf} glyph:
       \describe{
        \item{Dot.conf}{= 95, percent confidence interval }
        \item{Dot.conf.lwd}{= 2, * line width of confidence interval lines.}
        \item{Dot.conf.cex}{= 0.55, * size of confidence interval}
       }
       The following variables are used by the \option{id} glyph:
       \describe{
        \item{Id.Text.cex}{= 0.9, text side of ID column}
        \item{Id.Text.adj}{= .1/3, offset to align state names}
        \item{Id.Dot.pch}{= 21, * pch symbol value to plot next to state name/abbrev.}
        \item{Id.Dot.cex}{= 0.6, * size of dot symbol for state ID}
        \item{Id.Dot.Outline.col}{= "dark gray", * outlines of ID}
       }
       The following variables are used by all of the 'map' type glyphs:
       \describe{
        \item{Map.Bg.col}{= #262626FF", * color of state fill in maps}
        \item{Map.Bg.Line.col}{= "white", * background of maps}
        \item{Map.Bg.Line.lwd}{= 1,  * line weight for map background boundaries}
        \item{Map.Fg.Line.col}{= "black", * foreground color of maps}
        \item{Map.Fg.Line.lwd}{= 1, * line weight for map foreground boundaries}
        \item{Map.Nation.Line.col}{= "black", * color of national outline in maps}
        \item{Map.Nation.Line.lwd}{= 1. * line weight for national boundaries}
        \item{Map.State.Spec.cex}{= 0.32, * font size for state labels}
       }
       The following variables are used by the \option{ctrbar} glyph:
       \describe{
        \item{CBar.Zero.Line.col}{= "white", * centered bar "zero" vertical line color}
        \item{CBar.Zero.Line.lwd}{= 1, * line width for centered bar "zero" vertical line}
        \item{CBar.Zero.Line.lty}{= "dotted", * type of centered bar "zero" line}
        \item{CBar.varht}{= FALSE,  * enables variable height bars.}
        \item{CBar.two.ended}{= FALSE, * request two ended variable height bars be used 
        (small to large to small). (*** Not implemented ***)}
       }
       The following variables are used by all of the horizontal stacked bar glyphs 
       (\option{ctrbar}, \option{segbar} and \option{normbar}):
       \describe{
        \item{CSNBar.Outline.col}{= "#262626FF", * very dark gray, color of 
        bar segment outline in segmented bar plots.}
        \item{CSNBar.Outline.lwd}{= 0.75, * line weight for bar segment outline in 
        segmented bar plots.}
        \item{CSNBar.Outline.lty}{= 'solid', * line type for bar segment outline 
        in segmented bar plots.}
        \item{CSNBar.First.barht}{= 0.3333, * height of first bar when variable 
        height bars are used. Must be less than SBar.Last.barht and in the range 
        of 0.333 to 0.6667,  SNBar.varht or CBar.varht must be TRUE for this option of function.}
        \item{CSNBar.Last.barht}{= 0.80, * height of last bar when variable height bars are used.  
        Must be greater than CSNBar.First.barht and in the range of 0.6667 to 0.90. CSNBar.varht or 
        CBar.varht must be TRUE for this option of function.}
       }
       The following variables are used by the \option{segbar} and \option{normbar} glyphs:
       \describe{
        \item{SNBar.barht}{= 0.66667, * fixed height of bar when variable height 
        bars are is not used.  Should never be greater than 0.90. Usable range 
        is 0.333 to 0.90.}
        \item{SNBar.varht}{= FALSE, * enables variable height bars from 
        SBar.First.barht to SBar.Last.barht.}
        \item{SNBar.two.ended}{= FALSE, * request two ended variable height bars be used 
        (small to large to small).(*** Not implemented ***)}
        \item{SNBar.Middle.Dot}{=FALSE, * request a dot be draw in at the mid point in the 
        segmented bars.}
        \item{SNBar.MDot.pch}{=21, * type of point/symbol used as the mid point dot. SNBar.Middle.Dot must be TRUE for this parameter to function.}
        \item{SNBar.MDot.pch.col}{='white', * color of the point/symbol used as the 
        mid point dot. SNBar.Middle.Dot must be TRUE for this parameter to function.}
        \item{SNBar.MDot.pch.size}{=0.3, * size of point/symbol used as the mid point dot. SNBar.Middle.Dot must be TRUE for this parameter to function.}
        \item{SNBar.MDot.pch.border.lwd}{=NA, * line width of outline of point/symbol 
        used as the mid point dot. SNBar.Middle.Dot must be TRUE for this parameter to function.}
        \item{SNBar.MDot.pch.border.col}{=NA, * color of outline of point/symbol used as 
        the mid point dot. SNBar.Middle.Dot must be TRUE for this parameter to function.}
       }
       The following variables are used by the scatter dot glyph (\option{scatdot}):
       \describe{
        \item{SCD.Bg.pch}{= 21, * type of point/symbol to be used for background 
        data points (not active) - state's dots.}
        \item{SCD.Bg.pch.lwd}{= 0.6, * line width of outline of point/symbol used as 
        non-active data points.}
        \item{SCD.Bg.pch.size}{= 0.75, * size of point/symbol used as non-active 
        data points.}
        \item{SCD.Bg.pch.fill}{= 'transparent', * fill color for not selected state's 
        dots.}
        \item{SCD.Fg.pch}{= 21, * type of point/symbol for active data points}
        \item{SCD.Fg.pch.lwd}{= 0.6, * Scatter dot symbol outline line weight for 
        active data points}
        \item{SCD.Fg.pch.size}{= 1, * size of point/symbol for active data points 
        in scatter dot}
        \item{SCD.Median.pch}{= 21, * shape of filled symbol for median value - 
        scatter dots}
        \item{SCD.Median.pch.lwd}{= 0.6, * line width used on the median symbol 
        - scatter dots}
        \item{SCD.Median.pch.size}{= 1, * symbol size median value 
        - scatter dots}
        \item{SCD.Median.pch.fill}{= "black", * color of filled symbol for median value 
        - scatter dots}
        \item{SCD.Axis,cex}{= 0.49, font size for Y axis labels for scatter dots}
        \item{SCD.xsc}{= 1.1,  x range multiplier to keep dots from being clipped}
        \item{SCD.ysc}{= 1.08, y range multiplier to keep dots from being clipped}
        \item{SCD.hGrid}{= FALSE, * whether or not to include horizontal grid lines in panel}
        \cr
        \item{SCD.DiagLine}{= TRUE, * whether or not to include x=y sloped line}
        \item{SCD.DiagLine.col}{= colGrid, * color of sloped line, default, grid line color.  SCD.DiagLine must be TRUE.}
        \item{SCD.DiagLine.lwd}{= 1, * line weight of sloped line, default, grid line color.  SCD.DiagLine must be TRUE.}
        \item{SCD.DiagLine.lty}{= 'solid', * line type of sloped line, default, grid line color.  SCD.DiagLine must be TRUE.}
       }
       The following variables are used by the time series glyphs (\option{ts} and \option{tsconf}):
       \describe{
        \item{TS.lwd}{= 1.1, * time series line weight}
        \item{TS.Axis.cex}{=0.49, * font size for Y axis labels}
        \item{TS.hGrid}{= FALSE, * whether or not to include horizontal 
        grid lines in panel}
       }
   When changing the colors list, then the entire list must be specified.  To change the values of items
   in the details list, only the variable(s) requiring change need to be specified as a list for the 
   details=  parmater in the call.  In general, the beginning of the variable names indicates
   the glyph the variable is associated, in most cases.
   \preformatted{
       Arrow.    -> arrow glyph
       Bar.      -> bar glyph
       BoxP      -> boxplot glyph
       CBar      -> ctrbar glyph
       CSNBar    -> ctrbar, segbar and normbar glyphs. 
       Dot.      -> dot, dotconf, and dotse glyphs
       Dot.conf. -> dotconf glyph
       Grid      -> grid elements of all glyphs
       Id.       -> id glyph
       Map.      -> map glyphs
       Panel     -> general glyph panel
       Ref       -> Reference text and line
       SCD.      -> scatter dot glyph
       SNBar     -> segbar and normbar glyphs
       Title     -> page and column labels and titles
       TS        -> ts and tsconf glyph
       TSconf    -> tsconf glyph
       
      }
   The remaining variables deal with page and glyph layout and spacing and should not
   be adjusted.
   \cr
   For example: to turn off the midpoint dot in the segmented bar glyphics, 
   all that is required is:
   \preformatted{

      details = list(SNBar.Middle.Dot=FALSE)
    }
   } 
  }
}
\details{
The micromapSTDefaults data.frame is built by the micromapSTSetDefaults function
when the micromapST package is called. Once built it cannot be changed.
To change one or two (a few) variables, construct a list of these variables and pass
it to micromapST via the details parameter in the call.
To do large scale customization, call the micromapSTSetDefaults function to get
a copy of the entire data.frame and modify this copy.  This is not recommended.  
}
\author{Daniel B. Carr, George Mason University, Fairfax VA, 
with contributions from Jim Pearson and Linda Pickle of 
StatNet Consulting, LLC, Gaithersburg, MD
}
\keyword{data structure}
\seealso{
  \code{\link{micromapSTSetDefaults}},
  \code{\link{micromapST}}
}

