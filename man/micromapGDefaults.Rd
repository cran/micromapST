\name{micromapGDefaults}
\alias{micromapGDefaults}
\docType{data}
\title{micromapGDefaults data.frame structure }
\description{
The micromapGDefaults data.frame provides all of the detailed structure,
colors, sizing, font sizes, separation distances, line weights and types, spacing, etc.
required to physically construct the requested micromapST graphic in portrait or 
landscape modes from a letter size (8.5 x 11) up to a tabloid (11 x 17) page.
The data.frame is mainly used internal to micromapST, but a copy can 
be obtained by a user when a large number of changes are required.  
This is not recommended.
The primary purpose of this section is to provide a list and description of 
many of the variables in the details list that can be used to enable or disable
functions of micromapST and its glyphs.  These internal variable are identified 
by a "*" after the variable name.  These are the only variables that generally 
safe to be modified by the user. 
}
\details{The data.frame contains two lists: colors and details.
 \describe{
  \item{colors}{The colors vector is the name of a color palette or a vector of 
    12 or 24 color names or values ("#xxxxxx" or name). The first twelve (12) colors
    are used to link the areas to the glyphs.  The second 12 colors are used with 
    the Time Series glyphs when transparent colors are required for the 
    confidence band.  The vector defines the 12 colors and their transparent equal are: 
    \itemize{
      \item{The 6 colors in each group for the states/areas and symbols in the glyphcs. 
      One color per row (area). The 6th color is not used at this time.}
      \item{1 color for the median state and glyphics and is generally black,}
      \item{1 foreground color for highlighted states in the map.  This is used to 
       highlight states already referenced previously or have meaning depend on 
       the type of map requested. The usage is as follows:
       \preformatted{
        "map"       - not used.
        "mapcum"    - highlight states previously 
                      referenced above (a previous 
                      group/row).
        "maptail"   - highlight states previously 
                      referenced above the median row 
                      and highlight remaining states not 
                      featured below the median row.
        "mapmedian" - highlight all states not featured 
                      above the median in maps above the 
                      median row and highlight all states 
                      not featured below the median in maps 
                      below the median row.
       }         
      }
      \item{2 colors to represent non-featured areas above the median row and below
       the median row.
       }
      \item{1 color to fill in non-referenced areas on the map.  These are areas in the
       border group, but the user has not provided any data row in the statsDFrame supplied 
       in the \var{micromapST} function call. 
       }
      \item{1 color to fill in non-active area.  That is an area that in not referenced in
       the name table and can't be matched to any user data.
       }
    }
    The additional 12 colors are the same colors defined above but modified 20\% tranparency
    to provide a set of "transparent" colors for confidence graphs like the time series.  
    This is done via the adjustcolors(colors,0.2) function. Only the first 6 of the transparency
    colors are used.  The other 6 colors are reserved for future requipments.
    
    If colors parameter can also be set to a single value to enable black and white color 
    schemes.  The acceptable values are; "greys" or "grays" or "bw".  When specified,
    the entire plot will be done using the packages standard black/white/gray shades designed
    to support b&w duplication, color blindness and non-color publication. 
    
    Additional color palettes may be supported in future releases.
    
    The package default 24 colors will be used:
    \itemize{
       \item{ 5 state colors: "red", "orange", "green", "greenish blue", "lavender", "magenta",}
       \item{ 1 median state color: "black",}
       \item{ 1 highlighted area: "light yellow",}
       \item{ 2 above and below median highlighted areas: light red, light blue,}
       \item{ 1 color for non refernce (used) areas in the data,}
       \item{ 1 color for non-active areas in the border group,}
       \item{ 12 translucent colors using the above colors at 20\%.}
    }
    It is strongly recommended to use the default. When changing the colors list, then 
    the entire list must be specified.  
  }
  \item{details}{is a list structure that contains the internal variables and values used by 
       micromapST to create the graphics structure layout and guide the operations
       of the \var{micromapST} function. The details internal variables provide a way to 
       tune the look of the created link micromap and its glyphs. These internal variables
       are divided into two groups: General and Advanced.  The general variable don't affect how
       the panels are constructed, but allow you to change the looks of the graphics: dot, shapes, 
       colors, line weights, etc.  The advanced variable affect the structure of the panels 
       and how areas are presented.  The following internal variable accessible through the details 
       named list are grouped by general usage and their glyph types.
       
       To change the values of items in the details list, only the variable(s) requiring change 
       need to be specified as a list for the details=list() parameter in the call.  
       In general, the beginning of the variable names indicates the glyph or glyph group 
       the variable is associated, in most cases.
             \preformatted{
               Arrow.    -> arrow glyph
               Bar.      -> bar glyph
               BoxP      -> boxplot glyph
               CBar      -> ctrbar glyph
               CSNBar    -> ctrbar, segbar and normbar glyphs. 
               Dot.      -> dot, dotconf, and dotse glyphs
               Dot.conf. -> dotconf glyph
               Dot.Signf -> dot with significance overlay.
               Grid      -> grid elements of all glyphs
               Id.       -> id glyph
               Map.      -> map glyphs
               Panel     -> general glyph panel
               Rank      -> area ranking glyphs.
               Ref       -> Reference text and line
               SCD.      -> scatter dot glyph
               SNBar     -> segbar and normbar glyphs
               Title     -> page and column labels and titles
               TS        -> ts and tsconf glyph
               TSconf    -> tsconf glyph
              }
              
       For example: to turn off the midpoint dot in the segmented bar glyphics, 
       all that is required is:
       \preformatted{
          details = list(SNBar.Middle.Dot=FALSE)
       }
          
       \describe{
         \item{\bold{General Variables:}}{
          The following are the internal variables for the XAxis, Grid, Panels, Reference Line, and the Glyphs.
          \describe{
           \item{\bold{X-Axis variables:}}{
            \describe{
             \item{XAxis.Sp.mcex}{= 0.2  Size used for XAxis space between lines of labels}
             \item{XAxis.indent}{= 10 in 1000th of an inch. First and Last label indents from edge.}
             \item{XAxis.nGridpIn}{= 3.4 labels per inch in XAxis - initial objective.}
             \item{XAxis.gapPC}{= 0.75 (*100 for precentage). Percentage of the width of a space 
               used to determine label overlaps.}
            }
           }
           \item{\bold{Y-Axis variables:}}{
           
            \describe{
             \item{YAxis.width}{= 0.2 inches. Extra column gap size required to support drawing an 
               Y-Axis for TS and ScatDot glyphics}
            }
           }
           \item{\bold{Grid and Panel Variables:}}{
           
            \describe{
             \item{Grid.Line.col}{= "white" Grid line color}
             \item{Grid.Line.lwd}{= 1, Grid line width}\cr 
             \item{Panel.Fill.col}{= "#676767FF", defaults to light gray}
             \item{Panel.Outline.col}{= "black", color of panel outlines}
            }
           }
           \item{\bold{Reference Line and Text:}}{
            The following variable related to the reference text and line feature.
            \describe{
             \item{Ref.Val.lty}{= "dashed", set reference value line to dashed}
             \item{Ref.Val.lwd}{= 1.5, line width of reference line}
             \item{Ref.Val.col}{= "midgreen", color of reference line when color is used.}
             \item{Ref.Val.BW.col}{= "black", color of reference line when grays are used.}\cr            
             \item{Ref.Text.col}{= "black", color of reference line text when color is used.}
             \item{Ref.Text.BW.col}{= "black", color of reference line text when grays are used.}  
            }
           }
           \item{\bold{Arrow Glyph:}}{
            The following variables are used by the \option{arrow} glyph.
            
            \describe{
             \item{Arrow.lwd}{= 2.5, line width of arrow.}
             \item{Arrow.cex}{= 0.08, size of arrow  ( not implemented )}
             \item{Arrow.Head.length}{= 0.08, length of arrow.}\cr             
             \item{Arrow.Dot.pch}{= 21, arrow-dot symbol 19-25.}
             \item{Arrow.Dot.pch.size}{= 0.9 cex, arrow-dot size.}
             \item{Arrow.Dot.pch.lwd}{= 0.5, line weight used on filled arrow-dot symbols.}\cr
             \item{Arrow.Dot.Outline}{= FALSE, include dot outline when filled. FALSE=NO, TRUE=YES}
             \item{Arrow.Dot.Outline.col}{= "black", color used for arrow-dot outline.}
             \item{Arrow.Dot.Outline.lwd}{= 0.5, line weight for arrow-dot outline.}
            }
           }
           \item{\bold{Bar Glyph:}}{
            The following variables are used by the \option{bar} glyph.
            
            \describe{
             \item{Bar.barht}{= 2/3,  fraction of line height for bar. 
               Should never be > .90. Usable range is 0.333 to 0.90}
             \item{Bar.Outline.col}{= "black", color of bar outline.}
             \item{Bar.Outline.lwd}{= 0.5, line width for bar outline }
             \item{Bar.Outline.lty}{= "solid", line type for bar outline }
            }
           }
           \item{\bold{Boxplot Glyph:}}{
           
            The following variables are used by the \option{boxplot} glyph.
            
            \describe{
             \item{BoxP.thin}{= 0.2, line width of box.}
             \item{BoxP.thick}{= 0.6, thick line width.}
             \item{BoxP.Use.Black}{= FALSE, whether to outline the outlier points.}\cr           
             \item{BoxP.Median.col}{= "black", color of median box}
             \item{BoxP.Median.Line}{= 0.80, line width of median line.}\cr             
             \item{BoxP.Median.Dot.lwd}{= 2, line width for median.}\cr
             \item{BoxP.Outlier.lwd}{= 0.4, line width of outlier outlines.}
             \item{BoxP.Outlier.cex}{= 0.7, size of outlier dots.}
             \item{BoxP.Outlier.BW.col}{= "#4c4c4cFF" color of outliner lines when greys used.}
            }
           }
           \item{\bold{Dot, Dotconf, and DotSE glyphs:}}{
           
            The following variables are used by the \option{dot}, \option{dotconf}, \option{dotsignif}, 
            and \option{dotse} glyphs.
            \describe{
             \item{Dot.pch}{= 21, solid circle (S compatible).}
             \item{Dot.pch.size}{= 0.9 cex, size of dot.}
             \item{Dot.pch.lwd}{= 0.5, linen weight for dot outline when 0:18 dot used.}\cr         
             \item{Dot.Outline}{=FALSE, whether to outline the dots.}
             \item{Dot.Outline.col}{= "black", color of dot outline.}
             \item{Dot.Outline.lwd}{= 0.5, line width of dot outline.}
            }
           }
           \item{\bold{Dotconf Glyph:}}{
           
            In addition to the variables listed above, the \option{dotconf} glyph also 
            has the following variable.
            \describe{
             \item{Dot.Conf,pch}{= 21, solid circle (S compatible).}
             \item{Dot.Conf.pch.size}{= 0.9 cex, size of dot.}
             \item{Dot.Conf.pch.lwd}{= 0.5, linen weight for dot outline when 0:18 dot used.}\cr
             \item{Dot.Conf.Outline}{=FALSE, whether to outline the dots.}
             \item{Dot.Conf.Outline.col}{= "black", color of dot outline.}
             \item{Dot.Conf.Outline.lwd}{= 0.5, line width of dot outline.}\cr           
             \item{Dot.Conf.lwd}{= 2, line width of confidence interval lines.}
            }
           }
           \item{\bold{Dotse Glyph:}}{
           
            In addition to the variable defined above for the \option{dot}, \option{dotconf}, 
            \option{dotsignif} and \option{dotse} glyphs, the \option{dotse} glyph also has the 
            following variable define.
            \describe{
             \item{Dot.SE,pch}{= 21, solid circle (S compatible).}
             \item{Dot.SE.pch.size}{= 0.9 cex, size of dot.}
             \item{Dot.SE.pch.lwd}{= 0.5, linen weight for dot outline when 0:18 dot used.}\cr
             \item{Dot.SE.Outline}{=FALSE, whether to outline the dots.}
             \item{Dot.SE.Outline.col}{= "black", color of dot outline.}
             \item{Dot.SE.Outline.lwd}{= 0.5, line width of dot outline.}\cr           
             \item{Dot.SE}{= 95, percent confidence interval }
             \item{Dot.SE.lwd}{= 2, line width of confidence interval lines.}
            }
           }
           \item{\bold{dotsignif Glyph:}}{
           
            In addition to the variable defined above for the \option{dot}, \option{dotconf}, 
            \option{dotsignif}
            and \option{dotse} glyphs, the \option{dotsignif} glyph also has the following variable define.
            \describe{
             \item{Dot.Signif.pch}{= 4, overprint character "x" when not significance.}
             \item{Dot.Signif.pch.size}{= 0.9*1.2 cex size of the overprint character.}
             \item{Dot.Signif.pch.lwd}{= 0.5, linen weight for dot outline when 0:18 dot used.}
             \item{Dot.Signif.pch.col}{= "black", color of overlaid symbol on DOT.}\cr
             \item{Dot.Signif.Outline}{=FALSE, whether to outline the dots.}
             \item{Dot.Signif.Outline.col}{= "black", color of dot outline.}
             \item{Dot.Signif.Outline.lwd}{= 0.5, line width of dot outline.}\cr
             \item{Dot.Signif.pvalue}{= 0.05, p-value for testing significance.}
             \item{Dot.Signif.range}{= c(0,1), valid range for significant test data for p-value}
            }
           }
           \item{\bold{id Glyph:}}{
           
            The following variables are used by the \option{id} glyph.
            \describe{
             \item{Id.Cex.mod}{= 1, fudge adjustment for ID Text.}/cr
             \item{Id.Title.1.pos}{= 0.9 inches, top panel 1st line id title placement above the first panel, 
               used with lab1}
             \item{Id.Title.2.pos}{= 0.1 inches, top panel 2nd line id title placement above the first panel, 
               used with lab2}\cr
             \item{Id.Text.cex}{= 0.65, text side of ID column}\cr
             \item{Id.Dot.pch}{= 22, pch symbol value to plot next to state name/abbrev.}
             \item{Id.Dot.cexm}{= 1.5, size of dot symbol for state ID}
             \item{Id.Dot.lwd}{= 0.8, size of solid dot symbol for state ID}
             \item{Id.Dot.width}{= 0.1 inches.  With of the ID symbol (box)}\cr
             \item{Id.Space}{= 0.03125,  width of a space in inches.}
             \item{Id.Start}{= 0.055,  offset from left for start of ID column.}
            }
           }
           \item{\bold{map, mapcum, maptail, and mapmedian Glyphs:}}{
           
            The following variables are used by all of the "map" type glyphs.
            \describe{
             \item{Map.Area.Spec.cex}{= 0.32, font size for state labels}
             \item{Map.Bg.col}{= "#262626FF",  (grey(0.88)) color of background (not active) sub-areas fill in maps}
             \item{Map.Bg.Line.col}{= "white", background of maps}
             \item{Map.Bg.Line.lwd}{= 0.3,  line weight for map background boundaries}
             \item{Map.Fg.Line.col}{= "black", foreground color of maps}
             \item{Map.Fg.Line.lwd}{= 0.3, line weight for map foreground boundaries}
             \item{Map.L2.Line.col}{= "lighter grey", color of Layer 2 outline in maps}
             \item{Map.L2.Line.lwd}{= 0.35. line weight for Layer 2 boundaries}
             \item{Map.L3.Line.col}{= "black", color of Layer 3 (national) outline in maps}
             \item{Map.L3.Line.lwd}{= 0.4. line weight for Layer 3 (national) boundaries}
             \item{Map.Lab.Box.Width}{= 0.09, width in inches of box symbols using in titles for maps}
             \item{Map.Max.width}{= 2.5 inches, maximum width of each map}
             \item{Map.Min.width}{= 1.5 inches, minimum width of each map}
             \item{Map.Median.text}{= "Median for Sorted Panel", text used in single row median panel instead of a map.}
            }
           }
           \item{\bold{Rank Glyph:}}{
           
            The following variable is used by the \option{rank} glyph.
            \describe{
             \item{Rank.width}{= 0.25 inches - column fixed width}
             \item{Rank.method}{= 1,  rank method - to be defined.}
            }
           }
           \item{\bold{Scatter Dot Glyph:}}{
           
            The following variables are used by the scatter dot glyph (\option{scatdot}).
            \describe{
             \item{SCD.Axis,cex}{= 0.52, font size for Y axis labels for scatter dots}\cr
             \item{SCD.Bg.pch}{= 21, type of point/symbol to be used for background 
               data points (not active) - state's dots.}
             \item{SCD.Bg.pch.fill}{= "transparent", fill color for not selected state's 
               dots.}
             \item{SCD.Bg.pch.col}{= "black", border color used for non-active data points.}
             \item{SCD.Bg.pch.lwd}{= 0.6, line width of outline of point/symbol used as 
               non-active data points.}
             \item{SCD.Bg.pch.size}{= 0.75, size of point/symbol used as non-active 
               data points.}
             \item{SCD.Fg.pch}{= 21, type of point/symbol for active data points.}
             \item{SCD.Fg.pch.col}{= "black", border color for foreground dots.}
             \item{SCD.Fg.pch.lwd}{= 0.6, Scatter dot symbol outline line weight for 
               active data points.}
             \item{SCD.Fg.pch.size}{= 1, size of point/symbol for active data points 
               in scatter dots.}
             \item{SCD.Median.pch}{= 21, shape of filled symbol for median value - 
               scatter dots.}
             \item{SCD.Median.pch.col}{= "black", border color for median symbol - scatter dots.}
             \item{SCD.Median.pch.lwd}{= 0.6, line width used on the median symbol 
               - scatter dots.}
             \item{SCD.Median.pch.size}{= 1, symbol size median value 
               - scatter dots.}
             \item{SCD.Median.pch.fill}{= "black", color of filled symbol for median value 
               - scatter dots.}
             \item{SCD.hGrid}{= FALSE, whether or not to include horizontal grid lines in panel.}\cr    
             \item{SCD.xsc}{= 1.08,  x range multiplier to keep dots from being clipped}
             \item{SCD.ysc}{= 1.12, y range multiplier to keep dots from being clipped}\cr    
             \item{SCD.DiagLine}{= TRUE, whether or not to include x=y sloped line}
             \item{SCD.DiagLine.col}{= colGrid, color of sloped line, default, grid line color.  
                SCD.DiagLine must be TRUE.}
             \item{SCD.DiagLine.lwd}{= 1.25, line weight of sloped line, default, grid line color.  
                SCD.DiagLine must be TRUE.}
             \item{SCD.DiagLine.lty}{= "solid", line type of sloped line, default, grid line color.  
                SCD.DiagLine must be TRUE.}
            }
           }
           \item{\bold{Segmented Stacked Bar Glyphs:}}{
           
            micromapST support three types of stacked bar graphs:  Centered, Segmented, and Normalized.
            The following section describes the internal variable used by all of these glyphs and then
            the unique variables used by each type.
            \describe{
             \item{\bold{Centered, Segmented, and Normal Stacked Bar Glyphs:}}{
              The following variables are used by all of the horizontal stacked bar glyphs 
              (\option{ctrbar}, \option{segbar} and \option{normbar}).  Any variable for a 
              specific type of stacked bar are listed following this section.  The CSN at the 
              beginning of the name of each variable indicates they are part of this group.
              \describe{
               \item{CSNBar.barht}{= 0.66667, fixed height of bar when variable height 
                bars are is not used.  Should never be greater than 0.90. Usable range 
                is 0.333 to 0.90.}
               \item{CSNBar.First.barht}{= 0.3333, height of first bar when variable 
                height bars are used. Must be less than SBar.Last.barht and in the range 
                of 0.333 to 0.6667,  SNBar.varht or CBar.varht must be TRUE for this option 
                of function.}
               \item{CSNBar.Last.barht}{= 0.80, height of last bar when variable 
                height bars are used. Must be greater than CSNBar.First.barht and in the 
                range of 0.6667 to 0.90. CSNBar.varht or CBar.varht must be TRUE for 
                this option of function.}\cr          
               \item{CSNBar.Outline.col}{= "black", color of stacked bar outlines.}
               \item{CSNBar.Outline.lwd}{= 0.75, line weight for bar segment outline in 
                segmented bar plots.}
               \item{CSNBar.Outline.lty}{= "solid", line type for bar segment outline 
                in segmented bar plots.}
              }
             }
             \item{\bold{Centered Stacked Bar Glyph:}}{
              The following variables are only used by the \option{ctrbar} glyph.
              \describe{
               \item{CBar.varht}{= FALSE,  enables variable height bars.}
               \item{CBar.two.ended}{= FALSE, request two ended variable height bars be used }\cr          
               \item{CBar.Center.Line.enable}{= FALSE, request a line is drawn at center point.}
               \item{CBar.Center.value}{= 0, value of the center stacked bar glyph. def=0}\cr
               \item{CBar.Zero.Line.col}{= "white", centered bar zero vertical line color}
               \item{CBar.Zero.Line.lwd}{= 1, line width for centered bar zero vertical line}
               \item{CBar.Zero.Line.lty}{= "dotted", type of centered bar zero line.}
              }
             }
             \item{\bold{SegBar, and NormBar Glyphs:}}{
              The following variables are used by the \option{segbar} and \option{normbar} 
              glyphs:
              \describe{
               \item{SNBar.varht}{= FALSE, enables variable height bars from 
                SBar.First.barht to \cr SBar.Last.barht.}
               \item{SNBar.two.ended}{= FALSE, request two ended variable height 
                bars be used (small to large to small).( Not implemented )}           
               \item{SNBar.Middle.Dot}{=FALSE, request a dot be draw in at the 
                mid point in the segmented bars.}
               \item{SNBar.MDot.pch}{=21, type of point/symbol used as the mid 
                 point dot. SNBar.Middle.Dot must be TRUE for this parameter 
                 to function.}
               \item{SNBar.MDot.pch.fill}{="white", color of the point/symbol 
                 used as the mid point dot. SNBar.Middle.Dot must be TRUE for 
                 this parameter to function.}
               \item{SNBar.MDot.pch.size}{=0.3, size of point/symbol used 
                 as the mid point dot. \cr SNBar.Middle.Dot must be TRUE for this 
                 parameter to function.}\cr             
               \item{SNBar.MDot.pch.border.lwd}{=NA, line width of outline of 
                 point/symbol used as the mid point dot. \cr
                 SNBar.Middle.Dot must be TRUE for this parameter to function.}
               \item{SNBar.MDot.pch.border.col}{=NA, color of outline of 
                 point/symbol used as the mid point dot. SNBar.Middle.Dot 
                 must be TRUE for this parameter to function.}
              }
             }
            }
           }
           \item{\bold{Time Series and Time Series with Confidence bands Glyphs:}}{
            The following variables are used by the time series glyphs 
            (\option{ts} and \option{tsconf}):
            \describe{
             \item{TS.lwd}{= 1.1, time series line weight}
             \item{TS.Axis.cex}{= 0.52, font size for Y axis labels}
             \item{TS.hGrid}{= FALSE, whether or not to include horizontal 
              grid lines in panel}
            }
           }
          }
         } 
         \item{\bold{Advanced Internal Variables:}}{
          The advanced internal variable should not be modified unless you have 
          lot of time.  If any are modify, the operation of the package may become 
          unpredicatable and can't be supported.  However, through the use of 
          gentle changes and experimentation, you can modify the look of 
          the resulting micromapST output.  While not recommended, the authors 
          felt access to these internal variable will help a user in some 
          unexpected situations. \cr
          These variables should not be modified unless absolutely necessary.  
          IF there area modified, the outcome can not be predicted and can't 
          be supported.  Modify at your own risk:
          \describe{
           \item{\bold{Page and Panel Layout:}}{
            \describe{
             \item{topMar}{= 1.2, top margin in inches}\cr
             \item{botMar}{= 0.5, bottom margin in inches}
             \item{botMarLegend}{= 0.75, bottom margin for legend }
             \item{botMardif}{= 0.2,  bottom margin difference}\cr
             \item{leftMar}{= 0.0, left margin in inches}
             \item{leftMarAxis}{= 0.2, left margin when Y axis labels and title are required}
             \item{rightMar}{= 0.0, right margin in inches}\cr            
             \item{borderSize}{= 0.5 inches. The border space between the page 
              edges and the margins. This value is used for the top, bottom, 
              left and right border spacing.}
            }
           }
           \item{\bold{Margins and Axis Layout:}}{
            \describe{
             \item{mgpLeft}{= c(0.75, 0.1, 0), Left Y axis margin line for axis labels and axis line.}
            }
           }
           \item{\bold{Scaling, Separation, and Padding:}}{
            \describe{
             \item{pad}{= 0.67, y axis padding for integer plotting locations}
             \item{padex}{= 0.34 inches, total panel padding (i.e., 0.17 at top and bottom of panel)}
             \item{padMinus}{= 0.63 inches, spacing to keep reference line off panel edge}
            }
           }
           \item{\bold{Row and Column Parameters:}}{
            \describe{
             \item{rowSepGap}{= 0.075 inches. Space between panel groups at or 
               around the median panel. There are 7 units per panel. The average 
               unit is between 1/10 and 1/8 inches.}
             \item{rowSizeMn}{= 0.5 inches. Minimum panel group height.}
             \item{rowSizeMx}{= 1.25 inches. Maximum panel group height.}\cr       
             \item{rowSizeMin}{= 1.65 units. Minor panel group height in units 
              for single area panel.}
             \item{rowsizeMaj}{= 7 units. Major panel group height for all panels, 
              except single area panel.}\cr
             \item{colSepGap}{= 0.75 inches.  Glyphics column separator space.}\cr 
             \item{colSizeMin}{= 0.75 inches. Minimum Glyph column size.}
             \item{colSizeMax}{= 2.0 inches.  Maximum Glyph column size.}
            }
           }
           \item{\bold{Title Variables:}}{
            \describe{
             \item{Title.Line.1.pos}{= 1.27 inches, top panel 1st line placement above 
              the first panel, used with lab1}
             \item{Title.Line.2.pos}{= 0.64 inches, top panel 2nd line placement above 
              the first panel, used with lab2}
             \item{Title.Line.2x.pos}{= 0.01 inches, top panel X-Axis line placement 
              above the first panel}
             \item{Title.Line.3x.pos}{= 0.01 inches, bottom panel X-Axis line placement 
              below the last panel}
             \item{Title.Line.3.pos}{= 0.64 inches, bottom panel line placement below 
              the last panel, used with lab3}
             \item{Title.Line.4.pos}{= 1.27 inches, reference line legend below last panel, 
              used with reftext}
             \item{Title.Line.5.pos}{= 0.35 inches, Y axis label placement (to the left 
              of panel), used with lab4}
             \item{Title.cex}{= 1.0, text size of title, used with title}
            }
           }
           \item{\bold{Debug Variable:}}{
            The following variable is reserved for package testing only and 
            should not be used.
            \describe{
             \item{MST.Debug}{= 0, disabled.  Do not use.}
            }
           }
          }
         } 
         \item{Unused Variables:}{
          The following variables are not implemented and reserved for future use.
          \describe{
           \item{(small to large to small).}{( Not implemented )}\cr         
           \item{rcRatioMin}{= 0.25, minimum row size to col size ratio permited. (not implemented)}
           \item{rcRatioMax}{= 2, maximum row size to col size ratio permited. (not implemented)}
           \item{mgpTop}{= c(3.2,0.1,0), Top margin line for X axis labels and axis line.}
           \item{mgpBottom}{= c(3.2,0.1,0), Bottom margin line for X axis labels and axis line.}
           \item{padjBottom}{= -0.35, Axis tick label placement adjustment}
           \item{sc}{= 1.08, x axis scale expansion factor.  Applied to the data range to 
            calculate the graph\'s range.}\cr           
           \item{XAxis.staggered}{= TRUE,  enable staggered label feature - NOT USED}
           \item{XAxis.L.mcex}{= 0.888889 - actually 0.6667  Size used for large XAxis labels}
           \item{XAxis.M.mcex}{= 0.777778 - actually 0.5833  Size used for medium XAxis labels}
           \item{XAxis.S.mcex}{= 0.666667 - actually 0.5  Size used for small X Axis labels
             For line labels - Normal = .75, space = .15 (20\%),  small space = .15 .5 = .075}
           \item{XAxis.offset}{= 0.0 inches. X Axis offset}
           \item{YAxis.cex}{= 0.33332 cex size of Y Axis labels}
           \item{YAxis.offset}{= 0.0 lines. Offset of Y Axis labels from panel edge.}
           \item{YAxis.nGridpIn}{= 5 labels. Number of labels per inche for Y Axis - initial goal.}
           \item{YAxis.staggered}{= TRUE.  Enable staggered labels on Y Axis - NOT USED.}/cr
           \item{Ref.Text.cex}{= 0.75, size of reference line text}\cr         
           \item{Arrow.Shadow.lwd}{= 4.0, line width of arrow shadow to create outline ( not implemented )}
           \item{Arrow.Shadow.col}{= "black", arrow shadow color ( not implemented )}\cr
           \item{BoxP.Outlier.pch}{= 20, symbol for outlier - 19-25.}
           \item{BoxP.Outline.col}{= "#262626FF", boxplot outline color}
           \item{BoxP.Median.Dot.pch}{= 19,  solid circle symbol.}
           \item{BoxP.Median.Dot.col}{= "white", color of median dot.}
           \item{BoxP.Median.Dot.cex}{= 0.95, size of circles.}\cr         
           \item{Dot.Conf.size}{= 0.55, size of confidence interval}
           \item{Dot.SE.size}{= 0.55, size of confidence interval}\cr          
           \item{Id.Dot.Outline.col}{= "dark gray", color of outlines of ID symbols}
           \item{Id.Dot.Outline.lwd}{= 0.8, line weight for outlines of ID symbols}\cr          
           \item{Map.Bg.Line.lty}{= "solid",  line type for map background boundaries}
           \item{Map.Fg.Line.lty}{= "solid", line type for map foreground boundaries}
           \item{Map.L2.Line.lty}{= "solid", line type for Layer 2 boundaries}
           \item{Map.L3.Line.lty}{= "solid", line type for Layer 3 (national) boundaries}
           \item{Map.Panel.col}{= "white",  background color of map panels}
           \item{Map.Unu.col}{= "lightest grey",  color of sub-areas not referenced in maps}\cr  
           \item{staggered}{= FALSE.  Position of last staggered label.  FALSE = low, TRUE = high.}
           \item{Text.cex}{= 0.75 cex, general text size}		
          }
         }
        }
       }
  }
 The micromapGDefaults data.frame is built by the micromapGSetDefaults function
 when the micromapST package is called. Once built it cannot be changed.
 To change one or two (a few) variables, construct a list of these variables and pass
 it to micromapSEER via the details parameter in the call.
 To do large scale customization, call the micromapGSetDefaults function to get
 a copy of the entire data.frame and modify this copy.  This is not recommended.  
}
\author{Daniel B. Carr, George Mason University, Fairfax VA, 
with contributions from Jim Pearson and Linda Pickle of 
StatNet Consulting, LLC, Gaithersburg, MD
}
\keyword{data structure}
\seealso{
  \link{micromapGSetDefaults},
  \link{micromapGSetPanelDef}
}

