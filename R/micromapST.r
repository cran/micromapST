#
# Updated Package Version 120828
# Updated Package Version 130426
# Updated Package Version 130506 - V0.94
# Updated Package Version 130510 - V0.95  - fixes.
# Updated Package Version 130511 - V0.96  - attempt to complete - 
# Updated Package Version 130511 - V0.97  (8:00pm) - fixes 
# Updated Package Version 130513 - V0.98  (8:00am) - fixes and testing
# Updated Package Version 130517 - V0.99  - fixes and work with BW.
#                                         - correct ref line color and minor updates.
#                                         - corrected micromapSTDefaults and Arrows errors.
#                                         - label adjustment and fix parameter checking for boxplots
# Updated Package Version 130604 - V1.0.0  - Final Edit and fixes for release.
#                                         - Dynamically defined variables must be globalVariables add.
#                                         - Formal Release of package.
# Updated Package Version 131127 - V1.0.1 - Correct segmented and centered  bars to handle only two data columns
# Updated Package Version 140104 - V1.0.2 - Add diagonal line in scatter plot with equal x and y values.
#                                         - Update NormSeg, Seg, Centered Seg to use variable width bars.
#                                         - Changed method of providing colors and details parameters.
#                                         - Correct median dot in scatter plots
#                                         - Add logic to allow numeric (integer) or column names in col1, col2, col3
#                                         - Correct logic to handle multiple columns in sortVar.
# Updated Package Version 140307 - V1.0.3 - Add Rank Glyph
#                                         - Remove limit on number of time series elements.
#                                         - Plot the median time series data in the panels above and below
#                                           the median row.
#                                         - Adjusted defaults on stacked bar graphs
# Updated Package Version 140712 - V1.0.4 - Correct usage of single and double quote marks in examples.
# Updated Package Version 141107 - V1.0.5 - Parameter checking of the panelDesc arguments is incorrect.
#                                           Logic is rewritten and migrated into this package.
#
# Updated Pagkage micromapSEER - 141023 - V0.90 - Modified package to meet NCI Seer's requirements.
#                                           Rewrote map... logic to handle different number of rows
#                                             per panel, scaled glyphs to be the same size in 
#                                             panels of 1, 2, 3, 4 or 5 areas.
#                                           Modified SegBar, NormBar, Bar, and BoxPlot glyphs 
#                                             to handle different number of areas per panel and 
#                                             present the same sized glyph
#                                           Modified logic to accept a SEER border and area dataset or
#                                             the full US States area dataset.
#                                           Fixed logic in mapcum, mapmedian and maptail to correctly 
#                                             draw a square rectangle in the title, independent on the 
#                                             number of columns or rows of panels.
#                                           Fixed ID glyph to dynamic determine width of column 
#                                             based on the abbreviated or fullname text in the SEER
#                                             or US state datasets.  Corrected code to properly draw
#                                             the same sized square box and align with text for all 
#                                             ID glyph lines.  
#                                           Added logic to force min. and max column widths.
#                                           Added logic to force min. and max panel row height.
#                                           Correct distance from axis labels to tics to be the same
#                                             on the top and bottom axis labels.
#                                           Initially setup tables to provide uniform distribution 
#                                             of areas across panels.  This caused to many 3 and 4 
#                                             area panels.  Re-did the setup tables to minimize the
#                                             number of panels and use 4 and 5 areas per panel when
#                                             ever possible.
#                                           Correct datasets to contain all UPPER case abbreviations
#                                             and properly capitalized Full Names.
#                                             Internal to program all matching is done using UPPER
#                                             case strings.
#                                           Added logic to include a "like" string for each SEER area
#                                             to allow matching with SEERStat generated datasets.
#                                           Since data.frames are mostly constructed with factors,
#                                             the user may pass us a statsDFrame containing factors 
#                                             instead of numeric values.  Code added to check for
#                                             numerics in the statistical data provided, convert
#                                             from character is required, and convert from factors
#                                             if required.
#                                           User data may have extra line at the end of the data, added
#                                             option to delete last line. if not a match.
#                                           Fixed validating the data in the statsDFrame columns in 
#                                             each glyhpic.
#                                           Fixed logic handling odd number of groups with the 
#                                             middle group having > 1 areas.
#                                           Added logic to detect empty data columns in the statsDFrame.
#                                             character contain had to be checked if it can be converted to 
#                                             numeric.
#                                           Corrected logic to handle multiple border groups.  Default for 
#                                             Seer is "USSeerBG".  However, not providing the argument 
#                                             set no values or the wrong value in BordGrpName.
#                                           The aspect of the US maps was off. Corrected the maximum
#                                             height value from 0.6 to 0.8 inches.
#                                           Changed the name of the main module to micromapPLUS.  Add two front-end 
#                                             functions - micromapST and micromapSEER to provide a dual interface for
#                                             existing users.
#                                           Separated micromapGSetDefaults and micromapGSetPanelDef functions into 
#                                             a separate ".r" to share with micromapSEER.
#
#                               - 150112 Updates:
#                                           Corrected problem with printing US "DC", "HI", "AK" labels on non-US 
#                                             maps.  Used the areaParms$areaUSData to control this feature.  This
#                                             bordGrp parameter should only be set to "TRUE" when the full US map
#                                             and states are used in the bordGrp.
#                                           Changed the deleteLast option to ignoreNoMatch options and 
#                                             redid the code to do this function and generate the information
#                                             and error messages.
#                                           Changed module name back to micromapST.
#                                           Changed version number to 1.1.0 to note major revision.
#
#                               - 150312 Updates:
#                                           Change USStatesBG user data.frame check from: must be 51 rows to 
#                                             must be 51 or less rows.  Allow data with subsets of states.
#                               - 150713 Updates:
#                                           Update structure of areaParms table in border groups 
#                                           Add several more border groups to the package: UK-Ireland, Seoul, Utah
#                                           Add staggered x-Axis labels to keep ends from overlapping.
#                                           Add feature to allow user to specify x-Axis for glyph
#                                           Update glyphs to formally handle NA values in data.
#                                           Update X-Axis to include labels if grid is drawn (???)
#                                           Update map code to enforce minimum width to make sure 
#                                              space is wide enough for all titles and labels.
#                                           Add "Alt_Abbr" option for rowNames
#                                           Update code to use "LINK" or make sure "Abbr" works.
#                                           Changed Border Group .rda file name from ????DF.rda to ????BG.rda.
#                                           Added MapLabel field to areaNamesAbbrsIDs tables - to be used to generalize
#                                              the over printing of sub area names on first map - AK, HI, DC like.
#                               - 150715 Updates:
#                                           Changing name table structure to have "full", "ab", "alt_ab", "id"
#                                           pointing to "key" rather than abbr.  This is to handle any cases
#                                           down the road that don't have abbr, full or ID.   If the 
#                                           column is not present, the option will not be available.
#                               - 160807 Updates:
#                                           Fix position of first title above the columns. Too close to 
#                                           axis by about 1/2 a line.
#                               - 160812 Updates:
#                                           add individual DOT symbol control to dotconf and dotSE.
#                                           updated detailsVariables to reflect the new details options
#                                           and future conversions.  Check code and added all missing
#                                           variables.
#                               - 160816 Updates:
#                                           modified labels code to use odd number of labels and a minimum of 3.
#                               - 161120 Updates:
#                                           added regional IDs and Names to the name table. 
#                                           Added better overlay print control for L2, Reg, and L3.
#                                           Corrected X Axis label logic - removed duplications and 
#                                           parameter resetting.
#                               - 161206 Updates:
#                                           Changed NAMESPACE, DESCRIPTION files to meet new
#                                           CRAN requirements.
#                                           Modified code to not directly use assigns for 
#                                           variables in the .GlobalEnv space.
#                                           Modified all data() function calls to load
#                                           the data into the current environment.
#     Release 1.1.1  -------------------
#
#                               - 161207 Updates:
#                                         - update all calls to foreign functions to include package 
#                                           names (utils::data(), stringr::str_XXX, 
#
#
#
#  discussion points:  not all border groups have abbreviations or IDs.  Names yes, but need to 
#       handle the value inputed by the user and link data to boundaries. May be needed
#       to build "internal" link and have all else point to it? (if present.)
#
#  Update Log and change details by Jim Pearson
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
#             details$Ref.Text.col
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
#        - changed parameter for BoxPlots colMedian to BoxP.Median.col to kill duplication with the colMedian 
#          used on the general graphic
#        - Modified "Details" and "Colors" variable to be unique and
#          re-ordered by subroutine usage.
#    October 5, 2012 - update documentation for review.
#        - deleted second version of panelGroupOutline- in panelFunctions.r
#        - Changed rlAreaRefText function to build a legend with a line followed by 
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
#          done by using the sorted stateId column in the statsDFrame to re-order the panelData frames.
#        - added programing feature to permit adjustments to colsize, left and right margins of a 
#          panel based on the type of panel to be created.  Needed to allow space for the 
#          left axis labels for the time series panels (4).
#    May 4, 2013 - remove prototype strip time series - did not work, code deleted.
#        - Added centered stacked bars.
#        - changed circle size on Scatdot of non-colored dots to 75 smaller.
#        - Changed source of data for "scatdot", "segbar", "normbar", and "ctrbar" from 
#           an extra panelData structure to using columns in the statsDFrame call parameters data.frame.
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
#        - created segbar data file.
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
#        - Added Id.Text.adj parameter to details and rlAreaID to adjust text alignment.
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
#    Jan 4-9, 2014 - The diagonal line added to the scatter plots must reflect equal x and y values. 
#          Current line is diagonal to the box not the data.
#         - Add option to vary the segment bar width from small to larger from left to right for
#           the NormSeg, SegBar, and Centered SegBar glyphs.
#         - Changed method of setting up details variables within the micromapST namespace.
#           Originally, user had to provide a complete list of all of the details variables.  If
#           one was missing or misspelled, no detection or correction.  New method, starts by 
#           assigning all of the variables from the default values. Then takes the provided details
#           list from the user and merges it into the already declared variables.  If a variable
#           does not exist or is misspelled, it is caught by checking against the default list of names
#           and not processed.  In the future, a similar structure will be used to check the 
#           ranges or types of information to validate the user provided details variable values.
#         - Correct median dot in scatter dot plots to only appear in the 4 and 6 rows (just either side
#           of the median row.
#         - Update logic in sortVar option to correctly handle multiple sort columns.  
#         - Add ability to reference data.frame columns by name in the col1, col2, col3 and sortVar
#           parameters.
#         - Enhanced parameter verification and error checking to help user understand the specific
#           problem and correct it fast.  Don't allow R to abort if possible.
#    March 7, 2014 - Removed limit on the number of points in Time Series
#         - Add code for Rank glyph
#         - The time series line and confidence band are squeezed in the median row space and do not
#           properly show the data.  The median time series data is plotted in the panel above and below
#           to median row to properly present the data using the same aspect ratio as the other data.
#         - Adjusted the defaults for the segbar, ctrbar, and normbar graphics to have no center dot
#           and fixed bar height.
#    July 12, 2014 - Corrected single and double quote marks usage in examples.
#    November 7, 2014 - Rewrote panelDesc argument checking.
#    November 1-7, 2014 - Updated logic to handle the number of areas dynamically and support
#           US States or US Seer areas and data.
#         - Added logic to handle the specification of the link row names as a column of the 
#           area Data.frame columns instead of requiring the link to be the Abbr of the area
#           as the row.names of the statsDFrame data.frame.
#    April, 2015 - generalize package for NCI and CRAN release.  Add additional border groups.
#           Work on the scaling issues for the larger maps and number of rows and columns.
#    July, 2015 - Updated code to handle new border group structures
#         - Add "***" to the beginning of each error message and restructuring the message with a new 
#           message id, and to include name of the glyphs and the panel column number.
#         - Found error in multiple column sort feature. Rewrote code to handle.
#         - Found rank functions and code can not handle multiple columns.  Implemented
#           rank code to only handle 1 column.  But on new feature list.
#         - Updated code to work with Abbr, Alt_Abbr, Full Names, ID or Alias and 
#           map them to the border Vis Files key value.  This was done to handle 
#           cases where the user border group may not have an abbreviation to use as 
#           the link. If at least one exist, then it can be linked to the key.
#         - Updated code to correctly calculate the width of the mapxxx and id glyphs
#           columns using the basic font and text.cex sizes.  Must update when scaling is 
#           implemented.
#         - Modified the colors table to include a color for a 6th row in each group/rows
#           and two more shading colors for mapmedian and maptail to compensate for issues
#           when there is a median group/row with more than 1 row.
#         - Modified all glyphs to handle situations when an NA is present in the user data.
#           The general rule is not, all of the data or no plot. Ploting anything would 
#           possibly lead to an incorrect reading by the user.
#    August 2, 2015 - Rewrote the mapping routine to properly handle holes when filling
#           the polygons in the right order and to draw the borders in the order of 
#           Not Used polygons, background polygons, highlighted polygons, and active polygons.
#           This code also supported NotUsed sub-areas (color very light grey) and two color
#           highlights of sub-areas above and below the median when a map is used for the median 
#           group row.  
#         - Fixed problem with title parameter checking to handle 1 or 2 values in the vector.
#         - Tested 8.5x14 and 11x17 page sizes for Kansas, New York and UKIreland.  UKIreland is 
#           still very small but works.  Noticed line weight need to be adjusted when images are
#           small.
#         - added two colors for the median map to show above and below clear.  This is important
#           then the area has median group row with more than one row.  The above and below
#           are shown on the same map, so must be distinquished.
#         - corrected the calculations and implementation of the number of Groups, number of rows 
#           per group, number of median group, number of median rows to handle no median group (even
#           number of groups), a median group with 1 row, and a median group with > 1 row.  Adjusted
#           the code in all glyphs to handle the new implementation.
#         - implemented MapPolySetup function to do common setup and calculations for all Map functions.
#         - added check to warn user if there are more data rows, then sub-areas in the border group.
#           There are move checks later to identify the extra data.frame rows to the user.
#         - remove any check, stop or warning if the number of data rows are less than then number
#           of sub-areas in the border group.
#         - Changed the selecting the number of rows per group pattern from a very large table to a 
#           a calculation with a table override when needed.  User is also allowed to specify a 
#           pattern to override micromapST's calculation.
#         - changed titles on Mapmedian map from "Featured above/below" to "above median" and 
#           "below median".  Mapcum map from "Featured above/below" to "above/below panel rows"
#           Still thinking about the maptail titles.
#         - Implemented function to center box and text for column headers.
#      August 4, 2015 - Updated logic for x-Axis labeling.
#      August 8, 2015 - Fixed/Add ability to specify symbol for the ID glyphs (half implemented, now working.)
#         - Added details option "Map.Median.text" to allow the Median for Sorted Panels text to be changed.
#         - Added below column label ("lab3") to the map and id columns.
#         - Added the ability to change the areaParms variables via the details=list() feature.
#         - Corrected and re-implemented Id.Dot.pch feature for the ID glyph
#      August 16, 2015 - Corrected the reference text and line drawing logic - rewrote.  Line can now be what's left
#           up to 1/2 inch in length.  Text and line centered in column.
#         - Added options to specify type of scaling for the axis.  Original = pretty function limited by 
#           range of data.  Scaled range = subtitle used to identify units and values scaled by the units.  
#           Scaled number = each number in axis scaled, adjusted, and label with suffix to scaling (e.g., M, B, etc.)
#           Scaling below 1 is also done and properly identified using the International Standards of Units (SI) 
#           symbols.
#         - Added option to stagger the axis labels on the X axis to prevent overlaying.
#      August 20, 2015 - changed default labeling algorithm from rpretty to wilkinson. ("o" to "w")
#         - Implement test X axis labeling and column titling function (DrawXAxisAndTitles) in 
#           all glyphs.
#         - Reduced size of ID symbols by 20% - looks better.  
#         - Added ConvLinesToUnits function to help convert line coordinates to Unit coordinates and 
#           handle the offset if the bottom left corner is not 0,0.
#         - Fixed the refText and line problem to place the line in the middle of the text.
#      September 14, 2015 
#         - Add additional panelDesc column "adv" to support "new" parameters on a glyph column 
#           basis.  Column is a list of lists.  The lists in the column is contains
#           new and old options/parameters.  panelDesc column name is "adv".
#           Any of the old panelDesc columns can have values in the adv list.
#         - (FUTURE) add ability to detect if panelDesc is the original data.frame,
#           or the new list of list format.
#         - Cleaned up warning messages by adding "call.=FALSE" option to remove
#           calling routine information from warning.
#         - Started adding validation code for user provided details and colors.  This
#           will later be applied to the glyph parameters set by the user.
#      September 19, 2015
#         - constructed table of details Variables and properties to be used in verifying
#           the details variables (from system or user).  The table also contains information
#           to permit translation of existing details variables into glyphs based variables.
#      January 20, 2016
#         - Added ability to save list of called variable names for warnings and error messages.
#           saved the values in list in details.
#         - Added and tested "regions" call argument to allow only regions (l2) to be mapped 
#           if no data in other regions.
#         - Added code to capture call variable names (not values) for use in warning messages.
#         - Added check for rowNames = NULL
#      February 20, 2016
#         - Updated warning message identifiers and documentation to match.
#         - Corrected statsDFrame column checking routines to handle character numbers and 
#           provided the correct warning messages.
#         - Add CheckParmColx function to properly handle checking statsDFrame column names 
#           and numbers for rowNamesCol and sortVar call arguments.
#      February 29, 2016
#         - Changed wilkinson labeling algorithm to extended. The option is also changed from "w" to "e".
#           The wilkinson algorithm generated to many grid lines and labels vs. what was 
#           requested.
#      May 5, 2016
#         - changed alias code to edit user strings to delete special characters, blanks (multiple, trailing and 
#           leading), control characters, punctuation characters, and convert the string to all upper case.
#           Seer Stat has changed the default registry names to include "_"s instead of " " character between
#           the words.   The extra editing neutralizes the impact.  Function CleanString was added to handle lists
#           of registry names.
#      August 7, 2016
#         - first line of column titles too close to plot area by about 1/2 a line.  Found calculation off.  
#           Re-implemented using table of line heights and intra line spacing requirements.
#      August 8, 2016
#         - Started reimplementation of colSize call parameter in the panelDesc data.frame.  Document feature,
#           Added code to validate parameter.  Implemented code in panelLayout function.
#      August 10, 2016
#         - Changed the min and max column sizes to 0.25 to 2.5 inches.
#         - Changed the calculation for the user coordinates width of a panel to include 1/2 the "usr" width of a 
#           a character instead of a fixed amount to ensure the symbol for a dot or arrow head fits within the panel.
#         - Glyphs that don't use dots or symbols that occupy space around the low and high data 
#           points were offset/padded resulting in the graphics incorrectly floating inside the graph.
#           Example: bar graphs that not start at left or right edge of graph.  time series graphs
#           don't tough sides of the graph.  All of these issues have been corrected to only pad
#           (expand graph X range) when required - dot, dotconf, dotsignif, dotse, arrow, scatdot.
#           Any graph that is anchored to the left or right edge is not padded - bar, segbar, normbar.
#           Changes made in DrawXAxisandTitles function using generalize call perameters.
#      August 12, 2016
#         - Fixed reversed glyph header titles Lab1 and Lab2 problem.
#      August 13-16, 2016
#         - Cleaned up the colSize implementation and added validate checks and warning messages.
#           Values of NA, "", and " " are acceptable in 'mapxxx', and 'id' columns.  Cannot set colSize 
#           for these columns.  Other columns must have a numerical value from 0.01 to 200 to use
#           as the width proportion.  Algorithm is each column gets "N1"/sum(all "Ns") percentage of the 
#           available space is allocated to each column.  If a column is below the minimum width,
#           it is set to the minimum.  The calculation is then repeated minus the minimum width columns.
#           The column widths are then compared to the maximum width allows.  Any columns over the 
#           maximum are reduced to the maximum width.  The algorithm is run one more time minus the 
#           columns set to the minimum or maximum values.  
#         - During the testing of the colSize feature when setting column to small sizes, it was
#           found the "extended" label algorithm does not behave well when the number of labels is set 
#           less than 2.  Also, zero labels were being lost.  The general goal of the labeling algorithm
#           was changed to at a minimum request three labels, even on small columns.  The number of labels 
#           per inch was increased from 4 to 5.  The algorthim was also modified to handle staggering of 
#           labels when only one label is present.  The routine now also gets an odd number of labels 
#           when less or equal 7 labels are wanted.  If the column is near the minimum width, any labels 
#           outside the range of the data are stripped, except zero.   If the column is over 1",
#           and > 7 labels, the range is increased to include the label values. 
#           These are signicant changes and will be tested and monitored over the next couple of weeks and
#           tuned as needed.
#         - To help stablize the axis labeling, the extended and wilkinson algorithms will be compared.
#         - Update VisBorders structures and name table to add regional Vis Border support.  Also updated all 
#           border groups to new variable names and to support regions features.
#         - Renamed "regions" feature to "dataRegionsOnly" feature.  
#         - Added "regionsB" options to control overlaying region boundaries when "dataRegionsOnly" not active.
#         - Fixed mapping with region boundaries to do overlays in the correct order.  
#         - Fixed correction of washington DC and all of its forms to a pure character
#           string with no punctuation.  "DC" instead of "D.C." or "DISTRICT OF COLUMBIA".
#         - Added code to do the washington dc comparisons in all upper case only.
#     December 7, 2016 (releasing to CRAN)
#         - Added envir=environment() to all load and data functions
#         - Hide assign to .GlobalEnv in eval-parse
#         - Save and restore Sys.getlocale() to compensate for other 
#           country settings that can interfer with the operation of the 
#           package.
#     April 25, 2022 (update for re-release as 1.1.2
#         - Corrected email addresses in documentation
#         - Corrected bordGrp processing for user provided border groups
#         - Corrected the sort order for user provided grpPattern
#         - Corrected general code to handle multiple elements in call parameters.
#           Previously this could cause the package to abort if more than 1 element
#           was provided.
#         - Added call parameter "maxAreasPerGrp" to set the maximum number of 
#           areas a group/row can represent.  The default is 5 areas.  The value
#           can range from 2 to 5.
#
#
#  Used packages: RColorBrewer, stringr, R.rsp, labeling, 
#
#  Used internal packages: utils, graphics, R.utils, 
#
#      
########

########
#
# Copyrighted 2013, 2014, 2015, 2016 - by: Dan Carr, GMU and Linda Pickle and Jim Pearson of StatNet Consulting, LLC.
#
########

########
#
#  functions used from base:       pretty, load, 
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
#  functions used from stringr:    str_trim, str_split, str_replace_all, str_sub
#
#  functions used from labeling:   extended, wilkinson, 
#
########
#
#  With the generalization of micromapST to cover other geographic area beyond the US, micromapST will 
#  still be called micromapST.  A separate function call has been added to help migrate 
#  uses of the test/prototype SEER version "micromapSEER".  The default border group will be 
#  "USStatesBG" to support existing users of micromapST.  
#
#  Initial Variables that require setting before running this file:
#
#   The current r directory <-  location of the three micromapST source files
#                   micromapST.r
#                   panelFunctions.r
#                   micromapDefSets.r
#
#   The current data directory <- location of the supporting border Group datasets and test datasets
#                   USStatesBG.rda
#                   USSeerBG.rda
#                   KansasBG.rda
#                   NewYorkBG,rda
#                   MarylandBG.rda
#                   ChinaBG.rda
#                   UtahBG.rda
#                   UKIrelandBG.rda
#                   SeoulSKoreaBG.rda
#                   AfricaBG.rda
#
#  Future plans are to do the county map for all U. S. States containing Seer Registries,
#  include a function to validate a user provided Border Group, and to provide functions or 
#  guideance on how to charaterize a collection of boundaries.
#
#  The following datasets must be included in the package to provide the boundaries:
#
#  Each border group contains five R objects.  These objects provide the unique 
#  data for the border group's geographic areas, names, abbreviations, numerical ID,
#  alternate_abbreviation, and alias.
#
#  The areaParms object provides defaults for several run parameters that tune micromapST
#  execution.  The list of variables are:
#             bordGrp      = a character vector - name of the border group.  Must be the same
#                            as the dataset filename minus the ".rda" extension.
#             Map.Hdr1 = a character vector - title header for the Map and ID glyphs.
#                            This is set to the general name of the geographic area, e.g., "U. S."
#                            or "Kansas".
#             Map.Hdr2 = a character vector - title header for the Map and ID glyphs.
#                            This identifies the areas used in the linked micromap, e.g., "States"
#                            or "Counties"
#             Map.L2Borders = a logical variable - if the L2VisBorders need to be overlaid on the 
#                            maps, this variable must be set to TRUE.  So far, only the U. S. 18 
#                            Seer Areas have required this feature.  Most other border groups 
#                            will have this set to FALSE.  (Old variable name = mapL2Borders)
#             Map.Aspect   = a numerical value. The micromapST package does not know what the 
#                            correct aspect ratio is for the map boundaries.  Rather than guess,
#                            Map.Aspect is set to the map's aspect ratio when the boundary data
#                            is converted into the micromapST boundary data format.  The value
#                            is used to control the width of the map glyph column to ensure
#                            the map is properly presented.  Only values between 0.5 and 2.0 are 
#                            allowed.  This aspect is y/x (height / width)
#
#             Map.MinH     = Minimum height for the row if maps are included - units = inches.  
#                            Default is 0.5 inches.
#             Map.MaxH     = Maximum height for the row if maps included - units - inches.
#                            Default value is 1 inch.
#
#             Id,Hdr1      = First line of ID glyph column title 
#             Id.Hdr2      = Second line of ID glyph column title.
#
#             areaUSData   = a logical variable - if set to TRUE, the package assumes the geographic
#                            areas and boundaries are the USStatesBG or USSeerBG datasets and will
#                            overlay the first map in the column with labels for "AK", "HI", and "DC".
#                            This variable should be set to FALSE for all other border groups.
#
#             enableAlias  = Some data may not contain the names or abbreviations contained in 
#                            the border group dataset.  In the case of the U. S. Seer data, the 
#                            Seer Stat output has the area names hidden in the "Registry" label.
#                            The alias feature provides a means of doing a partial match or 
#                            "contains" to link the data labels to the geographic objects.
#                            This variable should be TRUE only for the USSeerBG border group.
#                            In all other cases, it should be FALSE.
#             aP_Proj      = proj4 string describing the project used on the boundary data.
#             aP_Units     = x and y coordinates units of boundary data (lat-long, meters, kilometers)
#             aP_Regions   = a logical variable - diaables or enables the regional area mapping feature.
#                            If TRUE, the areasNamesAbbrsIDs data.frame must contain the information
#                            to group sub-areas by regions. Indicates dataRegionsOnly can be used.
#             Map.RegBorders = Mostly an internal variable - indicated the RegVisBorders bounaries
#                            should be drawn (TRUE).  Works with the "regionsB" call option to 
#                            control regional area boundary overlay.
#             Map.L3Border - a logical variable - mostly for internal use - To indicate if 
#                            the L3 borders should be drawn.
#
#  All variable names in the areaParms data.frame must be unique within the micromapST package.
#
#  The areaNamesAbbrsIDs R object is a table of the full names, abbreviations, alternate 
#  abbreviations, alias strings, and numeric ID for each geographical area in the 
#  boundary dataset.  The abbreviation is used as the internal link between the data 
#  and the boundary of the area.  The table provides a means 
#  of allowing the user to use the area's full name, abbreviation, the numerical IDs,
#  alternate abbreviation, and alias as the area's label
#  in the data provided micromapST in the statsDFrame parameter.  The full names, abbreviations,
#  numerical IDs, and alternate abbreviation must match entries in this table or the user 
#  is notified and data ignored.  If the alias location id option is used, the alias character
#  string must match in a wildcard match (*alias*) one of the location ids in the user provided data.
#  See the documentation on the areaNamesAbbrsIDs for the data structure of this object and
#  the documentation on each border group for the values for that specific border group.
#
#  The areaVisBorders R object contain sets of boundary data points for each area listed in the 
#  areaNamesAbbrsIDs table.  Since the space for the map is limited, these boundaries should be 
#  very simplified or characterized to permit fast drawing and keep the size of the data to a 
#  minimum.  See the documentation on the areaVisBorders R object for more details on the structure
#  of this object.
#
#  The L2VisBorders R object contains a set of boundary data points to outline a set of area 
#  like U. S. states when the areaVisBorders represents subareas.  This layer is overlayed
#  optionally and is only used in the USSeerBG border group, at the present time.
#
#  The L3VisBorders R object contains the outline of the geographic area that contains the 
#  the areaVisBorders' areas.  This would be the outline of a country (U.S. or China) or a 
#  state (Kansas, New York, Maryland).  This provides a accent to the region's borders.
#
#  Regional mapping feature allows a subset of an area (a collections of sub-areas) to 
#  be mapped based on the data provided by the caller.  Sub-areas in regions not
#  referenced in the statsDFrame are not mapped.  When a subset is mapped, the L3VisBorders
#  and related L2VisBorders outlines are NOT drawn.  The regional groupping is based
#  on the region field in the areaNamesAbbrsIDs table (regID).  There are no boundaries 
#  for regions.
#
#  See the documentation on each object for its particular structure and usage.
#
#  See the documentation on each border group for details.
#
######

######
#
#  Basic data structures to convey information and controls between main function and sub-functions.
#
#  mmSys$sDFName  - name of the statsDFrame data frame provided by caller. Not the data itself, 
#                  the name of the variable.
#
#  mmSys$pDName   - name of the panelDesc data frame provided by caller. Not the data itself,
#                  the name of the variable.
#
#
#

######
#
#  gC contains the fun information for each glyph column (gC).  The index is 1 to "n"
#       general items for all glyphs.
#
#  gC[j]$cIdx    - integer index of the current glyph column (1 to "n")
#
#  gC[j]$cTxt    - text version of the integer index of the current glyph column (1 to "n")
#
#  gC[j]$type    - glyph type
#
#  gC[j]$lab1    - character 
#  gC[j]$lab2    - character
#  gC[j]$lab3    - character
#  gC[j]$lab4    - character

#  gC[j]$refText
#  gC[j]$refVal

#  gC[j]$col1Name  - statsDFrame column name
#  gC[j]$col1Num   - statsDFrame column number 
#
#  gC[j]$col2Name
#  gC[j]$col2Num
#
#  gC[j]$col3Name
#  gC[j]$col3Num

#  gC[j]$panelData - data structure name for column in panelData.

#  gC[j]$...     - glyph specific parameters and variables (panelDesc expanded.)

#
#####


######
#
# Intent:
#   This function suppresses the following notes generated by "R CMD check":
#   - "Note: no visible binding for global variable '.->ConfigString'"
#   - "Note: no visible binding for '<<-' assignment to 'ConfigString'"
# Usage:
#   Add the following right in the beginning of the .r file (before the Reference
#   class is defined in the sourced .r file):
#   suppressBindingNotes(c(".->ConfigString","ConfigString"))
#

suppressBindingNotes <- function(variablesMentionedInNotes) {
    for(variable in variablesMentionedInNotes) {
        wstr <- paste0("assign(variable,NULL,envir=",".GlobalEnv)")
        eval(parse(text=wstr))
       
    }
}

#
#
######

######
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
#
######

#####
#
#  asc and chr
#  chr(x) returns character value for "x".
#      if x is a character, x is returned.
#      if x is numeric, it is converted to character value
#
chr <- function(x) {
        if (is.character(x)) {
           return(x)
        } else {
           if (is.numeric(x)) {
              as.character(rawToChar(as.raw(x)))     
           } else {
              return("\025")
           }
        }
     }

#
#  asc(x) returns the numerical value for the character "x"
#    
#
asc <- function(x) {
         wX <- x
         if (is.numeric(wX))   {
            # numeric - turn into character
            wX <- as.character(wX)
         }
         if (is.character(wX)) {
            if (nchar(wX) > 1) {  wX <- substr(wX,1,1)  }   # get only one character
            
            strtoi(charToRaw(x),16L)   # convert character to numericstrtoi(charToRaw(x),16L) 
         } else {
            NA
         }
      }

#
#
#### Global functions


######
#
#   Update  ---  If a variable is used but does not seem to be set, RCMD 
#                generates an error.  This compensates for the dynamic reference
#

gVarList  <-  c("lastLab2Space","lastLab3Space", "staggered")

suppressBindingNotes(gVarList)

#
# Create key global variable before referenced - These variables are referencable by all subroutines and functions 
# in this package.
#

utils::globalVariables(c( 
          # Call Parameters
                "sDFName",            "pDName",
                "wSFName",
                "callVarList",
                
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
                "detailsVariables",   "varName",            "mstColorNames", 

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
                
                "Map.Area.Spec.cex",

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
                "MST.Debug"),
                    
                "micromapST", add=TRUE)

#
#   Would rather have these variable in the local "micromapST" environment.
#
######

######
#
# GlobalEnv Level Functions / micromapST Namespace Functions
#    accessible by everyone, but can't access variables within caller's space.
#
# groupPanelOutline 
#

groupPanelOutline = function (panelGroup, j )
   ## used in micromapST function  - assumes 3 rows in the panels..
{
  iE <- panelGroup$dim[1]
  
  for (i in 1:iE){
     panelSelect(panelGroup,i,j)     # select a space
     x <- panelScale()               # scale it
     panelOutline()                  # outline it.
  }
}   

####
#
#  Clean up strings - remove 
#    1) special single and double quotes (open and closed)
#    2) tick mark
#    3) general punctuation (periods, etc.)
#    Designed to allow strings that may have different times of quotes, apos. to be compared.
#
ClnStr <- function(x) {

    z <- gsub("[[:punct:]\u2018-\u201F]", "", x, perl=TRUE)
    z <- stringr::str_trim(z)
    return(z)
 
}

#
####

####
#
#  Find shortest format for Axis labels 
#
#  Test the following formats on the Axis Labels and determine
#   the narrowest format.
#  The formats checked are:
#       fixed format (up to 1 decimal place)
#       general format (including scientific notation)
#       fixed with KMB modification
#       fixed with "in thousands" type label
#

FindShorest <- function(x, w) {
     #   x is a vector of numbers
     #   w is the width of the target column (inches)
     #
     n <- as.integer(w / 4)  # number of labels required
     xr <- range(x)          # get range of the values
     
     if (!odd(n))  n = n + 1
     xW <- labeling::wilkinson(xr[1],xr[2], n, mrange=c(n/2,n))
     xE <- labeling::extended( xr[1],xr[2], n, w = c(0.25, 0.2, 0.5, 0.05))
     #                                   simp, cover, densi, legible
     
     #  Function is incomplete...
     
}

#
####

####
#
# is.Color takes a hex string, the name of a color (from grDevices::colors()), or palette number
#   and validates it as a color variable.  TRUE - is a valid color, FALSE - not a color.
#   
# Inputs:  values can by any color names that matches the grDevices::colors() name list, 
#    a 6 or 8 character hex string starting with a "#" character, or 
#    the palette color number (1 to 8) as integer or character.
#
#    Examples:   "white", "red", "lightgreen", "#232323", "#234Ad3", or "#FFDDCC80"
#                1, or "1"
#
#    On hex strings, the alpha value is optional (last 2 hex digits)
#
#
is.Color  <- function(x) {
    # handle a vector of colors
    vapply(x, is.Color2, logical(1))
  }

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
#  single value test function for colors.
#
#  The test is done against the standard color list and the micromapST color list.
#  The value can be a color name or a color pallet value.
#
is.Color2 <- function(x) {
    ErrFnd <- FALSE
    # check one color "x"
    if (is.numeric(x)) {
       # numeric color value - if so its a relative color number within the pallet.
       if (x < 0) {
          # can not be a negative value..
          ErrFnd    <- TRUE
          warnCnt()
          xmsg      <- paste0("***0910 is.color2 The color value must be a positive number. Value seen:",x,"\n")
          stop(xmsg,call.=FALSE)
       } else {
          # if value is numeric, convert to integer character string.
          x <- as.character(x)
       } 
    }
    
    if (!ErrFnd) {
       # convert factor to character
       if (is.factor(x)) x <- as.character(x)
 
       if (is.character(x)) {
          #   character string, check for palette number or color name.
          if (!is.na(match(x,c(as.character(c(1:8)),grDevices::colors(),mstColorNames)))) {  # test name and/or number
             TRUE   # good color value.
             
          } else {
             
             # No match with character version of palette number or grDevices::colors(),
             # so try conversion from color to rgb, if it works, got a color - return TRUE 
             # if it fails, it will return error - catch and return "FALSE"
 
             res     <- try(grDevices::col2rgb(x),silent=TRUE)
             #  if class of res is not "try-error", return TRUE, 
             #  if class of res is "try-error", then return FALSE (not a color)
             return(!inherits(res,c("try-error"),which=TRUE))
          }
       } else {
          # not a integer or character
          FALSE  # not a color
       }
    }
 }
 
#
####

####
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
 ####
 
 ####
 #
 # functiion to test if "x" is within or equal to the range of "r".
 #    "r" must be a vector of length 2 to be evaluated.
 #
is.between.r <- function(x,r) {
    # the x must be within or equal to the range spacified in R
    #
    if (length(r) != 2) {
         warnCnt()
         xmsg    <- "***0491 INB is.between.r The r range value is not a vector with length of 2. FALSE returned."
         warning(xmsg, call.=FALSE)
         return(rep(FALSE,length(x)))   # not valid range
    } else {
         return(is.between(x,r[1],r[2]))
    }
  }

#
####

####
#
# Testing function - print out key par() plot parameters
#

printPar <- function() {  
  cFin   <- par("fin")   # get parameters for current panel.
  cat("cFin:",cFin," (w,h)\n")
  cFig   <- par("fig")   # get parameters for current panel.
  cat("cFig:",cFig," (x,x,y,y)\n")
  cPin   <- par("pin")
  cat("cPin:",cPin," (w,h)\n")
  cPlt   <- par("plt")
  cat("cPlt:",cPlt," (x,x,y,y)\n")
  cMai   <- par("mai")
  cat("cMai:",cMai," (b,l,t,r)\n")
  cMar   <- par("mar")
  cat("cMar:",cMar," (b,l,t,r)\n")
  cUsr   <- par("usr")
  cat("cUsr:",cUsr," (x,x,y,y)\n")
  cPs    <- par("ps")
  cat("cPs :",cPs," pt.\n")
}  

#
####

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
#     make all caps.
#
CleanString <- function(wstr) {
   nstr <- toupper(stringr::str_trim(stringr::str_replace_all(wstr,"[[:space:][:cntrl:][:punct:]]+"," ")))
   return(nstr)
}

#
####

####
#
#  Scaler1 - find scale for range and appropriate axis sub-title
#
#  Find the size of the maximum value.
#  Select scaling label, and division factor to use on data.
#

Scaler1 <- function(var) {

   #  xAxis is the number for the Axis labels
   var1 <- as.numeric(var)
   
   if (var1 < 0) {
      var1 <- abs(var1)
   }   
   vc <- c(1,"")
   
   if (var1 > 1) {
      # value > 1  --- OK to do log10 to get index.
      
      varLog <- as.integer(log10(var1))
      
      vc <- switch(varLog,
                      c(1,""),    #  0 - < 10
                      c(1,""),    #  1 - < 100
                      c(1,""),    #  2 - < 1000
                      c(100,"in hundreds"),              # 3 - < 10,000
                      c(1000,"in thousands"),            # 4 - < 100,000
                      c(10000,"in ten thousands"),       # 5 - < 1,000,000
                      c(100000,"in hundred thousands"),  # 6 - < 10,000,000
                      c(1000000,"in millions"),          # 7 - < 100,000,000
                      c(10000000,"in ten millions"),     # 8 - < 1,000,000,000
                      c(100000000,"in hundred millions"),# 9 - < 10,000,000,000
                      c(1000000000,"in billions"),       #10 - < 100,000,000,000
                      c(10000000000,"in ten billions"),  #11 - < 1,000,000,000,000
                      c(100000000000,"in hundred billions"),#12 - < 10,000.000,000,000
                      c(1000000000000,"in trillions"),   #13 - < 100,000,000,000,000
                      c(1,"")
                 )
   } else {
   
      # value < 1 and > 0,  do it differently.
      repeat {
         vc <- c(1,"")
         if (var1 >= 0.1) {           # 0.999999 => to >= 0.1    ->  9.99999 -> 1.0
            vc <- c(0.1,"in the tenth")
            break 
         }
         if (var1 >= 0.01) {          # 0.0999999 => to >=  0.01 ->  9.99999 -> 1.0
            vc <- c(0.01,"in the hundredth")
            break
         }
         
         if (var1 >= 0.001) {         # 0.00999999 => to     >= 0.001      -> 9.99999 -> 1.0
            vc <- c(0.001,"in the thousandth")
            break 
         }
         if (var1 >= 0.0001) {        # 0.0009999999 => to   >= 0.0001     -> 9.99999 -> 1.0
            vc <- c(0.0001,"in the ten thousandth")
            break 
         }
         if (var1 >= 0.00001) {            # 0.0000999999 => to   >= 0.00001    -> 9.99999 -> 1.0
            vc <- c(0.00001,"in the hundred thousandth")
            break 
         } 
            
         if (var1 >= 0.000001) {           # 0.00000999999 => to   >= 0.000001     -> 9.99999 -> 1.0
            vc <- c(0.000001,"in the millionth")
            break
         }
         if (var1 >= 0.0000001) {          # 0.000000999999 => to  >= 0.0000001    -> 9.99999 -> 1.0
            vc <- c(0.0000001,"in the ten millionth")
            break
         }
         if (var1 >= 0.00000001) {         # 0.000000999999 => to >=  0.0000001   -> 9.99999 -> 1.0
            vc <- c(0.00000001,"in the hundred millionth")
            break
         }
         if (var1 >= 0.000000001) {        # 0.0000000999999 => to       >= 0.000000001       -> 9.99999 -> 1.0 
            vc <- c(0.000000001,"in the billionth")
            break
         }
         if (var1 >= 0.0000000001) {       # 0.00000000999999 => to      >= 0.0000000001      -> 9.99999 -> 1.0 
            vc <- c(0.0000000001,"in the ten billionth")
            break
         }
         if (var1 >= 0.00000000001) {      # 0.000000000999999 => to     >= 0.00000000001     -> 9.99999 -> 1.0 
            vc <- c(0.00000000001,"in the hundred billionth")
            break
         }
   
         if (var1 >= 0.000000000001) {     # 0.0000000000999999 => to    >= 0.000000000001    -> 9.99999 -> 1.0
            vc <- c(0.000000000001,"in the trillionth")
            break
         }
         if (var1 >= 0.0000000000001) {     # 0.0000000000999999 => to   >= 0.0000000000001   -> 9.99999 -> 1.0
            vc <- c(0.0000000000001,"in the ten trillionth")
            break
         }
         if (var1 >= 0.00000000000001) {    # 0.00000000000999999 => to  >= 0.00000000000001  -> 9.99999 -> 1.0
            vc <- c(0.00000000000001,"in the hundred trillionth")
            break
         }
            
         if (var1 >= 0.000000000000001) {   # 0.000000000000999999 => to   >= 0.000000000000001     -> 9.99999 -> 1.0 
            vc <- c(0.000000000000001,"in the quadrillionth")
            break
         }
         if (var1 >= 0.0000000000000001) {  # 0.0000000000000999999 => to  >= 0.0000000000000001    -> 9.99999 -> 1.0 
            vc <- c(0.0000000000000001,"in the ten quadrillionth")
            break
         }
         if (var1 >= 0.00000000000000001) { # 0.00000000000000999999 => to >= 0.00000000000000001   -> 9.99999 -> 1.0 
            vc <- c(0.00000000000000001,"in the hundred quadrillionth")
            break
         }
      }
   }
   # vc <- c(divisor, <axis sub-title string>)
   #cat("returning vc:",vc,"\n")
   
   return(vc)     # return divisor [1] and subtitle string [2] 
   
   #  need to add code to handle width range of number, getting duplicates at low end. 
} 

#
####

####
#
#  Alt_Scaler
#
#  Find the scale of the number (not list of numbers)
#  Find divisor and apply
#  Changes number to string.
#  Apply scale character to end of string
#
#  Need to add logic to convert labels back to numbers and return both.
#
#  var is a vector of numeric values for the Axis labels.
#  lower is a logical flag.  If FALSE, the resulting strings are returned as is.  
#                            If TRUE, the resulting strings are converted to lower case.
#

Scaler2 <- function(var,lower=FALSE) {

   var1      <- as.numeric(var)
   minusFlag <- ""
   if (var1 < 0) {    # save fact the number was minus
        minusFlag = "-"
        var1 <- abs(var1)
   }
   vc <- c(1,"")
   var2 <- var1
   
   if (var1 != 0) {   # number zero, quick exit
      varLog <- as.integer(log10(var1))
      #cat("varLog:",varLog,"\n")
    
      if (varLog != 0) {
         if (varLog > 0) {
            vc <- switch(varLog,  #  0 - < 10          =>   [0.10000000001   to    10) 
                      c(1,""),    #  1 - < 100         =>   [10    to  100)
                         # hecto  (hunderds)
                      c(1,""),    #  2 - < 1,000       =>   [100   to  1000)
                         # kilo  (thousands) 
                      c(1000,"K"),    #  3 - < 10,000         =>   [1,000  to  10,000)
                      c(1000,"K"),    #  4 - < 100,000        =>   [10,000 to 100,000) 
                      c(1000,"K"),    #  5 - < 1,000,000      =>   [100,000 to 1,000K)
                         # mega (million)
                      c(1000000,"M"),    #  6 - < 10,000,000      => [1,000K   to 10,000K)
                      c(1000000,"M"),    #  7 - < 100,000,000     => [10,000K  to 100,000K)
                      c(1000000,"M"),    #  8 - < 1,000,000,000   => [100,000K to 1,000M)
                         # giga (billion)
                      c(1000000000,"B"),    #  9 - < 10,000,000,000      => [1,000M    to 10,000M)
                      c(1000000000,"B"),    # 10 - < 100,000,000,000     => [10,000M   to 100,000M)
                      c(1000000000,"B"),    # 11 - < 1,000,000,000,000   => [100,000M  to 1,000B)
                         # tera (trillion)
                      c(1000000000000,"T"),     # 12 - < 10,000,000,000,000     => [1,000B    to 10,000B) 
                      c(1000000000000,"T"),     # 13 - < 100,000.000,000,000    => [10,000B   to 100,000B)
                      c(1000000000000,"T"),     # 14 - < 1,000,000,000,000,000  => [100,000B  to 1,000T)
                      c(1,"")
                )
            var2 <- var1/as.numeric(vc[1])
    
          } else {
            #  negative log values are small numbers, so invert to 1 to N
            varLog <-  (-varLog)  #   (-1 => 1)
            repeat {
               vc <- c(1,"")
               if (var1 >= 0.1) {        # 0.999999 => to >= 0.1    ->  9.99999 -> 1.0
                  vc <- c(10,"d")   # deci
                  break 
               }
               if (var1 >= 0.01) {       # 0.0999999 => to >=  0.01 ->  9.99999 -> 1.0
                  vc <- c(100,"c")   # centi
                  break
               }
               if (var1 >= 0.001) {      # 0.00999999 => to >= 0.001 -> 9.99999 -> 1.0
                  vc <- c(1000,"m")    # milli
                  break 
               }
               if (var1 >= 0.000001) {   # 0.000999999 => to >=  0.000001  -> 999.999 -> 1.0
                  vc <- c(1000000,"u")     # micro
                  break
               }
               if (var1 >= 0.000000001) {        # 0.000000999999 => to       >= 0.000000001 -> 999.999 -> 1.0 
                  vc <- c(1000000000,"n")     # nano
                  break
               }
               if (var1 >= 0.000000000001) {     # 0.000000000999999 => to    >= 0.000000000001 -> 999.999 -> 1.0
                  vc <- c(1000000000000,"p")    # pico
                  break
               }
               if (var1 >= 0.000000000000001) {  # 0.000000000000999999 => to >= 0.000000000000001 -> 999.999 -> 1.0 
                  vc <- c(1000000000000000,"f")   # femto
                  break
               }
          
            }
            var2 <- var1*as.numeric(vc[1])
         }
      }
   }
   #cat("minus:",minusFlag,"  vc:",vc,"\n")
   
   cvx <- paste0(minusFlag, stringr::str_trim(formatC(var2,format="fg",width=5,digits=4,drop0trailing=TRUE)),vc[2])
   
   if (lower) { cvx <- tolower(cvx) }
   
   return(cvx)
   
   #  Need to check to see what happens if we have lowe end numbers that may be duplicated.
   
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
      s <- strsplit(x,"[ ._]")[[1]]    # split on boundaries " ", "." or "_".
      s1 <- s[s != ""]                 # skip empty strings
      
      paste0(toupper(substring(s1,1,1)),tolower(substring(s1,2)),collapse=" ")
   }
      
#
# Alternative:
#   gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
#
####

#####
#
#  plotPoint - takes a give x,y any type of point (0:18, 19:25, > 32 or character)
#    and correctly plots it at x,y.   Other parameters are required incase of outlines.
#
#
 plotPoint <- function(ppX, ppY, ppPch,  ppColor, ppSize, ppLwd, 
                       ppOutline, ppOutline.col, ppOutline.lwd) {                     
            #
            # Call parameters:  pchValue, x, y, pch.size, outline.lwd, outline.col, mstColor, 
            #
            
            pchValue   <- ppPch
            
            suppressWarnings(chrValue   <- as.numeric(pchValue))
            
            if (is.na(chrValue)) {
               # the pch value is not a numeric - check for character
               
               if (is.character(pchValue)) {
                  # character type value.  Get first character.  assume > 31
                  pchValue <- stringr::str_sub(stringr::str_trim(pchValue),1,1)
                  points(ppX, ppY, pch=pchValue,
                            cex=ppSize,
                            col=ppColor
                        )
                  #cat("points of character:",pchValue)
                 
               } else {
                  # set to default since we can't decode it.  Set to numeric value.
                  chrValue <- 21
                  #cat("not a character-typeof:",typeof(pchValue),"  setting chrValue to 21.","\n")
               }
            } 
            if (!is.na(chrValue)) {
               #cat("numeric - typeof:",typeof(pchValue), " ", typeof(chrValue)," ",typeof(chrValue)," ",pchValue," ",chrValue,"\n")
               
               # have a numeric value (still), got conversion - 0:255 range.
               # if it's NA, it's character and has been plotted.
               
               if (chrValue > 31) {
                  #cat("chrValue > 31 - normal points\n")
                  
                  # normal symbols (numeric) (no border)
                  # > 31 characters
                  points(ppX,ppY,pch=chrValue,
                             cex=ppSize,
                             col=ppColor 
                        )
               } else {
                  # <= 31
                  if (chrValue > 25) {
                     # 26:31 -> not used character use default
                     chrValue  <- 21
                     #cat("char 26:31 not used -> use default 21\n")
                  }
                  if (chrValue > 18) {
                    
                     #  19:25 value characters.
                     #  Dot.Conf.Outline set by user or by BW/Greya/Grays color scheme
                     if (ppOutline) {
                        #  19:25 with outline around symbol
                        #cat("19:25 -> filled with borders symbols - outline ON \n")
                        points(ppX, ppY, pch=chrValue, 
                                 cex=ppSize, 
                                 lwd=ppOutline.lwd, 
                                 col=ppOutline.col, 
                                 bg=ppColor                              )         
                     } else {
                        #  19:25 with no outline (border) 
                        #cat("19:25 -> filled with borders symbols - outline OFF \n")
                        points(ppX, ppY, pch=chrValue, 
                                 cex=ppSize,
                                 col=NA,
                                 bg=ppColor
                              )
                     }
                  } else {
                     # 0:18 symbols - line drawings
                     #cat("0:18 symbols - standard print.\n")
                     points(ppX, ppY, pch=chrValue,
                              cex = ppSize,
                              lwd = ppLwd,
                              col = ppColor
                           )
                  }
               }
            }
            
         }  
#
#  end of point ploter.
#
#####
####
#
#  micromapSEER  - to support previous users of micromapSEER NCI package.
#

micromapSEER <- function(statsDFrame,panelDesc,...) {

      micromapST(statsDFrame,panelDesc,..., bordGrp="USSeerBG", bordDir=NULL)
}

#
####

####
#
#   Get micromapST Version
#
micromapST.Version <- function() { return ("micromapST V1.1.3 built 2022-04-29 10:00am") }

#
####

####
#
#  micromapST
#
#  Using the technique of setting parameters to NULL.  Later during verification, if 
#  NULL, set to the default.  If not NULL, then verify the parameters value.
#
#

micromapST = function(
    statsDFrame,
    panelDesc,
    rowNamesCol = NULL,                      # Name of name link column.
    rowNames    = NULL,                      # default = "ab"   ### modify to SEER IDs
    sortVar     = NULL,                      # default = sort on plotNames values
    ascend      = TRUE,                      # default = ascending sorting order 
    title       = c("",""),                  # default = empty
    plotNames   = NULL,                      # default = "ab"  ### modify to SEER Abv and Names
    axisScale   = NULL,                      # axis Scale Method, default = "e" -> extended
    staggerLab  = NULL,                      # stagger Axis Labels, default = FALSE
    bordGrp     = NULL,                      # border and names group to use with micromapST, Def = "USStatesBG"
    bordDir     = NULL,                      # data directory containing the bordGrp .RDa file to use.  
                                             #    If null or NA, a DATA statement is used to load the 
                                             #    bordGrp from the included package datasets.
    dataRegionsOnly = NULL,                  # when regions are defined, permit package to map only regions containing data. Default=FALSE,
    regionsB    = NULL,                      # when regional boundaries are present, map regional overlays. Default = FALSE. 
    grpPattern  = NULL,                      # Override areas per panel/group pattern
    maxAreasPerGrp = NULL,                   # Maximum number of areas per group/row - default - 5
    ignoreNoMatches  = FALSE,                # How to handle statsDFrames that don't match.
    colors      = NULL,                      # Override colors structure
    details     = NULL )                     # Override details parameters.
    
{

#
#  Routine:   micromapST  (and micromapSEER)
#
#  Created by:  Dr. Dan Carr
#  Updated and Extended by:  Jim Pearson, April 20, 2009
#  Updated and Extended by:  Jim Pearson, August 28, 2012
#  Updated and Extended by:  Jim Pearson, May and June, 2013
#  Updated and Extended by:  Jim Pearson, Nov, 2013
#  Updated and Extended by   Jim Pearson, Jan, 2014
#  Updated and Extended by:  Jim Pearson, March, 2014
#  Updated and Extended by:  Jim Pearson, October-November, 2014
#       Updated impacted every function and feature of the package to generalize the panel layouts.
#  Updated and Extended by:  Jim Pearson, December 2014 and January 2015
#  Updated and Extended by:  Jim Pearson, March 2015, generalized the package for other geospatial areas.
#                                           and refined the scaling and sizing of the rows and columns.
#  Updated and Extended by:  Jim Pearson, September, 2015 and February, 2016
#  Updated and Extended by:  Jim Pearson, November, 2016
#  Updated and Extended by:  Jim Pearson, November, 2021
#
#  Packaged by: Jim Pearson
#
#  Dependencies:   micromapGSetDefaults
#                                  $colors
#                                  $details
#                  micromapGSetPanelDef
#                  panelFunctions.r
#
#  Included bordGrp DataSets:
#                  USStatesBG    - equivalent to original micromapST setup
#                  USSeerBG      - new setup for borders and behavior for US Seer Areas.
#                  KansasBG      - new setup for borders and behavior for Kansas County Areas.
#                  NewYorkBG     - new setup for borders and behavior for New York County Areas.
#                  MarylandBG    - new setup for borders and behavior for Maryland County Areas.
#                  ChinaBG       - new setup for borders and behavior for China.
#                  UKIrelandBG   - new setup for borders and behavior for UK-Ireland area
#                  UtahBG        - new setup for borders and behavior for Utah County Areas
#                  SeoulSKoreaBG - net setup for borders and behavior for the districts in the city of Seoul South Korea.
#                  AfricaBG      - net setup for borders and behavior for the countries of Africa.
#
#   Each contain the following DataFrames, Lists and Vectors::
#      Run Parameters:                             areaParms     
#      Data Level Names, Abbrs. IDs, and Labels:   areaNamesAbbrIDs       (Old stateNamesFips)
#      Data Level Boundaries:                      areaVisBorders         (Old stateVisBorders)
#      L3 (national) Level Boundaries              L3VisBorders           (Old stateNationVisBorders)
#
#      L2 (state) Level Boundaries  (Optional)     L2VisBorders           (Old stateNationVisBorders)
#      Reg Level Boundaries (Optional-Regions)     regVisBorders          (<NA>)
#
#   Each level draws there boundaries a little wider then the previous level.
#   The area level starts with the width at 
#   Currently the L2 Boundaries are only used with the "USSeerBG" border group at this time.
#
#
#   If L2 Boundaries are not included in the bordGrp, the L3 Boundaries are copied into 
#   the L2 boundaries as a place holder.
#
#   Source Files: panelFunctions.r,  micromapDefSets.r
#
#####

#####
#
#
#  Call Parameters:
#
#   Defaults List for call simulation
#     statsDFrame <- data
#     panelDesc   <- panel description data.frame  or panel description list of lists.
#     rowNames    <- "ab"                     # global
#     sortVar     <- NULL                     # global 
#     ascend      <- TRUE                     # global
#     title       <- c("titles")              # global
#     plotNames   <- "full"                   # global and glyph
#     axisScale   <- "e"                      # new extended method - global and glyph
#     staggerLab  <- FALSE                    # global and glyph
#     colors      <- NULL                     # global 
#     details     <- NULL                     # global and glyph
#     bordGrp     <- "USStatesBG"             # global
#     bordDir     <- NULL                     # global
#     ignoreNoMatches <- FALSE                # global
#     grpPattern  <- NULL                     # global - default = calculated row / panel pattern
#     maxAreasPerGrp <- NULL                  # global
#     regionsB    <- NULL                     # global - default = FALSE
#     dataRegionsOnly <- NULL                 # global - default = FALSE
#
#     colors and details are used to override/modify the basic default structure for the colors
#     and the operational details information.
#
#
#####
#
# statsDFrame  data.frame of area ID and data for micromaps.
#
#             rownames must be area abbreviations (Abbr), full names (Name), 
#             numerical id (fips codes) (ID), alternative abbreviation (alt_abr), or alias.
#
#             Provides the data for the dot, dotConf, dotSE, dotSignif, arrows, bars, 
#             segbar, ctrbar, and normbar glyph panels. 
#
#             Not used for boxplots or time series column panels.  Pointers to their
#             data is provided in the panelDesc data.frame.
#
#             The statsDFrame must have the area's abbr, name or ID code (like fips code) as 
#             the rownames of the data.frame.  As an alternate a column can contain the 
#             area's identifier and the "rowNameCol" parameter can be used to point to 
#             the column.  Once the column is verified, it is assigned to the rownames 
#             of the statsDFrame.
#     
#             The data.frame must be at least 2 columns for some of the functions
#             in R.  To compensate for possible 1 column data.frames, a column of zero 
#             is appended to the right side of the data.frame to ensure there is always 
#             2 columns.  (Work-a-round)
#
#             An example of the problem:
#               When the structure is ordered xxx[ord,] and then assigned to the working 
#               variable "dat", the dimensions are preserved. 
#               If the data.frame has only one column, the ordering and assigned, 
#               strips the rownames and leaves the dim(dat) = NULL.
#
#             The numerical data in the statsDFrame data frame may be in a numerical vector 
#             or a character vector.  If the data is found to be a factor, it is converted to 
#             a character vector.  If the data is a character vector, then the format of the 
#             numbers is validated.  The acceptable numerical value formats are:
#
#                         1, 1.1, 0.1, .1, +1, +1.1, -0.1, -.1, -13434.3 -1234,
#                         1.1e+01, 1e+01, 0.1e+01, 1e-1, 1.12355e-01, +1.23e+99,
#                         1,000; -1,343; +1,234; 23,235.00; -23,234.00001
#
#             Errors will be flagged if there is more than 3 digits between commas and commas or
#             decimal point, the exponent value is greater than 2 digits, a space is found
#             between any parts of the number, etc.
#
#             The name of the user provided statsDFrame data frame is stored in 
#             callVarList$statsDFrame variable for later reference.
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
#                colSize=c(NA,NA,1,1),
#                rmin=c(NA,NA,NA,1),
#                rmax=c(NA,NA,NA,5),
#                refVals=c(NA,NA,NA,wflungbUS[,1]),                          # optional
#                refTexts=c(NA,NA,NA,'US Rate'),                             # optional
#                panelData=c('','','',''),                                   # required if boxplot or time series used.
#                adv=list('',list(a=v),'','')                                # advanced parameters
#                )
#
#             The first description row describes the first column of panels
#             an so on.  This is a candidate for change since each column
#             describing a column avoids a mental transposition.  
#
#             The name of the user provided panelDesc data frame (or list) is stored in 
#             callVarList$panelDesc variable for later reference.
#
#      The alternate form of the panelDesc variable is a list of list.  
#      panelDesc is a list.  Each glyph column in the linked micromap is represented 
#      by a list in this list.  The glyph column list contains all of the 
#      panelDesc variable related and valid for the glyph indicated in the type= variable
#      in this list.  A example is provide at the end of the discussion on the panelDesc
#      variabls below.
#
# The type parameter must be present for each panel column.  The other parameters are optionals.
# However, if a parameter is required for any column, it is present for all columns.  
# If not used by a column, the parameter's value for that column should be set to "NA".
#
#  type refers the graphic panel type to be used. The valid types are  
#          "map", "mapcum","maptail","mapmedian",       for maps
#          "id",                                        for area ids
#          "dot", "dotse","dotconf", "dotsignif"        for dot plots
#          "arrow",                                     for arrow plots
#          "bar",                                       for simple bar plots
#          "ts", "tsconf",                              for time series plots
#          "scatdot",                                   for scatter dot plots
#          "normbar","segbar","ctrbar",                 for stacked bar plots
#          "boxplot",                                   for box plot 
#          "rank"                                       for ranking (not fully implemented)
#                   
#         For non-highlighted contours:
#             map accumulates areas top to bottom
#             maptail accumulates areas outside in
#             mapMedian feature above median area above the median and vis versa
#
#         bar  will accept negative values and plot from 0 in that direction.
#
#  col1, col2, col3
#    These values idenfity the column numbers or names in statsDFrame to be 
#       used as data for most of the panel glyph types.  They are used by:
#       "dot", "bar", "dotse", "dotsignif", "dotconf", "scatdot", 
#       "segbar", "ctrbar", "normbar"
#      
#     Panel types using only one column parameter (one data item) are:
#
#       dot:       col1 = dot value (estimate)
#       bar:       col1 = bar height from zero (0)
#
#     Panel types using two column parameters (two data items) are:
#     
#       dotse, dotsignif, arrow, and scatdot glyphs.
#
#       dotse:     col1 = dot value (estimate), col2 = standard error value
#       dotsignif: col1 = dot value (estimate), col2 = P Value for dot value
#       arrow:     col1 = starting value, col2 = ending value for arrow.  The arrow head
#                  is on the ending value.
#       scatdot:   col1 = "x" value of dot, col2 = "y" value of dot.
#
#     Panel types using two column parameters to specify a range of data columns are:
#      
#       segbar, ctrbar, normbar:  col1 = first data column in statsDFrame, 
#                  col2 = last data column in statsDFrame.   The data from 
#                  columns col1 to col2 are used as the length (values) for each 
#                  stacked bar segment.  The number of data columns must be between
#                  3 to 9.
#                  
#     Panel type dotconf using three column parameters: (col1, col2, col3):
#     
#        dotconf:  col1 = dot value (estimate), col2 = lower bound and col3 = upper bound
#
#     Panel following types do not requiring any column parameters:
#
#       boxplots uses the "panelData" vector in panelDesc to provide the name of a saved 
#           boxplot structure.  The boxplot structure is created by saving the 
#           results of aboxplot(...,plot=F) call.
#
#       ts and tsconf use the "panelData" vector in the panelDesc to obtain the name of 
#           a matrix the data for the time series. The name represents a array(51,"x",4).  
#           The first dimension represents the states (51) for the US 
#           or the number of areas in the border data. The number of entries must 
#           match the number of entries in the statsDFrame.  The second dimension 
#           represents the number of samples in the time series.  The third dimension 
#           are the "x", "low.y", "y", and "high.y" values for each sample.  
#           For ts glyphs, the "low.y" and "high.y" values are ignored, but required.
#  colSize
#     Specifies the proportional size of a glyph column in relation to the other glyph columns.
#     This is a numeric vector with one element for each glyph column.  The sum of the vector
#     is used as the denominator to calculate the percentage of available width is to be allocated
#     to the column.   For example:   colSize = c(NA, NA, 10, 10, 5, 15).  The first two columns are
#     map and id glyphs and are not involved in this feature.  The remaining 4 columns have a total
#     value of 40.  The percentage for each column is 25%, 25%, 12.5% and 37.5%  = 100%.  If 4" of 
#     space is available, then the width of each column will be 1", 1", 0.5", and 1.5".
#
#
#  rmin, rmax
#     Specify the min and/or max values for the X axis range for the any of the graphic
#     glyphs.  If no value is specified, the package will use the range of the 
#     data provided.  NA must be used when a value is not being specified.
#     The user provide range is checked against the range of the data to make sure
#     all of the data is contained in the range.  rmin must be less than rmax.
#     (in planning stages)
#
#  lab1, lab2
#     Two label lines at the top of columns. Use "" for blank, not NA or MULL.
#
#  lab3
#     One label line at the bottom of a each column, typically measurement units.
#     Supported under the "map" and "id" columns for use as a sub-title.
#
#  lab4
#     One label line for used with the Y axis on each panel.  Only used with time series and ScatDot panels.
#
#  refVals            # P-2010/07/23  changed variable from refvals to refVals 
#                     #    to be consistant.
#     name of objects providing a reference values shown
#     as a line down the column 
#
#  refTexts           # JP-2010/07/23 - New 
#     texts to be used as the legend for the reference values.
#     If refTexts for column is NA or "", then no legend is added.
#
#  colSize            # 8/8/16 - implemented to provide proportional column size control.
#     A vector of numeric values used to set a proportional column size within the 
#     space provided by the user.  The sum of all of colSize values are used as the 
#     demoninator to determine the percentage of the available space to allocate to the 
#     column.  The default value for each column is "1".  If a column's value is NA, NULL, or <=0.1,
#     then the column is allocated the 1/"n" of the available space, where "n" is the number
#     of columns.  The map and id columns are fixed width columns and are not effected by the 
#     colSize calculations.
#
#     example:  micromapST has 6 columns: map, id, dot, bar, arrow, dotconf.
#                  The available width provided is 6.5" in a PDF.
#                  colSize = c(0,0,5,5,10,3)
#                  Once the map and id column widths are subtracted, the available width for the 
#                  four columns is 4".  The total value of all columns is 23 (sum(5,5,10,3).
#                  The width of the dot and bar columns will be set at 5/23 * 4 = 0.87 ",
#                  arrow is allocated 1.74" and dotconf is allocated 0.52 ".
#
#     The values in this vector must be positive numerical values.  They can range from 0.1 to 100.
#     The sum of the values is used as the demoninator to calculate the percentage for each column.
#
#
#  panelData           # (old boxplot column)
#      names a list object with a boxplot data or time series data (x/y or x/yl/ym/yh 
#      data for each area.
#
#      The boxplot list the xxxx$names list must be the abbreviated area id
#      for the entry and the related data in the structure. 
#.
#      Used to link graphic to additional data beyond the 3 data elements 
#      provided in col1, col2, col3 indexes into the statsDFrame..
#
#      For boxplot graphics, a list of "boxplot" function values for each area and DC
#        with the names (2 characters) used as the row.names. 
#
#      For time series graphics, the object must be an array(51,"x",4), 
#         where the 1st index is the areas (1 to n), the second index is the number 
#         of time periods ("x") with a minimum of 2 and maximum of 30, and 
#         the third index is the type of variable. The rownames of array must
#         be the associate area id (a 2 character abbreviation if states).  This 
#         is required so the time series array can be properly associated 
#         with the data in the statsDFrame when it's sorted.
#         For time series with no confidence band, column 1 is the x value and 
#             column 2 is the y value.  
#         For time series with a confidence band, column 1 is the x value, 
#             column 2 is the y-low value, column 3 is the y-median value, 
#             and column 4 is the  y-high value.
#         The number of entries must be equal to the number of areas in the statsDFrame.
#                
#      Note:  Some descriptors may be omitted if none of the panel plots need them.
#         often refValues and boxplots can be omitted 
#
#  adv =  list of parameter lists of for each glyph column.  Each item in the list
#         represents a list of named parameters for that glyphs column.
#         Example:
#
#  An example of the list form of panelDesc is:
#
#      GC1 <- list(type="map",lab3="bottom lab")
#      GC2 <- list(type="id")
#      GC3 <- list(type="dot",lab1="Population",lab2="2010",col1="RATE.10",refVal=100,refTxt="Pop. Ref")
#      GC4 <- list(type="boxplot",lab1="boxplot",lab2="risk",panelData="BoxData")
#
#      panelDesc <- list(GC1, GC2, GC3, GC4)
#
#  > str(panelDesc)
#  List of 4
#   $ :List of 2
#    ..$ type     : chr "map"
#    ..$ lab3     : chr "bottom lab"
#   $ :List of 1
#    ..$ type     : chr "id"
#   $ :List of 6
#    ..$ type     : chr "dot"
#    ..$ lab1     : chr "Population"
#    ..$ lab2     : chr "2010"
#    ..$ col1     : chr "RATE.10"
#    ..$ refVal   : num 100
#    ..$ refTxt   : chr "Pop. Ref"
#   $ :List of 4
#    ..$ type     : chr "boxplot"
#    ..$ lab1     : chr "boxplot"
#    ..$ lab2     : chr "risk"
#    ..$ panelData: chr "BoxData"
#  > 
#
#  Each list in panelDesc represents a single glyph column in the output generated.
#  This makes it easier to create the glyph description, you only have to provide 
#  the information needed for the glyph, and allows you to quickly change the 
#  order of the glyphs in the results.  As new glyph variables are defined, the
#  only have to be included in the list for the specific glyph and column.  The 
#  same glyph may be used several times with different glyph variables settings.
#  Currently the glyph (details) variable names must contain the glyph name and 
#  a variable name.  With this approach, the variable names are simplified and 
#  have the same meaning across all of the glyphs but are specific to the glyph 
#  and column.  For more details see the panelDesc section of the documentation.
#
####
#
# Individual Call Arguments/Parameters:
#
# rowNamesCol: Optionally the name of the column in the area data.frame that
#           contains the link names associated with the rows.  If not specified,
#           the row.names of of the statsDFrame are used as the area names.
#           Using the row.names is the default method of linking the data to the 
#           border data.
#
# rowNames: Type of area id used as row.names in statsDFrame data.frame. 
#           Acceptable values are: "ab", "alt_ab", "full", "id",   Two additional options
#           have been added to accomodate the SEER data requirements:  "seer" or "alias".
#           This rowNames option requests the packet to do partial matches of an alias for 
#           area against the "registry" column/list outputted by SeerStat. If the partial
#           match succeeds, the associated area abbreviation is used.
#           By default the row.names of the statsDFrame are used.  Based on 
#           this option, the value is treated as an abbreviation, full area name,
#           or the numeric ID of the area..
#           The default is "ab" for abbreviation.
#
# ignoreNoMatches is a logical parameter.  The default is FALSE.  If FALSE, all of the 
#           data rows in the statsDFrame MUST match the area list in the boundaries datasets.
#           The there is not a match, an error is generated and the call is terminated.
#           If set to TRUE, any data row that does not match the areas in the boundaries dataset
#           are ignored and the user is notified of the situation.  This may be helpful, if you 
#           know the full names or abbreviations are correct, but the data has a row with "US" or "ALL"
#           as the link value or the source of the data generated comment lines that should be ignored.
#
# plotNames: When the ID glyphs is selected, this options informs the 
#           package which form of labels to use.  The options are "full" area name
#           or the abbreviated area name. The default is the "ab" for abbreviated name. 
#           Acceptable values are: "ab", "full"
#           The values of the "ab" and "full" labels are provided in the areaNamesAbbrsIDs
#           data.frame associated with the border structures provided to the package.
#
# sortVar   The column name or number in the statsDFrame to be used as the variable 
#           in sorting.  Can be a vector of column subscripts to break ties.
#           Warning: The sortVar parameter cannot be used to sort a boxplot 
#           or time series, since data is not contained in the statsDFrame.
#
# ascend    TRUE default sorts in ascending order.  FALSE indicated descending order.
#
# title     A vector with one or two character strings to use the title.for the page.
#
#   BORDER GROUPS
#
# bordDir   (optional) The path name to a directory containing the border group specified in 
#           bordGrp.  The file must be an ".rda" type file that contains the four border group
#           R objects: areaParms, areaVisBorders, L2VisBorders, L3VisBorders.  This parameter
#           can be used when the user has their own border group dataset or during developement
#           of a new border group or testing a modified border group before a package is created.
#           When this field is specified, the internal border groups are ignored.
#
# bordGrp   The package contains two border Groups:  USStatesBG and USSeerBG.
#           When using the "USStatesBG" border group, allows the package to function identically
#           to the original micromapST package.  When the "USSeerBG" border group is
#           used, the Seer Areas and structures are available to the micromapST user.
#           The USSeerBG border group contains the names, abbreviations, aliases, and border 
#           structures to support the micromap generation for US Seer Area data.
#
#           NOTE: For border groups to work, lazyloading and lazydata must be DISABLED.  
#           If enabled, the package is unable to load the correct border group dataset based
#           on the bordGrp parameter value.
#
#   PANEL LAYOUT:
#
# grpPattern A user provided area to panel group/row mapping.  The sum of the vector must 
#           be equal to the number areas provided in the statsDFrame data structure.
#           The values are the number of areas in each panel created by micromapST.
#           The values must be in the range of 2 through 5.   The value of 1 is allowed,
#           but only if the number of areas is odd, and in the median position of the 
#           vector.  Examples:  
#                 For 9 areas    grpPattern = c(3,3,3) for 3 areas per panel row.
#                 For 9 areas    grpPattern = c(4,1,4) for a pattern of 4 areas, 1 area, 
#                     and 4 areas per panel.
#                 For 17 areas   grpPattern = c(5,3,1,3,5)  or c(4,4,1,4,4) or c(4,3,3,3,4)
#                 For 18 areas   grpPattern = c(5,4,4,5)
#           The grouping pattern must also be symetric about the median point and have 
#           the number of rows per panel desend toward the median point.   This is required 
#           make the micromap graphics presentable.  A grpPattern = c(3,4,4,5) or c(3,4,4,3) 
#           are not allows.  The maximum value for the rows per group is 5.
#
# maxAreasPerGrp - a numerical parameter of the maximum number of areas that can be
#           be represented by a group/row in the link micromap.  The default value is
#           5 areas per group/row.  The value may be set to any integer between 2 and 5.
#
#   MAPPING:
#
# dataRegionsOnly is a logical parameter.  The default is FALSE.  If FALSE, the data is 
#           not inspected to determine if a subset of regions could be drawn saving 
#           mapping space in the map glyphs.  If set to TRUE, the data sub-areas 
#           are inspected to determine if a sub-set of regions can be drawn to 
#           save graphic space.  This feature is only active if the border group's
#           name table contain region identifiers for each sub-area.  This information
#           is used to determine how many sub-areas are required to be drawn and 
#           how to organize the map for presentation. As before any sub-areas 
#           in the mapped regions without data are only flagged with warning messages
#           and colored white, but still drawn.  If regional boundaries are present,
#           the boundaries are overlayed for regional with data.
#
# regionsB is a logical parameter.  The default is FALSE.  If FALSE, no regional 
#           boundaries are drawn.  If set to TRUE, if regional boundaries are 
#           present, they are drawn on the micromap.
#
#
#   Glyph Global parameters:
#
# axisScale A character string indicating the type of axis labels to be used
#           on the Y and X axis for glyphs with axis labels.  The acceptable
#           values are:
#                "o" -> original (pretty function)
#                "e" -> extended algorithm - no scaling. (new default)
#                "s" -> numbers scaled to millions, billions, etc. with
#                         extra header line 
#                     example:
#                          0    10    20    30    40 
#                            in millions
#
#                "sn" -> numbers scaled individually and marked with 
#                         the scaling factor.
#                     example:
#                          0    500M   1B    1.5B    2B
#
#                "s" and "sn" are based on the "e" algorithm (extended.)
#
#           This call arugment can be overriden for a specific glyph column by
#           including "axisScale=" in the panelDesc list for the column.
# 
# staggerLab A true/false flag to specify if the axis labels are staggered 
#           alternating low and high labels.  The default = FALSE.  If FALSE
#           the axis labels are NOT staggered.   If TRUE, two axis label 
#           lines are drawn, with the axis labels alternated low and high lines.
#
#           This call arugment can be overriden for a specific glyph column by
#           including "staggeredLab=" in the panelDesc list for the column.
# 
#
#####

#####
#
# List/Control Parameters:  (package default data.frames are used if the colors and 
#      details parameters do not specify an alternate data.frame.  
#      It is strongly recommended to use the default data.frame)
#
# colors   a color palette as a vectors of strings (character-vectors)
#              6 colors for areas in a group of 6
#              1 color for the median area
#              3 foreground color for non-highlighted areas in the map
#              2 background colors for not referenced and non-active sub-areas,
#          and 12 matching colors with 20% transparency for time series.
#
#          If a color vector is provided, it's length must = 24.
#
#          If the value of colors is "bw" or "greys", a grey scale is used instead 
#          of the default or user provided colors vector.
#      The default is NULL, which indicates the package default colors should used.
#
#      see rlmicromapGDefaults$colors for more details
#
#
# details   defines the spacing, line widths, colors and many many other details 
#      controlling the style and apparence of the generated glyphs.
#
#      see the micromapGDefaults$details section for more details.
#
#      The function automatically loads the default values into the code when the 
#      function is started.  The user can use the details parameter to override 
#      any of the items and values in the micromapST package.  To override a value, 
#      create a list as follows:
#
#      details = list(<variable name> = <value>,,,  )
#
#      See the micromapGSetDefaults function below for a definition of each 
#      micromapST variable and it's default.
#
#####

#####
#
#  Load working tables for verifications
#
#  details variable list
#

utils::data(detailsVariables,envir=environment())    # get validation and translation table for details variables to panelDesc variables.

#
#####

#####
#
#  Counter Initialization (Global)  - research code = to be removed.
#
#  Variable at the micromapST level.
#   

#Saved_Locale <- Sys.getlocale(category='LC_CTYPE')  # save existing locale
#x <- Sys.setlocale('LC_ALL','C')                         # set to 'C'

mstColorNames         <- "black"
mmSTEnvir             <- environment()
xmsg                  <- capture.output(mmSTEnvir)
#cat("micromapST envir:",xmsg,"\n")

#
# Set up global variables values.
#

#
#   create warning and stop counters - must be in .GlobalEnv so the panelXXXX functions can use them.
#
var  <- "warnCnt"
wstr <- paste0("assign(var,NewCounter(),envir=.GlobalEnv)")
eval(parse(text=wstr))

var  <- "stopCnt"
wstr <- paste0("assign(var,NewCounter(),envir=.GlobalEnv)")
eval(parse(text=wstr))

stopCntMsg <- function(xmsg) {
   stopCnt()
   stop(xmsg,call.=FALSE)
   return(TRUE)
}

errCntMsg  <- function(xmsg) {
   warnCnt()
   warning(xmsg,call.=FALSE)
   return(TRUE)
}
#
#   this should get the global variables set up so they can be referenced within all functions.
#
#  Cross column variables
#

lastLab2Space    <- NULL
lastLab3Space    <- NULL
staggered        <- NULL
staggering       <- NULL

var  <- "lastLab2Space"
wstr <- paste0("assign(var,0,envir=.GlobalEnv)")
eval(parse(text=wstr))
var  <- "lastLab3Space"
wstr <- paste0("assign(var,0,envir=.GlobalEnv)")
eval(parse(text=wstr))
var  <- "staggered"
wstr <- paste0("assign(var,FALSE,envir=.GlobalEnv)")
eval(parse(text=wstr))
var  <- "staggering"
wstr <- paste0("assign(var,FALSE,envir=.GlobalEnv)")
eval(parse(text=wstr))

#
#  glyph variables - at this time this is required to allow us to validate this variable.
#
Id.Dot.pch       <- NULL

var  <- "Id.Dot.pch"
wstr <- paste0("assign(var,22,envir=.GlobalEnv)")   # assign default of 22.
eval(parse(text=wstr))

#cat("envir=warnCnt:", find("warnCnt"),"\n") 

#
#####

#####
#
#  Save call parameter values for warning and error messages, not content, name of variables.
#
#     Can't do this in a function because the environment and frames will change.
#
frml         <- formals()                   # get list of call parameters - the formals - for the function and default values. (as defined).
frmlNames    <- names(formals())            # get the name of the parameters  (as we validate the parameter, we will back file the defaults.

callVar      <- as.list(match.call())[-1]   # get the names and values used on the current call.
callVarNames <- names(callVar)              # get the names of the used call parameters

# merge the formals parameter list with the parameter list used at the time of the micromapST call with user set values.

callVL       <- frml                        # Seed the call variable list with the formals and default values 
callVL[callVarNames] <- callVar[callVarNames]  # copy the values used in the call .

# save call parameter list and values to .GlobalEnv
var  <- "callValList"
wstr <- paste0("assign(var,callVL,envir=.GlobalEnv)")
eval(parse(text=wstr))

#  Extract the statsDFrame variable name
var  <- "sDFName"
wstr <- paste0("assign(var,callVL$statsDFrame,envir=.GlobalEnv)")
eval(parse(text=wstr))

#  Extract the panelDesc variable name
var  <- "pDName"
wstr <- paste0("assign(var,callVL$panelDesc,envir=.GlobalEnv)")
eval(parse(text=wstr))

#print(paste0("statsDFrame=",sDFName))
#print(paste0("panelDesc  =",pDName ))

#
#  callVarList is now a names list with the names of the parameter variables the the list content the 
#    values at the time of the call.  Any variables show up with a typeof "symbol" and class "name".
#    The value of the variable is not captured.
#
#  Later must copy this information up to the .GlobalEnv so it can be referenced by everyone.
#

#print(callVL)

#
#####

#print("callVarList Saved in .GlobalEnv")

#####
#
#  Verify Run Parameter:
#
#  Order of importants:
#    a) bordDir and bordGrp - needed to get the border group loaded and its particular parameters defaults
#    b) Validate statsDFrame (but not contents)
#    c) Validate panelDesc   (but not contents, yet)
#
#  bordDir and bordGrp  - 1st parameter to check - sets up the information for all of the other parameters.
#
#  Package contained border groups:
#

PkgBGs <- c("USStatesBG"
           ,"USSeerBG"
           ,"KansasBG"
           ,"MarylandBG" 
           ,"NewYorkBG"
           ,"UtahBG" 
           ,"AfricaBG"
           ,"ChinaBG"
           ,"UKIrelandBG" 
           ,"SeoulSKoreaBG"
          )

UserBordGrpLoad <- FALSE             # FALSE, load from package with data(),  TRUE load from directory with load()

#  Package Variables

#
#  bordDir   - if directory then private border group.
#
#   The bordDir is used to direct the border group load to a user directory or during testing 
#    of a new or modified border group.
#

bordDir   <- bordDir[[1]][1]

if ( is.null(bordDir) || is.na(bordDir) ) {

    # make sure its NULL
    bordDir <- NULL    # no directory provided.

} else {

    # validate the directory exists and is referencable.
    bordDir <- stringr::str_trim(bordDir)                   # trim spaces.
    
    if (!dir.exists(bordDir))  {
       # bordDir path does not exist.
       xmsg <- paste0("***0153 BGBD The directory specified in the bordDir call parameter does not exist. Value=",bordDir)
       stopCnt()
       stop(xmsg,call=FALSE)
    } else {
       UserBordGrpLoad = TRUE    # load() from directory don't data()
       xc <- stringr::str_sub(bordDir,-1,-1)  # get last character
       if (xc != "/" && xc != "\\") {
          bordDir <- paste0(bordDir,"/")   # add slash   if not present.  (must check for \ and / slashes.)
       }
    }

}

callVL$bordDir <- bordDir

bordGrp             <-  bordGrp[[1]][1]  # get first value only
BordGrpName         <-  bordGrp 
bgFile              <-  NA

#  border group Dir
if (!UserBordGrpLoad) {
   # no valid bordDir directory -> the bordGrp must be a .rda in this package. 
   # If no bordGrp parameter, set default to USStatesBG.
  
   if ( is.null(bordGrp) || is.na(bordGrp) ) {
      BordGrpName         <- "USStatesBG"               # indicates which structure .rda file in package to load.
      
   } else {
      BordGrpName         <- bordGrp
   
      bGM <- match(BordGrpName,PkgBGs)    # must be one of the packaged bordGrps

      if (is.na(bGM)) { 
         # no match to the bordGrps supported within the package.
         # Use variable to make message dynamic as more bordGrps are added.
      
         ymsg      <- paste0(shQuote(PkgBGs),collapse=", ")
         xmsg      <- paste0("***0150 BGBD The bordDir call parameter was set to NULL, the bordGrp must be one contain in the package:\n", ymsg, "\n")
         stopCnt()
         stop(xmsg, call.=FALSE)
  
         # alternative is to check for file in working directory and them varity it's structure.
         rm(ymsg)
      }
      rm(bGM)
   
      # DATA bordGrp
   }
   
} else {

   if ( is.null(bordGrp) || is.na(bordGrp) ) {
      #  bordDir provided, but no bordGrp - ouch! error
      stopCnt()
      xmsg    <- paste0("***0156 BGBD The bordGrp call parameter has not been specified. It is required when the bordDir is provided.")
      stop(xmsg, call.=FALSE)
      
   } else {
   
      # if not check to see if the .rda file exists.
      fnSplit     <- stringr::str_split(bordGrp,"[.]")[[1]]   # split up user provided name.
      BordGrpName <- fnSplit[1] 
      if (is.na(fnSplit[2])) {
         # if no extension - then add .rda
         bordGrp     <- paste0(bordGrp,".rda")
      } else {
         # if extension is present - must be .rda or .RData
         if (fnSplit[2] != "rda" && fnSplit[2] != "RData") {
            # error - extension must be .rda or .RData.
            xmsg <- paste0("***0154 BGBD The bordGrp filename must have an '.rda' or '.RData' file extension.")
            stopCnt()
            stop(xmsg,call = FALSE)
         }
      }
   } 
   # test to see if directory and file exist, before trying to load.
   
   bgFile             <-  paste0(bordDir,bordGrp)

   if (!file.exists(bgFile)) {
      xmsg       <- "***0155 BGBD The bordGrp file in the bordDir directory does not exist."
      stopCnt()
      stop(xmsg, call = FALSE)
   }
}

# got this far, variables to load/data the border group appear to be good.

callVL$bordGrp     <- bordGrp
callVL$bgFile      <- bgFile
callVL$BordGrpName <- BordGrpName

var     <- "callVarList"
wstr    <- paste0("assign(var,callVL,envir=.GlobalEnv)")
eval(parse(text=wstr))

#cat("bordDir     = ",bordDir,"\n","bordGrp = ",bordGrp,"\n")
#cat("BordGrpName = ",BordGrpName,"\n")
#cat("bgFile      = ",bgFile,"\n")

#
######
#
#   load micromap border and names tables based on type of run
#      Currently supported: USStatesBG and USSeerBG
#

#
## add code to pick up on "bordGrp" provided by user.  
##  If one of ours use data, otherwise use load or "copy" from structure of that name.
##  bordGrp must be data.frame containing "areaNamesAbbrsIDs, areaVisBorders, L2VisBorders, RegVisBorders,
##    L3VisBorders, and areaParms.
#
#  Thoughts on border group verication:
#  1) Do it once and get a md5 check sum on the files that pass.
#  2) Place name of border group file and directory and MD5 in 
#       file in micromapST/data folder under the "library".
#  3) Prior to using private border group check library information to see if verifcation must be done.
#

## for testing - use load instead of data.

# initialize border group variables to determine if they are correctly loaded.

areaParms         <- NULL
areaNamesAbbrsIDs <- NULL
areaVisBorders    <- NULL
L2VisBorders      <- NULL
RegVisBorders     <- NULL
L3VisBorders      <- NULL

if (!UserBordGrpLoad) {
  # System border group
  #print (paste0("reading border group ",BordGrpName, " via a data statement."))
  
  utils::data(list=BordGrpName,envir=environment())    # Group Border tables and parameters distributed with package.

} else {
  # user border group   
  #print (paste0("reading border group ",BordGrpName, " via LOAD since bordDir = ",bordDir))

  # need to put a try around this incase there is a problem with the user data file.

  res <- try(load(bgFile),silent=TRUE)                    # only error should be a lock or error in reading file.
  if (inherits(res,"try-error",which=FALSE)) {
     # TRUE - error occurred during user border group file loading.
     stopCnt()
     xmsg     <- paste0("***0157 BGBD System error encountered when loading the border group. See error message:")
     ymsg     <- paste0("***0157 >>",res[1])  # get message from error
     warning(xmsg, call.=FALSE)
     stop(ymsg,    call.=FALSE)
     # stopped.  
     rm(ymsg)
  }
}

#
#  Basic Verify that all of the bordGrp data.frames have been loaded.
#

MissInBG   <- NULL
ErrFnd     <- FALSE
if (is.null(areaParms)) { 
   ErrFnd   <- TRUE
   MissInBG <- paste0(MissInBG,", areaParms")
}
if (is.null(areaNamesAbbrsIDs)) { 
   ErrFnd   <- TRUE 
   MissInBG <- paste0(MissInBG,", areaNamesAbbrsIDs")
}
if (is.null(areaVisBorders)) { 
   ErrFnd   <- TRUE
   MissInBG <- paste0(MissInBG,", areaVisBorders")
}
if (is.null(L3VisBorders)) { 
   ErrFnd   <- TRUE
   MissInBG <- paste0(MissInBG,", L3VisBorders")
}
#if (is.null(L2VisBorders)) { 
   # No action at this time.  Check later when processing areaParms.  
   #   L2VisBorders is only needed if Map.L2Borders is TRUE. 
   #   If there is no L2VisBorders data.frame, set L2VisBorders to L3VisBorders, 
   #   or is equal to NA, then the Map.L2Borders are set to FALSE 
   #   and a warning message generated.
#}
#if (is.null(RegVisBorders)) { 
   # No action at this time.  Check later when processing areaParms.  
   #   RegVisBorders is only needed if aP_Regions or Map.RegBorders are set to TRUE. 
   #   If there is no RegVisBorders data.frame, it is set to L3VisBorders, 
   #   then Map.RegRegions is set to FALSE.  It is possible to do regional mapping without
   #   regional boundaries.
#}

#str(areaNamesAbbrsIDs)

if (ErrFnd) {
   # if ErrFnd, the MissInBG must contain at least one entry.
   MissInBG  <- substr(MissInBG,3,nchar(MissInBG))  # Kill leading ", "
   stopCnt()
   xmsg      <- paste0("***0151 BGBD After loading ",BordGrpName," border group data set, the following objects are missing: ",MissInBG)
   stop(xmsg, call.=FALSE)
}
rm(MissInBG,ErrFnd)

# Clean up and move data into old structures

#
# Later add code to validate possibly USER provided border groups.
#

if (UserBordGrpLoad) {

   # verify border group objects.  (columns, same number of rows, etc.)

   # objective is to only check data once - mark the data for future reference.
   
   # Want to keep run times VERY VERY low and not keep re-checking user data.

   # Lot of work to be done.  
   
   # OR set flag in BG.rda indicating it has been verified.  Do Once on request.
   
   # Check Validation by BGValidate function.
   #   md5sum file is in .Library directory under the name BGmd5.rda
   #   Contents is BG name and md5 check sum.
   #   run md5sum over the BG file and compare values with this file.
   #   if it matches, then BG file does not have to validated and waste time and CPU.
   #

}

##########
#
#  Merge the "areaParms" variables into the global variables.  
#
#  They may still be overridden by the details=list(...) parameter in the call.
#

    #  Set the type of everything to protect against factors on data.frames.
    bordGrp       <- as.character(areaParms$bordGrp)
    Map.Hdr1      <- as.character(areaParms$Map.Hdr1)
    Map.Hdr2      <- as.character(areaParms$Map.Hdr2)
    
    Map.MinH      <- as.numeric(areaParms$Map.MinH)
    Map.MaxH      <- as.numeric(areaParms$Map.MaxH)
    
    Map.Aspect    <- as.numeric(areaParms$Map.Aspect)
    
    if (is.null(areaParms$ID.Hdr1)) {
        # New variable names
        Id.Hdr1   <- as.character(areaParms$Id.Hdr1)
        Id.Hdr2   <- as.character(areaParms$Id.Hdr2)
    } else {
        # Old variable names
        Id.Hdr1   <- as.character(areaParms$ID.Hdr1)
        Id.Hdr2   <- as.character(areaParms$ID.Hdr2)
    }

    # Map.L2Borders  - draw L2 borders
    if (is.null(areaParms$Map.L2Borders)) {
       Map.L2Borders <- FALSE
    } else {
       Map.L2Borders <- as.logical(areaParms$Map.L2Borders)
    }

    # Map.L3Borders  - draw L3 borders   (option - also turned off if limited regional drawing is done.) 
    if (is.null(areaParms$Map.L3Borders)) {
       Map.L3Borders <- TRUE
    } else {
       Map.L3Borders <- as.logical(areaParms$Map.L3Borders)
    }

    areaUSData    <- as.logical(areaParms$areaUSData)
    enableAlias   <- as.logical(areaParms$enableAlias)
    
    #print("areaParms:")
    #print(str(areaParms))

# fix up areaParms to unique names
    
    # check for old field names.  If present - copy to new names.
    
    if (!is.null(areaParms$Regions)) {
       areaParms$aP_Regions     <- as.logical(areaParms$Regions)
       areaParms$Regions        <- NULL
    }
    if (!is.null(areaParms$Units)) {
       areaParms$aP_Units       <- as.character(areaParms$Units)
       areaParms$Units          <- NULL
    }
    if (!is.null(areaParms$Proj)) {
       areaParms$aP_Proj        <- as.character(areaParms$Proj)
       areaParms$Proj           <- NULL
    }
    
    #  Check regions, and boundary overlay flags.
    #
    #  aP_Regions - feature enabler - if RegVisBorder is present and information in name table.
    #    Referres to name table information, not regional boundaries
    #

    if (is.null(areaParms$aP_Regions)) {
       areaParms$aP_Regions     <- FALSE         # no regional mapping feature
    }
    
    #  region borders can be drawn or not.   If regions feature enable, default = TRUE.  If not, FALSE 
    #        
    
    if (is.null(areaParms$Map.RegBorders)) {
       areaParms$Map.RegBorders <- FALSE         # no regional boundaries available
    }
    
    # copy from data frame into work variables.
    
    aP_Regions           <- as.logical(areaParms$aP_Regions)
    aP_Units             <- areaParms$aP_Units
    aP_Proj              <- areaParms$aP_Proj
    
    Map.RegBorders       <- as.logical(areaParms$Map.RegBorders)
    
    #cat("Initial areaParms - Map. L2:",Map.L2Borders,"  Reg:",Map.RegBorders,"  L3:",Map.L3Borders,"  aP_Regions:",aP_Regions,"\n")   
    #print(str(areaParms))

#
#  after this point we do not reference areaParms again.
#
####

####
#
#   The following variables may be included in details, but are not configured here
#   with defaults.  They are variables initialized in the border group areaParms table.
#

detailExtra <- colnames(areaParms)   # get list of parameters from areaParms

x           <- match("bordGrp",detailExtra)   # if list contains "bordGrp"

if (!is.na(x)) {
   detailExtra <- detailExtra[-x]             # remove it from the list. 
}

    # When "bordGrp" is excluded, this would leave:
    #  "Id.Hdr1","Id.Hdr2", "Map.Hdr1", "Map.Hdr2", "Map.MinH", "Map.MaxH", "Map.L2Borders", 
    #  "areaUSData", "enableAlias", "aP_Regions" , "aP_Proj" , "aP_Units"
           
#
# This list is appended to the colname list derived from the default details data.frame to 
# create a list of valid variables.
#
##########

##########
#
#  areaNamesAbbrsIDs and xxxVisBorders tables
#
    #cat("dim of areaNamesAbbrsIDs:",dim(areaNamesAbbrsIDs),"\n")
    #cat("names of areaNamesAbbrsIDs:",names(areaNamesAbbrsIDs),"\n")
    row.names(areaNamesAbbrsIDs) <- areaNamesAbbrsIDs$Key   # ensure row.names match the keys
    
    if (is.null(areaNamesAbbrsIDs$regID)) {   
       # current name table does not have regional information.
       areaNamesAbbrsIDs$regID <- "<NA>"
       areaNamesAbbrsIDs$regName <- "<NONE>"
    }

    rlAreaNamesAbbrsIDs <- areaNamesAbbrsIDs                # save copy of name table
    #cat("dim of rlAreaNamesAbbrsIDs:",dim(rlAreaNamesAbbrsIDs),"\n")
    #print(str(areaNamesAbbrsIDs))

    # sub-area boundaries

    rlAreaVisBorders    <- areaVisBorders                   # save copy of sub-area boundaries
    #cat("dim of rlAreaVisBorders   :",dim(rlAreaVisBorders),"\n")
    #print(str(areaVisBorders))

    # total area boundaries

    rlL3VisBorders      <- L3VisBorders                     # save copy of area boundary
    #cat("dim of rlL3VisBorders     :",dim(rlL3VisBorders),"\n")
    #print(str(L3VisBorders))
    
    # Check on L2VisBorder and complete set up.
    
    if (Map.L2Borders) {
       if ( is.null(L2VisBorders) || identical(L2VisBorders,L3VisBorders) ) {
          # no L2VisBorders or L2VisBorders is the same as L3VisBorders
          # Map.L2Borders set on - but no boundaries to draw.
          xmsg             <- paste0("***0158 BGBD In the areaParms data.frame the Map.L2Borders is TRUE, but no level 2 boundaries are provided,  Level 2 overlay is disabled.\n")        
          warnCnt()
          warning(xmsg,call.=FALSE)
          L2VisBorders     <- L3VisBorders   # copy L3 to L2 data.frame (Place holder)
          Map.L2Borders    <- FALSE
       }
    }
    
    rlL2VisBorders      <- L2VisBorders
    
    #print(str(rlL2VisBorders))  
    #
    
    # Check on RegVisBorder and complete set up.

    if (Map.RegBorders) {
       if ( is.null(RegVisBorders) || identical(RegVisBorders,L3VisBorders) ) {
          # no RegVisBorders or RegVisBorders == L3VisBorders
          xmsg           <- paste0("***0159 BGBD The areaParms variable aP_Regions is TRUE and/or Map.RegBorders is TRUE, but no regional boundaries exist in border group. Regions overlay is disabled.")
          warnCnt()
          warning(xmsg,call.=FALSE)
          Map.RegBorders <- FALSE
          RegVisBorders  <- L3VisBorders   # copy L3 to L2 data.frame (placeholder)
       }
    }
    
    #
    rlRegVisBorders      <- RegVisBorders
    
    #print(str(rlRegVisBorders))  
    
#
# Implementation change note:  The regions feature will be implemented using the 
#   regID field in the areaNamesAbbrsIDs table and a RegVisBorders boundary data.frame.
#   The regID field associates the sub-areas to regions.  
#   If a RegVisBorder file is present, the boundaries
#   are grouped by the regID codes as it's keys. This permits 
#   sub mapping of its boundaries - hopefully they will 
#   match up with the area boundaries.
#
#  Map.L2Borders  > controls if L2VisBorders is drawn.
#  Map.RegBorders > controls if RegVisBorders is drawn.
#  Map.L3Borders  > controls if L3VisBorders is drawn.
#
#  Map.L3Borders is TRUE by default, but reset to FALSE when a sub-set of regions are drawn.
#  Map.RegBorders is only TRUE when there is a valid RegVisBorders data.frame.  This is not 
#    independent of the aP_Regions feature control flag.
#  Map.L2Borders is TRUE when a valid L2VisBorders data.frame is present.
#
#  When a subset of the regions in a border group are to be drawn,
#    a) The areaNamesAbbrsIDs name table is not modified.
#    b) L2VisBorders, RegVisBorders data.frames are edited to the limited group of areas.
#         It is assumed L2 is a subset of Reg.
#    c) Map.L3Borders is set to FALSE to not draw the outline of the total space.
#

#
#  If both L2 and Reg are persent,  The name table is used to know witch L2 boundaries to draw.
#  In regions mode:   
#
#        listUsedArea      <- areaNamesAbbrsIDs[IndexDFtoNT,"Key"]
#        listUsedL2        <- unique(areaNamesAbbrsIDs[IndexDFtoNT,"L2_ID"])
#        listUsedRegions   <- unique(areaNamesAbbrsIDs[IndexDFtoNT,"regID"])
#
#     a) if dataRegionsOnly=TRUE - enable regions feature.
#
#        regMatch          <- !is.na(match(areaNamesAbbrsIDs$regID,listUsedRegions))  # list of sub-areas in regions
#        listAllAreas      <- areaNamesAbbrsIDs[regMatch,"Key"]
#        listAllL2         <- unique(areaNamesAbbrsIDs[regMatch,"L2_ID"])
#        listAllRegions    <- unique(areaNamesAbbrsIDs[regMatch,"regID"]) 
#
#     b) if dataRegionsOnly=FALSE or not enabled.
#
#        listAllAreas      <- areaNamesAbbrsIDs[,"Key"]
#        listAllL2         <- unique(areaNamesAbbrsIDs[,"L2_ID"])
#        listAllRegions    <- unique(areaNamesAbbrsIDs[,"regID"]) 
#                
# ensure Abbreviations are all CAPS.
#
# The package carries two sets of names for each area
#     (in the areaNamesAbbrsIDs table and areaVisBorders matrix.)
#
#    abbreviated - always in CAPS.
#    fullname    - always with proper capitalization.
#                  but is CAP'd for all comparisons.

    # fix up Name Table for sub-area matching.
    
    #cat("dim of areaNamesAbbrsIDs:",dim(areaNamesAbbrsIDs),"\n")

    #    Matching strings
    rlAreaNamesAbbrsIDs$Abbr <- toupper(rlAreaNamesAbbrsIDs$Abbr)   #  Abbr Must be uppercase.

    areaNTAbbr   <- (rlAreaNamesAbbrsIDs$Abbr)               # Get list of abbrevations. (All CAPS)

    areaNTName   <- (toupper(rlAreaNamesAbbrsIDs$Name))      # get list of full area names in uppercase (All CAPS)
    
    areaNTAAbbr  <- (toupper(rlAreaNamesAbbrsIDs$Alt_Abbr))  # get list of alternate abbreviations.
    
    areaNTKey    <- (toupper(rlAreaNamesAbbrsIDs$Key))       # get key as uppercase. (links into VisBorder files.)
    
    #    Presentation strings
    ID.Abbr      <- areaNTAbbr                               #                           (All CAPS)
    # for ID.Name force proper capitalization on the name
    ID.Name      <- as.vector(sapply(areaNTName,function(x) simpleCap(x))) # proper cap.

    areaNTName   <- ClnStr(areaNTName)
    areaNTAbbr   <- ClnStr(areaNTAbbr)
    areaNTAAbbr  <- ClnStr(areaNTAAbbr)
    areaNTID     <- ClnStr(toupper(rlAreaNamesAbbrsIDs$ID))
 
    # fix up areaVisBorders data frame
    
    rlAreaVisBorders$Key  <- toupper(rlAreaVisBorders$Key)
    rlRegVisBorders$Key   <- toupper(rlRegVisBorders$Key)
    rlL2VisBorders$Key    <- toupper(rlL2VisBorders$Key)
 
    # Working vectors for PRINT out.
    
#
####

xps  <- par("ps")
xpin <- par("pin")
xdin <- par("din")

#cat("check point on par - ps:",xps," pin:",xpin," din:",xdin,"\n")

#print("Border Group Read and Setup")

####
#
#________________ Type of micromap Variable (for now)_______________

#
# extend hdr strings to include the other types of maps
#
   
#
####

#####################
#
#  Border Group now loaded and name table initial setup completed.
#
#####################
#
#  Finish check after the glyph function definitions.
#
#####################

#####################
#
# Define panel glyph functions=====================================
#
#    All of these glyph creating functions are internal to the micromapST function.
#

#####
#
# type = 'arrow' =========================================================
#
# rlAreaArrow
#
# JP - fixed error when difference is zero.
# JP - generalize for any collections of area
#
# PDUsed - list of column names from the panelDesc data.frame provided.
#
# The col1 and col2 values have already be converted to column numbers and verified.
#

rlAreaArrow = function(j){

   # glyph column setup section - start of code
  
   ###
   #  Split into header and trailer.
   ###
   
   #cat("Arrow-StartUp staggered:",staggered,"\n")
  
  
   # j = current panel column number
   #  
   #  col1[j] points to the statsDFrame column holding the first arrow end point.value
   #  col2[j] points to the startFrame column holding the second arrow end point value
   #
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
     # glyph description
     #xgl <- list(Name="ARROW", n=2, dCols=c("col1","col2"), tCols=("n","n"), iCols=c(col1[j],col2[j]), lCols=c(litcol1[j],litcol2[j])) 
     #  where   Name  = name of glyph
     #          dCols = panelDesc field names
     #          tCols = type of data in each statsDFrame column
     #          iCols = index # into statsDFrame data.frame
     #          lCols = value provided in colx list for this column
     #
   
  
   # "col1"
   stColName1   <- wstname[col1[j]]
   if (is.null(stColName1)) { stColName1 <- as.character(col1[j]) }
   
   pdUmsg       <- "(Beginning value of arrow)"
   xr           <- CheckPDCol('col1',  'ARROW', col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd    <- TRUE 
   } else { 
      xdat1     <- xr$Dat 
   }

   # "col2"
   stColName2   <- wstname[col2[j]]
   if (is.null(stColName2)) { stColName2 <- as.character(col2[j]) }
 
   pdUmsg       <- "{End value of arrow)"
   xr           <- CheckPDCol('col2', 'ARROW', col2[j], stColName2, j, 2, wstMax, dat, pdUmsg)

   if (xr$Err) { 
      ErrFnd <- TRUE 
   } else { 
      xdat2  <- xr$Dat 
   }
   
   if (ErrFnd) return()    # Error warning noted, return from function.
   
   # pull out the variables for this column "j"
   refval      <- lRefVals[j]             # change to lRefVals - JP-2010/07/23   Reference value for column
   reftxt      <- lRefTexts[j]            # added - JP-2010/07/23                Reference test for column
    
   good1       <- !is.na(xdat1)           # test to see if both values are present.
   good2       <- !is.na(xdat2)
   goodrow     <- !is.na(xdat1 + xdat2)   # used by code to skip bad entries.
   
   
   # Get the value range for the data (col1 and col2)
   rx          <- range(xdat1,xdat2,na.rm=T)              # range on of all x1 and x2 values for all areas.
   
   #cat("arrow-x range:",rx,"\n")
   
   #rx         <- sc*diff(rx)*c(-.5,.5)+mean(rx)    # 
                                   #  x-scale extention (sc) = 1.08 *
                                   #  diff of min and max of all * 1/2 + or - to get bracket around mean
                                   #  if range 1 to 25, mean is 13, diff(rx) = 24, --> 0.04 to 25.96 (almost + and - 1)
   
   lPad    <- TRUE
   rPad    <- TRUE
   #cat("arrow-x range adjusted:",rx,"\n")
   
   ry          <- c(0,1)                      # Y axis range = 0 to 1.. 

   # ____________labeling and axes_______________

   #####
   #
   #  Setup and draw top and bottom titles and axis for column  (header and trailer)
   #
   #        Split into two functions - setup and execute-header, execute-trailer.
   #
   #   Needs padding for tails and arrow heads (how much, not as much as dots.)
   #

   Res       <- DrawXAxisAndTitles(j, panels, rx, ry, reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx      <- Res$atRx
   rx        <- Res$rx
   ry        <- Res$ry
   
   #cat("arrow-rx after DrawXAxisAndTitles:",rx,"\n")
  
   #cat("Arrow-after DrawXAxisAndTitles-staggered:",staggered,"\n")
   
   #
   #####
   
   # End of Arrow Glyph Setup up section
   
   # Glyph Column Header section
   #    Titles
   #    X-Axis labels
   
   
   # Arrow Glyph Body Section
  
   #_________________drawing loop__________________  
  
   # The drawing may be for 1 to 5/6 rows.  Find out in the gsubs ->  ke.

   #  Draw all of the elements - one per area  - group number = 1 to ng.
 
   for (i in 1:numGrps){
   
      # loop to generate each panel in column
      
      ###
      #  Single Glyph group/row
      ###
      
      gsubs  <- ib[i]:ie[i]          # get range ib to ie (area indexes for this panel) ----  gsubs vector of the indexes for this panel.
      ke     <- length(gsubs)        # get length  (length = 1 up to 5)
  
      # offset in panel is 0 to 7 (8 units) or 0 to 2 (1 unit),  Under the US version
      #    the y would be 5/6 to 1, so lines would be draw at 5, 4, 3, 2, 1  Leaving 7 and 0 open.
      #
      # Now we have the challenge of drawing 2, 3, or 4 lines in the panel and make it look the same.
      #
      # May need to check to see if the scaling already handles this. So let it go for now.
      # 
      # One approach is to adjust the values based on the number to graph.  Use table.
      #
      laby   <- ke:1                 # labels n:1 depending on number of rows in group (US case 1:1 and 1:5).
      
      # select pen color or colors  7 or 1:n  
      pen    <- if(i == medGrp & medGrpSize == 1) 7 else 1:ke   # if index=medGrp (median group number, if present) then pen = 7, else 1:ke (length of line)
 
      #cat("Arrow - panelSelect - 3021 - i:",i,"  j:",j,"\n")
      
      panelSelect(panels,i,j)          # select current panel
 
      x <- panelScale(rx,c(1-pad,ke+pad))   # scale to rx by 1,ke (pad)  (ry = effectively 0.33 to 5.67 (pad = 0.67)
                                       #   Scale = rx by 0.33 to 5.67 with arrows at 1,2,3,4,5...
                                       # scaling of ry handles the issue with the number of rows in group.
                                       #   for 6, ry => c( 0.33 , 6.67 ) because of padding. (0 -> 7 so 1/2 margin on each side.)
                                       #   for 5, ry => c( 0.33 , 5.67 ) because of padding. (0 -> 6 so 1/2 margin on each side.)
                                       #   for 4, ry => c( 0.33 , 4.67 )
                                       #   for 3, ry => c( 0.33 , 3.67 )
                                       #   for 2, ry => c( 0.33 , 2.67 )
                                       #   for 1, ry => c( 0.33 , 1.67 ) (also median)  - single at "1", with + or - 0.6667 on each side.
                                       #			           c(1,1) -> (0.33, 1.67
      panelFill(col=Panel.Fill.col) 
   
      # don't like page real size being used here - ? re-sizing.
      
      # calculate arrow length the is to small and should be draw as a dot.
      
      arrLim  <- max(diff(rx)/par("pin")/1000) * 1.05 # diff of range / plot inches / 1000  * 1.05
      
      #  verical grid lines 
      axis(side=1, tck=1, labels=F, at=atRx,
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grid lines in panel
 
      # if a refval is provided and in the rx range, then add line.
 
      AddRefLine(refval, ke, rx)
      
      panelOutline(col=Panel.Outline.col)  # outline panel 
   
      oldpar <- par(lend="butt")           # save old 
  
      # draw the area rows in this panel-row.
      
      for (k in 1:ke) {
         # loop through each item in panel (5/6 or 1)
         m   <- gsubs[k]                      # get index into data array
        
         if (goodrow[m]) {                 #  if good value for this area
        
            #print(paste0(k,m,xdat1[m],xdat2[m],abs(xdat1[m]-xdat2[m])))
            # Getting warning for NON-ZERO length arrows - must be rounding error to <> 0.
            #  So, taking liberties to say 0 is .002 and below.  Arrow works in inches??
         
            #  Alternative is to suppressWarnings...
            
            #  xdat1 and xdat2 are the variables pass in.  m is the row index.
            
            if (abs(xdat1[m]-xdat2[m])> arrLim){         #  If arrow length is > 1.05/1000 inch do line draw...
           
               # long line/arrow
               arrows(xdat1[m],laby[k],xdat2[m],laby[k],col=mstColors[pen[k]],
                     length=Arrow.Head.length,lwd=Arrow.lwd)
       
            } else {
       
               # length of arrow is zero, so plot a dot..
               plotPoint(xdat1[m],laby[k], 
                         Arrow.Dot.pch, mstColors[pen[k]], Arrow.Dot.pch.size, Arrow.Dot.pch.lwd, 
	                 Arrow.Dot.Outline, Arrow.Dot.Outline.col, Arrow.Dot.Outline.lwd) 
               
               #points(xdat1[m],laby[k],pch=20,cex=Dot.pch.size,col=mstColors[pen[k]])
       
            }
         }  
         #  end of one row.
      }   
      #  y is from 0 to 7, so the enter line for each arrow is 1,2,3,4,5,6, etc.
 
      par(oldpar)
    
      ###
      # end of one Arrow glyph panel (row/group)
      ###
    
   }
   
   # end of Arrow glyph column

   # ____________________________PanelOutline____________________
 
   ###
   # glyph column trailer.
   ###
 
   groupPanelOutline(panelGroup,j)      # outline full group (column)
   
   # Column done, check to see reference line text is needed in footnotes..
  
}

#
#  End of Arrow Glyph
#
#####


#####
#
#  type = 'bar' =========================================================
#
#  rlAreaBar
#

rlAreaBar = function(j){
   # j = current panel column number
   #  
   #  col1[j] points to the statsDFrame column holding the bar height from zero.
   #
   #cat("Bar Startup staggered:",staggered,"\n")
   
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
     
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
     
   # "col1"
   stColName1  <- litcol1[j]
   pdUmsg      <- "(Bar length)"
   
   xr          <- CheckPDCol('col1', 'BAR', col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd    <- TRUE 
   } else { 
      xdat1     <- xr$Dat 
   }
   
   if (ErrFnd) return ()     # error warning found - return.
 
   py           <-  Bar.barht*c(-.5,-.5,.5,.5,NA)     #  Bar.barht = 2/3 (0.6667) - basic pattern to form a bar.
   
   ry           <- c(0,1)
   
   refval       <- lRefVals[j]    # changed to lRefVals - JP-2010/07/23
   reftxt       <- lRefTexts[j]   # new - JP-2010/07/23
 
   # ________scale x axis________________________
 
   good         <- !is.na(xdat1)
   
   # get x axis range
  
   rx           <- range(xdat1,na.rm=T)         # get range of values (min-1, max-2)
   #cat("bar-rx:",rx,"\n")
   lPad   <- TRUE
   rPad   <- TRUE
   if (rx[2]<=0){                 
      # max < 0..
      rx[2]     <- 0           # set max to zero
      #rx[1]     <- mean(1,sc)*rx[1]   # adjust min.  (average of 1 and sc)
      rPad      <- FALSE
   } else if ( rx[1] >= 0 ) {
             #  min > 0 
             rx[1]   <- 0      # set min to zero
             #rx[2]   <- rx[2]*(1+sc)/2  # adjust max
             lPad    <- FALSE
          } else {
             # min and max are both > 0 
             #rx      <- sc*diff(rx)*c(-.5,.5)+mean(rx)
          }
   # end of if / else if group

   #cat("bar-rx adjusted:",rx,"\n")
   
   # ____________label axis_______________

   #####
   #
   #  Bar Setup and draw top and bottom titles and axis for column
   #
   #  No padding if Zero is left or right side.  Otherwise minor padding.
   #
   #cat("Bar-Calling DrawXAxisAndTitle.\n")

   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
  
   #cat("Bar-After DrawXAxisAndTitles staggered:",staggered,"\n")
   
   #
   #####
  
   #####
   #
   # Bar Glyph body section
   #
   #####
  
   # _______________drawing loop___________________
 
   for (i in 1:numGrps){      
   
      ###
      # Glyph group/row body
      ###
      
      gsubs    <- ib[i]:ie[i]                      # index of elements in panel
      ke       <- length(gsubs)
 
      pen      <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke          # Pen indexes.
 
      laby     <- ke:1                             # laby (1 or 1:2, 3, 4, 5, or 6)
      
      panelSelect(panels,i,j)                     # select current panel
      x        <- panelScale(rx,c(1-pad,ke+pad))   # re-scale to 1 or 5/6 entries per panel/row (same physical height used.)         
      						  # for 1 -> 
      panelFill(col=Panel.Fill.col)
      
      # grid lines for bar
      axis(side=1, tck=1, labels=F, at=atRx,
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grids
      
      # if a refval is provided and in the rx range, then add line.
 
      AddRefLine(refval, ke, rx)
      
      panelOutline(col=Panel.Outline.col)     # outline panel/row
      
      ksc      <- SetKsc(ke)                       # get scaler for bar height
      
      # play like we have 5 areas in this panel/row
      wpy      <- py * ksc
      
      #
      #  All panel/rows are the same height (except when the a single area is used in the median panel/row.
      #  All graphic element heights are calculated based on 5 areas per panel/row.  
      #  This keeps the height the same in all panel/rows and provided a uniform graphic.
      #
      
      for (k in 1:ke){
         m     <- gsubs[k]                         # draw each entry (1 to ke), get index from gsubs
         if (good[m]){
            # good value - draw bars as polygons.
            val    <- xdat1[m]                       # get value for bar height
            polygon(c(0, val, val, 0, NA), rep(laby[k], 5) + wpy,
                    col=mstColors[pen[k]] )  # fill color 
            polygon(c(0, val, val, 0, NA), rep(laby[k], 5) + wpy,
                    col=Bar.Outline.col, lwd=Bar.Outline.lwd, density=0)            # outline of bar
         }
         lines(c(0,0), c(1-.5*Bar.barht,ke+.5*Bar.barht), col=1) # re-draw base line of bars   
      }
      
      #####
      #
      # End of one Group/Row Body for Bar
      #
      #####
   }

   #  end of bar glyph column
   
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)   # outline column of glyphs

   #####
   #
   # End of Bar Glyph Body section 
   #
   #####
 
}

#
# End of Bar Glyph
#
#####


#####
#
#  type = 'boxplot' ======================================================
#
#  rlAreaBoxplot
#

rlAreaBoxplot  <- function(j, boxnam){
   
   # boxnam -> name of the panelData value for the boxplot data structure.
   
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
      
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
   
   # can we get access to the boxplot list?
   
   boxlist     <- tryCatch(get(boxnam, pos=1),error=function(e) e)
  
   if (inherits(boxlist,"error")) {   
      # could not find object named in boxnam.   which=false
      ErrFnd   <- TRUE
      warnCnt()
      xmsg     <- paste0("***02B1 BOXPLOT ",pdColNum," The panelData value of ",boxnam," in the ", pDName," data frame does not exist or is not accessible.")
      warning(xmsg, call.=FALSE)
      
   } else {
      if (!is.list(boxlist)) {
        ErrFnd    <- TRUE
        warnCnt()
        xmsg      <- paste0("***02B3 BOXPLOT ",pdColNum," The ", boxnam, " data for the boxplot is not a list.")
        warning(xmsg, call.=FALSE)

      } else {
          
        lnam      <- names(boxlist)    # names of lists in boxlist data, one per variable
        
        if ( is.null(lnam) || any(is.na(lnam)) ) {
           ErrFnd     <- TRUE
           warnCnt()
           xmsg       <- paste0("***02B4 BOXPLOT ", pdColNum, " The ", boxnam, " structure does not have any name attributes for the boxplot data.")
           warning(xmsg, call.=FALSE)
           
        } else { 
           #  The correct structure should have 6 names of elements in the list.
           
           if (length(lnam) != 6) {    
              # must have at least 1 element and name
              ErrFnd    <- TRUE
              warnCnt()
              xmsg      <- paste0("***02B5 BOXPLOT ", pdColNum, " The ", boxnam, " boxplot data is not a valid structure. Must contain 6 boxplot sub lists.")
              warning(xmsg, call.=FALSE)
	
           } else {
           
              nbox      <- c("stats","n","conf","out","group","names")     # correct list of names for boxplot data.
              # all should be present to be a valid boxplot structure.
              
              if (any(is.na(match(lnam,nbox)))) {
                 # at least one of the list names does not match or is missing.
                 ErrFnd      <- TRUE
                 warnCnt()
                 xmsg        <- paste0("***02B6 BOXPLOT ", pdColNum, " The ", boxnam," boxplot data does not contain all of the lists of boxplot function output. ","Invalid structure.")
                 warning(xmsg, call.=FALSE)
        
              } else {
                 
                 # check on the number of rows/etc.   - the $names list must be present after the last check.
          
                 boxlist$names <- toupper(boxlist$names)   # force to upper case for match with areaNamesAbbrsIDs table (name table).
                 goodn         <- !is.na(boxlist$names)
                 nNams         <- length(boxlist$names)    # get number of names in structure
                 
                 if (any(!goodn)) {
                    # one of the boxlist names is "NA" - no match
                    warnCnt()
                    xmsg       <- paste0("***02B7 BOXPLOT ", pdColNum, " In the ",boxnam, " boxplot data, the $name named list contains one or more missing values.")
                    warning(xmsg, call.=FALSE)
                 }
                 
                 # how to find and edit out entries not being used.  ---- boxlist2 <- boxlist[good,]                # get only the entires with names != NA
                 
                 listUNames    <- unique(boxlist$names)    # get list of unique area ids used 
                 nn            <- length(listUNames)
                 nn2           <- length(boxlist$names)

                 if (nn != nn2) {
                    warnCnt()
                    xmsg       <- paste0("***02B8 BOXPLOT ", pdColNum, "There are duplicate sets of boxplot data for the same sub-area. ","Only the first one will be used.")
                    warning(xmsg, call.=FALSE)
                 }
   
   
                 ##  how to edit out duplicates.  - search may be from ID to boxlist, if so only first found will be used.
   
   
                 bpNumRows     <- nn                       # number of unique rows of data.
                 
                 nr = dim(boxlist$stat)[1]                 # get number of data elements per area
                 if (nr != 5) {
                    ErrFnd    <- TRUE
                    warnCnt()
                    xmsg      <- paste0("***02BA BOXPLOT ", pdColNum, " The $stats matrix in the ", boxnam, " boxplot data does not have 5 values per area.")
                    warning(xmsg,call.=FALSE)
                 }
                    
                 nc            <- dim(boxlist$stat)[2]     # number of rows in boxplot stats data list (is this needed?).
                 if (nc != nNams) {
                    ErrFnd    <- TRUE
                    warnCnt()
                    xmsg      <- paste0("***02BB BOXPLOT ", pdColNum, " The $stats matrix in the ", boxnam, " boxplot data must have ", nNams, " elements.")
                    warning(xmsg, call.=FALSE)                       
                 }
                 
                 goods <- !is.na(colSums(boxlist$stat))    # good rows from a missing value view point.
                 
                 if (any(!goods)) {
                    # data contains missing values
                    #ErrFnd   <- TRUE  not used - not a stopping warning.
                    warnCnt()
                    xmsg      <- paste0("***02BC BOXPLOT ", pdColNum, " The $stat matrix in the ", boxnam, " boxplot data has missing values. ", "Sub-areas with missing values will not be drawn.")
                    warning(xmsg, call.=FALSE)
                 }
                 
                 tnn = is.na(match(listUNames,areaDatKey))   # match should be against the plotNames variable.
                 
                 if (any(tnn))  {  # test to see if any did NOT match
                    ErrFnd    <- TRUE
                    warnCnt()
                    lnn       <- paste0(nn[tnn],collapse=" ")
                    xmsg      <- paste0("***02BD BOXPLOT ", pdColNum, " The sub-area names/abbreviations found in the ", boxnam, " boxplot data $names values do not match the border group names: ",lnn)
                    warning(xmsg, call.=FALSE)
                 } # end of missing sub-areas.
              }  # end of look at boxplot lists.
           }  # end of number of boxplot lists check.
        }  # end of get boxplot named list names (null check)
      }  # end of boxplot list structure test.   
   }  # end of fetch of boxplot boxnam variable.
   
   if (ErrFnd) return ()
 
   #  End of basic validation for BoxPlot glyph
 
   refval    <- lRefVals[j]              # get referrence to object, changed 
   reftxt    <- lRefTexts[j]             # new - JP-2010/07/23

   #_______________Good Rows__________
   
   #cat("Boxplot - goodn:",length(goodn),"  goods:",length(goods),"\n")
     # if off, why is number of names and number of stats groups different?
     
   if (length(goodn) != length(goods)) {
      print("good vectors for boxplot - Problem")
      print(goodn)
      print(goods)
   }
   
   goodAll <- goodn | goods               # must have name match and no NAs in data.

   #_______________Scaling____________
   #
   #  normally 5/7 - USStatesBG
   #  Since same height, 
   
   
   # y boxplot scaling               # standard - horizontal box - no vertical 
                                     #     (y) dimensions
   py        <- c(-.5,-.5,.5,.5)
   
   thiny     <- BoxP.thin*py
   thicky    <- BoxP.thick*py 
   medy      <- BoxP.Median.Line*c(-.5,.5)
   
   #cat("point sets for -- thiny:",thiny,"  thicky:", thicky, "  medy:",medy,"\n")
     
   ry        <- c(0,1)                       # used in y scaling for grid lines
  
   #_______________Gather stats and put in Area Order______________
  
   # For the moment match on names
   #                     Boxlist = names, stats, out, group, 
   #
   # Boxplot function generates a list value containing:
   #     stats  = matrix - each column is lower, lower hinge, median, upper hinge, 
   #                  upper wicker for plot/group
   #     n      = vector of number of observ in each group
   #     conf   = a matrix which each col contins the low/upper extremes
   #     out    = valies of any data points which lie extremes of whiskers
   #     group  = vector (same length as out) whose elements indicate to which group
   #     names  = vector of names for the groups  (must be 2 char area names)
   #              There must be unique names that match the area abbreviation list.
   #
   
   stats   <- boxlist$stats         # statistics: 1-low,2-25%,3-median,4-75%,5-high 
                                    #   - 5 variables for each sub-area.
   #  indexes to boxplot values.   (pull values into thin and thick)  (set up for "boxes")
   thin    <- stats[c(1,5,5,1),]    # a column for each area - thin line - outliers (Lower, upper wickers)
                                    #   - columns in boxlist (1,5,5,1)
   thick   <- stats[c(2,4,4,2),]    # a column for each area - thick line - 25% to 75% (lower and upper hinge)
                                    #   - columns in boxlist(2,4,4,2)
   med     <- stats[3,]             # a single value for each sub-area (median data value)
  
   nam     <- boxlist$names         # area name list of boxplots
  
   # conf  <- boxlist$conf          # matrix of extremes - not used.  Don't check for.
     
   outlier <- rep(F,length(med))    # build vector of all outliers - set to False
                                    # outlier length = number of boxplots precented by user.
                                 
   if (length(boxlist$out)>0) {     # changed from is.null to length check (is.null does not work)
       # if outliers exist
       out      <- boxlist$out
       group    <- boxlist$group      # group and out are same length..
       outlier[unique(group)] <- T  # get list of groups with outliners and set indicater TRUE
       # set to True if we have an outlier to graph.
   }

   #  if group length = 0 error -- message.
  
   #### Need to put in order (boxlist may not be in the same order as the statsDFrame)
     
   zBPord  <- match(dat$RN, nam)  #  ord based on match between boxplot$names and original link names in statsDFrame (dat).  
                                  #    (Convert XX to index.  ord is the sequence of boxplot data to graph.
                                  #  zBPord is in the statsDFrame order and points to the supporting boxplot row.
                                  #    if NA, it means the statsDFrame row does not have a boxplot to go with it.
   IndexDattoBP <- zBPord         #  should be one boxplot entry per user data.frame entry.
   
   if (any(is.na(zBPord))) {
       ErrFnd    <- TRUE
       warnCnt()
       xmsg      <- paste0("***02BE BOXPLOT ",pdColNum," There are one or more of rows in the ",sDFName, " that does not have matching boxplot data, (", boxnam, ") entries.")
       warning(xmsg, call.=FALSE)
       wx        <- is.na(zBPord)
       xmsg      <- paste0("***02BF BOXPLOT ",pdColNum," The missing sub-areas are: ", paste0(areaDatAbbr[wx],collapse=", ") )
       warning(xmsg, call.=FALSE)
   }
   
   # what about missing values  -  if NA do not plot on that line
  
   # What about name type inconsistency  
   # I will require use of area name abbreviation
     
   # area ID codes be useful
   #    split() based on first two digits of county fips  
   #    I could stash area fips in statsDFrame sorted order

   # For Boxplot median sorting    
   #   Currently the user would need to sort the 
   #   medians in the statsDFrame making sure the row.names were correct.
   #
   #   JP-no data in col1, col2, or col3 to sort like the other columns... All of the data is in these structures.
   #   
   #   boxlist$stats[3,]   # the median.
   #
   #   at present no re-ordering of the boxplots like the other plots.
   #   JP-if other column is sorted, boxplots will follow that order via the indexes.
   #

   # ___________ scale x axis_______________

   lPad    <- TRUE
   rPad    <- TRUE
 
   if (is.null(out)) {
      rx     <- range(stats,na.rm=TRUE) 
   } else {
      # if no outliers - range only on stats
      rx     <- range(stats,out,na.rm=TRUE)           # if outliers - range on stats and outliers
   }
   #cat("boxplot-rx:",rx,"\n")
     
   #rx      <- sc*diff(rx)*c(-.5,.5)+mean(rx)        # min to max range with expansion factors.
 
   #cat("boxplot-rx after padding:",rx,"\n")
   
   # are these used.
   dx          <- diff(rx)/200                          # difference / 200 (??? use)
   px          <- c(-dx,-dx,dx,dx)                      # is this used???

   # ____________titles and labeling axes_______________

   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  Needs Padding on both sides (again none if one is zero.)
   #
   
   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)
 
   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
  
   #cat("BoxPlot-Result staggering:",staggering,"  staggered:",staggered,"\n")
    
   #####
   #
   #  Basic setup and validation for BoxPlot Glyph
   #
   #####

   # _______________drawing loop___________________

   oldpar = par(lend="butt")
   
   for (i in 1:numGrps){
   
      # Cycle through the Row/Groups in the micromap column
        
      gsubs    <- ib[i]:ie[i]    # get beginning to end row number in group  
      ke       <- length(gsubs)     # get number of rows in group  
        
      pen      <- if(i == medGrp & medGrpSize == 1) 7 else 1:ke  # if median single group, then pen=6, otherwise pen = c(1...x)   
         
      laby     <- ke:1            # laby = reverse order list for row index.         
     
      ksc      <- SetKsc(ke)
    
      panelSelect(panels,i,j)   # select panel for group i in column j)
      panelScale(rx,c(1-pad,ke+pad))   # set scale for panel	  (0.33333 to 1.666666667)
              # should work, box plot is set up on 1 as base and + or - 0.5 from the base.
     
      panelFill(col=Panel.Fill.col)           # set fill for panel
     
      #  Grid lines
      axis(side=1, tck=1, labels=F, at=atRx, 
                 col=Grid.Line.col, lwd=Grid.Line.lwd) # grid lines
     
      # if a refval is provided and in the rx range, then add line.
      
      AddRefLine(refval, ke, rx)
      
      panelOutline(col=Panel.Outline.col)     # outline panel

      for (k in 1:ke){
         # cycle through row-groups and build each box plot
         
         m      <- zBPord[gsubs[k]]   # m is the location of the area in panelData item list (a boxplot element)
         
         if (is.na(m)) next           #   if no boxplot data - skip box plot drawing for sub-area
            
         if (goodAll[m]) {
         
            #cat("Grp:",i,"  k;",k," m:",m,"\n")
            
            kp     <- pen[k]          # color number
            ht     <- laby[k]         # vector of the index into the panel (for a 5/6 row group ->  6,5,4,3,2,1 (top to bottom)
                                      # for the median group/row ->  1  (that's it.)   1.65 box is set to [0:2]   7 box is set to [0:6]
           
            #  plot outlier points on graph
           
            if (outlier[m])  {         # flag indicator - saves scaning.
            
               #   plot points for outliers (rings)
               vals  <- out[group == m]  # get the list of values.
               if (colFull) {          # full color do the correct color
                  points(vals, rep(ht,length(vals)), pch=1,
                      col=ifelse(BoxP.Use.Black,"black",mstColors[kp]),
                      cex=BoxP.Outlier.cex, lwd=BoxP.Outlier.lwd)
               } else {
                  # Greys - do the a grey.
                  points(vals, rep(ht,length(vals)), pch=1,
                      col=BoxP.Outlier.BW.col,
                      cex=BoxP.Outlier.cex, lwd=BoxP.Outlier.lwd)
               }
            }  
 
            # Draw thin line for lower to upper confidence values - box (ht high).
         
            wthiny   <- thiny * ksc
            polygon(thin[,m], rep(ht,4)+ wthiny, col=mstColors[kp], border=NA)
                  
            # Draw middle think box  (25% to 75%)
            wthicky  <- thicky * ksc
            polygon(thick[,m], rep(ht,4)+ wthicky, col=mstColors[kp], border=NA)

            # Median Bar - Lines looked crooked  (Median verical bar)
            segments(med[m], ht+medy[1], med[m], ht+medy[2],         # use segment line.
                  col=BoxP.Median.col, lwd=BoxP.Median.Dot.lwd)
         }
      }    # end k loop   
   }  # end i loop
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

}

#
#  End of Box Plot Glyph
#
#####


#####
#
# type = 'ctrbar'  ====================================
#
#  rlAreaCtrBar   (Centered Bar chart)
#
#  The centered bars is a stacked bar chart with the middle segment centered on the "0" value
#  of the chart and extending 1/2 it's value in both directions (plus and minus).
#  The other bar segments are plotted to it's left and right as appropriate.
#
#  The data structure can have between 2 to 9 data values per area.
#  Each area must have the same number of values. This limitation may be removed in the future.
#
#  panelData => data.frame where each row is a area with the areaDatKey as the row.name.
#     The columns are the bar segment values.

#
rlAreaCtrBar = function(j) {
   #  j = the panel column number
   #  
   #   col1 and col2 indentify the starting column and ending column number in the statsDFrame
   #   that contains the bar values for each area.
   #
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
   # "col1"
   stColName1  <- wstname[col1[j]]
   pdUMsg      <- "(First data column)"
   
   xr          <- CheckPDColnCN('col1','CTRBAR', col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd = TRUE 
   #} else { 
   #  xdat1 <- xr$Dat      # with CheckPDColnCN, no xr$Dat is returned   
   }
   
   # "col2"
   stColName2  <- wstname[col2[j]]
   pdUMsg      <- "(Last data column)"
   
   xr          <- CheckPDColnCN('col2','CTRBAR', col2[j], stColName2, j, 2, wstMax, dat, pdUmsg)
   if (xr$Err) { 
      ErrFnd = TRUE 
   #} else { 
   #   xdat2 <- xr$Dat   # why
   }
   
   if (!ErrFnd) {
    
      if (col1[j] >= col2[j]) {
         ErrFnd    <- TRUE
         warnCnt()
         xmsg      <- paste0("***020A CTRBAR ", pdColNum, " The first column name/number (", stColName1, ") ","must proceed the last column name/number (", stColName2, ") in the ", sDFName," data frame.")
         warning(xmsg, call.=FALSE)
          
      } else {
   
         wD        <- ( col2[j] - col1[j] + 1 )  # corrected to properly calculate the number of data columns.

         if ( wD < 2 || wD > 9 ) {
            ErrFnd    <-  TRUE
            warnCnt()
            xmsg      <- paste0("***020B CTRBAR ", pdColNum, " The number of segments is ", wD, ". It must be between 2 and 9.  If over 9, only the first 9 will be used.")
            warning(xmsg, call.=FALSE)
         }
      }
   }

   if (ErrFnd) return ()  # if either column is missing or invalid - skip this column.

   #  Now check the data in the statsDFrame columns..

   stColNums   <- c(col1[j]:col2[j])
   workCB      <- dat[,stColNums]        # get bar segment data from the statsDFrame.
   colNums     <- c(1:dim(workCB)[2])
   
   for (ind in colNums)  {             # check and convert each column
      iC          <- stColNums[ind]         #    get stDF column number
       
      stColNam    <- wstname[iC]            #    get stDF column name
      F_ind       <- formatC(ind,format="f",digits=0,width=1)
      segNam      <- paste0("seg",F_ind)
      pdUmsg      <- paste0("(Bar segment ",F_ind," length)")
   
      x           <- CheckNum(workCB[,ind],'CTRBAR', ind, pdColNum, segNam, stColNam, pdUmsg)
   
      if (x$Err) { 
         ErrFnd       <- TRUE 
      } else { 
         workCB[,ind] <- x$Dat
      }
   }  # ind for loop

   #print("end of verification in CTRBAR - length of good")
   
   good      <- !is.na(rowSums(workCB))  # good values (one per row)
   
   #print(length(good))

   #
  
   refval    <- lRefVals[j]              # get referrence to object, changed 
   reftxt    <- lRefTexts[j]             # new - JP-2010/07/23
   
   #
   # mstColors - series of lighter colors of the base colors for each bar.
   #   Use an adjusted list of percentages based on the Number of Segments.
   #      2 step = 50, 100
   #      3 step = 33.3, 66.6, 100
   #      4 step = 25, 50, 75, 100
   #      5 step = 20, 40, 60, 80, 100
   #      6 step = 16.6, 33.3, 50, 66,6, 83.3, 100
   #    etc.
   #    1/(NumSegs)*step = transparency or lightness level  (100% full)
   
   #   Dan's addition ==> 
   #    as the colors are generated from the base color
   #
   #    pInc = 1 / NumSegs
   #
   #    cSteps = cumsum(rep(pInc,NumSegs))^1.35
   #
   #    thickness = constant  vs.  very based on 2 to 9th segment
   #
 
   #_______________Gather stats and put in area Order______________
  
   #
   #  Sorting has already been done - by areaDatKey or value.
   #  The areaID list has therefore been re-ordered accordingly.  
   #  Reorder the DataList to match.  The assumption was that the input data order for the panelData 
   #  matched the order of the original data in the statsDFrame.
   #
    
   workMatCB   <- as.matrix(workCB)

   CBLen       <- apply(workMatCB,1,length)  # get length of each row.
   CBLRange    <- range(CBLen,na.rm=TRUE)
 
   NumSegs     <- CBLRange[2]                # number of segments

   CBBarPt     <- cbind(rep(0,numRows),workMatCB)
   CBBarPt     <- t(apply(CBBarPt,1,cumsum))
   
   # _____________ Color Patterns _______________
    
   #  Inputs, NSegments, mstColors[1:7]       Output baseColRgb
    
   baseColRgb  <- BuildSegColors(NumSegs)
    
   #_____________Centering_____________
   
   CtrSeg      <- as.integer(NumSegs/2) + 1  # center segment

   if ((NumSegs %% 2) != 0) {  
      # old number of segments
      CtrPt    <- workMatCB[,CtrSeg]/2 + CBBarPt[,CtrSeg]
   } else {
      # even number of segments
      CtrPt    <- CBBarPt[,CtrSeg]
   }
 
   CBPlotPts   <- CBBarPt - CtrPt
 
   #_______________Scaling____________
   
   # x scaling
   lPad        <- TRUE
   rPad        <- TRUE
   
   rx          <- range(CBPlotPts,na.rm=TRUE)
   #cat("ctrbar-rx:",rx,"\n")
   
   #rx         <- sc*diff(rx)*c(-.5,.5)+mean(rx)
   #cat("ctrbar-rx after padding:",rx,"\n")
   
   ry          <- c(0,1)
 
   pyPat       <- c(-.5,-.5,.5,.5,NA) 
   py          <-  CSNBar.barht * pyPat            #  CBar.barht = 2/3 (0.6667) (fixed)
       # py    <- c( -1/3, -1/3, +1/3, +1/3, NA)
   
   # variable bar height calculations
   wYPdelta    <- (CSNBar.Last.barht - CSNBar.First.barht)/(NumSegs-1)  # increment
   wYP1        <- CSNBar.First.barht - wYPdelta
      
   # ____________titles and labeling axes_______________
   
   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  General padding on left or right if not zero.
   #

   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
   
   #cat("ctrbar-Result staggering:",staggering,"  staggered:",staggered,"\n")
   #
   
   #
   #  End of Basic Validation and Setup for CtrBar segmented glyph
   #
   #####
 
   # ___________________drawing loop_____________________

   oldpar     <- par(lend="butt")
 
   #  build each panel for each stacked bar set.
   
   for (i in 1:numGrps) {
      gsubs     <- ib[i]:ie[i]               # get beginning to end index row number in this group  
      ke        <- length(gsubs)                # get number of rows in group  (5 or 1)  
      # adjust if median group      
        
      pen       <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke        # if median group (7)(black), then pen=6, otherwise pen = c(1...x)   
      laby      <- ke:1 
        
      ksc       <- SetKsc(ke)
   	
      panelSelect(panels,i,j)
      x         <- panelScale(rx,c(1-pad,ke+pad)) #   1 to 5/6 are the y values for each bar.
      panelFill(col=Panel.Fill.col)
 
      axis(side=1, tck=1, labels=F, at=atRx,
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grid
        
      # if a refval is provided and in the rx range, then add line.

      AddRefLine(refval, ke, rx)
       
      #
      #  Process each area's line. 
      #
      for (k in 1:ke) {
         # cycle through row-groups and assign colors to associated areas dots.
         m        <- gsubs[k]
           
         if (good[m]) {
            wX    <- CBPlotPts[m,]            # Get Row of data.
     
            #wYP  <- rep(laby[k],5)+py
            wYP   <- rep(laby[k],5)
      
            # calculate box for each segment
            wYPht <- wYP1
      
            for (ik in 1:NumSegs) {
               #  Y values for segment box
               if (CBar.varht) {
                  # variable height bar segments
                     
                  wYPht    <- wYPht + wYPdelta
                  wYP2     <- wYP + ((pyPat * wYPht) * ksc)
               } else {
                  # fixed height bar segments
                  wYP2     <- wYP + ( py * ksc )
               }
               #  X values for segment box
               val0        <- wX[ik]     # start
               val1        <- wX[ik+1]   # end position
               wXP         <- c(val0,val1,val1,val0,NA)
                 
               # good value - draw bars are polygons.  (why to polygon)
               polygon(wXP, wYP2, col=baseColRgb[pen[k],ik], 
                                  lwd=CSNBar.Outline.lwd, border=CSNBar.Outline.col,
                                  lty=CSNBar.Outline.lty) 

               #polygon(wXP, wYP2, col=CSNBar.Outline.col, density=0)
            }
         }
      }   # end of k loop   
      # finish up panel
       
      # draw vertical line at zero.
      lines(rep(0,2), c(1-padMinus,ke+padMinus),
               lty=CBar.Zero.Line.lty, lwd=CBar.Zero.Line.lwd, 
               col=CBar.Zero.Line.col)
       
      panelOutline(Panel.Outline.col)
 
   }  # end of i loop
  
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

}

#
#  End of ctrbar Glyph
#
#####


#  NA check, if any NA exist in the series from col1 to col2, then the stacked bar will not
#  be drawn.  
#


#####
#
# type = 'dot'  and 'dotsignif'  =====================================================
#
# rlAreaDot     
#
#    glyph will test for significants and if not overlay dot with "x" is dSignif=TRUE is 
#    set in the glyphs call.
#

rlAreaDot = function(j,dSignif=FALSE){

   #
   # j = current panel column number
   #
  
   #
   # Single Dot, no extra line or interval
   #  
   #  col1[j] points to the statsDFrame column holding the first arrow end point.value
   #
  
   #   OR 
  
   #
   # Single Dot with signficants over print, no extra line or interval
   #
   #  col1[j] points to Dot value in the statsDFrame column holding the Dot Value
   #  col2[j] points to P value   - if P value > 0.05 then overprint "x" on the dot.
   #
   #
  
   #
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
   
   dotMsgHdr   <- "DOT"
   if (dSignif) dotMsgHdr <- "DOTSIG" 
  
   good1       <- TRUE      # vector length of xdat1.  TRUE = not NA,  FALSE = NA.
   good2       <- TRUE
   goodrow     <- TRUE
   
   pdColNum    <- formatC(j,format="f",width=2,digits=0,flag="0")
  
   # "col1"
   stColNum1   <- col1[j]
   stColName1  <- wstname[stColNum1]
   pdVarName1  <- "col1"
   
   pdUmsg      <- "(Dot value)"
   xr          <- CheckPDCol(pdVarName1, dotMsgHdr, stColNum1, stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd    <- TRUE 
   } else { 
      xdat1     <- xr$Dat    # get column of data  (xr$Dat returned by CheckPDCol)
      good1     <- !is.na(xdat1)
      
   }
     
   if (dSignif) {
      # "col2"
      stColNum2  <- col2[j]
      stColName2 <- wSFName[stColNum2]
      pdVarName2 <- 'col2'
   
      pdUmsg     <- "(Confidence P-Values)"
      xr         <- CheckPDCol(pdVarName2, dotMsgHdr, stColNum2, stColName2, j, 2, wstMax, dat, pdUmsg)
      
      if (xr$Err) { 
         ErrFnd    <- TRUE 
      } else { 
         xdat2     <- xr$Dat    # get column of data.
         good2     <- !is.na(xdat2)
         
         # some may be missing, but we have some, check range.
         
         if (any(xdat2[good2] > Dot.Signif.range[2] | xdat2[good2] < Dot.Signif.range[1] )) {
            # some values are out of range
            # ErrFnd  <- TRUE   # allow missing values in data column, send warning but do not stop plotting glyph.
            warnCnt()
            xmsg      <- paste0("***022Q", dotMsgHdr, " ", pdColNum, " One or more P_value data entries in the ", stColName2, " for the panelDesc ", pdVarName2 ," variable are out of range." )
            warning(xmsg, call.=FALSE)
         }
         
      }
   }
    
   #
   #  Change 7/24/15 - allow missing values in a column for a row.
   #  Change 7/24/15 - if not signif, copy good1 to good2
   #  Change 7/24/15 - plot row, only if both data columns are not NA.
   #
   
   if (!dSignif) {
      # dot function
      goodrow  <- good1
   } else {
      # dotsignif function
      goodrow  <- good1 & good2
   }
  
   if (ErrFnd)  return ()    # error/warning found and can't plot glyph - return

   #  JB - add "as.double(as.vector(" to handle variation in how objects are converted.

   #____________ref values_____________    
   
   refval      <- lRefVals[j]     # get reference value for this column, changed 
   reftxt      <- lRefTexts[j]    # new - JP-2010/07/23
   
   xps         <= par("ps")
   #cat("dot-par(ps):",xps,"\n")

   #_____________y axis________________
   ry          <- c(0,1)

   #____________scale x axis______________________
   lPad        <- TRUE
   rPad        <- TRUE
   
   rx          <- range(xdat1,na.rm=TRUE)
   #cat("dot-rx:",rx,"  cxy:",par("cxy"),"\n")
   
   #cxyAdj     <- par("cxy")/2
   #rx         <- sc*diff(rx)*c(-.5,.5)+mean(rx) # + c(-cxyAdj,cxyAdj)
                             # range = mean(rx)/2 * c(-1,1) * 1.08
                             
   #cat("dot-rx after padding:",rx,"\n")
   
   # ____________labeling axis_______________
  
   #####
   #
   #  Setup and draw top and bottom titles and axis for dot and dotsignif glyph column
   #
   #  Padding for the dot, regardless if zero is left or right.
   #

   Res         <- DrawXAxisAndTitles(j, panels, rx, ry, reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
  
   xps         <= par("ps")
   #cat("dot-par(ps)2:",xps,"\n")
  
   #
   #  Basic validation and setup done for dot and dotsignif glyph
   #
   #####
  
   #####
   #
   # _______________drawing loop___________________
   #
   for (i in 1:numGrps){
      gsubs <- ib[i]:ie[i]
      ke    <- length(gsubs)
     
      pen   <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke
     
      laby  <- ke:1 
     
      panelSelect(panels,i,j)
      x <- panelScale(rx,c(1-pad,ke+pad))
    
      panelFill(col=Panel.Fill.col)
     
      # grid lines    
      axis(side=1, tck=1,labels=F, at=atRx, 
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grid   # updated 7/24/15 to include at=
     
      # if a refval is provided and in the rx range, then add line.

      AddRefLine(refval, ke, rx)
     
      panelOutline(Panel.Outline.col) 
     
      for (k in 1:ke) {
         # step through values for this panel
         m   <- gsubs[k]
         if (goodrow[m]) {   # change 7/24/15 - goodrow reflect both columns of data.
                             #   can't plot dot, if signif data if missing.
            # data good for dot - plot dot.
            plotPoint(xdat1[m], laby[k], 
                      Dot.pch, mstColors[pen[k]], Dot.pch.size, Dot.pch.lwd,
                      Dot.Outline, 
                      Dot.Outline.col,Dot.Outline.lwd)
            
            if (dSignif) {   
               if (xdat2[m] > Dot.Signif.pvalue) {
            
                  dsCol <- Dot.Signif.pch.col
                  # if color is NA, then follow color for the row.
                  if (is.na(dsCol)) {  dsCol <- mstColors[pen[k]] }
                  
                  plotPoint(xdat1[m], laby[k], 
                            Dot.Signif.pch, dsCol, Dot.Signif.pch.size, Dot.Signif.pch.lwd,
                            Dot.Signif.Outline, 
                            Dot.Signif.Outline.col, Dot.Signif.Outline.lwd)
               }
            }
            # how to link an overprinting with criteria..        
         }
        
      }  # end of k loop
   }  # end of i loop

   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

}

#
#  End of dot and dotsignif glyphs
#
#####

#####
#
#  type = 'dotconf' ====================================================
#
#  flAreaDotConf
#

rlAreaDotConf <-  function(j){
   #
   #  j is the current panel column index
   #
   #   col1 indicates the column number for the dot value in the statsDFrame.
   #   col2 indicates the column number for the lower confidence value in the statsDFrame.
   #   col3 indicates the column number for the upper confidence value in the statsDFrame.
   
   #cat("\nDotConf:","\n")
   
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
   # "col1"
   stColNum1   <- col1[j]
   stColName1  <- wstname[stColNum1]
   pdVarName1  <- 'col1'
   
   pdUmsg      <- "(Dot value)"
   xr          <- CheckPDCol(pdVarName1, 'DOTCONF', stColNum1, stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd   <- TRUE 
   } else { 
      xmn      <- xr$Dat    # get column of data  (x$Dat returned by CheckPDCol)
      good1    <- !is.na(xmn)
   }
 
   # "col2"
   stColNum2   <- col2[j]
   stColName2  <- wstname[stColNum2]
   pdVarName2  <- 'col2'
   
   pdUmsg      <- "(Lower Confidence Value)"
   xr          <- CheckPDCol(pdVarName2, 'DOTCONF', stColNum2, stColName2, j, 2, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd   <- TRUE 
   } else { 
      lower    <- xr$Dat    # get column of data  (x$Dat returned by CheckPDCol)
      good2l   <- !is.na(lower)
   }
 
   # "col3"
   stColNum3   <- col3[j]
   stColName3  <- wstname[stColNum3]
   pdVarName3  <- 'col3'
   
   pdUmsg      <- "(Upper Confidence Value)"
   xr          <- CheckPDCol(pdVarName3, 'DOTCONF', stColNum3, stColName3, j, 3, wstMax, dat, pdUmsg)
   
   if (xr$Err) { 
      ErrFnd   <- TRUE 
   } else { 
      upper    <- xr$Dat    # get column of data  (x$Dat returned by CheckPDCol)
      good2u   <- !is.na(upper)
   }
  
   if (ErrFnd) return ()        # error warning found - return
 
   #cat("dotconf: data OK - plot","\n")
 
   # setup column good arrays
 
   #  xmn      <-  dat[,col1[j]]            # Col 1 = DOT - median/mean
   #  lower    <-  dat[,col2[j]]            # Col 2 = lower
   #  upper    <-  dat[,col3[j]]            # Col 3 = upper
 
   good2       <- !is.na(upper+lower)
   goodrow     <- good1 & good2l & good2u    # sum of all checks.

   refval      <- lRefVals[j]           # changed to lRefVals, JP-2010/07/23
   reftxt      <- lRefTexts[j]          # new - JP-2010/07/23

   #  Select the first panel in column to allow code to reference its characteristics
   
   panelSelect(panels, 1, j)
   #x          <- panelScale(rx, ry)
   #par(xpd=T)

   #_____________ y axis ____________________

   ry          <- c(0,1)

   #_____________scale x axis________________
   lPad        <- TRUE
   rPad        <- TRUE
  
   rx          <- range(upper,lower,xmn,na.rm=TRUE)
   #cat("dotConf-rx:",rx,"\n")
   
   #
   #  NOW DONE in DrawXAxisAndTitle
   #
   #  dealing with a dot, so padding should be 1/2 width of dot in rx units.
   #wP         <- par("pin")[1]  # width of panel
   #wD         <- strwidth(" ",cex=Dot.Conf.pch.size)/2        # get 1/2 of character width 
   #rwD        <- (wD/wP) * diff(rx)  # dot width as percentage of panel width  "times"   number of x units to graph
   
   #rx         <- rx + c(-rwD,rwD)    # make room for dot and no more.
   
   #cat("dotconf - dot adjust - widthPanel:",wP,"  widthSp:",wD,"   diff(rx):",diff(rx),"   rwD:",rwD,"\n")
   # The above is not done in DrawXAxis...
                         #  x may not be needed???
   #rx_old     <- sc*diff(rx)*c(-.5,.5)+mean(rx)
   #cat("dotConf-rx after padding:",rx,"  old way:",rx_old,"\n")
   
   # ____________labeling axes_______________
  
   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  padding on left and right for confidence and dot.
   #
   #cat("DotConf-calling DrawXAxisAndTitles","\n")
   
   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
   
   #cat("DotConf-back from DrawXAxisAndTitles","\n")
  
   #cat("dotconf-Result staggered:",staggered,"\n")
  
   #
   #  Basic setup and validation done for dotconf glyph 
   #
   #####
   
   #cat("Dot.Conf.pch:",Dot.Conf.pch,"  Dot.Conf.pch.size:",Dot.Conf.pch.size,
   #    " Dot.Conf.Outline:",Dot.Conf.Outline,"\n")
   
   doDotOutline   <- Dot.Conf.Outline
   
   #cat("doDotOutline:",doDotOutline,"\n")
   
   #cat("Dot.Conf.Outline.lwd:",Dot.Conf.Outline.lwd," .col:",Dot.Conf.Outline.col,"\n")
   
   
   #cat("dotconf - drawing loop  col:",j,"\n")
   
   #_____________drawing loop___________________
  
   for (i in 1:numGrps){
      gsubs   <- ib[i]:ie[i]
      ke      <- length(gsubs)
  
      pen     <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke
  
      laby    <- ke:1
  
      panelSelect(panels,i,j)   
      panelScale(rx,c(1-pad,ke+pad))   # Adjusted scale for interior
  
      panelFill(col=Panel.Fill.col)
  
      axis(side=1, tck=1, labels=F, at=atRx,             # change 7/24/15 - add at= to get grids at the same points as the ticks
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # vertical grid lines
     
      # if a refval is provided and in the rx range, then add line.
 
      AddRefLine(refval, ke, rx)
      
      panelOutline(col=Panel.Outline.col)     # outline scaled image.
      for (k in 1:ke){ 
         m    <- gsubs[k]
        
         if (goodrow[m]) { # if valid upper value.    # 7/25/15 changed to goodrow and covered all plotting.
        
            # draw confidence line.
            lines(c(lower[m],upper[m]), rep(laby[k],2),
                   col=mstColors[pen[k]], lwd=Dot.Conf.lwd)
            
            # plot dot.
            #cat("m:",m,"  lower:",lower[m],"  upper[m]:",upper[m],
            #    "  k:",k,"  laby[k]:",laby[k],"  pen[k]:",pen[k],"\n")
            #cat("Dot.Conf.lwd:",Dot.Conf.lwd,"\n")
            #
            #cat("xmn[m]:",xmn[m],"\n")
            
            #
            # doDotOutline - mostly related to black and white printing.  
            # However, users can also request it.
            #
            # 0:25 pch's are at 75% of cex.
            #  0:18  S compatible, vector symbols - uses lwd(lines), col(borders & fill)   
            #     1, 10, 13, 16 are circles.
            #     15:18 filled characters have no borders.
            #     0:14  line drawings
            #     15:18 fills, but no lines (lwd not used, but col is the fill, not bg)
            #  19:25 R vector symbols - uses lwd(lines-borders), col(border), bg(fill)
            #  26:31 not used
            #  32:127  Ascii Char
            #  128:255 local characters.
            #
            # The issue not is these points are written for 19:25 not the other.
            #    if 19:25  then bg = fill color, col = border color, lwd = weight of border, 
            #
            
            pchValue   <- Dot.Conf.pch
            pchOutline <- Dot.Conf.Outline  # enable outline of 19:25 characters.
            
            plotPoint(xmn[m], laby[k], 
                      Dot.Conf.pch, mstColors[pen[k]], Dot.Conf.pch.size, Dot.Conf.pch.lwd, 
                      Dot.Conf.Outline, 
                      Dot.Conf.Outline.col, Dot.Outline.lwd
                     )
 
         }
      }  # end of k loop   

   }  # end of i loop

   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)
   
   #cat("DotConf: END.\n")
  
}

#
#  End of dotconf glyph
#
#####


#####
#
# type = 'dotse' =======================================================
#
# rlAreaDotSe
#

rlAreaDotSe = function(j){
   #   j = current panel column
   #
   #   col1 indicates the column number for the dot value in the stamicroteFrame.
   #   col2 indicates the column number for the SE value in the statsDFrame.
  
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
   # "col1"
   stColName1  <- wstname[col1[j]]
   pdUmsg      <- "(Dot Value)"
   xr          <- CheckPDCol('col1', 'DOTSE', col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { ErrFnd = TRUE } else { xdat1 <- xr$Dat }
   
   # "col2"
   stColName2  <- wstname[col2[j]]
   pdUmsg      <- "{Standard Error Value)"
   xr          <- CheckPDCol('col2', 'DOTSE', col2[j], stColName2, j, 2, wstMax,  dat, pdUmsg)
   
   if (xr$Err) { ErrFnd = TRUE } else { xdat2 <- xr$Dat }
   
   if (ErrFnd) return ()   # error warning found - return
  
   good1       <- !is.na(xdat1)
  
   good2       <- !is.na(xdat2)
  
   goodrow     <- good1 & good2   # get sum of the checks - both must be their to plot dot and Se.
  
   zval        <- stats::qnorm(.5+Dot.SE/200)
   inc         <- zval * xdat2
   upper       <- xdat1 + inc
   lower       <- xdat1 - inc
 
   if (ErrFnd) return ()   # error warning found - return
  
   #______________Ref data______________
  
   refval      <- lRefVals[j]          # changed to lRefVals, JP-2010/07/23
   reftxt      <- lRefTexts[j]         # new - JP-2010/07/23

   #______________y range_______________
   ry          <- c(0,1)

   #_______________scale x axis__________________
   lPad        <- TRUE
   rPad        <- TRUE
   rx          <- range(upper,lower,xdat1,na.rm=TRUE)  # use upper, lower and xdat1 to find "range" of x
              # x may not be needed at all. But best to leave.
   #cat("dotSE-rx:",rx,"\n")
   
   #rx         <- sc * diff(rx) * c(-.5,.5) + mean(rx)
   #cat("dotSE-rx after padding:",rx,"\n")
   
   # ____________labeling axes_______________

   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  Padding on left and right for dot and confidence
   #
 
   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
    
   #cat("dotSE-Result staggering:",staggering,"  staggered:",staggered,"\n")
   
   #
   #  Setup and validation for dotse glyph completed.
   #
   #####
  
   #__________________drawing loop________________

   for (i in 1:numGrps) {
  
      gsubs <- ib[i]:ie[i]
      ke    <- length(gsubs)

      pen   <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke

      laby  <- ke:1 

      panelSelect(panels,i,j)
      x <- panelScale(rx,c(1-pad,ke+pad))

      panelFill(col=Panel.Fill.col)

      axis(side=1, tck=1, labels=F, at=atRx,
               col=Grid.Line.col, lwd=Grid.Line.lwd) # grid lines - 7/24/15 add at=atRx to force Grid line to match ticks.
     
      # if a refval is provided and in the rx range, then add line.

      AddRefLine(refval, ke, rx)
     
      panelOutline(Panel.Outline.col)
   
      for (k in 1:ke){
         m  <- gsubs[k]
         
         #   change 7/24/15 - only plot glyph if both data column are not NA.
         if (goodrow[m]) { # if all values are good
            # confidence interval based on SE - line .
            lines(c(lower[m],upper[m]), rep(laby[k], 2),
                    col=mstColors[pen[k]],lwd=Dot.SE.lwd)
            
            plotPoint(xdat1[m], laby[k], 
                      Dot.SE.pch, mstColors[pen[k]], Dot.SE.pch.size, Dot.SE.pch.lwd,
                      Dot.SE.Outline,
                      Dot.SE.Outline.col, Dot.SE.Outline.lwd
                     )
            
         }  
      }   
   }

   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

}

#
#  End of dotse glyph
#
#####

#####
#
# type = 'id' =======================================================
#
# rlAreaID
#

rlAreaID = function(j){

   #  j = panel column number

   #_____________ Scaling ______________________ 
   # get corners for the boxes.
 
   rx     <- c(0,diff(panels$coltabs[j+1,])) # column width in inches  - index to coltabs is +1 the column number
   ry     <- c(0,1)    # not inches, but 0-1

   #______________________panel labels_____________

   panelSelect(panels,1,j)      # start at I = 1, but j= is the current column.
 
   x <- panelScale(rx,ry)
  
   #
   #  ID text set based on Text.cex.. for 12 point text  in a 3/4 to over 1" height boxes.
   #
   
   xusr  <- par("usr")  # base decision on first panel - they should all be the same.
   xpin  <- par("pin")
   
   IDcex.mod <- Id.Cex.mod     # get multiplier based on 12 pt.
   pchSize   <- Id.Text.cex * IDcex.mod * Id.Dot.cexm
 
   if (xpin[2] < 0.75) {
      # panel height is getting smaller.  reduce text and symbol size.
      IDcex.mod <- (1 - (( 1 - xpin[2]/0.75 ) ^ 2 ))    # get ratio.
      #cat(" IDcex.mod change from 1 to :",IDcex.mod,"\n")
   }
   
   ### request to lower title into axis label space.
  
   xLab1 <- banner["id","H2"]
   xLab2 <- banner["id","H3"]
   
   if (xLab2 == "") {
      xLab2 <- xLab1
      xLab1 <- ""
   }
  
   # column titles
   if (xLab1 != "") {
      mtext(xLab1,side=3,line=Id.Title.1.pos,cex=Text.cex)
   }
   mtext(xLab2,side=3,line=Id.Title.2.pos,cex=Text.cex)
   
   widthPanel    <- xpin[1]   # inches

   widthxLab2    <- strwidth(xLab2,units="inch",cex=Text.cex)
   
   #  one label for ID column.  It's centered, so use 1/2 of the width. 
   lastLab2Space <<- ( widthPanel + colSepGap - widthxLab2 ) / 2  # pos - space (have), neg - overhang (need).
   
   #cat("ID - widthPanel:",widthPanel,"  width xLab2:",widthxLab2,"  lastLab2Space:",lastLab2Space," staggered:",staggered,"\n")
   
   
   # ______Bottom Label/Title - Lab3 ______

   lastLab3Space <<- ( widthPanel + colSepGap ) / 2     
  
   if (lab3[j] != "") {
      panelSelect(panels,numGrps,j)
      x <- panelScale(rx,ry)
  
      # bottom of column footnote (title)
      mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)                      # bottom labels.
  
      widthxLab3    <- strwidth(lab3[j],units="inch", cex=Text.cex)
      lastLab3Space <<- ( widthPanel + colSepGap - widthxLab3  ) / 2
   }

   #_____________________Square Sizing  and Symbol Placement

   #  square width
 
   # xstart =  Id.Start    # inches from left margins

   #### idstart = 0.137    # inches from base line  (not relative)  (appears to be replaced below..)

   TextH2  <- max(strheight(areaDatIDNames,units="inch",cex=(Id.Text.cex * IDcex.mod) )) / 2  # maximum length value /2

   par(pch = Id.Dot.pch)   # set up the character.
  
   
   #______________________main loop________________

   # Cycle thought the GROUPS (numGrps)
   for (i in 1:numGrps){

      npad  <- ifelse((i == medGrp & medGrpSize == 1),0.57,pad)  # single row = 0.57, or pad list for multiple rows.
       
      gsubs <- ib[i]:ie[i]           # first element of group to last element of group.
      ke    <- length(gsubs)         # number of elements. (rows per group)

      # since each panel may have different number of rows, this now must be done for each group.
     
      ryusr <- c(1-npad,ke+npad)     # set scale for the number of rows in group, plus padding.
                                     # y axis value = 1 to nRows..
      
      laby  <- ke:1                  # y index vector - like 5:1 for 5 areas per panel/row.
                                     # ke is the number of area per panel/row.

      pen   <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke

      panelSelect(panels,i,j)        # select and setup panel for this group of rows.
      x             <- panelScale(rx,ryusr)

      gnams         <- areaDatIDNames[gsubs]
  
      xusr          <- par("usr")
      xpin          <- par("pin")
      xUnitsPerInch <- diff(xusr[1:2]) / xpin[1]    # x units per inch 
      yUnitsPerInch <- diff(xusr[3:4]) / xpin[2]    # y units per inch 
      
      #cat("xUPI:",xUnitsPerInch,"  usr:",xusr,"  xpin:",xpin,"  TextH2:",TextH2,"\n")
   
      xHalfSym      <- ((Id.Dot.width * Id.Cex.mod) + Id.Space)/2  * xUnitsPerInch
      xStartu       <- xHalfSym                                # ID offset in units.   (a little more than 1/2 width of symbole
      xSymWu        <- xHalfSym - 0.25*Id.Space       # ID symbol now in units.
      
      #cat("xStartu:",xStartu,"  xHalfSym:",xHalfSym,"\n")
      
      xPosu         <- rep(xStartu,ke)
      xPos2u        <- xPosu + xSymWu
  
      yPosu         <- laby
      yPos2u        <- laby - TextH2 * 0.3 * yUnitsPerInch        # offset down by half the height  
   
       
      #cat("xPosu:",xPosu,"  xPos2u:",xPos2u,"\n")
      #cat("yPosu:",yPosu,"  yPos2u:",yPos2u,"\n")
      
      #cat("Id.Text.cex:",Id.Text.cex,"  IDcex.mod:",IDcex.mod,"  prod:",(Id.Text.cex * IDcex.mod),"\n")
      
      text(xPos2u, yPos2u, gnams,  cex=(Id.Text.cex * IDcex.mod ), xpd=T, pos=4)
      
          
      #  Note: the xPosu and yPosu coordinates is the center of the point not the starting edge of a character.
     
      plotPoint(xPosu, yPosu,
                Id.Dot.pch, mstColors[pen], Id.Dot.cexm, "black",
                TRUE, "black", Id.Dot.lwd
               )
     
        
   }

   # No reference values for this type of column
   # as we exit loop, we are in the last panel..
  
   xpin          <- par("pin")
   lastLab3Space <<- xpin[1]/2
  
   if (lab3[j] != "") {
      #panelSelect(panels,numGrps,j)
      #x <- panelScale(rx,ry)
      
      # ______Bottom Label/Title - Lab3 ______

      mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)   # bottom column titles
      
      lastLab3Space <<- ( xpin[1] - strwidth(lab3[j], units="inch", cex=Text.cex) ) / 2
   }

}

#
#  End of id glyph
#
#####

###### how to get right abbr or full ########


#####  MAP glyphs

#####
#
#  General Notes:
#
#  Data is always represented in the areaDatKey[] vector order.  The sDFdat data.frame has been pre-sorted per "sortVar".
#
#  VisKey contains the list of keys per polygons in areaVisBorders
#  VisCol should contain the mstColors for each polygon to allow a single "polygon" print.
#
#
#  General Map Notes:
#
#  NotUsed    = NT T/F list of sub-areas not referenced in the data.
#
#  back       = NT T/F list of not active sub-areas 
#
#  high       = NT T/F list of secondary sub-areas (not active or background or Not Used.)  (Color = pale Yellow)
#                  for map       -> not used
#                  for mapcum    -> accumulative list, colored pale yellow
#                  for mapmedian -> areas below or above median value (two colors and cross in median group.)
#                  for maptail   -> accumulative list to median then subtractive list to end.
#  highU      = NT T/F list of above median sub-areas (not active or Not used.) (color = pale red)
#  highL      = NT T/F list of below median sub-areas (not active or Not used.) (color = pale blue)
#
#  gnams      = NT T/F list of active colored to match links.
#
#  VisCol     = NT list of polygon keys.
# 
#
#   Map.Hdr1 and Map.Hdr2
#        Map.Hdr2 -> Type of sub-areas (Counties, Provinces, States, etc.)
#        Map.Hdr1 -> Top title in "map" (reserved)
#
#  column titles:
#
#     map                 mapcum                          mapmedian                 maptail
#
# 1)                    Cummulative Maps             Median Based Contours 
# 2)    Highlighted    b zzzzz Above Featured Rows    b zzzzz Featured Above        Two Ended Cumulative Maps                           
# 3)    States         b zzzzz Below Featured Rows    b zzzzz Featured Below           zzzzz Highlighted      
# 
#     Map.Hdr2     Map.Hdr2, X "Above Featured Rows"  Map.Hdr2  X "Featured Above"  Map.Hdr2 "Highlighted"
#                  Map.Hdr2, X "Below Featured Rows"  Map.Hdr2  X "Featured Below"
#
#
#    "Median For Sorted Panel"
#
#  Calculate width of each phrase.
# 

#####
#
# type = 'map'  =========================================================
#
# rlAreaMap
#

rlAreaMap = function(j) {

  # Works using area abbreviations
  # bnd.ord gives abbreviations in the
  #           the boundary are stored.
  # areaDatKey give the abbreviations in the order plotted 
  #  
  # Areas are colors if associated with active rows
  #
  #   j = column number,   i = row number
  
  #  bnd.ord is a list of Keys (one per polygon) in the border file.
  bnd.ord = rlAreaVisBorders$Key[is.na(rlAreaVisBorders$x)] # Area abbrev based on "NA" in point List.
  #cat("bnd.ord:",bnd.ord,"\n")
  
  #cat("Map-Overlays L2:",Map.L2Borders,"  Reg:",Map.RegBorders,"  L3:",Map.L3Borders,"\n")
  
  # the x,y limits must be based on the biggest area plotted, if the data level 
  #   does not cover the entire area, check the L2 and L3 borders.

  rPoly   <- MapPolySetup("map",panels,rlAreaVisBorders,rlL2VisBorders,rlRegVisBorders,rlL3VisBorders,Map.L3Borders)

  rxpoly2 <- rPoly$rxpoly2
  rypoly2 <- rPoly$rypoly2

  # must be done before panel is scaled.
  
  # Issue:  The median single row group does not print a map.  So, there aspect ratio normalizations
  #         could cause problems with median text.  Keep an eye on.
 
  # ____________labeling and axes_______________
  
  panelSelect(panels,1,j)
  x      <- panelScale()
  par(xpd=T)
  xpin   <- par("pin")
 
  #printPar()

     
  # column titles  - centered no boxes.
  
  # Use lines 2 and 3(tick) for two row title.
  #   no need for centering logic - no boxes.
  
  xLab1  <- banner["map","H2"]
  xLab2  <- banner["map","H3"]
  if (xLab2 == "") {
       xLab2 <- xLab1
       xLab1 <- ""
  }
  
  if (xLab1 != "") mtext(xLab1,side=3,line=Title.Line.2.pos,cex=Text.cex)
  mtext(xLab2,side=3,line=Title.Line.2x.pos,cex=Text.cex)           

  lastLab2Space  <<- - ( xpin[1] - strwidth(xLab2,units="inch",cex=Text.cex) ) / 2
  
  # Put the initial colors for all sub-areas into a vector.

  VisNodes       <- is.na(rlAreaVisBorders$x)                   # end of point elements for each polygon 
  VisKeys        <- rlAreaVisBorders$Key[VisNodes]              # key for that polygon
  VisHoles       <- rlAreaVisBorders$hole[VisNodes]             # is it a hole
  
  NotUsed        <- !is.na(match(rlAreaVisBorders$Key,NotUsedKeys)) # list of not used points.
  NotUsedFlag    <- any(NotUsed)                                # flag to indicate not used exists 
  VisNU          <- !is.na(match(VisKeys,NotUsedKeys))          # T/F list of not used polygons.
    
  #
  #   Panel Setup already calculated the following variables
  #
  #   numGrps     - number of group/rows
  #   medGrp      - the number of the median group/rows     (if number of groups is odd, otherwize = 0)
  #   medGrpSize  - number of rows in the median group/row  (if no median group, value = 0)
  #   medRow      - the number of the median row            (if number of rows is odd, othersize = 0)
  #   medRowBlw   - the number of the row just below the median
  #   medRowAbv   - the number of the row just above the median
  #   
  #
  
  #cat("map - areaDatKey:",areaDatKey,"\n")

  # Drawing Loop
  for (i in 1:numGrps) {

    if ( i == medGrp & medGrpSize == 1 ){                   # line break in maps.   Group n/2 - middle group of n (odd)
    
       # Setup Panel for single row median group

       panelSelect(panels,i,j)
       x <- panelScale()
       panelFill (col=Panel.Fill.col)
       panelOutline()
       
       # inform
       xmsg  <- banner["map","M1"]
       
       # Insert median - single group/row - centered on the middle of the rectangle (0.5, 0.5)
       text (.5,.5,xmsg,cex= Text.cex*0.8)   # center around 0.5, 0.5 (center)

       next  # skip to next FOR item
       ###  EXIT 
    }
    
    # handle groups with 2 or more rows
    
    panelSelect(panels,i,j)             # Do map in - Panels by group...
    x  <- panelScale(rxpoly2,rypoly2)   # apply the required scalling
    
    gsubs      <- ib[i]:ie[i]                 # get the index range for this panel
    
    blkAreaCol <- 0
  
    if (medGrp > 0 & medGrpSize == 1) {   
    
       # If this setup has a median group  with 1 row - then we must watch for the panel above and below it 
       # to highlight the median row in these panels.
       
       # Add median sub-area coloring to the row above and below the median line.
  
       if (i == (medGrp-1)) {
          gsubs      <- c(gsubs,medRow)  # slot med-1 - add med-row to this group
          blkAreaCol <- length(gsubs)    # indicate median/black area flag saves the "color" number the will be seen in the final.
       }
       if (i == (medGrp+1)) {
          gsubs      <- c(gsubs,medRow)  # slot med+1 - add med-row to this group
          blkAreaCol <- length(gsubs)    # indicate median/black area and the length of the gsubs vector
       }
       
       # blkAreaCol uses length(gsubs) as key - 2,3,4,5,6 used the index to match up later.
    }
    
    #print(paste0("gsubs:",paste0(gsubs,collapse=" ")))
    
    #    medRow - the median row, if number of rows is old.
    #         will always be in the medGrp group
  
    gnams    <- areaDatKey[gsubs]        # index to sub-area keys (translation)
   
    #print(paste0("gnams:",paste0(gnams,collapse=" ")))
    
    #
    #  Even though a hole in a sub-area may be later filled with a color or grey,
    #  it is always filled with tthe background map color.  The order of the 
    #  Polygons in the VisBorder files always have the holes following the basic sub-area
    #  and sub-areas filling other sub-areas holes after that's area's polygons.
    #
    #  mstColors = 1-6  -> active sub-area colors
    #  mstColors = 7    -> median sub-area color in panels above and below the median
    #  mstColors = 8-10 -> highlighted colors used in mapcum, mapmedian and maptail
    #  mstColors = 11   -> unreferenced sub-area
    #           12   -> background   sub-area (non-active)
    #           
    #  Run: sequence
    #       Set all colors in VisCol (based on polygons)
    #       Set all background and unused borders in VisLinexx
    #
    #       Separate highlight sub-area borders (2)
    #       Separate foreground sub-area borders (current set)
    #
    #       draw fill colors for all (VisCol)
    #
    #       draw background/Not Referenced lines
    #       draw highlighted lines 
    #       draw foreground lines
    #
    #  Get set up T/F vector for each type of group of sub-areas to plot
    
    #cat("length(NotUsedKeys):",length(NotUsedKeys)," with keys:",paste0(NotUsedKeys,collapse=", "),"\n")
    #cat("gnams:",paste0(gnams,collapse=", "),"\n")
    
    
    VisCol            <- rep(11,length(VisKeys))       # reduced size - color per polygon
    
    #  isolate foreground (active) sub-areas.
    foreKeys          <- gnams                                           # get list of keys
    fore              <- !is.na(match(rlAreaVisBorders$Key,foreKeys))    # find polygon points for each fore sub-areas and assign color based on order in gnams
    foreFlag          <- any(fore)                                       # set flags if any found
 
    VisForeCol        <- match(VisKeys,foreKeys)                         # get color index for each foreg subarea (1-6)
                                                                         #   NA=not foreground, #=foreground and order (foreKeys 1 to 6)
    VisFore           <- !is.na(VisForeCol)                              # T/F vector of VisKeys that are foreground
     
    VisCol[VisFore]   <- VisForeCol[VisFore]                             # transfer color index for each foreground polygon.
    
    if (blkAreaCol>0) {
      VisCol[VisCol == blkAreaCol] <- 7   # set to black                 # if VisCol ==  BlkAreaCol set previously - reset to "black" index (7)
    }
   
    # not really used - can we delete or set to empty?   Trying to standardize code??
    highKeys          <- NA                                              # clear high light vector - always none for "map"
    high              <- !is.na(match(rlAreaVisBorders$Key,highKeys))
    highFlag          <- any(high)
  
    VisHigh           <- !is.na(match(VisKeys,highKeys))
    VisCol[VisHigh]   <- 8
  
    # what is left - the background sub-areas.
    back              <- !(fore | high | NotUsed)                  # background is anything not active and not used.   T/F list
    backFlag          <- any(back)
    backKeys          <- unique(rlAreaVisBorders$Key[back])
  
    VisBack           <- !is.na(match(VisKeys,backKeys))
    VisCol[VisBack]   <- 12  
  
    VisCol2           <- mstColors[VisCol]   # translate to real colors
  
    VisCol2[VisHoles] <- Map.Bg.col       # set all holes to the panels background color.
   
    # colors are ready for ploting polygons
    
    ####
    #  Map background - Layer 2 borders   (regional areas  (US -> states))
    #
    if (Map.L2Borders) {    # area area overlay
        # map fill sub-areas
      polygon(rlL2VisBorders$x, rlL2VisBorders$y,
              density=-1, col=Map.L2.Fill.col, border=FALSE)
        # map borders of sub-areas
      polygon(rlL2VisBorders$x, rlL2VisBorders$y,
              density=0, col=Map.L2.Line.col, lwd=Map.L2.Line.lwd)  # white
      #cat("Drew L2 Borders\n")
    }
    #
    ####
    
    
    ####
    #
    #  Map sub-areas
    #
    #  Draw the colors for all active sub-areas.
    #
    polygon(rlAreaVisBorders$x,rlAreaVisBorders$y,
                  density=-1, col=VisCol2, border=FALSE)
    
    #cat("Drew active sub-area colors.\n")
    
    #
    #  setup each group of sub-areas and draw polygons.
    #    Not Referenced sub-areas  
    
    if (NotUsedFlag) {
       wVisBorders   <- NULL
       wVisBorders   <- rlAreaVisBorders[NotUsed,]
     
       # map sub-areas without data (not used)
       polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)   # white
       #cat("Drew not used sub-areas borders.\n")
    }
    
    #
    #    Background (not-active) sub-areas
    #
    if (backFlag) {
       wVisBorders   <- NULL
       wVisBorders   <- rlAreaVisBorders[back,]
       polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)    # white
       #cat("Drew not-active sub-areas borders.\n")
    }

    #
    #    Highlighted sub-areas
    #    
    if (highFlag) {
       wVisBorders   <- NULL
       wVisBorders   <- rlAreaVisBorders[high,]
       polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)    # black
       #cat("Drew highlighted sub-areas borders.\n")
    }

    # 
    #    Foreground (active) sub-areas
    #
    if (foreFlag) {
       wVisBorders   <- NULL
       wVisBorders   <- rlAreaVisBorders[fore,]
       polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)    # black
       #cat("Drew Active sub-areas borders.\n")
    }
        
    ####
    #
    #  map boundaries for regions.
    #
    
    if (Map.RegBorders && regionsB) {       # regions boundaries overlay
       polygon(rlRegVisBorders$x, rlRegVisBorders$y,
               density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)     # black
       #cat("Drew Region Borders\n")
    }
    #
    ####
    
    ####
    #
    #    Outline L3 (total) area (complete area boundary)
    #
    if (Map.L3Borders) {
       polygon(rlL3VisBorders$x, rlL3VisBorders$y,
           density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)      # black - outside US boundary
       #
       #  If U. S. map, add extra labels for sub-areas moved.
       #
       if (areaUSData) {                                             ##### replace with feature based code.
          if (i == 1) {
             # if first map in column
             text(135,31,'DC',cex=Map.Area.Spec.cex, adj=.5, col=1)
             text(22, 17,'AK',cex=Map.Area.Spec.cex, adj=.5, col=1)
             text(47, 8, 'HI',cex=Map.Area.Spec.cex, adj=.5, col=1)
          }
       }
    } 

   # no reference values for this type of column. If present - ignor.
   
  }  # i loop
  #   as we finish i loop, we end up in the last panel
  xpin          <- par("pin")
  
  lastLab3Space <<- xpin[1]/2

  if (lab3[j] != "") {
     #panelSelect(panels,numGrps,j)
     #x <- panelScale(rxpoly2,rypoly2)
     
     # ______Bottom Label/Title - Lab3 ______
     mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)   # bottom column titles   
 
     lastLab3Space <<- (xpin[1] - strwidth(lab3[j], cex=Text.cex, units="inch")) / 2
  }
  
}



#####
#
# type = 'mapcum'   ========================================================
#
# rlAreaMapCum
#

rlAreaMapCum = function(j) {

  # Works using area abbreviations
  # bnd.ord gives abbreviations in the order the boundary are stored.
  # areaDatKey give the abbreviations in the order plotted 
  #
  # Areas are colored if active in row.
  # Areas are colored cream is they were active in previous groups/rows.
  #
  
  #  bnd.ord is a list of Keys (one per polygon) in the border file.
  bnd.ord = rlAreaVisBorders$Key[is.na(rlAreaVisBorders$x)]   # area abbrev for areas with boundaries
  
  # the x,y limits must be based on the biggest area plotted, if the data level 
  #   does not cover the entire area, check the L2 and L3 borders.


  rPoly   <- MapPolySetup("mapcum",panels,rlAreaVisBorders,rlL2VisBorders,rlRegVisBorders,rlL3VisBorders,Map.L3Borders)

  rxpoly2 <- rPoly$rxpoly2
  rypoly2 <- rPoly$rypoly2

  # must be done before panel is scaled.
  
  # Issue:  The median single row group does not print a map.  So, there aspect ratio normalizations
  #         could cause problems with median text.  Keep an eye on.
 
  # ____________labeling and axes_______________

  panelSelect(panels,1,j)
  x <- panelScale()   #  default scale 0:1, 0:1  not very useful
  par(xpd=T)
  xpin      <- par("pin")
  
  #    make adjustments to handle variable scaling of first panel - at this point its 0,1 by 0,1
  #                      par("fin") has width and height in inches..  (2.4 x 3.6)
  #                      par("pin") has plot width and height in inches  ( 1.4 x 1.111 )
  #                      So, at 0,1 by 0,1  the aspect is really 1.111/1.4 = 0.79 about.
  #
  
  #
  #    draw box for title label   (convert inches into points for the panel.)
  #
  
  # line 1 - title, no boxes.
  mtext(banner["mapcum","H1"],side=3,line=Title.Line.1.pos,cex=Text.cex)   # use line position..

  # Line 2 - box and title
  DrawBoxAndText(banner["mapcum","H2"], Text.cex, Map.Lab.Box.Width, mstColors[8],  "black", Title.Line.2.pos)
  
  DrawBoxAndText(banner["mapcum","H3"], Text.cex, Map.Lab.Box.Width, Map.Bg.col, "black", Title.Line.2x.pos)

  lastLab2Space  <<- - ( xpin[1] - ( strwidth(banner["mapcum","H3"],units="inch",cex=Text.cex) + 0.15 ) ) / 2
  
  VisNodes       <- is.na(rlAreaVisBorders$x)
  VisKeys        <- rlAreaVisBorders$Key[VisNodes]
  VisHoles       <- rlAreaVisBorders$hole[VisNodes]  
  NotUsed        <- !is.na(match(rlAreaVisBorders$Key,NotUsedKeys))
  NotUsedFlag    <- any(NotUsed)
  VisNU          <- !is.na(match(VisKeys,NotUsedKeys))

  #
  #
  #####
 
  #
  # Alternative is to must plot the text at x,y points.
  #
    
  # Drawing Loop
  #cat("mapcum - areaDatKey:",areaDatKey,"\n")

  
  for (i in 1:numGrps) {

     if (i == medGrp & medGrpSize == 1) {
        panelSelect(panels,i,j)
        x       <- panelScale()
        panelFill (col=Panel.Fill.col)
        panelOutline()
   
        text (.5,.5,banner["mapcum","M1"],cex=Text.cex*0.8)   # centered around 0.5 0.5
        next
     }
     
     panelSelect(panels,i,j)
     x <- panelScale(rxpoly2,rypoly2)

     gsubs      <- ib[i]:ie[i]
     blkAreaCol <- 0
 
     ke = length(gsubs)    # get number of rows.
     
     ##  if a single row is not the median then the middle group is the median.
     
     if ( medGrp > 0 & medGrpSize == 1) {
     
         if (i == (medGrp-1)) {
            gsubs      <- c(gsubs,medRow)
            blkAreaCol <- length(gsubs)
         }
         if (i == (medGrp+1)) {
            gsubs      <- c(gsubs,medRow)
            blkAreaCol <- length(gsubs)
         }
     }

     gnams = areaDatKey[gsubs]    # translate from sequence number to sorted order of areas (abbrev)
                       # list of areas in this row (group) panel.

     ####
     #  Map background - Layer 2 borders   
     #
     if (Map.L2Borders) {    # area area overlay
         # map fill areas
       polygon(rlL2VisBorders$x, rlL2VisBorders$y,
               density=-1, col=Map.L2.Fill.col, border=FALSE)
         # map borders
       polygon(rlL2VisBorders$x, rlL2VisBorders$y,
               density=0, col=Map.L2.Line.col, lwd=Map.L2.Line.lwd)   # white
     }
     #
     ####
     
     ####
     #
     #  map boundaries for regions.
     #
     
     if (Map.RegBorders && regionsB) {       # regions boundary overlay
        polygon(rlRegVisBorders$x, rlRegVisBorders$y,
                density=0, col=Map.L2.Line.col, lwd=Map.L2.Line.lwd)
     }
     #
     ####
   
     VisCol          <- rep(11,length(VisKeys))     # reduced size
        
     foreKeys        <- gnams
     fore            <- !is.na(match(rlAreaVisBorders$Key,foreKeys))            # find fore sub-areas and assign color based on order in gnams
     foreFlag        <- any(fore)
     
     VisForeCol      <- match(VisKeys,foreKeys)
     VisFore         <- !is.na(VisForeCol)
        
     VisCol[VisFore] <- VisForeCol[VisFore]
        
     if (blkAreaCol>0) {
       VisCol[VisCol == blkAreaCol] <- 7   # set to black
     }
  
     highKeys        <- areaDatKey[1:ib[i]-1]   # vector of names used areas include this panel.
     high            <- !is.na(match(rlAreaVisBorders$Key,highKeys))
     highFlag        <- any(high)
   
     VisHigh         <- !is.na(match(VisKeys,highKeys))
     VisCol[VisHigh] <- 8
    
     # what is left - the background sub-areas.
     back            <- !(fore | high | NotUsed)                  # background is anything not active and not used.   T/F list
     backFlag        <- any(back)
     backKeys        <- unique(rlAreaVisBorders$Key[back])
   
     VisBack         <- !is.na(match(VisKeys,backKeys))
     VisCol[VisBack] <- 12  
        
     VisCol <- mstColors[VisCol]   # translate to real colors
    
     VisCol[VisHoles] <- Map.Bg.col
          
     polygon(rlAreaVisBorders$x,rlAreaVisBorders$y,
                 density=-1, col=VisCol, border=FALSE)
     
     #  setup each group of sub-areas and draw polygons.
     #    Not Referenced sub-areas  
     if (NotUsedFlag) {
        wVisBorders   <- rlAreaVisBorders[NotUsed,]
        polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)    # white
     }
     #    Background (not-active) sub-areas
     if (backFlag) {
        wVisBorders   <- rlAreaVisBorders[back,]
        polygon(wVisBorders$x,wVisBorders$y,
               density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)    # white
     }
     #    Highlighted sub-areas
     if (highFlag) {
        wVisBorders   <- rlAreaVisBorders[high,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)    # black
     }
     #    Foreground (active) sub-areas
     if (foreFlag) {
        wVisBorders   <- rlAreaVisBorders[fore,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)    # black
     }
     
     ####
     #
     #  map boundaries for regions.
     #
     
     if (Map.RegBorders && regionsB) {       # regions boundaries overlay
        polygon(rlRegVisBorders$x, rlRegVisBorders$y,
                density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)     # black
     }
     #
     ####
     
     ####
     #
     # Outline Country area (total area).
     #
     if (Map.L3Borders) {
        polygon(rlL3VisBorders$x, rlL3VisBorders$y,
            density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)      # black - outside US boundary
        if (areaUSData) {
           if (i == 1) {
              text(135,31,'DC',cex=Map.Area.Spec.cex,adj=.5, col=1)
              text(22, 17,'AK',cex=Map.Area.Spec.cex,adj=.5, col=1)
              text(47, 8, 'HI',cex=Map.Area.Spec.cex,adj=.5, col=1)
           }
        } 
     }
   
  }  # i loop

  # no reference values for this type of column. If present - ignor.
  #  as we leave i loop - we are in the last group panel
  
  xpin          <- par("pin")
  lastLab3Space <<- xpin[1]/2
  
  if (lab3[j] != "") {
     #panelSelect(panels,numGrps,j)
     #x <- panelScale(rxpoly2,rypoly2)
     
     # ______Bottom Label/Title - Lab3 ______
     mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)   # bottom column titles   
  
     lastLab3Space <<- (xpin[1] - strwidth(lab3[j], cex=Text.cex, units="inch")) / 2
  }
  
}


#####
#
# type = 'mapmedian'  =================================================
#
# rlAreaMapMedian
#

rlAreaMapMedian = function(j){

   # Works using area abbreviations
   # bnd.ord gives abbreviations in the
   #           the boundary are stored.
   # areaDatKey give the abbreviations in the order plotted
   # This MapMedian cream colors all areas above and below the median area.
   #   Areas < median are colored very light red in upper half of groups,
   #   Areas > median are colored very light blue in lower half of groups.
   #   In the median group when there is more than one row, both above and below
   #   shading are done as a cross over.
   #

   bnd.ord = rlAreaVisBorders$Key[is.na(rlAreaVisBorders$x)] # area abbrev
   # the x,y limits must be based on the biggest area plotted, if the data level 
   #   does not cover the entire area, check the L2 and L3 borders.
   
   rPoly   <- MapPolySetup("mapmedian",panels,rlAreaVisBorders,rlL2VisBorders,rlRegVisBorders,rlL3VisBorders,Map.L3Borders)
   
   rxpoly2 <- rPoly$rxpoly2
   rypoly2 <- rPoly$rypoly2

   
   # ____________labeling and axes_______________

   panelSelect(panels,1,j)
   x <- panelScale()
   par(xpd=T)
   xpin        <- par("pin")
   
   #
   #    draw box for title label   (convert inches into points for the panel.)
   #
   
   # line 1 - title, no boxes.
   mtext(banner["mapmed","H1"],side=3,line=Title.Line.1.pos,cex=Text.cex)   # use line position..

   # Line 2 - box and title
   DrawBoxAndText(banner["mapmed","H2"], Text.cex, Map.Lab.Box.Width, mstColors[9],  "black", Title.Line.2.pos)
   
   DrawBoxAndText(banner["mapmed","H3"], Text.cex, Map.Lab.Box.Width, mstColors[10], "black", Title.Line.2x.pos)
   
   lastLab2Space  <<- - ( xpin[1] - ( strwidth(banner["mapmed","H3"],units="inch",cex=Text.cex) + 0.15 ) ) / 2
   
   #cat("mapmed - areaDatKey:",areaDatKey,"\n")

   #
   
   VisNodes       <- is.na(rlAreaVisBorders$x)
   VisKeys        <- rlAreaVisBorders$Key[VisNodes]
   VisHoles       <- rlAreaVisBorders$hole[VisNodes]  
   NotUsed        <- !is.na(match(rlAreaVisBorders$Key,NotUsedKeys))
   NotUsedFlag    <- any(NotUsed)
   VisNU          <- !is.na(match(VisKeys,NotUsedKeys))

   highUKeys      <- areaDatKey[1:medRowAbv]
   highU          <- !is.na(match(rlAreaVisBorders$Key,highUKeys))
   highUFlag      <- any(highU)
   VisHighU       <- !is.na(match(VisKeys,highUKeys))
   
   highLKeys      <- areaDatKey[medRowBlw:numRows]
   highL          <- !is.na(match(rlAreaVisBorders$Key,highLKeys))
   highLFlag      <- any(highL)
   VisHighL       <- !is.na(match(VisKeys,highLKeys))
    
   # Drawing Loop


   # if this is the median group, the both get shaped.

   for (i in 1:numGrps) {

      # Median Group/Row with 1 row
      if (i == medGrp & medGrpSize == 1) {
         # median group/row with 1 row - do text instead of map.
         
         panelSelect(panels,i,j)
         x <- panelScale()
         panelFill (col=Panel.Fill.col)
         panelOutline()
         
         text (.5,.5,banner["mapmed","M1"],cex=Text.cex*0.8)
         next   # exit for loop to next group/row  
      }
      

      # All panels now have 2 or more rows
          
      panelSelect(panels,i,j)
      x <- panelScale(rxpoly2,rypoly2)
   
      gsubs      <- ib[i]:ie[i]
    
      blkAreaCol <- 0

      # Median Group/Row Panel
      if (medGrp > 0 & medGrpSize == 1) {

         # if we had a median group/row with 1 row, then accent median row in panels above and below.
        
         if (i == medGrp-1) {
            gsubs <- c(gsubs,medRow)   # add median row to list
            blkAreaCol <- length(gsubs)
            # accent in above panel
         }
         if (i == medGrp+1) {
            gsubs <- c(gsubs,medRow)   # add median row to list
            blkAreaCol <- length(gsubs)
            # accent in below panel
         }
      }
   
      #  gsubs <- current area list
      gnams <- areaDatKey[gsubs]          # set of areas for normal coloring.  (get keys from index #s)
      
      #
      # Sub Divide into four groups:
      #    1) background,   2) Above Median with data   3) Below Median with data,  4) Active 
      #       Whats left        1:medRowAbv                  medRowBlw:numRows         gsubs
      #    note: medRowBlw:numRows will catch NA data items.  (ignore is sorted column and NA (st bottom.)
      #    note: non-data sub-area will not any row in the data, but will have a row in the areaNamesAbbrsIDs.
      #          if we don't reference them, then boundaries may not be completely drawn.
      #
     
      ####
      #  Map background - Layer 2 borders   (regional areas  (US -> states))
      #
      if (Map.L2Borders) {    # area area overlay
          # map fill areas
        polygon(rlL2VisBorders$x, rlL2VisBorders$y,
                density=-1, col=Map.L2.Fill.col, border=FALSE)
          # map borders
        polygon(rlL2VisBorders$x, rlL2VisBorders$y,
                density=0, col=Map.L2.Line.col, lwd=Map.L2.Line.lwd)   # white
      }
      #
      ####
      
      VisCol      <- rep(11,length(VisKeys))
   
      highUbdr    <- FALSE
      highLbdr    <- FALSE
   
      if (i < medGrp ) {
         high             <- highU
         highUbdr         <- TRUE
         VisCol[VisHighU] <- 9
      }
      if (i > medGrp) {
         high             <- highL
         highLbdr         <- TRUE
         VisCol[VisHighL] <- 10
      }
      if (i == medGrp) {
         high             <- highU | highL
         highUbdr         <- TRUE
         highLbdr         <- TRUE
         VisCol[VisHighU] <- 9
         VisCol[VisHighL] <- 10
      }

      foreKeys         <- gnams
      fore             <- !is.na(match(rlAreaVisBorders$Key,foreKeys))            # find fore sub-areas and assign color based on order in gnams
      foreFlag         <- any(fore)
      
      VisForeCol       <- match(VisKeys,foreKeys)
      VisFore          <- !is.na(VisForeCol)
         
      VisCol[VisFore]  <- VisForeCol[VisFore]
         
      if (blkAreaCol>0) {
        VisCol[VisCol == blkAreaCol] <- 7   # set to black
      }
    
      # what is left - the background sub-areas.
      back             <- !(fore | high | NotUsed)                  # background is anything not active and not used.   T/F list
      backFlag         <- any(back)
      if (backFlag) {
         backKeys         <- unique(rlAreaVisBorders$Key[back])
         VisBack          <- !is.na(match(VisKeys,backKeys))
         VisCol[VisBack]  <- 12  
      }
      
      VisCol           <- mstColors[VisCol]   # translate to real colors
     
      VisCol[VisHoles] <- Map.Bg.col
           
      polygon(rlAreaVisBorders$x,rlAreaVisBorders$y,
                  density=-1, col=VisCol, border=FALSE)

      #  setup each group of sub-areas and draw polygons.
      #    Not Referenced sub-areas  
      if (NotUsedFlag) {
         wVisBorders   <- rlAreaVisBorders[NotUsed,]
         polygon(wVisBorders$x,wVisBorders$y,
                 density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)  # white
      }
      #    Background (not-active) sub-areas
      if (backFlag) {
         wVisBorders   <- rlAreaVisBorders[back,]
         polygon(wVisBorders$x,wVisBorders$y,
                 density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)  # white
      }

      #    Highlighted sub-areas (2)
      if (highUbdr) {
         wVisBorders   <- rlAreaVisBorders[highU,]
         polygon(wVisBorders$x,wVisBorders$y,
                 density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)  # black
      }
      if (highLbdr) {
         wVisBorders   <- rlAreaVisBorders[highL,]
         polygon(wVisBorders$x,wVisBorders$y,
                 density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)  # black
      }
   
      #    Foreground (active) sub-areas
      if (foreFlag) {
         wVisBorders   <- rlAreaVisBorders[fore,]
         polygon(wVisBorders$x,wVisBorders$y,
                 density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)  # black
      }
   
      ####
      #
      #  map boundaries for regions.
      #
      
      if (Map.RegBorders && regionsB) {       # regions boundaries overlay
         polygon(rlRegVisBorders$x, rlRegVisBorders$y,
                 density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)   # black
      }
      #
      ####

      ####
      #
      # Outline Country area (total area).
      #
      if (Map.L3Borders) {
         polygon(rlL3VisBorders$x, rlL3VisBorders$y,
             density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)      # black - outside US boundary
       
         if (areaUSData) {
            if (i == 1) {
               text(135,31,'DC',cex=Map.Area.Spec.cex,adj=.5, col=1)
               text(22, 17,'AK',cex=Map.Area.Spec.cex,adj=.5, col=1)
               text(47, 8, 'HI',cex=Map.Area.Spec.cex,adj=.5, col=1)
            }
         }
      }

   }   # i loop

   # no reference values for this type of column. If present - ignor.
   # as we finish i loop - we are in the last group panel.

   xpin <- par("pin")
   lastLab3Space <<- xpin[1]/2

   if (lab3[j] != "") {
      #panelSelect(panels,numGrps,j)
      #x <- panelScale(rxpoly2,rypoly2)
      
      # ______Bottom Label/Title - Lab3 ______
      mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)   # bottom column titles   
   
      lastLab3Space <<- (xpin[1] - strwidth(lab3[j], cex=Text.cex, units="inch")) / 2
   }

}


#####
#
# type = 'maptail' ====================================================
#
# rlAreaMapTail
#

rlAreaMapTail = function(j){
 
   # Works using area abbreviations
   # bnd.ord gives abbreviations in the
   #           the boundary are stored.
   # areaDatKey give the abbreviations in the order plotted
   # MapTail shows current areas in a group as colored and
   # a tail of areas (in cream color) from the outside inward.  
   # 
 
   #browser()
 
   bnd.ord <- rlAreaVisBorders$Key[is.na(rlAreaVisBorders$x)]         # area Key
   
   rPoly   <- MapPolySetup("maptail",panels,rlAreaVisBorders,rlL2VisBorders,rlRegVisBorders,rlL3VisBorders,Map.L3Borders)
    
   rxpoly2 <- rPoly$rxpoly2
   rypoly2 <- rPoly$rypoly2
   
   #cat("maptail - areaDatKey:",areaDatKey,"\n")

   # ____________labeling and axes_______________
   
   #   Panel # 1 - header
 
   # column header titles and "box"
   
   panelSelect(panels,1,j)    #  Line 1 and Line 2 - panel 1
   x <- panelScale()
   par(xpd=T)
   xpin     <- par("pin")
 
   #
   #    draw box for title label   (convert inches into points for the panel.)
   #
   
   # Line 1 - Not used
   
   # line 2 - title, no boxes.
   mtext(banner["maptail","H2"],side=3,line=Title.Line.2.pos,cex=Text.cex)   # use line position..
 
   # Line 3 - box and title
   DrawBoxAndText(banner["maptail","H3"], Text.cex, Map.Lab.Box.Width, mstColors[8],  "black", Title.Line.2x.pos)
 
   lastLab2Space  <<- - ( xpin[1] - ( strwidth(banner["maptail","H3"],units="inch",cex=Text.cex) + 0.15 ) ) / 2
 
   #  If needed this work be the place for Panel # N - Trailer code.
   
   #  JP - removed - temp
   #  mtext('Further From Median',side=3,line=Title.Line.2x.pos,at=.15,cex=Text.cex,adj=0)
   
   # need a median group point for calculations on the two tailed maps
   if (medGrp > 0 ) {
      # odd number of groups
      medGrpPt <- medGrp
   } else { 
      medGrpPt <- (numGrps/2) # + one lower
   }
   
   VisNodes       <- is.na(rlAreaVisBorders$x)
   VisKeys        <- rlAreaVisBorders$Key[VisNodes]
   VisHoles       <- rlAreaVisBorders$hole[VisNodes]  
   NotUsed        <- !is.na(match(rlAreaVisBorders$Key,NotUsedKeys))
   NotUsedFlag    <- any(NotUsed)
   VisNU          <- !is.na(match(VisKeys,NotUsedKeys))
 
   # Drawing Loop
 
   for (i in 1:numGrps) {
 
      if(i == medGrp & medGrpSize == 1 ) {
         panelSelect(panels,i,j)
         panelScale()
         panelFill (col=Panel.Fill.col)
         panelOutline()
         text (.5,.5,banner["maptail","M1"],cex=Text.cex*0.8)
         next
      }
      
      panelSelect(panels,i,j)  
      x <- panelScale(rxpoly2,rypoly2)
      
      # get list of areas in this group.
     
      gsubs      <- ib[i]:ie[i]
      ke         <- length(gsubs)
      
      blkAreaCol <- 0
     
      
      if (medGrp > 0 & medGrpSize == 1) {
         if (i == (medGrp-1)) {
            gsubs      <- c(gsubs,medRow)
            blkAreaCol <- length(gsubs)
         }
         if (i == (medGrp+1)) {
            gsubs      <- c(gsubs,medRow)
            blkAreaCol <- length(gsubs)
         } 
      }
      
     # get list of group area names 
     gnams = areaDatKey[gsubs]
 
     ####
     #
     #  Map background - Layer 2 borders   (regional areas  (US -> states))
     #
     if (Map.L2Borders) {    # area area overlay
         # map fill areas
       polygon(rlL2VisBorders$x, rlL2VisBorders$y,
               density=-1, col=Map.L2.Fill.col, border=FALSE)
         # map borders
       polygon(rlL2VisBorders$x, rlL2VisBorders$y,
               density=0, col=Map.L2.Line.col, lwd=Map.L2.Line.lwd)  # white
     }
     #
     ####
     
     VisCol          <- rep(11,length(VisKeys))
     
     highKeys        <- NA
     highFlag        <- FALSE
   
     if (i < medGrpPt)  highKeys <- areaDatKey[1:ib[i]]         # areas below the median highlighted.
     if (i > medGrpPt)  highKeys <- areaDatKey[ie[i]:numRows]
     
     if (length(highKeys) > 0) {
        high            <- !is.na(match(rlAreaVisBorders$Key,highKeys))
        highFlag        <- any(high)
        
        VisHigh         <- !is.na(match(VisKeys,highKeys))
        VisCol[VisHigh] <- 8
     }
    
     foreKeys         <- gnams
     fore             <- !is.na(match(rlAreaVisBorders$Key,foreKeys))            # find fore sub-areas and assign color based on order in gnams
     foreFlag         <- any(fore)
      
     VisForeCol       <- match(VisKeys,foreKeys)
     VisFore          <- !is.na(VisForeCol)
         
     VisCol[VisFore]  <- VisForeCol[VisFore]
         
     if (blkAreaCol>0) {
        VisCol[VisCol == blkAreaCol] <- 7   # set to black
     }
      
     # what is left - the background sub-areas.
     back             <- !(fore | high | NotUsed)                  # background is anything not active and not used.   T/F list
     backFlag         <- any(back)
     if (backFlag) {
        backKeys         <- unique(rlAreaVisBorders$Key[back])
        VisBack          <- !is.na(match(VisKeys,backKeys))
        VisCol[VisBack]  <- 12  
     }
      
     VisCol           <- mstColors[VisCol]   # translate to real colors
      
     VisCol[VisHoles] <- Map.Bg.col
           
     # draw the combined fill colors in VisBorder file order.
      
     polygon(rlAreaVisBorders$x,rlAreaVisBorders$y,                    # plot all polygons
                  density=-1, col = VisCol, border = FALSE)            # fill in all areas. (1 to 6, 7, hole)
     
     #  setup each group of sub-areas and draw polygons.
     #    Not Referenced sub-areas  
     
     if (NotUsedFlag) {
        wVisBorders   <- rlAreaVisBorders[NotUsed,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)   # white
     }
     #    Background (not-active) sub-areas
     if (backFlag) {
        wVisBorders   <- rlAreaVisBorders[back,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Bg.Line.col, lwd=Map.Bg.Line.lwd)   # white
     }
     #    Highlighted sub-areas 
     if (highFlag) {
        wVisBorders   <- rlAreaVisBorders[high,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)   # black
     }
     #    Foreground (active) sub-areas
     if (foreFlag) {
        wVisBorders   <- rlAreaVisBorders[fore,]
        polygon(wVisBorders$x,wVisBorders$y,
                density=0, col= Map.Fg.Line.col, lwd=Map.Fg.Line.lwd)   # black
     }
   
     ####
     #
     #  map boundaries for regions.
     #
     
     if (Map.RegBorders && regionsB) {       # regions boundaries overlay
        polygon(rlRegVisBorders$x, rlRegVisBorders$y,
                density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)    #  black
     }
     #
     ####

     ####
     #
     # Outline Country area (total area).
     #
     if (Map.L3Borders) {
        polygon(rlL3VisBorders$x, rlL3VisBorders$y,
             density=0, col=Map.L3.Line.col, lwd=Map.L3.Line.lwd)      # black - outside US boundary
        if (areaUSData) {
           if (i == 1) {
              text(135,31,'DC',cex=Map.Area.Spec.cex, adj=.5, col=1)
              text(22, 17,'AK',cex=Map.Area.Spec.cex, adj=.5, col=1)
              text(47, 8, 'HI',cex=Map.Area.Spec.cex, adj=.5, col=1)
           }
        }
     }
 
  }   #  i loop
 
  # no reference values for this type of column. If present - ignor.
  # as we finish i loop - we are in the last group panel
  
  xpin          <- par("pin")
  lastLab3Space <<- xpin[1]/2
  
  if (lab3[j] != "") {
     #panelSelect(panels,numGrps,j)
     #x <- panelScale(rxpoly2,rypoly2)
     
     # ______Bottom Label/Title - Lab3 ______
     mtext(side=1,lab3[j],line=Title.Line.3.pos,cex=Text.cex)   # bottom column titles   
  
     lastLab3Space <<- (xpin[1] - strwidth(lab3[j], cex=Text.cex, units="inch")) / 2
  }

}  

#
#


#####
#
#  Area Rank Number ================================================================
#
#  rlAreaRank   # based ID dot.
#      display the sorted rank.
#      need to update to reflect RANKing based on sorted value.  Could have ties.
#
#####
#
#   Re-Think and rewrite before documenting.
#
#####

rlAreaRank = function(j){
  #  j = panel column number

  #________________ Scaling _______________

  rx        <- c(0,1)
  ry        <- c(0,1)
  rankstart <- 0.137
 
  #______________________panel labels_____________

  panelSelect(panels,1,j)
  panelScale(rx,ry)
  mtext('Area Rank',side=3,line=Title.Line.1.pos,cex=Text.cex)
  # mtext('areas',side=3,line=Title.Line.2.pos,cex=Text.cex)
 
  for (i in 1:numGrps){
     gsubs <- ib[i]:ie[i]
     ke    <- length(gsubs)
     laby  <- ke:1

     rsubs <- xDFrame$Rank[gsubs]
     
     pen   <- if(i == medGrp & medGrpSize == 1) 7 else 1:ke
     
     panelSelect(panels, i, j)
     x <- panelScale(rx, c(1-pad, ke+pad))
    
     Fgsubs <- formatC(rsubs, format="f", width=3, digits=0)
     text(rep(rankstart, ke), laby+.1, Fgsubs, adj=0, cex=Text.cex)
  }

  #  No reference values for this type of column.
}



#####
#
# type = 'ScatDot'   =====================================================
#
# rlAreaScatDot  (Scattered Plot Dots)
#

rlAreaScatDot = function(j){
   #
   #  j = panel column number
   #
   #  col1 and col2 point to the X and Y data values in the statsDFrame data.frame (known here as "dat").
   # 
   #
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
   # "col1"
   stColName1  <- wstname[col1[j]]
   pdUmsg      <- "(X coordinates)"
   
   xr          <- CheckPDCol('col1', 'SCATDOT', col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   
   if (xr$Err) { ErrFnd <- TRUE } else { xdat1 <- xr$Dat }
      
   # "col2"
   stColName2  <- wstname[col2[j]]
   pdUmsg      <- "(Y coordinates)"
   
   xr          <- CheckPDCol('col2', 'SCATDOT', col2[j], stColName2, j, 2, wstMax, dat, pdUmsg)
   
   if (xr$Err) { ErrFnd <- TRUE } else { xdat2 <- xr$Dat }
   
   
   if (ErrFnd) return ()
   
   good1       <- !is.na(xdat1)      # test to see if both values are present.
   good2       <- !is.na(xdat2)
   goodrow     <- !is.na(xdat1 + xdat2)   # used by code to skip bad entries.
   
     
   # x and y data loaded into workSCD data.frame
   workSCD           <- data.frame(x=xdat1,y=xdat2)      # get x and y data from the statsDFrame.
   #   x and y are the coordinates of each dot.
   #
   #  other fields added later
   #    $pch  - symbol code (only 19:25 are supported)
   #    $cex  - symbol size
   #    $bg   - background color - symbol fill color
   #    $col  - color of line
   #    $lwd  - line weight of outline of symbol
   #  
   rownames(workSCD) <- rownames(dat)          # transfer row.names
    
   refval            <- lRefVals[j]              # get referrence to object, changed 
   reftxt            <- lRefTexts[j]             # new - JP-2010/07/23
 
   #_______________Gather stats and put in area Order______________
  
   #  Sorting has already been done of the statsDFrame (dat) by areaDatKey or value 
   #     in the function startup.
    
   #_______________Scaling____________
    
   # x scaling
   lPad        <- TRUE
   rPad        <- TRUE
   
   rx          <- range(workSCD$x,na.rm=TRUE)       # range of X values
   #cat("scatdot-rx:",rx,"\n")
   
   #rx         <- SCD.xsc*diff(rx)*c(-.5,.5)+mean(rx)     # min to max range with expansion factors.
   #cat("scatdot-rx after padding:",rx,"\n")
   
   # y scaling                  
   ry          <- range(workSCD$y,na.rm=TRUE)       # range of Y values
   ry          <- SCD.ysc*diff(ry)*c(-.5,.5)+mean(ry)
    
   # diagonal end points
   dx          <- max(rx[1],ry[1])
   diagr       <- c(max(rx[1],ry[1]), min(rx[2],ry[2]))
   
   # ____________titles and labeling axes_______________
 
   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  Padding on left and right for dots.
   #
  
   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad, YAxisPad=TRUE)
 
   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
   
   #cat("ScatDot-Result staggering:",staggering,"  staggered:",staggered,"\n")
   
   #
   #####
 
   # ___________________drawing loop_____________________
 
   # in the ordered list, the median should be 26 of 51 items.  changed because of generalization.
      
   oldpar      <- par(lend="butt")
 
   #  build each panel for scatter plot dots
   
   # Y axis & text - can do once for all  
   YAxis_cex <- TS.Axis.cex * 0.75
   xPs       <- par("ps")
   xHPsLU    <- strheight("00000",cex=1,units="user")
   xHDesPsLU <- strheight("00000",cex=YAxis_cex,units="user")
   xDifHLU   <- xHPsLU - xHDesPsLU
   YAxis_adj <- xDifHLU / xHPsLU
   #cat("YAxis adjustment - YAxis_adj:",YAxis_adj,"  YAxis_cex:",YAxis_cex,"\n")
     
   for (i in 1:numGrps) {  # groups from 1 to 5, 6, 7 to 11   ##  6 is the median group.
     
      # Cycle through the Row/Groups in the micromap column
      
      #  This glyph is special in that it draws the data in every panel for all of the scatdot data points.
      #  Only the ones related to the group/row are modified and colored.
 
      # Set defaults values for all dots for this panel
      
      workSCD$pch   <- SCD.Bg.pch         # default pch code.
      workSCD$cex   <- SCD.Bg.pch.size    # default size, except median
      workSCD$bg    <- SCD.Bg.pch.fill    # default symbol color file   - was SCD.Bg.pch.fill
      workSCD$col   <- SCD.Bg.pch.col     # default line color of outline  ("black")
      workSCD$lwd   <- SCD.Bg.pch.lwd     # default line weight of outline         

      if (medGrp > 0 & medGrpSize == 1) {
         # if there is a median Group/Row and it contains one row, then 
       
         if (i >= medGrp-1 && i <= medGrp + 1) {    # force median dot to be highlighted in median and near groups. 
           
             # modify characteristics of the point in previous and following group/rows to the median group/row
             workSCD$pch[medRow] <- SCD.Median.pch
             workSCD$cex[medRow] <- SCD.Median.pch.size
             workSCD$bg[medRow]  <- SCD.Median.pch.fill
             workSCD$col[medRow] <- SCD.Median.pch.col
             workSCD$lwd[medRow] <- SCD.Median.pch.lwd
         }  
      }
       
      # plot points.
       
      # get list of active rows in this group/row
      gsubs <- ib[i]:ie[i]               # get beginning to end index row number in this group  
      ke    <- length(gsubs)                # get number of rows in group  (5 or 1)  
 
      # Get color indexes.
      # adjust if median group      
      pen   <- if(i == medGrp & medGrpSize == 1 ) 7 else 1:ke        # if median group (6)(black), then pen=6, otherwise pen = c(1...x)   
 
             
      panelSelect(panels,i,j)           # select panel for group i in column j)
      x <- panelScale(rx,ry)                 # set scale for panel  (should this be ry * 5 or 1?)
      panelFill(col=Panel.Fill.col)            # set fill for panel
       
      # vertical grid lines.
      axis(side=1, tck=1, labels=F, at=atRx,
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grid lines
  
      # y axis labels
      if (i == medGrp & medGrpSize == 1) { # median panel 
         # special for median group/row with one row 
         atRy    <- c(saveAtRy[1],saveAtRy[length(saveAtRy)])   # for margin panel, print the lowest and highest.   
      } else {
         atRy    <- panelInbounds(ry)                           # prettyprint a range.
      }
      # optional horizontal grid.
      if (SCD.hGrid) {
         axis(side=2,tck=1,labels=F,col=Grid.Line.col,lwd=Grid.Line.lwd, at=atRy) # Grid lines
      }
       
      # parameters and variable setup outside of loop.
      
      axis(side=2, tick=F, cex.axis=YAxis_cex, 
             mgp=mgpLeft, line= -YAxis_adj*0.3,
             at=atRy, 
             labels=as.character(atRy))
      mtext(lab4[j],side=2,
             line=Title.Line.5.pos,
             cex=TS.Axis.cex)
      
      panelOutline(col=Panel.Outline.col)     # outline panel
 
      # dv <- c(gsubs[1:ke],medRow)          # was 26.
      #
      # draw diagonal line of symetry from c(min (x, y),min(x,y)) to 
      #     c(max(x,y), max(x,y)), all point have x=y.
      #
      if ((diagr[1] < diagr[2]) && SCD.DiagLine) {  
          # draw symetric line if within box range.
          dx    <- c(diagr[1],diagr[2])
          dy    <- c(diagr[1],diagr[2])
          lines(dx,dy, col=SCD.DiagLine.col, lwd=SCD.DiagLine.lwd, lty=SCD.DiagLine.lty)  # place a diagonal line on plot.
          
          #  print out the statistics for the line
          if (MST.Debug == 1) {
              print(paste0("line:",paste0(c(dx,dy),collapse=" ")))
              print(paste0("usr:",paste0(par("usr"),collapse=" ")))
              print(paste0("pin:",paste0(par("pin"),collapse=" ")))
              MST.Debug = 0 # turn off.
          }
      }      
     
      #  plot points    
      if (i == medGrp & medGrpSize == 1) {
         
          wS <- workSCD[gsubs[1],]      # get one entry - the median   (Median group/row with 1 row).
         
      } else {
           
          #  standard group/row or median without single row.
          for (k in 1:ke) {                  # Process each slot of panel - step 1 to 5/6 or 1 to 1
             # cycle through row-groups and assign colors to associated area's dots.
              m    <- gsubs[k]
          
              workSCD$pch[m]  <- SCD.Fg.pch              # only 19:25 are supported.
              workSCD$cex[m]  <- SCD.Fg.pch.size
              workSCD$bg[m]   <- mstColors[pen[k]]       # set approvate color to circle fill.
              workSCD$col[m]  <- SCD.Fg.pch.col          # color of outline of symbol
              workSCD$lwd[m]  <- SCD.Fg.pch.lwd          # weight of outline of symbol
        
          }
          wS  <- workSCD[order(workSCD$cex,decreasing=FALSE),]  # sort by text size to get active point on top.
          # plot all points by size, others first, colored and median last.   
      }
      #  Have lists of points to plot in wS
      #  Since the points we plot must have outlines and have fill colors, 
      #  only the graphic points  19:25 are supported. 
      #
       
      points(wS$x, wS$y, pch=wS$pch, col=wS$col, bg=wS$bg, cex=wS$cex, lwd=wS$lwd)  # removed 
      #     col = border of symbol,  bg = background color of symbol.
    
      #   related to NA processing, points will just not draw a symbol if one of the x,y coordinates is NA.
       
      saveAtRy <- atRy  # save for possible use on median panel.
        
   }   # end of i loop
   par(oldpar)
   # ____________________________PanelOutline____________________
 
   groupPanelOutline(panelGroup,j)
 
}

############################################


####
#
# type = 'segbar' and 'normbar'  ====================================
#
#  rlAreaSegBar   (Segmented Bar chart)
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
#  The data structure can have between 2 to 9 values per area.
#  Each area must have the same number of values. This limitation may be removed in the future.
#
#  Feature added to make each segment a different thickness. 1/4/2014
#
#  panelData => data.frame where each row is a area with the areaIUKey as the row.name.
#     The columns are the bar segment values.

#

rlAreaSegBar = function(j, SBnorm=FALSE) {
   #  j = the panel column number
   #  SBnorm  (FALSE = stacked,  TRUE = normalized)

   #   col1 indicates the starting or first column in the statsDFrame data for bar segment values.
   #   col2 indicates the ending or last column in the statsDFrame data.
   #
   #   The bar segment values are in the statsDFrame for each area in columns "col1" to "col2".
   #
   wstname     <- names(dat)   # names of columns in statsDFrame
   wstdim      <- dim(dat)
   wstMax      <- wstdim[2]    # number of columns in statsDFrame
   
   ErrFnd      <- FALSE
   pdColNum    <- formatC(j,format="f",digits=0,width=2,flag="0") 
  
   gName       <- "SEGBAR"
   if (SBnorm) gName <- "NORMBAR"
   
   # "col1"
   stColName1  <- wstname[col1[j]]
   #print("col1")
   
   pdUmsg      <- "(First Segment Data Column)"
   xr          <- CheckPDColnCN('col1', gName, col1[j], stColName1, j, 1, wstMax, dat, pdUmsg)
   #print(xr)
   if (xr$Err) { 
      ErrFnd <- TRUE 
   #} else { 
   #  xdat1 <- xr$Dat 
   }
        
   # "col2"
   stColName2  <- wstname[col2[j]]
   #print("col2")
   
   pdUmsg      <- "{Last Segment Data Column)"
   xr          <- CheckPDColnCN('col2', gName, col2[j], stColName2, j, 2, wstMax, dat, pdUmsg)
   #print(xr)
   if (xr$Err) {
      ErrFnd <- TRUE 
   #} else { 
   #   xdat1 <- xr$Dat 
   }
        
   if (!ErrFnd) {
     
      if (col1[j] >= col2[j]) {
          
          ErrFnd    <- TRUE
          warnCnt()
          xmsg      <- paste0("***020A ", gName, " ", pdColNum, " The first column name/number (", stColName1,") must proceed the last column name/number (", stColName2,") in the ", sDFName," data frame.")
          warning(xmsg, call.=FALSE)
         
      } else {
   
          wD      <- ( col2[j] - col1[j] + 1 )   # corrected to calculate the number of data columns
          if ( wD < 2 || wD > 9 ) {
             ErrFnd  <- TRUE
             warnCnt()
             xmsg <- paste0("***020B", gName, " ", pdColNum, " The number of segments is ", wD, ". It must be between 2 and 9. If over 9, only the first 9 will be used.")
             warning(xmsg, call.=FALSE)
          }
      }
   }

   if (ErrFnd) return ()               # error warning found - return
 
   stColNums <- c(col1[j]:col2[j])
   workSB    <- dat[,stColNums]        # get bar segment data from the statsDFrame.
   colNums   <- c(1:dim(workSB)[2])
    
   for (ind in colNums)  {             # check and convert each column
      iC         <- stColNums[ind]        #    get stDF column number
        
      stColNam   <- wSFName[iC]            #    get stDF column name
      F_ind      <- formatC(ind,format="f",digits=0,width=1)
      segNam     <- paste0("seg",F_ind)
      pdUmsg     <- paste0("(Bar segment ",F_ind," length)")
      
      x          <- CheckNum(workSB[,ind], gName, ind, pdColNum, segNam, stColNam, pdUmsg)

      if (x$Err) { 
         ErrFnd       <- TRUE 
      } else { 
         workSB[,ind] <- x$Dat 
      }
   }

   good        <- !is.na(rowSums(workSB))    # all good values.  if any are na 

   #
   
   refval      <- lRefVals[j]              # get referrence to object, changed 
   reftxt      <- lRefTexts[j]             # new - JP-2010/07/23

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
   #    thickness = constant  vs.  very based on 2 to 9th segment
   #
   
   #_______________Gather stats and put in area Order______________
  
   #  Sorting has already been done - by areaDatKey or value.
   #  The areaID list has therefore been re-ordered accordingly.  
   #  Reorder the DataList to match.  The assumption was that the input data order for the panelData 
   #  matched the order of the original data in the statsDFrame.
   #
   #cat("SBBar - areaDatKey:",areaDatKey,"\n")

   
   workMatSB   <- as.matrix(workSB)
   
   SBLen       <- apply(workMatSB,1,length)  # get length of each row.
   SBLRange    <- range(SBLen,na.rm=TRUE)

   NumSegs     <- SBLRange[2]                # number of segments (Max Length)
 
   SBBarPt     <- cbind(rep(0,numRows),workMatSB)
   SBBarPt     <- t(apply(SBBarPt,1,cumsum))
 
   #_______________Scaling____________
   
   # x scaling
   lPad        <- TRUE
   rPad        <- TRUE
 
   rMax        <- max(SBBarPt)
   if (SBnorm) {
      rx       <- c(0,100)
      lPad     <- FALSE
      rPad     <- FALSE
   } else {
      rx       <- c(0,rMax*1.02)
      lPad     <- FALSE
   }

   #cat("seg/normbar-rx:",rx,"\n")

   ry          <- c(0,1)
   
   pyPat       <- c(-0.5,-0.5,0.5,0.5,NA)
   py          <-  CSNBar.barht * pyPat     #  SNBar.barht = 2/3 (0.6667) (fixed)
          # py <- c( -1/3, -1/3, +1/3, +1/3, NA)
   
   # variable bar height calculations
   
   wYPdelta    <- (CSNBar.Last.barht - CSNBar.First.barht)/(NumSegs-1)  # increment
    
   wYP1        <- CSNBar.First.barht - wYPdelta
      
   # _____________ Color Patterns _______________
   
   baseColRgb  <- BuildSegColors(NumSegs) 

   # ___________titles and labeling axes_______________
   
   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  if segmented stacked - no padding on side with zero.
   #  if normalized stacked - no padding on either side.
   #
   
   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
  
   #cat("SN-staggering:",staggering,"  Result staggered:",staggered,"\n")
  
   #
   #####

   # ___________________drawing loop_____________________

   oldpar      <- par(lend="butt")
   
   #  build each panel for each stacked bar set.
 
   #printPar()

   #print(paste0("rx:",paste0(rx,collapse=" "),"  ry:",paste0(c(1-pad,ke+pad),collapse=" ")))
 
   for (i in 1:numGrps)  {
     
        gsubs  <- ib[i]:ie[i]               # get beginning to end index row number in this group  
        ke     <- length(gsubs)                # get number of rows in group  (5 or 1)  
        # adjust if median group      
      
        pen    <- if(i == medGrp & medGrpSize == 1) 7 else 1:ke        # if median group (6)(black), then pen=6, otherwise pen = c(1...x)   
       
        laby   <- ke:1 
        
        ksc    <- SetKsc(ke)
     	
        panelSelect(panels,i,j)
        x <- panelScale(rx,c(1-pad,ke+pad)) #   1 to 5 are the y values for each bar.
        panelFill(col=Panel.Fill.col)
 
        axis(side=1, tck=1, labels=F, at=atRx,
                     col=Grid.Line.col, lwd=Grid.Line.lwd) # grid
        
        # if a refval is provided and in the rx range, then add line.

        AddRefLine(refval, ke, rx)
        
        #
        #  Not checking "good" values provided.
        #
        
        #
        #  Process each area's line. 
        #
        for (k in 1:ke) {
           # cycle through row-groups and assign colors to associated areas dots.
       
           m     <- gsubs[k]

           if (good[m]) {
              wX    <- SBBarPt[m,]            # Get Row of data.
            
              if (SBnorm) {
                   wX    <- wX / wX[NumSegs+1] * 100   # last segment value is in NumSegs + 1 to get last column (end point)
              }
              
              #wYP  <- rep(laby[k],5)+py   # height of segment (laby[k] => center line of segbar)
              wYP   <- rep(laby[k],5)        # height of segment (laby[k] => center line of segbar)
         
              # calculate box for each segment
              wYPht <- wYP1
         
              for (ik in 1:NumSegs) {
                 if (SNBar.varht) {
                     # variable height bar segments
                     
                     wYPht <- wYPht + wYPdelta
                     wYP2  <- wYP + ((pyPat * wYPht) * ksc )
                     #print(paste0("Seg:",ik,"  wYP2:",wYP2))
                     
                 } else {
                     # fixed height bar segments
                     wYP2  <- wYP + (py * ksc)
                 }
                 
                 val0 <- wX[ik]     # start
                 val1 <- wX[ik+1]   # end position
                 wXP  <- c(val0,val1,val1,val0,NA)
                 
                 # good value - draw bars are polygons.  (why to polygon)
       
                 polygon(wXP,wYP2,col=baseColRgb[pen[k],ik],lwd=CSNBar.Outline.lwd,border=CSNBar.Outline.col,lty=CSNBar.Outline.lty) 
                 
                 #polygon(wXP,wYP2,col=CSNBar.Outline.col,density=0)
             
              } # end of ik loop (plotting Segments)
              #
              if (SNBar.Middle.Dot) {   # do we graph a middle dot on the row?
                 mY    <- laby[k]      # get Y position
                 # put dot on boundary if even number of segments or in middle of middle segment if odd.
                 if ((NumSegs %% 2) == 1) {
                     
                    # put dot in middle of middle segment.                 
                    mSeg <- NumSegs %/% 2 + 1
                    mX   <- (wX[mSeg] + wX[mSeg+1])/2   # middle of segment
                 } else {
                    # put dot on border between two middle segments.                 
                    mSeg <- NumSegs %/% 2
                    mX   <- wX[mSeg+1]
                 }
                 if (SNBar.MDot.pch >= 21 && SNBar.MDot.pch <= 25) {
                    #  treat filled and non-filled symbols differently - get close to same results.
                    #  with filled, fill is bg, col and lwd deal with border
                    #  with non-filled, fill is col, lwd deals with border using col.
                    #   filled symbol
                    
                    points(mX,mY,pch=SNBar.MDot.pch, cex=SNBar.MDot.pch.size, 
                             bg=SNBar.MDot.pch.fill,      # fill color  
                             col = SNBar.MDot.pch.border.col,    # border color 
                             lwd = SNBar.MDot.pch.border.lwd)
                 } else {
                    # non filled symbol
                    points(mX,mY,pch=SNBar.MDot.pch, cex=SNBar.MDot.pch.size, 
                             col = SNBar.MDot.pch.fill,   # fill and border color 
                             lwd = SNBar.MDot.pch.border.lwd)
                 }
              }  # end of Middle Dot drawing.
              
           } # end of "good" check for row.  
        }  # end of k loop     (group/row)
        # finish up panel
        
        panelOutline(Panel.Outline.col)
 
      } # end of i loop
  
  par(oldpar)
  # ____________________________PanelOutline____________________

  groupPanelOutline(panelGroup,j)

}




###################################################
#
#  For TS, and TSConf I could not find a way to  use to have areaDatKeys as the names of 
#  each area matrix, in list or data.frame.   So, the out at this time is
#  to assume the original panelData array is in the order of the original statsDFrame data.frame.
#  When statsDFrame is re-ordered, I have captured the re-ordering. Using the "order" index
#  the raw panelData is used via the order index to associate the line on the micromap to the data.
#   
#  Boxplot uses $names to look up to find out the record and link the Boxplot list to the 
#  statsDFrame data.
#
#
#####


#####
#
# type = TS and TSConf   =====================================================
#
# rlAreaTSConf  (Time Series with and without confidence interval in panel groups)
#
#     Plot all data for panel's areas as one graph in panel.
#


rlAreaTSConf = function(j,dataNam,conf=TRUE){
   #
   #  j = panel column number
   #
   #  dataNam = Name of large data array containing the x, y (or y low, med and high) values 
   #     for each time period and area.  Data element is three dimensions (area, sample, value)
   #     The area index is limited to 1:51.  The value index is limited ot 1:4.  
   #     The sample index is not limited, but a practical limit is around 200-250 samples.
   #
   #  conf = logical.  
   #    If TRUE, do the confidence band using y-low, y-med, and y-high values (columns 2, 3, 4)
   #    If FALSE, only plot the Y value (column 2)
   #
   #cat("TS - areaDatKey:",areaDatKey,"\n")

   
   ErrFnd               <- FALSE
   TSMsgLabel           <- "TS"
   if (conf) TSMsgLabel <- "TSCONF"
   pdColNum             <- formatC(j,format="f",digits=0,width=2,flag="0") 
   
   # Check data
   
   DataList = tryCatch(get(dataNam,pos=1),error=function(e) e)      # get name of array data object list.
   
   if (inherits(DataList,"error")) {    # default where = FALSE
        # error could not find the data.frame name in memory.
        ErrFnd  <- TRUE
        warnCnt()
        xmsg    <-paste0("***02T1", TSMsgLabel, " ", pdColNum, " column in data.frame ", dataNam, " does not exist or is not valid.")
        warning(xmsg, call.=FALSE)
        
   } else {
     
        # data.frame (r object) exists - can do other checks
        workDArr   <- DataList            # transfer the data to workDArr.
        wDArrNames <- rownames(workDArr)  # get rownames
  
        if (!is.array(workDArr))  {
            ErrFnd  <- TRUE
            warnCnt()
            xmsg    <- paste0("***02T2", TSMsgLabel, " ", pdColNum, " The ", dataNam, " data structured in the panelData field is not an array.")
            warning(xmsg, call.=FALSE)
        }
   
        dimDArr <- dim(workDArr)
   
        if (dimDArr[2] < 2 ) {
            ErrFnd   <- TRUE
            warnCnt()
            xmsg     <- paste0("***02T4", TSMsgLabel, " ", pdColNum," The ", dataNam, " array\'s 2nd dimension (time periods) must have at least 2 points.  It is ", dimDArr[2], ".")
            warning(xmsg, call.=FALSE)
        }
  
        if (conf) {   # TSCONF option.  
          
            # Time Series with Confidence Bands
            if (dimDArr[3] !=4) {
                ErrFnd  <- TRUE
                warnCnt()
                xmsg    <- paste0("***02T5", TSMsgLabel, " ", pdColNum, " The ", dataNam, " array\'s 3rd dimension is not 4.  It is ", dimDArr[3], ",")
                warning(xmsg, call.=FALSE)
            }
       
        } else {
            # Time Series without Confidence Bands
            
            if (dimDArr[3] < 2) {
                ErrFnd  <- TRUE
                warnCnt()
                xmsg <- paste0("***02TA", TSMsgLabel, " ", pdColNum, " The time series array\'s 3nd dimension must be at least 2.  It is ", dimDArr[3], ".")
                warning(xmsg, call.=FALSE)
            }
            
            if (dimDArr[3] != 2 && dimDArr[3] != 4) {
               # accept confidence data - don't stop run.
                ErrFnd  <- TRUE
                warnCnt()
                xmsg    <- paste0("***02T6", TSMsgLabel, " ", pdColNum, " The time series array\'s 3rd dimension must be 2 or 4. It is ", dimDArr[3], ".")
                warning(xmsg,call.=FALSE)
            }
        }
  
        if (is.null(wDArrNames)) {  # names are not present
            ErrFnd  <- TRUE 
            warnCnt()
            xmsg    <- paste0("***02TB", TSMsgLabel, " ", pdColNum, " The time series array does not have rownames assigned to the 1st dimension. Data cannot be paired up with area.")   
            warning(xmsg, call.=FALSE)
        } else {
            tnn <- is.na(match(wDArrNames,areaDatKey))
            if (any(tnn)) {   # non-match found.
                 ErrFnd  <- TRUE
                 warnCnt()
                 lnn     <- paste0(wDArrNames[tnn],collapse=" ")
                 xmsg    <- paste0("***02T7", TSMsgLabel, " ", pdColNum," Rownames on array do not match subarea ID list. The bad area IDs are:",
                                        lnn)
                 warning(xmsg, call.=FALSE)
            }
        } 
   }

   if (ErrFnd) return ()                # if any errors found - don't draw column.
  
   refval   <- lRefVals[j]              # get referrence to object, changed 
   reftxt   <- lRefTexts[j]             # new - JP-2010/07/23

   # structure of dataArr
   #     dataList is a 3 dim array :
   #          a * b * c, where: 
   #          a is the area index number (1 to "n") (sub-area)
   #          b is the time period index (2 to "n" range) (Limited only by R and memory)
   #          c is the type of value (1=x, 2=low, 3=mid, 4=high) or (1=x, 2=y)
   #
      
   
   #_______________Scaling of TS Axis____________
   
   # x scaling
   lPad        <- FALSE
   rPad        <- FALSE
   
   rx          <- range(workDArr[,,1],na.rm=TRUE)           # x range from all values in vector
   #cat("ts-rx:",rx,"\n")
   
   #rx         <- sc*diff(rx)*c(-.5,.5)+mean(rx)        # min to max range with expansion factors.
   #cat("ts-rx after padding:",rx,"\n")
   
   # y scaling                  
   if (conf) {
        # range of line, high and low.
        ry    <- range(workDArr[,,c(-1)],na.rm=TRUE)       # range of all Y values
   } else {
        # range of line.
        ry    <- range(workDArr[,,2],na.rm=TRUE)           # range for the one Y value
   }
   #cat("ts-ry:",ry,"\n")
  
   ry         <- sc*diff(ry)*c(-.5,.5)+mean(ry)        # min to max range with expansion factors.
   #cat("ts-ry after padding:",ry,"\n")
   
   #_______________Find range/min/max of median row line/high/low.____________
   
  
   #_______________Gather stats and put in area Order______________
  
   #
   #   JP-no data in col1, col2, or col3 to sort like the other columns... 
   #     All of the data is in these structures.
   #   
   #   at present no re-ordering of the time series like the other plots.
   #   JP-if other column is sorted, time series will follow that order via the indexes.
   #

   ####
   
   # ____________column titles and axis_______________

   #####
   #
   #  Setup and draw top and bottom titles and axis for column
   #
   #  TS, TS-Conf no padding on either side - graph starts at first data point to last data point.
   #   Check out this effects labeling.
   #

   Res         <- DrawXAxisAndTitles(j, panels, rx, ry,reftxt, refval, leftPad=lPad, rightPad=rPad, YAxisPad=TRUE)

   atRx        <- Res$atRx
   rx          <- Res$rx
   ry          <- Res$ry
  
   #cat("Ts-Result staggering:",staggering,"  staggered:",staggered,"\n")
  
   #
   #####


   oldpar      <- par(lend="butt")
 
   #####  Can be done once for all interations of loop.
   YAxis_cex <- TS.Axis.cex * 0.75
   xPs       <- par("ps")
   xHPsLU    <- strheight("00000",cex=1,units="user")
   xHDesPsLU <- strheight("00000",cex=YAxis_cex,units="user")
   xDifHLU   <- xHPsLU - xHDesPsLU
   YAxis_adj <- xDifHLU / xHPsLU
   #cat("YAxis adjustment - YAxis_adj:",YAxis_adj,"  YAxis_cex:",YAxis_cex,"\n")
  
  
   # _______________drawing loop (panels 1->11)___________________

   for (i in 1:numGrps) {      #   1,2,3,4,5,    6,     7,8,9,10,11    ng=11  (for US)

      # Cycle through the Row/Groups in the micromap column
      
      gsubs    <- ib[i]:ie[i]               # get beginning to end index row number in group  
      ke       <- length(gsubs)                # get number of rows in group  (5/6 or 1)  

      pen      <- if(i == medGrp & medGrpSize == 1) 7 else 1:ke        # if middle group (7), then pen=7 (Black), otherwise pen = c(1...5) or c(1...6)   
      
      kcol     <- c(mstColors[c(1:ke,7)])                # get major colors

      addBlack <- 0      
            
      if (medGrp > 0 & medGrpSize == 1) {
         if (i == (medGrp-1)) { 
              # panel before the median row
              gsubs    <- c(gsubs,ib[i+1]:ie[i+1]) # extend one more to get median row
              addBlack <- 7
         }
         if (i == (medGrp+1)) {
              # panel after the median row
              gsubs    <- c(gsubs,ib[i-1]:ie[i-1]) # extend to include at end of the list
              addBlack <- 7
         }
      } 
     
      gnams <- areaDatKey[gsubs]            # get list of area ids for data group of data.

      # adjust if middle group      
    
      if ( addBlack > 0 ) pen <- c( pen, 7 )
      
      #  do panel - 
      panelSelect(panels,i,j)           # select panel for group i in column j)
      panelScale(rx,ry)                 # set scale for panel  (should this be ry * 5 or 1?)
                                        # scale x and y to the shape of the panel (6 - median is squeezed.)
      panelFill(col=Panel.Fill.col)     # set fill for panel
      
      # draw grid lines in panel - vertical (x axis)
      axis(side=1, tck=1, labels=F, at=atRx,
                   col=Grid.Line.col, lwd=Grid.Line.lwd) # grid lines (x axis)
      
      if (i == medGrp & medGrpSize == 1 ) {  # median panel
        
          # median panel 
          atRy    <- c(saveAtRy[1],saveAtRy[length(saveAtRy)])    # median panel range (Get copy of first and last number)  
        
      } else {
        
          # all other panels 
          atRy    <- panelInbounds(ry)   # get labels for y-axis
     
      }
      if (TS.hGrid) {   # horizontal grids on Y axis
           axis(side=2,tck=1,labels=F,col=Grid.Line.col,lwd=Grid.Line.lwd, at=atRy) # Grid lines
      }
     
      ## Y axis values and labels
      #axis(side=2, tick=F, mgp=mgpLeft, cex.axis= TS.Axis.cex*.75 , 
      #       at=atRy, labels=as.character(atRy)) # Y axis labels
      #mtext(lab4[j],side=2,line=Title.Line.5.pos,cex=TS.Axis.cex)  # Y axis title
      #
       
        
      axis(side=2, tick=F, cex.axis=YAxis_cex, mgp=mgpLeft, line= -YAxis_adj*0.3,
             at=atRy, 
             labels=as.character(atRy))
      mtext(lab4[j],side=2,
             line=Title.Line.5.pos,
             cex=TS.Axis.cex)

      panelOutline(col=Panel.Outline.col)     # outline panel
   
      #####
      # Issue with median row - line drawing.  The y axis is squeezed
      # to about 1/5 of the scale used in the other rows.  This distorts
      # the line graph and any confidence band.
      #####
     
      #####
      #
      #  Current take each row and:
      #       draw confidence (if required)
      #       draw line
      #     next row.
      #  This leads to confidence overlaying the lines of rows.   - need to do confidence blocks, then all lines.
      #  Change Sept 1, 2015
      #
      #####
      
      
      # handle confidence bands
      if (conf) {
      
         for (k in 1:ke) {                  # Process each slot of panel - step 1 to 5 or 1 to 1
      
            # cycle through row-groups and build each time series
               
            kp = pen[k]          # color number
        
            wDArr <- workDArr[gnams[k],,]
               
            wX   <- wDArr[,1]    # get X values for line and polygon plots
            wLine = wDArr[,2]    #  Get Y values for mid line 
                   
            #  build polygon of confidence band to fill (y-low to y-high) and draw first.
                  
            # new logic to handle NA in X or Y data.  Have to break up the polygons into separate plots.  
               
            cX  <- c(wX,NA)
            cY1 <- c(wDArr[,3],NA)     # lower Y data points
            cY2 <- c(wDArr[,4],NA)     # upper Y data points
                  
            #cat("cY1:",paste0(cY1,collapse=", "),"\n")
            #cat("cY2:",paste0(cY2,collapse=", "),"\n")
            #cat("cX :",paste0(wX ,collapse=", "),"\n")
         
            Breaks  <- is.na(c(cX+cY1+cY2))
            #cat("Breaks:",paste0(Breaks,collapse=", "),"\n")
            
            # we found at least one NA in the data.
            # at X find the L and U Ys.
        
            wXz  <- MMVSplit(wX, Breaks)
            wY1z <- MMVSplit(cY1,Breaks)
            wY2z <- MMVSplit(cY2,Breaks)
         
            #cat("wY1z:",paste0(wY1z,collapse=", "),"\n")
            #cat("wY2z:",paste0(wY2z,collapse=", "),"\n")
            #cat("wXz :",paste0(wXz ,collapse=", "),"\n")
        
            vL <- length(wXz)  # if only one list - then length = 15 installed of one. *****************
        
            #cat("vL:",vL,"\n")
        
            #  draw confidence shades
            for (ind in c(1:vL)) {
               if (length(wXz[[ind]])>0) {
                  xL <- c(wXz[[ind]], rev(wXz[[ind]] ), NA)
                  yL <- c(wY1z[[ind]], rev(wY2z[[ind]]), NA)
                  wPoly <- data.frame(x=xL, y=yL)
                  
                  #print(wPoly)
                  #cat("colors:", mstColors[kp+12],"  kp+12:", kp+12,"\n")
                   
                  polygon(wPoly, col=mstColors[kp+12], border=NA)
               }
            }
         
            # shaped polygons of confidence band have been plotted.
                   
         }  # end of k loop rows.
      
      }  # end of confidence test.       
     
      
      #  draw lines
      
      for (k in 1:ke) {                  # Process each slot of panel - step 1 to 5 or 1 to 1
     
         # cycle through row-groups and build each time series
         
         kp = pen[k]          # color number
      
         wDArr <- workDArr[gnams[k],,]
         
         wX   <- wDArr[,1]    # get X values for line and polygon plots
     	 wLine = wDArr[,2]    #  Get Y values for mid line 
              
         #  Plot mid Line
         lines(wX,wLine,col=mstColors[kp],lwd=TS.lwd)
         
         #   NA processing,  in the lines call, the missing point (x,y) is just not drawn or other points connected to it.
         #    a gap is generated.
     
      }  # end of k loop rows.
                
      saveAtRy <- atRy
   }
   
   par(oldpar)
   # ____________________________PanelOutline____________________

   groupPanelOutline(panelGroup,j)

}

#####
#
#
#####  end of glyph functions  #####
#
#
#############################
#############################

#print("Glyph functions loaded")

#############################
#############################
#
#
#   General Functions for micromapST and glyphs
#

#
#  AddRefLine - adds the reference line to the current panel (wKe).
#

AddRefLine <- function (wRefVal, wKe, wRx) {
     if (!is.na(wRefVal))  {
        if(is.between.r(wRefVal,wRx)) {
           # reference line
           lines(rep(wRefVal,2),c(1-padMinus,wKe+padMinus),lty=Ref.Val.lty,lwd=Ref.Val.lwd,col=iRef.Val.col)
        }
     }
  }

#
#_________ function to pattern match alias names
#
AliasToIndex <- function(xR,aNAIAlias) {
   #
   #   xR is the string list, aNAIAlias is the Name Table $ Alias column 
   #   return index into the NAI table
   #   The user string must be cleaned up to make sure it can match one of the wildcard alias strings.
   #   The user strings are edited to convert any punctuation marks, control characters, spaces, tabs, cr, etc.
   #   into blanks, multiple blanks, leading and trailing blanks are eliminated and the string is converted to 
   #   all uppercase.
   #   
   
   # xR --> a vector of the registry names from SeerStat output
   wReg     <- CleanString(xR)
   wIndex   <- rep(NA,length(wReg))                  # match results - NA default - no match 
  
   # wild card match of input character vector to alias in name table.
   
   xouta    <- t( sapply(c(1:length(aNAIAlias)), function(x) { 
                                      y=grep(aNAIAlias[x],wReg,ignore.case=TRUE)  # user string list against each entry.
                                      ifelse((length(y) == 0),return(c(NA,NA)),return(c(x,y)))  # if result length = 0 -> no match. otherwise return the string and index.
                                      }
                  ))
   # result - matrix is column 1 = aNAI index that matched, column 2 = index into char vector .
   xoutb    <- xouta[!is.na(xouta[,1]),]  # keep only matches.
   
   wIndex[xoutb[,2]]  <- xoutb[,1]
   
   wMissing <- is.na(wIndex)
   wMissingList <- paste0(xR[wMissing],collapse=", ")
   
   #if (any(wMissing)) {
   #
   #    xmsg    <- paste0("***0195 ALIAS Alias Name(s) in the data does not match the name table for the area. The unmatched data rows are:",wMissingList)
   #    stopCnt()
   #    stop(xmsg, call.=FALSE)
   #
   # } 
   
   # let duplicate and missing through.  Handled by caller.
   
   return(wIndex)    # return index to name table
}   

#
###

###
#
#_________ function to pattern match alias names
#
AliasToKey <- function(xR,aNAI) {
   #   xR is the string list, aNAI is the Name Table 
   #   return index into the NAI table
   
   # x --> a vector of the registry names from SeerStat output
   ErrFnd     <- FALSE
   
   wReg       <- toupper(xR)
   wIndex     <- rep(NA,length(wReg))                  # NA results of keys
   wKey       <- rep(NA,length(wReg))                  # NA results of keys
   xout1      <- sapply(c(1:length(aNAI$Alias)), function (x) grep(aNAI$Alias[x], wReg, ignore.case=TRUE))
      # one entry per aNAI row,  NA or # of wReg Row of match.
   xout1a     <- unlist(xout1)  # list of matched locations for each item.
      # NA's and lists removes, just a list of matches.
      
      
   #   Get list of those items that did not find a match. - find list of wReg item that did not match.
   xout2      <- !is.na( lapply( xout1, function(x)   ifelse(length(x) == 0,NA,x)             ) )
       # xout2 is converts results from "" into NA. 
   xout3      <- unlist( lapply( xout1, function(x) { if(length(x[]) > 1) { x } else { NA } } ) )
       # xout3 is string or NA - string if no match.
       
   if (any(!is.na(xout3))) {
      ErrFnd  <- TRUE 
      StopFnd <- TRUE
      xout4   <- paste0(xout3[!is.na(xout3)], collapse=" ")
      xmsg    <- paste0("***0196 ALIAS Sub-area names in the data have duplicate name in rows:",xout4, " Only one row per sub-area is permitted.\n")
      stopCnt()
      stop(xmsg, call.=FALSE)
   }
   
   wIndex[xout1a] <- aNAI$Key[xout2]
   
   wKey[xout1a] <- aNAI$Key[xout2]
   
   return(wKey)    # return list of abbreviates or NA if no match.
}   

#
###

###
#
# Function to generate the segment blended colors for the stacked bar glyphs.
#   It takes the base 5 or 6 colors used in the maps and other glyphs
#   and generates a progression of light to full color for use in the 
#   segments of a stacked bar glyph.
#
      
BuildSegColors <- function(NumSegs) {

   #  Build color patterns for all bar charts
   baseColors  <- t(grDevices::col2rgb(mstColors[1:7]))    #  "#ffffff" to x, y, z  
   bgColors    <- t(grDevices::col2rgb("white"))
   
   #  New Way with lighter colors - but opaque 

   x1         <- cumsum(rep(1/NumSegs,NumSegs))          #  x1 vector of accum values from 1/NumSegs  to 1, NumSegs values.
   x2         <- x1 ^ 1.9                                #  raised by 1.9 (exponential curve
   pInc       <- (x2 * 0.6) + 0.4                      #  multiply and shift (want to run from 0.4 to 1.

   # baseColors -- base 255...
   baseCol2   <- baseColors/255   # convert each value from 0:255 to 0:1

   # baseCol2[Colors,RGB]
   #  Apply the pInc (5,1) modifier vector to each color (1,7). -> full color table (5,7) 
   baseCol3   <- sapply(pInc,function(x) baseCol2 * x)  # mstColors(1-7),  segment(1-5) for (Rgb=RED)
                                                        # mstColors(8-14), segment(1-5) for (Rgb=GREEN)
                                                        # mstColors(15-21),segment(1-5) for (Rbg=BLUE)

   # baseCol3[(Colors-Red,Colors-Grn,Colors-Blu),Segments]
   
   baseColMod <- array(baseCol3,c(7,3,NumSegs))       # we only use the first 7, so ignore 8, 9, 10 (shading colors)
                         #   [x,,]   x = color (1-7)
                         #   [,,y]   y = segment (1-5)
                         #   [,z,]   z = RGB 1=RED, 2=GREEN, 3=BLUE
                         #
                         #   [1,2,3]   1 fills first, 2 fills next, 3 fills last.
                         #  
   
   # invert the modifier vector and apply it to the white background colour (for BW images)
   pIncM      <- 1-pInc
   bgCol2     <- bgColors/255
   bgCol3     <- sapply(pIncM,function(x) bgCol2 * x)   # [rgb,segment]
   bgColMod   <- t(bgCol3)                            # [segment, rgb]
   #  bgColMod[Segments,RGB]   (Segment =5 ==> 0)p
   #   NumSegs, RGB value
   
   baseColRgb <- matrix(rep(0,7*NumSegs),nrow=7,ncol=NumSegs)   
   #  baseColRgb[Colors, Segment]
   
   # Convert Rgb matrix back to a matrix of segment by color.
   for (isg in 1:NumSegs) {  # [,,isg]   Level
      
       for (icl in 1:7) {  # colors   [icl,,]
         
           wC <- baseColMod[icl,,isg] + bgColMod[isg,]          
           baseColRgb[icl,isg] <- grDevices::rgb(wC[1],wC[2],wC[3])
       }
   }
   #
   #  Resulting colors are in baseColRgb[color,segment]
   #
   #  Now I have a matrix of colors - [x,y] where
   #   x is the color base - 1 to 7 (we use 1 to 6).
   #   y is the level based on the number of segments = 1 : NumSegs
   #
   #   rows - color ID
   #   columns - segment 1:x

   # result => baseColRgb [color (1:7), segmentNum (1:n)]
   return(baseColRgb)
   
 }  

#
###

###
#
#  Subroutine to take values in the col<x> vectors (panelDesc variable), 
#   convert numerics to integer, convert character (column names)  
#   by matching with statsDFrame column names to statsDFrame column numbers.   
#   NA's (no name match) and out of range numbers are set to "0" - NOT VALID. 
#
#  Used to check column specifications for sortVar, rowNamesCol and colx variables during 
#  initial setup.   By the time the glyphs runs, the col1,...,col3 variables are translated
#  into column numbers and no long needs to be checked.  Except to validate they exist when needed.
#
#  This routine takes any number/name of columns provided by user and validates it and translates to 
#  column number.   Will not translate "NA", missing, "" or "0" values.  glyph will test if 
#  data is missing.
#
#  This routine does a general check of a named list of statsDFrame column names or numbers. 
#  At the end of the verification, the names are translated into statsDFrame column numbers.
#  
#  The caller should save the original named list vectors for diagnostic messages.
#
#  Used mostly used by sortVar, rowColName, and other arguments.
#
#####
#
#    CheckColx appears to not be used any more.   Verify.
#
#####
#
#
CheckColx <- function(wcol, colname, wnam2, len_wnam) 
   {
     # wcol     = col vector of names/number in statsDFrame from panelDesc
     # colname  = literal character name of col vector for error message.(panelDesc variable name (col1, col2, col3))
     # wnam2    = character list of column names and row numbers (in character format)
     # len_wnam = number of original set of columns. (length(wcol)
     #
     # Results Rules:  "0" means invalid number, out of range number or invalid name.
     #                 NAs are converted to "0" values.
     #                 glyphs check for valid values based on need.  
     #
     #
     xwcol   <- wcol
     l_wcol  <- length(wcol)
     ErrFnd  <- FALSE
          
     if (is.factor(xwcol)) 
       { xwcol <- as.character(xwcol) }
        
     if ( is.numeric(xwcol) || is.logical(xwcol) ) {  # have number (double, single, integer, or logical)
         # we are dealing with numeric or logical
         
         rcol              <- as.integer(xwcol)  # convert numeric to integer.
         rcol[is.na(rcol)] <- 0     # get rid of NA.  Turn to zeros doesn't get rid of negatives.
      
         if (any(rcol < 0))
           {
             ErrFnd  <-  TRUE
             xmsg    <-  paste0("***0201 PDCOL In the ",colname," named list in the ", pDName," panelDesc structure there are one or more negative values: ",paste0(rcol,collapse=", ")," Literal:",wcol)
             warnCnt()
             warning(xmsg,call.=FALSE)
           } else {
             if (any(rcol > len_wnam))
               {
                 ErrFnd  <- TRUE
                 xmsg    <- paste0("***0202 PDCOL One or more of the values in the \var{<pdVarName>} named list in the \var{<panelDesc>} structure is greater than the columns in the \var{<statsDFrame>} data.frame: ",paste0(rcol,collapse=", "))
                 warnCnt()
                 warning(xmsg,call.=FALSE)
               } 
           
           }
         # if ErrFnd = FALSE, the all number in vector are within range.
         # check valid range in glyph  (NA become zeros.) Leave the final check to the glyphs.
       } else {     
         if (is.character(xwcol))
           {  # have character - may be name or number - check each
              # get number for other code, if column name.

              xcol <- match(xwcol,wnam2,nomatch=0)             # match against column names and numbers (as characters)          
              rcol <- ifelse(xcol > len_wnam, xcol-len_wnam, xcol)  # adjust matches to row numbers to real row numbers.       

              # name and character number converted to integer       
              # bad and NA values are "0" and will be caught in the glyph    
           
           } else {
              # invalid variable type
              ErrFnd  <- TRUE
              xmsg    <- paste0("***CCOL-03 The type of ",colname," panelDesc variable is invalid. ",typeof(xwcol),".  Must be integer or character.")
              warnCnt()
              warning(xmsg,call.=FALSE)
           } 
      }
     if (ErrFnd)
       { 
         return (rep.int(0,l_wcol))
       } else {
         # clean up any NAs in list, set to 0
         rcol[is.na(rcol)]  <- 0   # set NA to 0 (invalid)
         return (rcol)
         #print(rcol)
       }
        
   }        

#
###

###
#
#

CheckColx2 <- function(colValues, varName, varNum, gNameList, stColNames, len_sCN)  {
   
     # xx  <- gsub(",","",<value>,fixed=TRUE)
     # gc4real <- "^[-+]?[ ]?[0-9]{1,3}(,[0-9]{3})*(\\.[0-9]*)?$|^[-+]?[ ]?[0-9]*(\\.[0-9]*)?$"  # is real number with commas
     
     gc4int  <- "^[-+]?[ ]?[0-9]{1,3}(,[0-9]{3})*$|^[-+]?[ ]?[0-9]*$"                          # is integer number with commas
     
     #cat("colValues:",paste0(colValues,collapse=", "),"\n")
     #cat("varName  :",varName,"\n")
     #cat("varNum   :",varNum,"\n")
     #cat("gNameList:",paste0(gNameList,collapse=", "),"\n")
     #cat("stColNames:",stColNames,"\n")
     #cat("len_sCN  :",len_sCN,"\n")
     #
     #
     # Routine is used to check out the information provided by the user.  If 
     # vector contains a number or a character string, it will validate the number against
     # the column number range of statsDFrame.  If a character string vector is provided,
     # each items is checked for being a number or non-number string.  If numeric,
     # the value is converted to integer and validated for <= 0 and range.  If a non-numeric
     # string, the string to match against the column names on statsDFrame and translated to 
     # the column number.  "" and NA values are ignored and not translated or matched.
     # If a string does not match, then it resolves to NA.  "" are converted to NA.
     #
     # The glyphs are left to determine if all of the needed data columns are provided.
     # This only validates the information present.  If the data column is not used, we don't care.
     #
     # pdVarData - colValues  = character or numeric vector of column names/numbers in statsDFrame.  
     #               Can be a list from sortVar, rowColName, or panelDesc col1, col2, or col3..
     # pdVarName    = name of variable - vector being checked. (used in messages.). (example: col1, col2, sortVar, rowColName, etc.)
     # pdVarNum     = 3rd character in message identifiers: "0" for sortVar and rowColNames and 1 to 3 for panelDesc columns
     # gNameList  = associated "type" list of glyphs per entry in vector.  Used in messages.  Must be the same length as
     #               colValues.  For sortVar and rowColName, this parameter is set to "".
     # stColNames = character list of column names and numbers (in character format) (statsDFname column names)
     # len_sCN    = number of original number of columns.  dim(statsDFrame)[2]  The stColNames list is 2 x this value.
     #
     # Rules:   Not provided = "" and NA.  
     #          "0" means invalid number or name.  Error message already generated.
     #          glyphs check for valid values based on need. We just make sure the column has a 
     #           a valid reference and can be accessed. Not valid content.
     #
     #  Working variables:
     #    FvarNum = 1 character version of varNum (if positive) othersize set to "0"  (single value)
     #
     #cat("len_sCN:",len_sCN,"  varNum:",varNum,"\n")
     
     
     ErrFnd      <- FALSE                    # no errors indicator
     
     xwcol       <- colValues                # get working copy of panelDesc contains of a variable list.
     l_xwcol     <- length(colValues)        # length of variable contents vector
     l_gNameList <- length(gNameList)        # length of type list (number of glyphs)

     rcol        <- rep(NA,l_xwcol)          # results column number list.

     if (varNum >= 0) {
         FvarNum <- formatC(varNum,format="f",digits=0,width=1)
     } else {
         FvarNum <- "0"
     }

     if (l_xwcol != l_gNameList) {
        # panelDesc variable not same length as number of types
        if (l_gNameList == 0)  { 
           # gNameList is absent - possible sortVar or rowColNames arguments.
           l_gNameList <- l_xwcol
        } else {
           # error - they should be the same length, possible type-o in variable list.
           ErrFnd <- TRUE
           warnCnt()
           xmsg <- paste0("***0205 ",gNameList," The length of the glyph type list is different the length of the variables list.")
        }
     }
    
     #cat("l_xwcol:",l_xwcol,"  len_sCN:",len_sCN,"  varNum:",varNum," FvarNum:",FvarNum,"\n")
     #print(stColNames)
     
     FndNames <- rep(TRUE,l_xwcol)
     
     skipList <- (is.na(xwcol) | xwcol == "")   # no values provided in entry  ("", "" from ,,  or NA)
     
     if (is.factor(xwcol))  {
        # if a factor convert to character  - should not be a factor if it's a numeric,
        xwcol <- as.character(xwcol)    # remove factor index.
     }
     if (is.numeric(xwcol)) {  
        # have number (double, single, integer)
        
        rcol              <- as.integer(xwcol)     # convert numeric to integer (column indexes are integers)
                                                   # NA values still show up as NA.   Still need to validate.
    
        #print("numeric")
     } else {
        if (is.character(xwcol)) {
    
           xTcol           <- grepl(gc4int,xwcol)  # T/F vector of type of string   T=Numeric
           xTcol[skipList] <- FALSE
    
           if (any(xTcol)) {
              rcol[xTcol]  <- as.integer(xwcol[xTcol])   # convert chars to numbers 
           }
     
           xTcol           <- !xTcol               #  reverse flags     T = character
           xTcol[skipList] <- FALSE                # don't check any NA fields.
       
           if (any(xTcol)) {
     
              #  check all, but we will only update the character ones.
              xcol         <- match(xwcol,stColNames)                   # match against column names and numbers (as characters)
                 # and translate column names and numbers to integers.
              
              xcol2        <- ifelse(xcol>len_sCN,xcol-len_sCN,xcol)    # adjust matches to verified/matched column numbers to real column numbers.       
              
              rcol[xTcol]  <- xcol2[xTcol]
              
              FndNames[xTcol] <- !is.na(xcol)[xTcol]  
           }
        } else {
           # invalid type of vector, numeric, integer, or character
           
           # is this possible because prior checks???
           stopCnt()
           StopFnd    <- TRUE
           xmsg       <- paste0("***02",FvarNum,"0 PDCOL The ",varName," named list is type ",typeof(xwcol)," is invalid. Must be a numeric, integer or character vector.")
           stop(xmsg, call.=FALSE)
          
        }
     }
     # validate
     #print("xwcol")
     #print(xwcol)
     #print("rcol")
     #print(rcol)
     #cat("FndNames:",FndNames,"\n")
     #cat("len_sCN:",len_sCN,"\n")
    
     for (ind in c(1:l_xwcol)) {
        # validate each one - rcol.
        pdColNum <- formatC(ind, format="f",digits=0, width=2, flag="0")
        
        xRcol <- rcol[ind]
        if (!is.na(xRcol)) {
           #cat("ind:",ind,"  xRcol:",xRcol,"\n")
        
           if (xRcol <= 0) {
              warnCnt()
              ErrFnd      <- TRUE
              xmsg        <- paste0("***02",FvarNum,"2 PDCOL ",gNameList[ind]," ",pdColNum," The column number of ",xwcol[ind]," in '",varName,"' is negative or zero. Must be a positive integer or the name of a column in statsDFrame.")
              warning(xmsg,call.=FALSE)
              rcol[ind]   <- 0
           } else {
              if(xRcol > len_sCN) {
                 warnCnt()
                 ErrFnd      <- TRUE
                 xmsg        <- paste0("***02",FvarNum,"3 PDCOL ",gNameList[ind]," ",pdColNum," The column number of ",xwcol[ind]," in '",varName,"' is greater than the number of columns ",len_sCN," in the statsDFrame data.frame.")
                 warning(xmsg,call.=FALSE)
                 rcol[ind]   <- 0
              }
        
           }
        }
        if (!FndNames[ind]) {
           # did not find the name in statsDFrame
           warnCnt()
           ErrFnd      <- TRUE
           xmsg        <- paste0("***02",FvarNum,"1 PDCOL ",gNameList[ind]," ",pdColNum," The column name of ",xwcol[ind]," in '",varName,"' does not exist in the statsDFrame data.frame.")
           warning(xmsg,call.=FALSE)
           rcol[ind]   <-  0
        }      
     } 
     return(rcol)
}
     
#
###
     
CheckParmColx <- function(colNames, parmCode, wSDFNames, len_wSDFNames) 
   {
     #  This function validates the statsDFrame column name/numbers for call arguments/parameters.
     #  It is essentually the same function as CheckColx, but does not generate error messages
     #  related to panelDesc variables or lists.  If the list of names/numbers is limited to "N", 
     #  then this check is done prior to calling this function.
     #
     #  Used by sortVar and rowNamesCol argument checks
     #
     # colNames     = col Name vector of names/number in statsDFrame from panelDesc
     # parmCode     = is a vector containing the error message identifier and string and the parameter name. 
     #     parmCode[1] = second part of the "CARG-" tag. 
     #     parmCode[2] = name of the calling argument/parameter
     #           c("RNC","rowNamesCol")
     #     Any invalid names/numbers are passed by as 0. 
     # wSDFNames    = character list of column names and numbers (in character format)
     # len_wSDFNames= number of original set of columns. (length(wcol)
     #
     # Results Rules:  "0" means invalid number, out of range number or invalid name.
     #                 NAs are converted to "0" values.
     #                 glyphs check for valid values based on need.  
     #
     # The check for zero length value is done before the call to this routine.
     #
     
     xColNames   <- colNames
     l_wcol      <- length(colNames)  # number of values
     ErrFnd      <- FALSE
    
     if (l_wcol == 0) {
        ErrFnd   <- TRUE
        xmsg     <- paste0("***0124 CARG-",parmCode[1]," The ",parmCode[2]," call argument is empty.  Argument ignored.")
        warnCnt()
        warning(xmsg, call.=FALSE)
        res      <- NA
     } else {
        # number of values are 1 or more
        
        res      <- rep(0,l_wcol)   # default results are none found.
     
        if (is.factor(xColNames)) 
           { xColNames <- as.character(xColNames) }   # if factors make then non-factored..
           
        if ( is.character(xColNames) || is.numeric(xColNames) ) {
    
           # what are each element in the vector - a number or name?
        
           xColType <- unlist(gregexpr("^[ \t]*[+-]?[0-9]*[ \t]*$",xColNames))    # find out number or name
        
           #  NA - NA (missing)
           #  -1 - Character
           #   1 - Number
           
           #print("parameter value to check:")
           #print(xColNames)
           #print(xColType)
           
           # Loop through list and check each one based on its type.
           for (ind in c(1:l_wcol)) {
              # get value type
              wCT    <- xColType[ind]    # get type
              wCName <- xColNames[ind]   # get value
           
              if (is.na(wCT)) {
                # NA value - pass it back to caller as 0 - not found.
                res[ind] <- 0
              } else {
                if (wCT < 0) {
                
                  # have character type value (not a just numbers) - should be column name
                  
                  wColN   <- match(wCName,wSDFNames,nomatch=0)                          # match against column names and numbers (as characters)          
                  wColN   <- ifelse(wColN > len_wSDFNames, wColN-len_wSDFNames, wColN)  # adjust matches to row numbers to real row numbers.       

                  res[ind] <- wColN     # save resulting column index number
                  
                  # check if column name found.       
                  
                  if (wColN <= 0) { 
                     # if it was a no match ...
                     ErrFnd     <- TRUE
                     xmsg       <- paste0("***0123 CARG-",parmCode[1]," A column names in the ",parmCode[2], " call argument does not exist in the ",sDFName," data.frame:", wCName)
                     warnCnt()
                     warning(xmsg,call.=FALSE)
                  }  # end of name valid check.   
                
                } else {
                
                  # numeric value - (integer, numeric, or character format) - convert and check                 
  
                  wColN <- as.integer(wCName)      # convert to number
                  
                  if (is.na(wColN)) {
                    # string did not convert to integer - Error.  (unexpected since we validated character string first.
                    ErrFnd  <-  TRUE
                    xmsg    <-  paste0("***0125 CARG-",parmCode[1]," A column index number in the ",parmCode[2]," call argument did not convert from character to integer: ",wColN)
                    warnCnt()
                    warning(xmsg, call.=FALSE)
                 
                  } else {
                    if (wColN < 0) {
                      ErrFnd  <-  TRUE
                      xmsg    <-  paste0("***0120 CARG-",parmCode[1]," A column index number in the ",parmCode[2], " call argument is a negative or zero: ",wColN)
                      warnCnt()
                      warning(xmsg, call.=FALSE)
                    } else {
                      if (wColN > len_wSDFNames) {
                        ErrFnd  <- TRUE
                        xmsg    <- paste0("***0121 CARG-",parmCode[1]," A column index number in the ",parmCode[2]," call argument is a greater than the number of columns in ",sDFName," data.frame: ", wColN)
                        warnCnt()
                        warning(xmsg, call.=FALSE)
                      } else {
                       
                        res[ind] = wColN   # save the column number
                      }  #  end of range check.
                    }  #  end of neg check.
                  }  # end of integer convert error check
                }  # end of char vs numeric check
              } # end of NA vs other type check
           }  # end of for loop
       
        } else {
           # invalid variable type
           ErrFnd     <- TRUE
           xmsg      <- paste0("***0122 CARG-",parmCode[1]," The call argument/parameter, ",parmCode[2]," is not a valid variable type. It must be a numeric or character type value.")
           warnCnt()
           warning(xmsg,call.=FALSE)
        
        }  # end of type check.
     }  # end of zero length check
     
     #cat("CheckParmColx Results:",paste0(res,collapse=", "),"\n")
     return(res)
   }        

#
###

###
#
# function CheckNum takes a vector or data.frame of numbers provided in the statsDFrame by the 
# user.  It check to make sure they are numeric via "is.numeric" and a grep string comparison.
# In the process, it checks for factors and converts them to character vectors.  
# Character vectors are scan to eliminate commas in numbers and verify the string is only
# digits and decimal points.  A list of Err and Dat is returned.
# If an error is found, Err is set to TRUE,  The cleaned up numeric vector is returned as Dat.
#
# Input:  xd        <- data column    (from statsDFrame data.frame) 
#         gName     <- glyph Name     (character)
#         pdVarNum  <- pd variable    (col1, col2, col3) number (integer)
#         pdColNum  <- glyph Column Number (2 character)
#         pdVarName <- pd variable name (col1, col2, col3)
#         stColName <- stDF column reference
#         pdUsage   <- brief usage description for error messages.
#

CheckNum <- function(xd, gName, pdVarNum, pdColNum, pdVarName, stColName, pdUsage) {
        #   for error messages, the last digit of 7 and 8 is reserved for this check.
        ErrFnd <- FALSE
        xn     <- formatC(pdVarNum,format="f",width=1,digits=0)
        
   
        #cat("CheckNum - gName:",gName," pdVarNum:",pdVarNum," pdColNum:", pdColNum," pdVarName:",pdVarName,"\n")
        #cat("     stColName:",stColName," pdUsage:",pdUsage," xn:",xn," length(xd):",length(xd),"\n")
        #cat("     xd:",paste0(xd,collapse=", "),"\n")
   
        if (length(xd) == 0) {
           # invalid vector - length = 0 -> no data
           ErrFnd   <- TRUE
           warnCnt()
           xmsg     <- paste0("***02", xn, "D ", gName, " ", pdColNum, " The ", stColName, " data column in the ", sDFName, " data frame does not contain any data. Data vector length has length of zero. ", pdUsage) 
           warning(xmsg, call.=FALSE)
        
           # can't process or check return NULL vector
           xdr      <- xd    # return short vector
           #print("zero length vector")
        } else {
  
           xdr    <- rep(NA,length(xd))    # default results - vector of NAs.
  
           # have data to check
          
           #   Convert factors to characters - this applies even if vector is numeric or character.
           #   Normally only strings are saved as factors in data.frames, but a numeric vector can also
           #   be converted to a factor.  It then becomes a character value.
           #   if it is a factor, we will eventually be headed down the character path.
          
           if (is.factor(xd)) {
              xd <- as.character(xd)    # convert factors to characters
              #print("converted from factor to character")
           }
  
           # check for missing values in the vector
           #   Check # 1 - all missing
           if (all(is.na(xd))) {
              warnCnt()
              ErrFnd    <- TRUE   # no data can be converted.  ALL NA.  could be all blanks.
              xmsg      <- paste0("***02", xn, "A ", gName, " ", pdColNum, " The data provided in the ", stColName, " column of the ", sDFName, " data frame does not contain any numerical data. No rows will be drawn. ", pdUsage)
              warning(xmsg,call.=FALSE)
              # return all NA vector
              #print("all are NA")
           } else {        
              #   Check # 2 - one or more missing.
              if (any(is.na(xd))) {
                 lenxd <- length(xd)
                 seqxd <- seq(1,lenxd)
                 BadSeqNum <- seqxd[is.na(xd)]
                 # one or more entires are NA (missing) - This check should be done before manipulating the vectors.
                 #    check is primarily if user leaves entries missing, not is the translation to numeric leaves them NA.
                 warnCnt()
                 xmsg   <- paste0("***02", xn, "B ", gName, " ", pdColNum, " The ", stColName, " data column in the ", sDFName, " data frame contains one or more missing values. "," Rows with missing values will not be drawn. ", pdUsage)
                 warning(xmsg, call.=FALSE)
                 xmsg   <- paste0("***02", xn, "C ",gName," ",pdColNum,"   The rows with missing data are:",paste0(BadSeqNum,collapse=", "))
                 warning(xmsg, call.=FALSE)
                 #print("one or more are NA")
              }
      
              # we may have missing values, but we can still check the vector.
  
              if (!is.numeric(xd)) { 
                 
                 #print("not numeric")
                 # no numeric - better be character type..

                 if (is.character(xd)) {

                    #print("character")
                    # its character (from factor or has always been character)
           
                    # check character string for valid numerical format and allow for commas.  Any NA values are passed through as NA in the results.
                   
                    x   <- gregexpr("^[ \t]*[+-]?((([0-9]{1,3}[,])?([0-9]{3}[,])*[0-9]{3})|([0-9]*))?(([.][0-9]*)|)([eE][-+]?[0-9]+)?[ \t]*$",xd)      # verify characters are all numeric  (not scientific notation)
            
                      #  regexpr notes:
                      #  ^      - begin of string.
                      #  [ \t]* - any number of leading spaces or tabs. 
                      #  [-+]?  - optional at most one (sign)
                      #  (    - leading digits patterns
                      #  leading digits pattern 1 - leading numbers with commas - logic catches 1,000 and higher, 999 falls through to second pattern.
                      #     (([0-9]{1,3}[,])?([0-9]{3}[,])*[0-9]{3})
                      #       (              - leading digits
                      #        [0-9]{1,3}    - 1 to 3 digits  (could be 1,2, but left 1,3
                      #        [,]           - comma 
                      #       )?             - leading 1,2,3 digits and comma,  optional - no more than once.
                      #       (              - body 3 digits
                      #        [0-9]{3}      - body 3 digit sets
                      #        [,]           - comma
                      #       )*             - zero or more times
                      #       (              - last section of digits
                      #        [0-9]{3}      - body 3 digits
                      #       )              - one time
                      #
                      #  or alternate pattern 2 - leading numbers without commas 
                      #
                      #       (
                      #        [0-9]*        - zero or more digits
                      #       )
                      #   )?                 - leading digits are optional, but can happen just once
                      #
                      # section to handle option decimal point and following digits
                      #
                      #      ([.][0-9]*)     - decimal and digits
                      #       
                      #        or
                      #
                      #      ()              - nothing.      (maybe I could have used ? after the {[.][0-9]*) group
                      #
                      # section to handle possible scientific expression after decimal point and digits or nothing.
                      #
                      #      ([eE][+-]?[0-9]*)?  - optional scientific expression appendage
                      #
                      #  [ \t]* - any number of trailing spaces or tabs
                      #  $      - end of string
                      #  
                      # February 15-16, 2016 - improved reg-exp to handle:
                      #      a) leading + or -
                      #      b) commas in number - correct format.  Needed to do this before
                      #         removed commas since an incorrect format could be handled.
                      #      c) redid how decimal point and following digits are handled.
                      #      d) added logic for scientific notation (e+10) 
                      #
                      #   This led to redoing the other validation coding since we had more 
                      #   information on valid numbers.
                      #

                    xtf <- unlist(x) > 0             # get list of valid numbers in vector. (TRUE = good number / FALSE = bad number)    
                                                     # use this vector to only convert valid numbers.
                                            
                    xd  <- gsub(",","",xd)           # eliminate commas in number
         
                    xdr <- rep(NA,length(xd))        # default return value.
         
                    # all checking for missing numbers has already been done.
           
                    xdr[xtf] <- as.numeric(xd[xtf])  # only convert good formats. 
                   
                    xtf2 <- !is.na(xdr[xtf])         # check the conversion and see if all were converted?
                  
                    if (any(xtf2)) {
                       # something happened and a number we thought was good did not get converted to numeric.
                       print("Internal Note - good numeric format did not get converted")
                       print(paste0("Input :",paste0(xd[xtf], collapse=", ")))
                       print(paste0("Output:",paste0(xdr[xtf],collapse=", ")))
                    }   
                
                 } else {
             
                    # not a numeric or character type vector  
             
                    ErrFnd <- TRUE
                    warnCnt()
                    xmsg <- paste0("***02", xn, "9 ", gName, " ", pdColNum, " The ", stColName, " data column in the ", sDFName, " data frame is not a character or numeric vector. ", pdUsage) 
                    warning(xmsg, call.=FALSE)
          
                 } # end of character/invalid
              } else {
                 # numeric
                 xdr <- xd
              }  # end of not numeric
              
           } # end of all missing or process.
        }  # end of vector length check
        
        return(list(Error=ErrFnd,Dat=xdr))
}

#
###

###
#
#
# Input:  xd        <- data column    (from statsDFrame data.frame) 
#         gName     <- glyph Name     (character)
#         pdVarNum  <- pd variable    (col1, col2, col3) number (integer)
#         pdColNum  <- glyph Column Number (2 character)
#         pdVarName <- pd variable name (col1, col2, col3)
#         stColName <- stDF column reference
#

# Input:  xd        <- data column    (from statsDFrame data.frame) 
#         gName     <- glyph Name     (character)
#         pdColNum  <- glyph Column Number (2 character)
#         pdVarName <- pd variable name (col1, col2, col3)
#         pdVarNum  <- pd variable    (col1, col2, col3) number (integer)
#         stColName <- stDF column reference
#

# x <- CheckNum2(xd, gName, pdColNum, pdVarName, pdVarNum, stColName)   - retired.
#
####
#
# Why was checknum2 created???   now CheckNum is much better.   - This routine appears to be RETIRED>
#
####

CheckNum2 <- function(xd, gName, pdColNum, PDVarName, pdVarNum, stColName) {
        #   for error messages, the last digit of 3 and 4 is reserved for this check.
        ErrFnd    <- FALSE
        xn        <- formatC(pdVarNum,format="f",digits=0,width=1)
        
        if (is.factor(xd)) {          # numeric or character can be factored - must go to character to get to numeric.
           xd <- as.character(xd)
        }

        # check for missing values.          
        if (is.na(xd)) {
           # one or more entires are NA (missing)
           warnCnt()
           xmsg <- paste0("***02",xn,"B ",gName," ",pdColNum," The ",stColName," data column in the ",sDFName," data frame contains one or more missing values.  Rows with missing values will not be drawn.")
           warning(xmsg,call.=FALSE)
        }

        #   Check for factors
        if (is.factor(xd)) {
           xd <- as.character(xd)
        }

        #   if numeric vector - just return the vector.
        if (!is.numeric(xd)) {
           if (is.character(xd)) { 
              # can it be translated without error to numeric.
              
              # determine if string is a valid number format.
              x <- gregexpr("^[ \t]*[+-]?((([0-9]{1,3}[,])?([0-9]{3}[,])*[0-9]{3})|([0-9]*))?(([.][0-9]*)|)([eE][-+]?[0-9]+)?[ \t]*$",xd)      # verify characters are all numeric  (not scientific notation)
           
              # eliminate commas from number text.
              xd <- gsub(",","",xd)      
              
              #  regexpr notes:
              #  ^      - begin of string.
              #  [ \t]* - any number of leading spaces or tabs. 
              #  [-+]?  - optional at most one (sign).
              #  [0-9]* - any number of digits
              #  [.]?   - optional at most one (decimal point)
              #  [0-9]* - any number of digits
              #  (...)? - option field at most once (scientic exponent)
              #           ... => [e][-+][0-9]{1,2}
              #  [ \t]* - any number of trailing spaces or tabs
              #  $      - end of string
              #  
   
              if (any(x < 0)) {
                 # one of the values failed the numeric test.
                 ErrFnd  <- TRUE
                 warnCnt()
                 xmsg    <- paste0("***02",xn,"7 ",gName," ",pdColNum," The data provided in the ",stColName," column in the ",sDFName," data frame contains one or more non-numeric characters.")
                 warning(xmsg,call.=FALSE) 
              } else {
                 # convert to numeric for the return.
                 xd <- as.numeric(xd)
                 
                 areNA <- is.na(xd)
                 
                 if (all(areNA)) {
                    ErrFnd  <- TRUE   # no data can be converted.  ALL NA.  could be all blanks.
                    warnCnt()
                    xmsg <- paste0("***02", xn, "A ", gName, " ", pdColNum, " The ", stColName, " data column in the ", sDFName, " data frame contains one or more missing values. "," Rows with missing values will not be drawn.")
                    warning(xmsg, call.=FALSE)
                 } else {
                    if (any(areNA)) {
                       warnCnt()
                       xmsg    <- paste0("***02", xn, "8 ", gName, " ", pdColNum, " The data provided in the ", stColName, " column in the ", sDFName, " data frame contains one or more entries have non-numeric characters.")
                       warning(xmsg,call.=FALSE)
                    }
                 }
              }
           } else {
              # not a valid type of vector 
              ErrFnd <- TRUE
              warnCnt()
              xmsg <- paste0("***02",xn,"9 ",gName," ",pdColNum," The ",stColName," data column in the ",sDFName," data frame is not a character or numeric vector.") 
              warning(xmsg,call.=FALSE)
           }
        }
        return(list(Error=ErrFnd,Dat=xd))
}

#
###

###
#
# function to verify the presents and type of data in a statsDFrame column.
#
  
CheckPDCol <- function(pdVarName, gName, stColNum, stColName, gColNum, pdVarNum, stMaxColNum, stDat, pdUsage) {
   #
   xr        <- list(Err = FALSE, Dat = c(0))
   
   xn        <- formatC(pdVarNum,format="f",width=1,digits=0)     # get last character (number of col1, 2, 3)
   pdColNum  <- formatC(gColNum,format="f",width=2,digits=0,flag="0")
  
   wstname   <- names(stDat)
   wstMax    <- dim(stDat)[2]

   #cat("CheckPDCol-pdVarName:",pdVarName," gName:",gName," stColNum:",stColNum," stColName:",stColName," gColNum:",gColNum,"\n")
   #cat("     pdVarNum:",pdVarNum," stMaxColNum:", stMaxColNum," pdUsage:",pdUsage," xn:",xn," pdColNum:",pdColNum,"\n")
   #cat("     stDat:",paste0(stDat,collapse=", "),"\n")
   #cat("     wstname:",paste0(wstname,collapse=", "),"\n")
   #cat("     wstMax :",wstMax,"\n")
   
 
   if (is.na(match(pdVarName,PDUsed))) {
      #  pdVarName is not present in the panelDesc data.frame variable lists.
      
      xr$Err  <- TRUE
      warnCnt()
      xmsg    <- paste0("***02",xn,",5 ", gName," ",pdColNum," The required panelDesc variable ",  pdVarName, " is missing from the ", pDName, " data.frame. ", pdUsage)
      warning(xmsg, call.=FALSE)
   }

   if (stColNum == 0) {
  
      xr$Err      <- TRUE
      # if stColNum is zero, then error message already generated.  So signal error and stop.
   }
   
   if (!xr$Err) { 
      # no error found yet....
     
      if (is.na(stColNum)) {  # missing stColName 
         xr$Err   <- TRUE
         warnCnt()
         xmsg     <- paste0("***02", xn, "4 ", gName, " ", pdColNum, " There is no ",sDFName, " column was specified in ", pdVarName, " variable in the ", pDName, " panelDesc data.frame.", " A data column name/number is required. ", pdUsage)
         warning(xmsg, call.=FALSE)
   
      } else {
  
         xr        <- CheckNum(stDat[,stColNum], gName, pdVarNum, pdColNum, pdVarName, stColName, pdUsage)      # check and get the data in col"x" 

      }
   }
   #print("CheckPDCol - Output")
   #print(xr)
   return(xr)
}

#
###

###
#
# function to verify the presents and type of data in a statsDFrame column.
#  Same as the CheckPDCol function, but without any CheckNum call to verify the data. 
#  Used by ctrbar, segbar, normbar glyphs.  They do a CheckNum on each column as they 
#  pull the data.
#
  
CheckPDColnCN <- function(pdVarName, gName, stColNum, stColName, gColNum, pdVarNum, stMaxColNum, stDat, pdUsage) {

   xr        <- list(Err = FALSE, Dat = c(0))
   
   xn        <- formatC(pdVarNum,format="f",width=1,digits=0)     # get last character (number of col1, 2, 3)
   pdColNum  <- formatC(gColNum,format="f",width=2,digits=0,flag="0")
  
   wstname   <- names(stDat)
   wstMax    <- dim(stDat)[2]

   #  Can't create stColName - if not valid, stColNum was set to 0 if bad or NA if pdVarName variable vector was missing.
 
   #  Check if the pdVarName exist in the panelDesc data.frame
  
   if (is.na(match(pdVarName, PDUsed))) {

      xr$Err  <- TRUE
      warnCnt()
      xmsg    <- paste0("***02",xn,",5 ", gName, " ", pdColNum, " The required panelDesc variable ", pdVarName, " is missing from the ", pDName, " data.frame. ", pdUsage)
      warning(xmsg, call.=FALSE)

   }

   if (!xr$Err) { 
  
      # no error found yet....
  
      # Check to see if statsDFrame column in the panelDesc variable was found to be valid by CheckColx function earlier.
      
      if ( is.na(stColNum) || stColNum == 0 ) {  # invalid name or column number in statsDFrame
    
           xr$Err  <- TRUE
           warnCnt()
           xmsg    <- paste0("***02",xn,"6 ", gName, " ", pdColNum, " The specified column name or number in ", pdVarName, " panelDesc variable (", stColName, ") does not exist in the for ", sDFName, " data frame or is out of range. ", pdUsage)
           warning(xmsg, call.=FALSE)
           
      }
   }
   return(xr)
}

#
###

###
#
# function to verify the presents and type of data in a statsDFrame column,  Used with "col1" "col2", and "col3" panelDesc lists.
#    remember - convertion to numeric column number - done
#               verification of column names - done
#               NA value = means was not provided
#               0  value = means invalid value or name provided and error message already generated.
#               col1, col2, col3 values not needed by a glyph are not checked or processed.
#
#   xr <- CheckPDCol2(pdVarName, pdVarNum, pdColNum, gName, stColNum, stDat)                ---- Retired
#
  
CheckPDCol2 <- function(pdVarName, pdVarNum, pdColNum, gName, stColNum, stDat, pdUsage) {

   #
   #  PDVarName   = "col1", "col2", or "col3" - character string name of the panelDesc list (field): 
   #  pdVarNum    = numeric 1, 2, 3 for messages.
   #  pdColNum    = panelDesc glyph column number
   #  gName       = character string name of the calling glyph
   #  stColNum    = column number in statsDFrame data.frame
   #  stDat       = statsDFrame data.frame
   #
   #  xr          = result - xr$Err - error indicator
   #                         xr#Dat - statsDFrame data column
   #
   
   xr        <- data.frame(Err = FALSE, Dat = c(0))
   xn        <- formatC(pdVarNum,format="f",digits=0,width=1)
   stColName <- wSFName[stColNum]
   
   # it's not a given that the colx was in the panelDesc data.frame
  
   #  is the panelDesc variable list (col1, col2, etc.) exit in the panelDesc?
   if (is.na(match(pdVarName,PDUsed))) {
      # no - not present
      xr$Err    <- TRUE
      xmsg      <- paste0("***02", xn, "5 ", gName, " ", pdColNum, " The required panelDesc variable ", pdVarName, " is missing from the ", pDName, " data frame. ", pdUsage)
      warnCnt()
      warning(xmsg, call.=FALSE)
   }
  
   if (stColNum == 0) {
      xr$Err     <- TRUE
      # if stColNum is zero, then error message already generated.  So signal error and stop.
   }
  
   if (!xr$Err) { 
      # no error found yet....   
    
      if (is.na(stColNum)) {  # Required col value not provided.
         
         xr$Err    <- TRUE
         xmsg      <- paste0("***02", xn, "4 ", gName, " ", pdColNum, " There is no ", sDFName, " column was specified in ", pdVarName, " variable in the ", pDName, " panelDesc data.frame."," A data column name/number is required. ", pdUsage)
         warnCnt()
         warning(xmsg, call.=FALSE)
           
      } else {
         # stColNum is in range - already checked before and error message generated.
 
         xr        <- CheckNum(stDat[,stColNum], gName, pdVarNum, pdColNum, pdVarName, stColName, pdUsage)

      }    
   }
}

#
###

###
#
# ConvertDV  - Converts original details variable list into the new by glyph variable list.
#

ConvertDV <- function(DV) {
 
     # This routine converts an old details variables structure into a new structure.
     # Each named list in panelDesc is the same length, but may or may not be used
     # by the glyph.
     #
     # Generate a list containing a list for each glyph column.  The glyph list
     # contains all of the variable (named lists) for it operation.
     # This is organized vertically, instead of horizontally.   
     # The glyph list need only contain the variables required/used for a glyph.
     #
     # Variables and table for Convertion of PD from old format to new format.
     #
     # DV is the details variable structure.  a list of named lists.
     # 
     # Return value is the "NewDV" with new variable names grouped by glyph name.
     #
     
     #
     
     #utils::data(detailsVariables)  # already loaded.
     
     #
     #  For testing - load
     
     #DVFile <- "c:/projects/statnet/r code/micromapST/data/detailsVariables.rda"
     #load(DVFile)    # loads detailsVariables structure
     
     glyphNames <- c("arrow",
                       "bar","boxplot",
                       "ctrbar",
                       "dot", "dotsignif", "dotconf", "dotse",
                       "id", 
                       "map", "mapcum", "mapmedian", "maptail",
                       "normbar",
                       "panel", 
                       "rank",
                       "scatdot", "segbar", "system",
                       "ts", "tsconf"
                    )
     
     initDVList <- function(glyphNames) {

          NewDV <- NULL
          NewDV <- list()
     
          for (iDx in seq_along(glyphNames)) {
             NewDV[[glyphNames[iDx]]]  <- list()
          }
     
          return(NewDV)
     }
     
     DVTable            <- detailsVariables
     DVTable$varName    <- stringr::str_trim(DVTable$varName)
     DVTable$newVarName <- stringr::str_trim(DVTable$newVarName)
     
     #
     
     ErrFnd  <- FALSE
 
     if (!is.list(DV)) {

        ErrFnd  <- TRUE
        xmsg    <- "convertDV - DV structure is not a list."
        warning(xmsg,call.=FALSE)

     }
     
     varsNum   <- length(DV)   # number of variables
     varsName  <- names(DV)    # names of variables 
     
     #cat("varsNum :",varsNum,"\n")
     #cat("varsName:",paste0(varsName,"\n"),"\n")
     
     #
     NewDV      <- initDVList(glyphNames)             # initializes each glyph list to a list.
     
     for (ind in seq_along(varsName)) {                 # step through each variable name

        # validate value
        vName   <- names(DV)[ind]                       # get name
        vValue  <- DV[[ind]]                            # get value
        
        xIndex  <- match(vName,DVTable$varName)
        
        #cat("vName:",vName,"  vValue:",vValue," xIndex:",xIndex,"\n")
        
        if (is.na(xIndex)) {

            xmsg <- paste0("variable: ",vName," not found in master variable list.  Name is not valid, skipped")
            warning(xmsg,call.=FALSE)

        } else {

           varData <- DVTable[xIndex,]                  # get info to validate and translate
           
           #cat("validate-method:",varData$method," v1:",varData$v1," v2:",varData$v2,"\n")
           tag <- paste0(varName," variable") 
          
           res <- switch(varData$method,
        
                 "colors"  = { is.Color(vValue) },
                 "numeric" = { 
                       if (is.numeric(vValue)) {
                          (is.between(vValue,as.numeric(varData$v1),as.numeric(varData$v2)))
                       }
                   },
                 "integer" = {
                       if (is.numeric(vValue)) {
                          (is.between(as.integer(vValue),varData$v1,varData$v2))
                       }
                   },
                 "lines"   = { 
                       wS     <- c('1','2','3','4','5','6','blank','solid','dashed','dotted','dotdash','longdash','twodash')
                       wV     <- as.character(vValue)
                       xIdx   <- match(wV,wS)
                       !is.na(xIdx)
                   },
                 "logical" = { 
                       is.logical(vValue) 
                   },
                 "match"   = {
                       wS     <- eval(parse(text=varData$v1))  # must do this to build vector.
                       wV     <- as.character(vValue)
                       xIdx   <- match(wV,wS)
                       !is.na(xIdx)
                   },
                 "text"    = { 
                       if (is.character(vValue)) {
                          (is.between(nchar(vValue),as.integer(varData$v1),as.integer(varData$v2)))
                       }
                   },
                 "vectOf3" = { 
                       if (is.atomic(vValue)) {
                          if (length(vValue) == 3) {
                             (all(is.between(vValue,varData$v1,varData$v2)))
                          }
                       }
                   },
                 { FALSE }
              )
           # res has the validation results 
           #cat("res:",res," typeof(res):", typeof(res)," ",class(res),"\n")
           
           if (!res) {
              xmsg      <- paste0("***01N0 DETS The ",tag," does not have a valid value: ",vValue,"  Check type ",varData$method," used.")
              warning(xmsg)
           } else {
              # translate
              # replicate variable for each glyph that uses it.
              newVarN   <- varData$newVarName
              
              #cat("usedBy:",varData$usedBy,"\n")
              
              GNList    <- eval(parse(text=varData$usedBy))   # list of glyph that use this variable.
              
              # build the new variable for each glyph.
              
              for (jnd in seq_along(GNList)) {
                  GName    <- GNList[jnd]
                  #cat("Added GN:",GName," / ",newVarN," = ",vValue,"\n")
              
                  NewDV[[GName]][[newVarN]] <- vValue   # add list with single element.
              }      # end of jnd loop
           }         # end of test results from validation.
        }            # end of check for match variable name.
        #cat("Check next variable in list.\n")
        
     }               # end of ind loop
     
     return(NewDV)

  } # end of ConvertDV function
  
#
###


###
#
#  How to convert old panelDesc structure to a new panelDesc structure
#
#  "advanced" named list used to add new variables to the panelDesc instead
#  of keep adding named lists across all of the glyph columns.
#
#  Old Structure:
#
#  panelDesc
#       type    = c( 1,     2,       3,       4,       5,        6, ...)
#       lab1    = c( 1,     2,       3,       4,       5,        6, ...)
#       lab2    = c( 1,     2,       3,       4,       5,        6, ...)
#       lab3    = c( 1,     2,       3,       4,       5,        6, ...)
#       col1    = c( 1,     2,       3,       4,       5,        6, ...)
#       col2    = c( 1,     2,       3,       4,       5,        6, ...)
#       col3    = c( 1,     2,       3,       4,       5,        6, ...)
#       colSize = c( 1,     2,       3,       4,       5,        6, ...)
#       lab4    = c( 1,     2,       3,       4,       5,        6, ...)
#       refText = c( 1,     2,       3,       4,       5,        6, ...)
#       refVal  = c( 1,     2,       3,       4,       5,        6, ...)
#       panelData=c( 1,     2,       3,       4,       5,        6, ...)
#
#  types:
#     "map"        lab1,       lab3
#     "mapcum"     lab1,       lab3
#     "mapmedian"  lab1,       lab3
#     "maptail"    lab1,       lab3
#     "id"         lab1,       lab3
#
#     "arrow"      lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "bar"        lab1, lab2, lab3, col1,                        refText, refVal
#     "dot"        lab1, lab2, lab3, col1,                        refText, refVal
#     "dotsignif"  lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "dotse"      lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "dotconf"    lab1, lab2, lab3, col1, col2, col3,            refText, refVal
#     "scatdot"    lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "rank"       lab1, lab2, lab3,                                  
#     "normbar"    lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "segbar"     lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "ctrbar"     lab1, lab2, lab3, col1, col2,                  refText, refVal
#     "ts"         lab1, lab2, lab3,                   panelData
#     "tsconf"     lab1, lab2, lab3,                   panelData
#     "boxplot"    lab1, lab2, lab3,                   panelData, refText, refVal
#    
#

ConvertPD <- function(PD) {
 
     # This routine converts an old panelDesc structure into a new structure.
     # Each named list in panelDesc is the same length, but may or may not be used
     # by the glyph.
     #
     # Generate a list containing a list for each glyph column.  The glyph list
     # contains all of the variable (named lists) for it operation.
     # This is organized vertically, instead of horizontally.   
     # The glyph list need only contain the variables required/used for a glyph.
     #
     # Variables and table for Convertion of PD from old format to new format.
     #
 
     PDFldDef     <- c("type",   "lab1",     "lab2",     "lab3",
                       "col1",     "col2",     "col3", "colSize", 
                       "panelData","refTexts", "refVals", "rmin", "rmax",
                       "adv"
                  )
     
     PDGlyphReq <- matrix(c(
             # glyph       lab1,  2,     3,     col1,  2,     3,   colSize, panelData, refT, refV, rmin, rmax, adv   
             c("map",      TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("mapcum",   TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("mapmedian",TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("maptail",  TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("id",       TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("arrow",    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("bar",      TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("dot",      TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("dotsignif",TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("dotse",    TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("dotconf",  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("scatdot",  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("rank",     TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
             c("segbar",   TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("normbar",  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("ctrbar",   TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("ts",       TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("tsconf",   TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE),
             c("boxplot",  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE)
            ), ncol=11, byrow=TRUE)
     
     
     PDGlyphDF           <- as.data.frame(PDGlyphReq,stringsAsFactors=FALSE)
     colnames(PDGlyphDF) <- PDFldDef
     #
     numVars             <- dim(PD)[2]
     numMMCols           <- dim(PD)[1]
     #
     wNames              <- colnames(PDGlyphDF)[2:11]
     wPDNames            <- colnames(PD)
     #
     #print(wNames)
     #print(wPDNames)
     #
     NewPD               <- list()

     for (ind in c(1:numMMCols)) {                # step through each column and convert vertically
        # step through by glyph column
        wType   <- as.character(PD$type[ind])     # get glyph type for column
        wSel    <- (PDGlyphDF$type == wType)
        wList   <- as.logical(unlist(PDGlyphDF[wSel,c(2:11)])) # get associated usage row 
        
        wNames2 <- wNames[wList]                  # get associated variable names
        
        gList   <- list(type=wType)               # initialize output list for column
        xVar    <- " "

        for (jnd in c(1:length(wNames2))) {       # step through possible variables
           varName   <- wNames2[jnd]             # get next variable name
  
           if (!is.na(match(varName,wPDNames))) {
              # PD variable is present in the panelDesc
               
              # build string for command and execute to get value of variable
              cmdStr1 <- paste0("xVar <- as.character(PD$",varName,"[",ind,"])")
      
              #print(cmdStr1)
              eval(parse(text=cmdStr1))   #  get value of variable into xVar

              if ((!is.na(xVar)) && (xVar != "") && (xVar != "NA")) {  # check to see if the value is "not present"
                 # Only process if the variable contains something.
    
                 if (varName == "adv") {
                    # if the variable is "adv", then process a little differently
                    #  adv is a list of variables and values, must add it to gList.
                    
                    gList    <- c(gList, xVar)   # add adv list to the output list
                 
                 } else {
                    # other variables, built string and set up variable in output
                    # xVar is the value of varName, so create the variable and set the value
                    
                    cmdStr2  <- paste0("gList$",varName," <- xVar")
                    #print(cmdStr2)
                    eval(parse(text=cmdStr2))
                 }
              }  # end of check for good data to convert (not NA, "", "NA") 
  
           } # end of check if variable present
   
        }  # end of jnd loop
        #str(gList)
        NewPD[[ind]] <- gList
   
     } # end if ind loop
     #
     return(NewPD)
} 

#
###


  ####
  #
  #  The panels work on scales user units.  In some cases this is 0,1 on both axis.
  #  In many cases, this is the rx and ry ranges for the graphs.
  #  The axis and titles lines are outside of the plotting area and 
  #  most of the text is written with 'mtext' function that place the text using 
  #  the "line" offset from the plot area.  In the case a line must be draw and
  #  text must be written, this must be done with the line and text functions
  #  under par(xpd=TRUE) using user units to position the text and line.
  #  
  #  The drawing of the refTexts and dotted line must use this approach.
  # 
  
  ConvLineToUser <- function(iSide, iLine) {
  
     #  iSide is the side of the plot to draw the text and line.
     #     1 = bottom, 2 = left, 3 = top, 4 = right margins
     #  iLine is the line offset from the plotting area.
     #     0 = next to the plot box,  4 = 5th line away from the box.
     #
     #  Returns the iLine position in user scales in units.
     #
 
     xpin      <- par("pin")
     xusr      <- par("usr")
     xmar      <- par("mar")
     xmai      <- par("mai")
   
     #printPar()
     
     if ( iSide == 1 || iSide == 3 ) {
        # for top and bottom sides, get Y units per inch
        UnitsPerInch <- diff(xusr[3:4])/xpin[2]  
     } else {
        # for left and right sides, get X units per inch
        UnitsPerInch <- diff(xusr[1:2])/xpin[1]
     }
     InchesPerUnit <- 1/UnitsPerInch
     #cat("iSide:",iSide,"  UnitsPerInch:",UnitsPerInch,"  InchesPerUnit:",InchesPerUnit,"\n")
 
     # side = below, left, above, right 
     distZerou <- NULL
     distZerou <- switch(iSide,
                       #     1     2 3 4     5      6 
                       # usrZoffs  d s s  adjA   adjL
                       c(-xusr[3],-1,1,1,  0.25,    0),        # bottom 1   # 5 was -0.25 changed to 0.25
                       c(-xusr[1],-1,2,2, -0.05,    0),        # left   2
                       c( xusr[4], 1,1,3, -0.30,    0),        # top    3
                       c( xusr[2], 1,2,4,  0.10,    0),        # right  4
                       c(       0, 0,0,0,     0,    0)         # null
                   ) 
                   
                       # item 1 =>  base value at edge line.
                       # item 2 =>  sign + or -  (add or subtract distance calculated)
                       # item 3 =>  not used.
                       # item 4 =>  mar and mai reference indexes.
                       # item 5 =>  basic adjustment amount.  (offset)
                       
     #cat("distZerou:",distZerou,"\n")
     
     LinesPerInch  <- xmar[distZerou[4]]/xmai[distZerou[4]]         # below, left, above, right    mar/mai -> "5"
     InchesPerLine <- 1/LinesPerInch                                #   1/5 = 0.2 inches per line.
     UnitsPerLine  <- InchesPerLine * UnitsPerInch + distZerou[6]   # adjust line height
     
     #cat("distZerou:",distZerou,"\n")
     #cat("LinesPerInch:",LinesPerInch,"  InchesPerLine:",InchesPerLine,"  UnitsPerLine:",UnitsPerLine,"\n")
        
     # if convert line to user scale Position in user scale
     
     # Line to user scale Pos conversion
     
     Posu           <- distZerou[2] * ( ( ( iLine    + distZerou[5] ) * UnitsPerLine   ) + distZerou[1]  ) 
     #                  direction   * ( ( (  line #  +  offset      ) * Units per Line ) + unit offset (base axis line value.) )
          
     #cat("iLine:",iLine,"  Posu:",Posu,"\n")
     return(Posu)             
  }
 
  #
  ###
  
### 
#
# Function used by mapping functions to draw the column titles for MapCum,
#    MapMedian, and MapTail.  These titles have colored boxes preceeding 
#    the titles.  This function adds four blanks lead of the title as placeholders,
#    draws the text center, then overlays the boxes as required.
#
DrawBoxAndText <- function(wTxt, wTxt.cex, sq.width, sq.col, sq.border.col, yposl) {
   # 
   #  function to draw and center the glyphs column titles with a preceeding
   #    colored box.   Used by the MapMedian, MapTail, and MapCum mapping 
   #    functions.
   #
   #  yposl = mtext line position - 0 on top edge to 3 lines???
   # 
   xps      <- par("ps")
   xpin     <- par("pin")
   xusr     <- par("usr")
   xmar     <- par("mar")
   xmai     <- par("mai")
   #cat("xmai:",xmai,"  xmar:",xmar,"  xusr:",xusr,"  xpin:",xpin," xps:",xps,"\n")
   
   itouX    <- diff(xusr[c(1,2)])/xpin[1]
   itouY    <- diff(xusr[c(3,4)])/xpin[2]
   
   inchPerLine   <- xmai[1]/xmar[1]      # top lines  -> inches per line.   (line position to inches).
  
   sqSize   <- sq.width * ( xps / 9 ) * wTxt.cex   # scale size of square based on the point size of the font
 
   #   may need to add logic to change number of leading blanks based on point size.
   
   wLeni    <- strwidth(paste0("    ",wTxt),units="in", cex=wTxt.cex)
   
   #wLenu   <- strwidth(paste0("    ",wTxt),units="us", cex=wTxt.cex)
   #cat("len i:", wLeni, "  len u:",wLenu,"  ratio:",wLenu/wLeni,"\n")
   
   nStr1i   <- (xpin[1]/2) - (wLeni/2)
   nStr1u   <- nStr1i * itouX
   
   #wUseru   <- diff(xusr[c(1,2)])
   #nStr2u  <- (wUseru/2)  - (wLenu/2)
   #cat("nStr1u:",nStr1u,"  nStr2u:", nStr2u,"\n")
   
   yadji    <- 0.045                  # inches (subtracted)
   
   #xadji   <- 0.10
   xadji    <- (1.25 ^ ( xps * wTxt.cex )) / 2100          # + ( 0.1 * 1/ScR)    # inches
   if (xadji > 0.05) xadji = 0.05
   #            0.08   at 28pt
   #            0.04   at 24pt
   #   value of 0.04   at 20pt.
   #            0.025  at 16pt.
   #            0.01   at 14pt.
   #            0.005  at 12pt.
   #            0.005  at 10pt.
   #            0.001  at  9pt.
   #            0.001  at  8pt.
   #            0.001  at  6pt.
   #
   # Going to try -->  ( 1.25 ^ ( Points * wTxt.cex ) ) / 2100 = xadji
   #
   
   box.xi   <- c(0, 0, sqSize, sqSize, NA) + xadji
   box.yi   <- c(0, sqSize, sqSize, 0, NA) + yadji
   
   # y baseline = line positiion * inchToLIne + height of plot area.
   yposi    <-  yposl * inchPerLine  +  xpin[2]
   
   #  add base position and convert to units.
   box.yu   <- ( ( box.yi + yposi  ) * itouY )   # then convert to units
   box.xu   <- ( ( box.xi + nStr1i ) * itouX ) 
   
   #cat("yposl:",yposl,"  yposi",yposi, "\n")
   #cat("box.xu:", box.xu, "\n  box.yu:", box.yu,"\n")
   
   # use text to print the string centered.
   
   # line one.  (four blanks for box padding. May have to vary as font size changes.
   
   # write text (centered)
   mtext(paste0("    ",wTxt),line=yposl,side=3, cex=wTxt.cex)   # pos = below centered.
  
   # draw square over the blanks in the title on the left.
   polygon(box.xu, box.yu, col=sq.col, border=sq.border.col)
   #polygon(bpx/xu, box.yu, col="black",density = 0)  # draw borders if needed.
   
}

#
###

######
##
## CleanXLabels - clean up set of labels 
##
##     
##
#
#CleanXLabels <- function(rx,atRx,nTicks) {   
#                
#                ## expand range of x if needed.
#                rAtRx          <- range(atRx)
#                #if(rAtRx[1] < rx[1])     rx[1] <- rAtRx[1]
#                #if(rAtRx[2] > rx[2])     rx[2] <- rAtRx[2]
#                
#                # get number of labels
#                lAtRx          <- length(atRx)
#                #cat("CXL-lAtRx:",lAtRx," trim.\n")
#                #  trim labels outside of data range.
#
#                nT   <- 7
#                if (lAtRx <= nT) {
#                   #  Delete labels below actual data point
#                   if((atRx[1]     < rx[1]) & (atRx[1] != 0) )      atRx <- atRx[-1]      # delete grid line below rx minimum
#                }
#                #  Delete labels above actual data points
#                lAtRx         <- length(atRx)
#                if (lAtRx <= nT) {
#                   if((atRx[lAtRx] > rx[2]) & (atRx[lAtRx] != 0) )  atRx <- atRx[-lAtRx]  # delete grid line above rx maximum  
#                }
#                lAtRx         <- length(atRx)  
#                #cat("DrawXAxis s adjusted atRx:",paste0(atRx,collapes=", "),"\n")
# 
#                return(atRx)
#           }                
#
##
######

#####
#
#  CleanXLabels2 - 
#     If greater than 3 labels - trims off any label point outside of the range of the data and not zero.
#     expands data range(rx) to cover remaining edge labels.
#


CleanXLabels2 <- function(rx, atRx) {
     lAtRx          <- length(atRx)    # length of atRx and number of labels.
     #cat("CXL2-lAtRx:",lAtRx," trim.\n")

     if (lAtRx > 3) { 
        # if greater than 3 labels - large number of labels - trim labels that are out of range.
        
        # Check low end label
        if (atRx[1] < rx[1] & atRx[1] !=0 ) {
           atRx  <- atRx[-1]   # trim first value
           lAtRx <- length(atRx)
        }
        
        # Check high end label
        if (atRx[lAtRx] > rx[2] & atRx[lAtRx] != 0 ) {
           atRx  <- atRx[-lAtRx]
           lAtRx <- length(atRx)
        }
     } 
     
     #  Extend data range based on labels and grid lines
   
     # Check low end data range vs. label
     if (atRx[1] < rx[1]) {
        # first label outside of data range.
        rx[1] <- atRx[1]   # expand low end.
     }
   
     # Check high end data range vs. label
     if (atRx[lAtRx] > rx[2])  {
        # last label outside of data range.
        rx[2] <- atRx[lAtRx]  # expand high end
     }
     
     #cat("After Extended - rx:",rx,"  atRx:",atRx,"\n")
     return(list(rx=rx,atRx=atRx))
   }

#
#####

#####
#
TestOverlap <- function(Acex, atLab, atRx, nSp) {    
    
     lAtLab      <- length(atLab)
     widthSp     <- strwidth("0",cex=Acex,units="user")        
     widthSpN    <- widthSp * nSp
     #cat("TestOverlap-cex:",Acex," nSp:",nSp,"  widthSpN:",widthSpN," len(atLab):",lAtLab,"\n")
     
     widthAtLabH <- strwidth(atLab,cex=Acex,units="user")/2
     SrtLab      <- atRx - widthAtLabH
     EndLab      <- atRx + widthAtLabH
     
     #cat("SrtLab:",SrtLab,"\n")
     #cat("EndLab:",EndLab,"\n")
     
     #  number of labels 1 to n, so check space between 1-2, 2-3, ... , nm1-n
     OverLapFnd <- FALSE
     #  Check to see if any labels would overlap each other based on width and grid point location.
     for (ind in c(1:(lAtLab-1)) )  {
        wX <- SrtLab[ind+1] - EndLab[ind] 
        #cat("ind:",ind,"  wX:",wX,"\n")
        if (wX < widthSpN) {
           OverLapFnd <- TRUE
        }
     }
     #cat("OverLapFnd:",OverLapFnd,"\n")
     return(OverLapFnd)
  }
#
#####

#####
#
#  Test to see if labels overlap text from neighboring columns.   

TestLabAtEdge <- function(atLab,atRx,YAxisPad,rx,lineAxisSizes) {
        
        # function to test edges for possible shift.
        # returns atRx adjusted.
        xusr              <- par("usr") 
        xpin              <- par("pin")
	xupi              <- diff(xusr[1:2])/xpin[1]
	#cat(" TestLabAtEdge - xusr:",xusr,"  xpin:",xpin,"  xupi:",xupi,"\n")
 
        #  width of each label.
        WidthOfLabs       <- strwidth(atLab,cex=lineAxisSizes["Ax1"],units="user")
        #  half of the width of each label
        HalfWidthOfLabs   <- WidthOfLabs/2
        #  starting "x" position of each label
        SrtOfLabs         <- atRx - HalfWidthOfLabs
        #  ending "x" position of each label
        EndOfLabs        <- atRx + HalfWidthOfLabs
        #  number of labels.
        lAtLab            <- length(atLab)
     
        #
        #cat("Label Specifcations: (width, half, srt, end)\n")
        #print(WidthOfLabs)
        #print(HalfWidthOfLabs)
        #print(SrtOfLabs)
        #print(EndOfLabs)
     
        #  get 1/2 of the column sep gap (in units)
        wColSepGapHU      <- (colSepGap/2)*xupi
        
        #cat("half of colSepGap in units:",wColSepGapHU,"\n")
        
        #   Viable left edge of column (rx[1] - col sep gap)
        leftEdge          <- rx[1] - wColSepGapHU     # 1/2 col sep converted to units.
     
        #  adjust left edge is Y Axis is present - have more room.
        if (YAxisPad) {
            # y Axis present - add standard 0.2 inches of padding.
            wYAGapHU      <- (YAxis.width * xupi)
            leftEdge      <- leftEdge - wYAGapHU
            #cat("wYAGapU:",  wYAGapHU," added to leftEdge.\n")
        }
         
        #   Viable right edge of column (rx[2] + col sep gap)
        rightEdge         <- rx[2] + wColSepGapHU
    
        #cat("leftEdge:",leftEdge,"   rightEdge:",rightEdge,"  units.\n")
        #cat("atRx:",atRx,"  rx:",rx,"\n")
        
        #
        #   Adjust first and last label point inward for apperance. 
        #
        #   Check overhangs of last column and this column.
        #     pos values - have space (inches)
        #     neg values - need space
        #     sum < 0 - needed more space then available - problem - go do stagger
        #     sum >=0 - had enough space - no problem.
        #        
        
        wAtRx          <- atRx
        lAtRx          <- length(atRx)
        rAtRx          <- range(atRx)
      
        WidthRx        <- diff(rAtRx)
        edgeRxAdj      <- (WidthRx / 1000) * XAxis.indent
      
        #cat("edgeRxAdj:",edgeRxAdj,"\n")
        
        #
        #  Is not getting applied if staggered.  Problem.
        #
     
        #
        # Adjustments label atRx to bring the first and last "atRx" points in a little.
        #
      
        if (SrtOfLabs[1] < leftEdge) {
           #cat("overlap left edge:", leftEdge - SrtOfLabs[1], " units\n")
           #  Adjust both edge at points inwared by 1/1000 the range of labels * XAxis.indent(5)
           wAtRx[1]          <- wAtRx[1]     + edgeRxAdj   # key adjustment move inward.
           SrtOfLabs[1]      <- SrtOfLabs[1] + edgeRxAdj
           EndOfLabs[1]      <- EndOfLabs[1] + edgeRxAdj
           #cat("adj - SrtOfLabs[1]:",SrtOfLabs[1],"  EndOfLabs[1]:",EndOfLabs[1],"\n")
        }
        
        if (EndOfLabs[lAtRx] > rightEdge) {
            wAtRx[lAtRx]     <- wAtRx[lAtRx]      - edgeRxAdj     # key adjustment
            EndOfLabs[lAtRx] <- EndOfLabs[lAtLab] - edgeRxAdj
            SrtOfLabs[lAtRx] <- SrtOfLabs[lAtLab] - edgeRxAdj
            #cat("adj - SrtOfLabs[lAtRx]:",SrtOfLabs[lAtRx],"  EndOfLabs[lAtRx]:",EndOfLabs[lAtRx],"\n")
        }
   
        #   add check to see if shift causses overlap with neighbor label.

        #cat("after 1st and last shift-rx:",rx," atRx:",wAtRx,"\n")
        #cat(" atLab:",atLab," axisSubTitle:",axisSubTitle,"\n")
     
        atRx            <- wAtRx   # update label points.  Shift completed, if needed.

        #
        # Deal with overlap to over columns.   ( see is overlap is happening ) 
        #
     
        #   Check for overlap with previous column.
     
        w1stLabOverU    <- SrtOfLabs[1] - leftEdge   # have number of units over the edge of the plot.
        w1stLabOverI    <- (w1stLabOverU / xupi)     # Convert units to inches of overhang.
        
        #  if negative, then label is extended into next column
        #  if positive or zero, then label is within column
        
        # add the values:  if negative - OVERLAP.
        #                  if positive - space was available.
        #
        #  TEST for overlap done outside of this routine, we just calculate the variable.
       
        #
        #  Calculate the right edge overlap being used.  Will use as lastLab2Space handoff to next column.
        #
           
        wLastLabOverU   <- rightEdge - EndOfLabs[lAtRx]
        wLastLabOverI   <- (wLastLabOverU / xupi)
        
        # if pos value - we have room.  neg - we need room.
       
        return(list(atRx=atRx,w1stLabOverI=w1stLabOverI,wLastLabOverI=wLastLabOverI))
      }

#####
#
#  DrawXAxisAndTitles - This functions takes the rx range of the data and calculates the X axis labels and 
#    grid line positions.  Four methods are supported:
#         original ("o")   - the original method of labeling best on panelInbound and pretty functions
#         extended ("e")   - use of the extended algorithm and no panelInbound limiting.
#         wilkinson("w")   - use the wilkinson algorithm.
#         scale ("s")      - use of the extended algorithm and then scaling based on the largest value
#                             and sub titling the scale used.  (e.g.,  100000 -> 10  in the ten thousands.
#         scale number ("sn") - use of the extended algorithm and then scaling each number and adding a suffix 
#                            to the number to indicate the scale used.  (e.g.,  10000 ->  10M)
#
#   New Feature - lastLab2Space and lastLab3Space.  this allows us to determine if the lab2 or lab3 lines on
#      maps and ids to axis on glyphs.
#      Process:
#        1) if staggered, exit
#        2) get width of axis first label.
#        3) discount offset (indent)
#        4) get amount of room for handover of label - space between plot and mid point.
#        5) see if room for remainder in lastLab2Space (and lab3).  If no room,
#           instigate staggerLab.
#
#   Take into account, user's request for scaling and staggering first.
#   If they don't fit, warn user, do scaling first ("sn") and try again.  If 
#   unsuccessful, then force staggering of labels.
#
#   Other options to consider:
#          Force Zero to be gridded.
#          Optional edge grid line labels.
#          Enlarge edge buffer space to handle labels.
#          Modification of "referred" number created by the expended algorithm.
#
#
#   titles may run into each other.  
#
#   The function also handles the staggering of labels if requested.
#
#   Since the type of axis labeling impacts the lab1, lab2, lab3, and reftxt titles, this function also 
#   handles the placement and drawing of the column titles and footnotes.
#
#   Subdivide into X-Axis and Title processing.  Let X-Axis find out how much space it needs, fill it
#   and pass to Titles, where to pick up the labeling.  If no X-Axis, then a known spacing will be passed
#   to the titles.  
#
#   Basically start at 0 off the axis (panel) line.  
#        Simple X-Axis is font "9" and takes up 1-line of space.
#        Staggered X-Axis is font "9" * 0.65(?) and takes up 1.5-lines of space.
#        Scaled with subtitel X-Axis is font "9" * 0.75 font and takes up 0.8 lines of space.
#
#        Combinations are (on top of title labels (1 or 2):
#
#               Simple ---------------    1  line  (font = 9) = axis label(0.75) + space(0.25)                           = 1
#               Staggered, Simple ----  1.5 lines  (font = 9) = axis small label(2*0.625) + space(0.125) + space(0.125)  = 1.5
#               Scaled with subtitle -  1.5 lines  (font = 9) = axis small label(2*0.625) + space(0.125) + space(0.125)  = 1.5 
#               Stag. Scaled ---------  2.0 lines  (font = 9) = axis small label(3*0.625) + space(2*0.125) + space(0.125)= 2.25 
#               one or two labels   --  2.0 lines
#
#        So header can range from 1 line (no X-Axis) to 2 lines X-Axis with 1 label or 3 lines X-Axis and 2 labels
#           to a complex X-Axis > 1 to 2.05 lines plue the 1 or 2 lines of title.
#        
#        Need space for 1 to 4.05 lines with gaps.
#
#        The same applies to the bottom labels.  Lab3 is a title, and refText is the other title.
#
#  Other discussion:  Indenting edge labels. 
#        1) get length of labels
#        2) determine how much room is available from edge to next inner label (length of that label and position.)
#        3) How much to move to position inside box (or at least no further then 0.05" over the edge?)
#        4) Is staggering label requested or required.
#             If labels fit, staggered may need to be turned off.
#        5) If size of labels (all) do not fit, will staggering help?
#        6) How to keep key values like "0" always labeled?  What does the Axis algorithm use to omit labels.
#
#

DrawXAxisAndTitles <- function(j, panels, rx, ry, reftxt, refval, leftPad=TRUE, rightPad=TRUE, YAxisPad=FALSE ) {

     #####  Start of Scaling and alternate labeling algorithms
     #
     #   parameters needed:  rx, ry, j, panels, reftxt, refval, XAxis=TRUE
     #
     #   globals:  Title.Line.X.pos  set of variables.
     #             axisMethod
     #             Text.cex
     #             staggerLab
     #             staggered
     #             lab1
     #             lab2
     #             lab3
     #             refTexts
     #             refVals
     # 
     #   functions: Scaler1, Scaler2, extended, panelSelect, panelScale, warning
     #
     
     # must initially select panel to start getting widths and heights for scaling.
     
     #cat("\n\nEntered DrawXAxisAndTitle function","\n")
     
     #cat("DX01-panels and j:\n")
     #print(panels)
     #cat("i:",1," j:",j," rx:",rx," ry:",ry,"\n")
     
     panelSelect(panels, 1, j)              # select panel
     x           <- panelScale(rx, ry)      # set scale for panel based on rx, ry
    
     xpin        <- par("pin")
     xusr        <- par("usr")
     xupi        <- diff(xusr[1:2])/xpin[1]
     xps         <- par("ps")
     
     staggering  <- staggerLab              # default is the user request.  May be changed if needed.
     
     #cat("DXAT-start staggered:",staggered,"  staggerLab:",staggerLab,"  staggering:",staggering,"\n")
     #cat("Initial rx  :",rx,"\n")
     
     #  range adjustment
    
     xcxy        <- par("cxy")              # must be in the scale of the panel to get the right character width.
     
     #cat("xcxy:", xcxy,"   usr :",xusr,"  pin :",xpin,"  upi :",xupi,"  ps:",xps,"\n")
     
     xcxyMod     <- xcxy[1]/4               # assume dot is least then the width of a average character.  Use 1/4 for spacing.
     #cat("xcxyMod:",xcxyMod,"\n")
     
     if (leftPad)  rx[1] <- rx[1] - xcxyMod
     if (rightPad) rx[2] <- rx[2] + xcxyMod
     
     #cat("Adjustment made for dot size - new rx:",rx,"\n")
    
     # reset scaling based on new rx range.
     x           <- panelScale(rx, ry)           # re-scale panel
     
     # get new values for user and units per inch
     xusr        <- par("usr") 
     xupi        <- diff(xusr[1:2])/xpin[1]
    
     #cat("After dot re-scaling - usr :",xusr,"  pin :",xpin,"  upi :",xupi,"\n")
     
     par(xpd=T)                                  # turn off clipping for panel
     
       
     # based on the axis method - prepare the atRx tick points and the atLab vector of labels
     #
     #  Setup axis variables and get the axis "at" points and labels.
     #
     #####
     
     #####
     #
     #   Scan possible labels and save heights.  Lab1, Lab2, Lab3, refTexts
     #    Check axis scaling and staggered and setup Axis1, Axis2 and subTitles
     #    adjust Labx heights and spacings if required.l
     #    Spaces and Heights are the constants, not the positions.  We set them here.
     #
     #    This also makes it simplier to have generic code further on.
     #
     #    This will be coded to automatically:
     #       labels at 3 points lower (25%) reduction of given point size.
     #       Axis Large at 1 point lower than labels. (about 11.11%)  
     #       ability to reduce Axis Large by 1 pt for sizing of labels. (another 11.11%)
     #       Axis Small (stagger) at 2 point lower than large labels.  (22.22% below labels.)
     #       
     #
     
     axisSubTitle   <- ""             # starts empty.
                                      
                                      # everything is based on a starting pointsize of 12.
                                                            
     atLabcex       <- Text.cex       # Text.cex              # 0.75 of 12 pt, -> 0.75 %  (9 pt.) 
     #cat("atLabcex:",atLabcex,"\n")
     
     #
     # Build elements to construct header and footer title and axis label positions.
     # 
     xps            <- par("ps")   # current point size.
     ippt           <- 1/72        # inches per point
     lppt           <- 1/xps       # line per point at par("ps") value (default = 12 pt. for 12 pt per line)
     ipl            <- xps * ippt  # points * inches per point at par("ps") -> inches per line.
    
     #cat("Step size 1 pt:",ippt," in.  ",ippt*xupi," usr - lppt:",lppt," pt/line. \n")

                    # 12pt * 0.75 -> 9pt,   18pt * 0.75 -> 13.5pt,  24pt * 0.75 -> 18 point 
                     
     lineNLabSize   <-  Text.cex    # par("ps") - 3 points                                
                    # 0.75  -> 0.75   %  1 line    (0.75% of point size)   (9 pt)
     
     lineNSpLabSize <-  lineNLabSize * XAxis.Sp.mcex   # PS * 15%         
                    # 0.75 * 0.2   -> 0.15  ->  20% of title line (1.8 pt)
     
     axisNLabSize   <-  lineNLabSize - (lppt)       # - 1 pt delta / alternate ->  XAxis.S.mcex = 0.666667       
                    # 0.75 (9pt) - 1 pt  -> 0.6667 %  89% line   (8 pt)
     
     axisMLabSize   <-  lineNLabSize - (2 * lppt )  # - 2 pt delta
                    # 0.75 (9pt) - 2 pt -> 0.5833 %  78% line   (7 pt)
         
     axisSLabSize   <-  lineNLabSize - (3 * lppt )  # - 3 pt delta    
                    # 0.75 (9pt) - 3 pt -> 0.5 %     66.7% line (6 pt)
                    
     axisLowestSize <-  lineNLabSize - (4 * lppt )  # - 4 pt delta (lowest limit.) (5 pt)
                    # 0.75 (9pt) - 4 pt -> 0.4167 %  55.5% line (5 pt)
                    
     axisSubTSize   <-  axisSLabSize    
     
     lineSSpLabSize <-  lineNSpLabSize * 0.5   #  0.15 * 0.5 ->  0.075   %   10% line 
     
                    # calculations are dynamic - using ratios and percentages.
                    
                    # on the X-Axis, the height stays the same regardless of font size.
                    # Only if staggered or scale is imployed is the height changed to 1.5 time.
                              
     #
     #  Two labels and Axis = 0.66667 + 0.15 + 0.75 + 0.75 => 2.316667  + line height.
     #  Axis Stag, Title, two labels = 0.5 + 0.5 + 0.075 + 0.75 + 0.75 -> 2.575 + line height.
     #      must have at least 3.25 lines available.
     #
     #cat("lineNLabSize  :",lineNLabSize,"\n")
     #cat("axisNLabSize  :",axisNLabSize,"\n")
     #cat("axisMLabSize  :",axisMLabSize,"\n")
     #cat("axisSLabSize  :",axisSLabSize,"\n")
     #cat("axisSubTSize  :",axisSubTSize,"\n")
     #cat("\n")
     #cat("lineNSpLabSize:",lineNSpLabSize,"\n")
     #cat("lineSSpLabSize:",lineSSpLabSize,"\n")
     #cat("\n") 
     
     #cat("lineNLabSize-ps:",lineNLabSize*xps,"\n")
     #cat("axisNLabSize-ps:",axisNLabSize*xps,"\n")
     #cat("axisMLabSize-ps:",axisMLabSize*xps,"\n")
     #cat("axisSLabSize-ps:",axisSLabSize*xps,"\n")
     #cat("axisSubTSize-ps:",axisSubTSize*xps,"\n")
     #cat("\n")
    
     xusr        <- par("usr") 
     xupi        <- diff(xusr[1:2])/xpin[1]
     #cat("A-usr:",xusr,"  xupi:",xupi,"\n")
    
     #
     #  Work pattern, list for which to draw and where
     #
     lineTopSizes         <- c(0,     0)
     #                         Lab2   Lab1    
     lineBotSizes         <- c(0,     0)
     #                         Lab3   refText    
     lineAxisSizes        <- c(0,  0,  0,  0,  0)
     #                         Ax2 Ax1 SP  AST`SP
     names(lineAxisSizes) <- c("Ax2","Ax1","SPT","AST","SP")   # Axis spacing
     
     names(lineTopSizes)  <- c("L2","L1")  
     names(lineBotSizes)  <- c("L3","L4")   
     
     lineDo               <- c( F,    F,    F,    F,    F,   F,   F,   F,   F)
     names(lineDo)        <- c("Ax2","Ax1","SPT","AST","SP","L2","L1","L3","L4")
     xAxisDo              <- FALSE
     xAxisDoOverlap       <- TRUE

     lineMultiT        <- c(1,    0.9,  0.9,  0.9,  1,   1,    1,   1)  # size multiplier for proper spacing.
     names(lineMultiT) <- c("srt","Ax2","Ax1","SPT","AST","SP","L2", "L1")

     lineMultiB        <- c(1,    0.9,  0.9,  0.9,  1,   1,    1,   1)  # size multiplier for proper spacing.
     names(lineMultiB) <- c("srt","Ax2","Ax1","SPT","AST","SP","L3", "L4")

     
     # as of 8/17/16, we always print double axis labels to get them all printed. 
     # atLab1 and atLab2 with atRx1 and atRx2 are created as the two halfs of the labels.
  
     #  Set indicators if title/labels are present.
     
     if (lab1[j] != "")    { 
        lineDo["L1"]       <- TRUE
        lineTopSizes["L1"] <- lineNLabSize
     }
     if (lab2[j] != "")    { 
        lineDo["L2"]       <- TRUE 
        lineTopSizes["L2"] <- lineNLabSize
     }
     if (lab3[j] != "")    { 
        lineDo["L3"]       <- TRUE 
        lineBotSizes["L3"] <- lineNLabSize
     }
     
     if (!is.na(reftxt)) { 
        if ( reftxt != "" || reftxt != " " ) {
           lineDo["L4"]       <- TRUE 
           lineBotSizes["L4"] <- lineNLabSize
        }
     }
     
     #  test to see if we have an axis to label.  rx is not null.
     
     if (!is.null(rx)) {  # X axis range present
         # initialize - we will have at least 1 X Axis line. - minimum setup.
         xAxisDo               <- TRUE
         lineDo["Ax1"]         <- TRUE      # X Axis labels # 1
         lineAxisSizes["Ax1"]  <- axisNLabSize
         lineDo["Ax2"]         <- TRUE      # X Axis labels # 2
         lineAxisSizes["Ax2"]  <- 0         # zero to allow the overlap.
         lineDo["SP"]          <- TRUE      # Add spacing between title and X Axis.
         lineAxisSizes["SP"]   <- lineNSpLabSize 
     }
     
     #
     #   Use lineAxisSizes["Ax2"] to allow overlaying of Ax1 and Ax2 and use lineAxisSizes["Ax1"]
     #    as the cex/font size for both Ax1 and Ax2 lines.
     #
     #   if scales to TextCex = 0.7 then all times cex.  = 4 * 0.7 => 2.8 lines of margin.
     #      therefore, must have space for 3 mcex=1 height lines.
     #      
     
     
     #########
     #
     #  Processing XAxis and rx.
     
     #  Generate axis labels, scale and subtitle as required.
     
     #  Results may be - single XAxis labels or XAxis labels with subtitle
     
     #cat("axisMethod:",axisMethod, "  rx:",rx,"\n")
     
     if ( axisMethod < 1 || axisMethod > 5 ) {
        #cat("***01X1 CARG-AX The Value for axisMethod internal variable is out of range 1-5 : ",axisMethod,"\n")
        axisMethod <- 4
     }
     
     ###
     #
     #  methods:
     #      1 = "o" use pretty to generate labels (original method), no scaling of labels.
     #      2 = "s" scale full range of numbers,       
     #      3 = "sn" scale each number in label list.
     #      4 = "e" use extended labeling method.
     #      5 = "w" use wilkinson method
     #      6 = "a" automatics - evalute number, range, possible results of labeling calls,
     #          edge number requiredments, range containing zero - and pick best set of tools.
     #          (Future not coded - using "4" code.
     #
     # Future - add automatic - look at spacing and do scaling if required   # auto scale to be done. 
     #                          look at edges and do edge labels if required #
     #                          make sure zero is seen    # wilkinson an extended handle
     #                          do staggered if edges overlap.   # implemented
     #                          check for overlap with map or id column. # ID done
     #
     # Rules Axis labels:
     #     a) 3.4 labels (rows) per inch    (9 rows = 2.647") (9 / 2.5 = 3.6 label rows per 1")
     #     b) number of labels must be at least 3.
     #     c) request odd number of labels 3, 5, 7, 9 (expect no more than 9 labels (rows) on 2.5")
     #     d) if number of labels > 3, trim labels not within rx data range, except zero value.
     #     e) if panel width < 0.5, trim first and/or last labels if not within data range and zero
     #     f) Never trim Zero.
     #
     
     ErrFnd    <- FALSE   # note errors
     
     #DoNarCol <- FALSE   # indicate we are in the "narrow" column situtation.
     
     #cat("start of label fitting\n")
     #cat("lastLab2Space:",lastLab2Space,"\n")
     #cat("par('pin')   :",par('pin'),"\n")
     #cat("par('usr')   :",par('usr'),"\n")
     #cat("xupi         :",xupi,"\n")
     ###
     #
     #  estimated number of labels for glyph and make it an odd number.
     #
     
     reqNumLabels    <- ((( xpin[1] * XAxis.nGridpIn ) %/% 2) * 2) + 1 # average 3.4 ticks/grid per inch made an odd number                
    
         # average of 3.4 per inch * width in inches of panel.
    
     # force a minimum of 3 labels.
     if (reqNumLabels < 3)   reqNumLabels <- 3     
    
     #cat("Start-reqNumLabels:", reqNumLabels," width in:",xpin[1],"  XAxis.nGridlIn:",XAxis.nGridpIn,"\n")
     #cat("rx   :",rx,"  axisMethod:",axisMethod,"\n")
     
     #cat("setup - colSepGap:",colSepGap,"    staggered:",staggered,"\n")
    
     # get sequence of possible number of labels 
     listNumLabels <- seq(reqNumLabels, 3,by=-2)
     
     if (axisMethod==1) listNumLabels <- c(reqNumLabels)   # method 1 does not use # of labels seed.
    
     #
     # main loop to find a set of X Axis labels that will file the space for the range.
     #
     # The major steps are repeated until a fit is found.
     #
     for (numLabels in listNumLabels) {
     
        #cat("Loop Start:",numLabels,"\n")
        #cat("lineAxisSizes:\n")
        #print(lineAxisSizes)
        #cat("lineDo       :\n")
        #print(lineDo)
        
        ##### start of big loop to get solution between font size and number of labels.
        
        #  Step 1 - generate list of labels for the requested number of labels.
    
        # do requested label generation and scaling.
        # Label Generation:    o = panelInbounds,  e = extended.
        # Scaling Methods :    None,  Scale range,  Scale individual number.
     
        switch (axisMethod,  
              { # method 1 - pretty - the "original"           "o"
                #cat("Method 1-atRx:",atRx,"\n")

		# get reference points.
                atRx         <- panelInbounds(rx)               # list of ticks within range of x. (n=5 with triming)
                                # pretty does n=5 by default.
                res          <- CleanXLabels2(rx, atRx)
                atRx         <- res$atRx
                rx           <- res$rx
                
                # convert to character.
                atLab        <- as.character(atRx)
              },
              
              { # method 2 - scale range with subtitle         "s"
                #    scaling range - may have subtitle to include
                #cat("Method 2-atRx:",atRx,"\n")
                
                #  get reference points
                atRx          <- labeling::extended(rx[1],rx[2],numLabels)
                
                res           <- CleanXLabels2(rx, atRx)
                atRx          <- res$atRx
                rx            <- res$rx
 
                #  get Scaler1 results on max.
                atLabVc       <- Scaler1(rx[2])                  # get divisor and subtitle based on max value
                #cat("atLabVc:",atLabVc,"\n")
                
                axisSubTitle  <- atLabVc[2]                      # get sub-title (if any)[2]  [1] multiplier
                                
                # scale the values into the character string.
                
                atLab         <- formatC(atRx / as.numeric(atLabVc[1]), format="f", digits=2, drop0trailing=TRUE)
                
                if (axisSubTitle != "") {     #  add sub-title to header
                
                   #cat("Add - axisSubTitle:",axisSubTitle,"\n")
                
                   #  Make adjustments
                   
                   # Scale each number (S)
                   #  Add subtitle and spacer at small axis size (Norm to Med - 1 pt)
                   lineAxisSizes["AST"] <- axisMLabSize
                   lineDo["AST"]        <- TRUE
                   lineAxisSizes["SPT"] <- lineSSpLabSize   # use 1/2 of axis to titles spacing.
                   lineDo["SPT"]        <- TRUE
                   
                   #  reduce size of axis labels
                   lineAxisSizes["Ax1"] <- axisMLabSize
                   lineDo["Ax1"]        <- TRUE
                   lineAxisSizes["Ax2"] <- 0                # no staggering yet - Ax1 and Ax2 on same line.
                   lineDo["Ax2"]        <- TRUE
                   
                   #  include spacing between title and axis.
                   lineAxisSizes["SP"]  <- lineNSpLabSize   # normal spacing because we have not staggered, yet.
                   lineDo["SP"]         <- TRUE
                   lineMultiB["SP"]     <- 2.25              # need a fudge on the Bottom.
                }
        
              },
              
              { # method 3 - scale numbers with suffix           "sn"
                #   no subtitle will be added.
                #cat("Method 3-atRx:",atRx,"\n")
                
                atRx         <- labeling::extended(rx[1],rx[2], numLabels)
      
                res          <- CleanXLabels2(rx, atRx)
                atRx         <- res$atRx
                rx           <- res$rx
                atLab        <- sapply(atRx, Scaler2)      # scale the X axis labels.  Scaler2 does label formating for each value.  
              },
             
              { # method 4 - extended algorithm (no scaling)     "e"
                # no scaling - no subtitles 
                #  replaced wilkinson algorithm with extended - better behaved in number of labels generated vs. request.
                #cat("Method4 - extended rx:",rx,"  numLabels:",numLabels,"\n")
                     
                atRx         <- labeling::extended(rx[1],rx[2],numLabels)
                
                res          <- CleanXLabels2(rx, atRx)
                atRx         <- res$atRx
                rx           <- res$rx
                atLab        <- as.character(atRx)
              },
            
              { # method 5 - wilkinson algorithm (no scaling)     "w"
                # no scaling - no subtitles 
                #  replaced wilkinson algorithm with extended - better behaved in number of labels generated vs. request.
                #cat("Method5 - wilkinson rx:",rx,"  numLabels:",numLabels,"\n")
                     
                atRx         <- labeling::wilkinson(rx[1],rx[2],numLabels)
                
                res          <- CleanXLabels2(rx, atRx)
                atRx         <- res$atRx
                rx           <- res$rx
                atLab        <- as.character(atRx)
              },
              
              { # method 6 - placeholder for automatic scaling, edge numbers, and staggering of labels.  "e"
                # for now same as 4
                #  Future Coding - place holder.
                #
                # Do each scaling and see which creates the smallest set of labels.
                #  Which way to do:  1) number of characters, 2) strwidth each summed,
                #  3) concat. labels with 1, 2 spaces?
                #
                
                #cat("Method6 - extended rx:",rx,"  numLabels:",numLabels,"\n")
                   
                atRx0         <- panelInbounds(rx)               # list of ticks within range of x. (n=5 with triming)
                res           <- CleanXLabels2(rx, atRx0)
                atRx0         <- res$atRx
                rx0           <- res$rx
                atLab0        <- as.character(atRx0)

                atRx1         <- labeling::extended(rx[1],rx[2],numLabels)
                res           <- CleanXLabels2(rx, atRx1)
                atRx1         <- res$atRx
                rx1           <- res$rx
                atLab1        <- as.character(atRx1)
      
                atLabVc       <- Scaler1(rx1[2])                  # get divisor and subtitle based on max value
                axisSubTitle  <- atLabVc[2]                      # get sub-title (if any)
                #cat("atLabVc:",atLabVc,"\n")
                atLab2        <- formatC(atRx1 / as.numeric(atLabVc[1]), format="f", digits=2, drop0trailing=TRUE)
        
                atLab3        <- sapply(atRx1, Scaler2)
                stop
              },
              
              {
                # default call
                #cat("axisMethod value unknown:",axisMethod,"\n")
                ErrFnd   <- TRUE
                stopCnt()
                xmsg     <- paste0("***0490 DMP Error in axisMethod set to ",axisMethod," in DrawXAxisAndTitles. ERROR. Default used.")
                stop(xmsg,call.=FALSE)
                atRx <- c(0,1)
              }
        )
        #
        #cat("Method executed\n")
        #cat("atRx :",atRx ,"\n")
        #cat("atLab:",atLab,"\n")
        #cat("rx   :",rx,"\n")
        #print(lineAxisSizes)
        #print(lineDo)
        
        #### Labels selected and Scaling done.
     
        #
        #  Step 2 - Split the labels into two overlaping vectors.
        #           and initialize for finding fit.
        #
     
        lAtRx       <- length(atRx)
        rAtRx       <- range(atRx)          # get first and last label values
        lAtLab      <- length(atLab) 
        #cat("lAtRx:",lAtRx,"  rAtRx:",rAtRx,"  lAtLab:",lAtLab," rx:",rx,"\n")
        #cat("  par(usr):",par('usr'),"  par(pin):",par('pin'),"  xupi:",xupi,"\n")
        #cat("staggered :",staggered,"  staggerLab:",staggerLab,"  staggering:",staggering,"\n")
        
        FndFit   <- FALSE
        MakeStag <- FALSE
       
        #
        # at this point we have:
        #        title1 (opt)
        #        title2 (opt)   (but title 1 or title 2 must be present)
        #        subtitle (optional)
        #        axis # 1 & 2   (both used to overlay axis label plotting.
        #
        #  Adjust the first and last atRx values to move number inward a little.
        #

        atLab1      <- atLab[seq(1,length(atLab),by=2)]
        atLab2      <- atLab[seq(2,length(atLab),by=2)]
     
        atRx1       <- atRx[seq(1,length(atRx),by=2)]
        atRx2       <- atRx[seq(2,length(atRx),by=2)]
     
        #cat("Split label list\n")
        #cat("atLab1:", atLab1 ,"\n")
        #cat("atRx1 :", atRx1  ,"\n")
        #cat("atLab2:", atLab2 ,"\n")
        #cat("atRx2 :", atRx2  ,"\n")
        
        #
        # test to see how axis may draw the labels.
        # if they will not fit our calculations, then must likely
        # will be dropped by R's axis function.  We are trying to out
        # guess R.  
        #
        # Test fitting of single line axis (if not staggerLab) at Normal, -1pt, and -2pt
        #   font sizes.  Then test stagger labels at Normal and -1 pt font size.
        # If these don't work = punt and let the main loop try few labels.
        #
        
        #
        #  Step 3 - Test single line style, if staggerLab not requested by caller.
        #
        
        if (!staggering) {
        
           # labels will not be stagger - by us or caller - at least not yet - so check single line format.
           #cat("NOT STAGGERING - Single Line Style Test\n")
           
           # check the fit of the axis labels, adjust as needed up to a point.
           wX        <- lineAxisSizes["Ax1"]             # original font size
           res       <- TestOverlap(wX, atLab, atRx, 1)  # space between must be 1 space.
           #cat("test1 - ces=wX:",wX,"  res:",res,"\n")
       
           # check X Axis fit as full non-staggered labels.
           if (!res) {
              #cat("full axis no staggered at font - OK -  wX:",wX,"\n") 
              # leave parameters as set.
              FndFit    <- TRUE
           } else {
              # did not fit single line normal point size.
              wX        <- wX - lppt     #   back up 1 point    # orig font - 1 pt
              res       <- TestOverlap(wX,atLab, atRx, 1)
              #cat("test2 - ces=wX:",wX,"  res:",res,"\n")
        
              if (!res) {
                 # Good solution  - update axis parameters
                 lineAxisSizes["Ax1"] <- wX
                 FndFit <- TRUE
              } else {
                 # did not fit single line normal-1pt size.
                 wX <- wX - lppt   #   back up 2 points        # orig font - 2 pt
	         res <- TestOverlap(wX,atLab,atRx, 1)
	         #cat("test3 - ces=wX:",wX,"  res:",res,"\n")
	 
	         if (!res) {
	            # Good Solution - 2 point. - update parameters   
	            lineAxisSizes["Ax1"] <-  wX
	            FndFit <- TRUE
                 } else {
                    # will not fit as single line axis labels.
                    FndFit <- FALSE      
                 }
              }
           }
          
           #  Note: if single line fits, it's still drawn as two overlapping label sets.
            
        } # end of single line checks.
         
        #
        #  Step 4 - if not fit as single or staggerLab requested, test a staggered label style
        #
         
        if (!FndFit) {
         
           # no fit found for single line (or it was bypassed), do staggerLab style.
           
           #cat("Testing staggering style\n")
           
           # find longest staggered label list.
            
           wX      <- lineAxisSizes["Ax1"]   # remember this is already small.
                        
           lAtLab1 <- nchar(paste0(atLab1,collapse=" "))  # space added between each label
           lAtLab2 <- nchar(paste0(atLab2,collapse=" "))  # space added between each label
            
           # Find the longest label set to use for test based on characters.
           if (lAtLab1 > lAtLab2) {
              wAtLab <- atLab1
              wAtRx  <- atRx1
           } else {
              wAtLab <- atLab2
              wAtRx  <- atRx2
           }
            
           #  wAtLab is the longest set of labels based on character count.
           lwAtLab   <- length(wAtLab)
           #cat("Longest of labels - wAtLab:",wAtLab,"  lwAtLab:",lwAtLab,"  axisLowestSize:",axisLowestSize,"\n")
           
           FndFit    <- FALSE
           res       <- TestOverlap(wX, wAtLab, wAtRx, 2)
           #cat("testS1 - cex=wX:",wX,"  2 space res:",res,"\n")
        
           if (!res) {
              # Good should fit using standard height and staggered
              #cat("Initial values are good - keep them:",wX,"  Fit found\n")
              MakeStag   <- TRUE
              FndFit     <- TRUE
           } else {
              # no fit - try one small font
              wX  <- wX - lppt                                 # reduce size 1 point.
              if (wX > axisLowestSize) {  # if bigger then smallest permitted. continue.
                 # test labels and cex
                 res <- TestOverlap(wX, wAtLab, wAtRx, 2)
                 #cat("test s2 - cex=wX:",wX,"  2 space res:",res,"\n")
        
                 if (!res) {
                    # good fit at small font.
                    lineAxisSizes["Ax1"] <- wX
                    #cat("fit found at ",wX,"\n")
                    MakeStag   <- TRUE
                    FndFit     <- TRUE
                 } else {
                    wX  <- wX - lppt                                 # reduce size 1 point.
                    if (wX > axisLowestSize) {  # if bigger then smallest permitted. continue.
                       # test labels and cex - 2 pts.
                       res <- TestOverlap(wX, wAtLab, wAtRx, 2)
                       #cat("test s3 - cex=wX:",wX,"  2 space res:",res,"\n")
                    
                       if (!res) {
                          # goo fit at smaller font
                          lineAxisSizes["Ax1"] <- wX
                          #cat("fit found at ",wX,"\n")
                          MakeStag   <- TRUE
                          FndFit     <- TRUE
                       }
                    }
                 }
              }
           }
        }
        
        if (FndFit)  break  # if have solution - stop looping.
         
        # if not fit, try reducing number of labels.
         
        #cat("End of Single and Staggered - FndFit:",FndFit,"  numLabels:",numLabels," len(atRx):",length(atRx),"\n")
     
     } # end of for loop on number of labels.
     
     # 
     #   Checking is done. Have fit or not.
     #
     #cat("exit numLabels loop\n")
     
     #####  end of loop - have solution???
       
     if (!FndFit) {
        # no solution found????
        
        cat("no XAxis labels fit found!!!\n")
        MakeStag <- TRUE
     }
  
     #cat("end of numLabels loop - FndFit:",FndFit,"\n")

     #cat("atLab1:",atLab1,"\n")
     #cat("atRx1 :",atRx1, "\n")
     #cat("atLab2:",atLab2,"\n")
     #cat("atRx2 :",atRx2, "\n")
  
     #cat("lineDo:\n")
     #print(lineDo)
     #cat("lineAxisSizes:\n")
     #print(lineAxisSizes)
     #cat("lineTopSizes:\n")
     #print(lineTopSizes)
     #cat("lineBotSizes:\n")
     #print(lineBotSizes)
  
     #cat("staggering:", staggering,"  staggered:",staggered,"  MakeStag:",MakeStag,"\n")
  
     #cat("start of edge checking - lastLab2Space:",lastLab2Space,"\n")

     #####
     #
     #  issues with labels - if label/grid near the edge - label hangs over the edge to next column.
     #    solutions:   a) move edge labels inward.   labels like 0 may not need to be moved.
     #                 b) enforce staggered, so next columns number is on a different level.
     #                 c) delete edge label (if > 3 labels)
     #
     #####
     
     #
     #  Step 5 - check edge labels to see if indenting them will help.  
     #
  
     #
     #   Have to sets of labels atLab1 and atLab2...
     #
     #  Situations:
     #       rx[1] = edge (always), no atRx is outside this value.
     #       atRx[1] - rx[1], is units from edge to grid for label
     #       colSepGap can also be used as working space.
     #
   
     #cat("par('usr'):",par("usr"),"\n")
     #cat("par('pin'):",par("pin"),"\n")
     #cat("atRx      :",atRx,"\n")
     #cat("atRx1     :",atRx1,"\n")
     res <- TestLabAtEdge(atLab1,atRx1,YAxisPad,rx,lineAxisSizes)
     # get results.
     w1stLabOverI  <- res$w1stLabOverI
     wLastLabOverI <- res$wLastLabOverI
     atRx1         <- res$atRx
     #cat("1-res$atRx:",res$atRx,"  $1st:",res$w1stLabOverI,"  $Last:",res$wLastLabOverI,"\n")
    
    
     #cat("atRx2     :",atRx2,"\n")
     res <- TestLabAtEdge(atLab2,atRx2,YAxisPad,rx,lineAxisSizes)
     # get results.
     #cat("2-res$atRx:",res$atRx,"  $1st:",res$w1stLabOverI,"  $Last:",res$wLastLabOverI,"\n")
     atRx2         <- res$atRx
     if (res$w1stLabOverI  < w1stLabOverI)  { w1stLabOverI  <- res$w1stLabOverI  }
     if (res$wLastLabOverI < wLastLabOverI) { wLastLabOverI <- res$wLastLabOverI }
     
     #cat("results -> w1st:",w1stLabOverI," in.  wLast:",wLastLabOverI," in.\n")
     #cat("lastLab2Space  :",lastLab2Space," in.\n")
     
     # check the column overlap:
     xW   <- strwidth("0",cex=lineAxisSizes["Ax1"],units="inch")  # get size of a digit in inches.
     xW   <- xW  * XAxis.gapPC    # 75% of the width.
     #cat("sum column overlap:",(w1stLabOverI+lastLab2Space)," in.  Size Digit:",xW," in.\n")
                
     if ((w1stLabOverI + lastLab2Space) <= xW ) {
        # overlap condition.  Force staggered.
        #cat("Lab2 text overlapping between columns - MakeStag set to TRUE\n")
        MakeStag <- TRUE    # set staggering active flag. (column request.)
     }
     
     #  lastLab2Space is the number of inches the left column has intruded into our column.
     #cat("lastLab2Space:",lastLab2Space,"  last column: + need space, - has space. lab 2 row.\n")
     
     # lastLab2Space < 0, last column needs space from us. 
     #     If sum(lastLab2Space,w1stLabOverI) =>  0 there is room.
     #                                        <   0 not enough room - overlap issue.
     #
     # lastLab2Space =>0, last column has space for us.
     #     if sum(lastLab2Space,w1stLabOverI) =>  0 there is room.
     #                                        <   0 not enough room - overlap issue.
     #
          
     lastLab2Space   <<- wLastLabOverI
     #cat("Setting lastLab2Space:",lastLab2Space,"\n")
     #cat("lastLab3Space:",lastLab3Space,"\n")
     
     #cat("staggering:", staggering,"  staggered:",staggered,"  MakeStag:",MakeStag,"\n")
     
     #
     #   Step 6 - if staggered was requested or found to be the solution, set up all parameters.
     #
     
     if (MakeStag) {
        
        # take the two label sets and make a staggered XAxis
        #cat("MakeStag = TRUE - Modifying vector to do staggered.\n")
        
        #  Adjust the sizes of font and spaces between lines for staggered style.
        
        # put in right order for neighboring column
        
        # check status of last column - staggered = TRUE, ended HIGH, = FALSE, ended LOW.
        #cat("Last Column position - staggered:",staggered,"\n")
        
        if (!staggered) {     # staggered = FALSE (no stagger or ended low.) start high. 
           # last column had no stagger, no stagger done, or ends in low position.
           #   move to start in high position.
           # get updated information.
           #
           # No change.
           #
           #s1            <-  seq(1,lAtLab,by=2)
           #s2            <-  seq(2,lAtLab,by=2)
    	   # start high (ax1)
    	   #cat("HIGH position, keep labels in same order - 1st value LOW - atLab1.\n")
        } else {
           # start low
           #s1            <-  seq(2,lAtLab,by=2)
           #s2            <-  seq(1,lAtLab,by=2)
           # switch them
           wAtLab         <- atLab1
           atLab1         <- atLab2
           atLab2         <- wAtLab
           wAtRx          <- atRx1
           atRx1          <- atRx2
           atRx2          <- wAtRx
           #cat("LOW position, swap labels - 1st value HIGH - atLab2.\n")
        }
        
        #cat("lineAxisSizes:\n")
        #print(lineAxisSizes)
        
        #if (lineAxisSizes["Ax1"] == axisNLabSize ) {
        #
        #   # change is not modified previously.
        #   lineAxisSizes["Ax1"] <- axisSLabSize      # set new height for axis # 1
        #}
        lineDo["Ax1"]        <- TRUE              # enable
        #  Change line size same as Ax1 - it may have been reduced.
        lineAxisSizes["Ax2"] <- lineAxisSizes["Ax1"]
        lineDo["Ax2"]        <- TRUE              # enable
           
        #  If subtitle, change it's size and spacing.
        if (lineDo["AST"])  {                     # if subtitle present from before.
           # Scale Subtitle is present with staggered.
           lineAxisSizes["AST"] <- lineAxisSizes["Ax1"]   # reduce title to axis line spacing   # set new subtitle height
           lineAxisSizes["SPT"] <- lineSSpLabSize                                 # set new subtitle space height
        }
           
        if (length(atRx1) != length(atRx2)) {
           # if not the same length the pattern is AVAVA or VAVAV pattern.
           #   in the AVAVA case, staggered must have been FALSE to start high.
           #   in the VAVAV case, staggered must have been TRUE to start low.
           #   in either case, reverse staggered
           staggered <<- xor(staggered, TRUE)
       
        } else {
           # same length pair - AVAV or VAVA pattern.  Leave staggered set the same.
        }     
        #  reduce spacing between titles and axis labels/subtitles.
           
        lineAxisSizes["SP"]  <- lineSSpLabSize     # reduce space to labels/titles       # set new title to axis space height.
        lineMultiB["SP"]     <- 2.25               # fudge on bottom.
                      
        #cat("Make Staggered - settings - lineAxisSizes:\n")
        #print(lineAxisSizes)
        #cat("lineDo:\n")
        #print(lineDo)
     }      
    
     #  #####
     #  #
     #  #  Process StaggerLab option.
     #  
     #  #  split up the labels for overlapping.
     #
     #  #cat("Done with methods - on to plotting.\n")
     #
     ##  now this is already done.  just need to change font sizes.
     #
     ##  if label staggering requested - add space for line NOW!
     ##     don't know by whom!!!
     ##
     ##  staggerLab set by package call parameter (user)
     ##  staggering set by internal code to force staggering for this column.
     ##
     #
     ##cat("staggerLab:",staggerLab,"  staggering:",staggering,"  staggered:",staggered,"\n")
     #
     #if (staggerLab) { staggering <- TRUE }
     #
     ##  atLab1, atLab2, atRx1, atRx2 already setup for the overlaid axis, with sequence normal 1... and 2....
     #
     #if (staggering) {
     #   # Check to make sure we have 2 or more labels.
     #   
     #   if (length(atLab) > 1) {
     #      
     #      # Can only stagger labels if more than one.  Code should not let this happen now.
     # 
     #      #cat("Process staggering request from user or axis label processing - staggerLab:", staggerLab,"  staggering:",staggering,"\n")
     #      # we have enough labels ( > 1 ) to stagger.
     #
     #      # We have already split the labels up into atLab1 and atLab2 for the overlap printing.
     #      # The only think to do to stagger the drawing is to change the 
     #      # spacing of the lines (especially "Ax2").
     #      #
     #      # we are doing staggered axis labels  (2 lines)
     #      
     #      # Staggered Labels - setup
     #    
     #      #cat("adjusting label sizes for staggering with two axis rows. Axis/SubT Size set to small.\n")
     #      
     #      #  Change line size.
     #      if (lineAxisSizes["Ax1"] == axisNLabSize ) {      
     #         lineAxisSizes["Ax1"] <- axisSLabSize      # set new height for axis # 1
     #      }
     #      lineDo["Ax1"]        <- TRUE              # enable
     # 
     #      lineAxisSizes["Ax2"] <- lineAxisSizes["Ax1"]      # add axis # 2   (will cause two rows.)
     #      lineDo["Ax2"]        <- TRUE              # enable
     #      
     #      #  If subtitle, change it's size and spacing.
     #      if (lineDo["AST"])  {                     # if subtitle present from before.
     #         # Scale Subtitle is present with staggered.
     #         lineAxisSizes["AST"] <- lineAxisSizes["Ax1"]   # reduce title to axis line spacing   # set new subtitle height
     #         lineAxisSizes["SPT"] <- lineSSpLabSize                                 # set new subtitle space height
     #      }
     #      
     #      #  reduce spacing between titles and axis labels/subtitles.
     #      
     #      lineAxisSizes["SP"]  <- lineSSpLabSize     # reduce space to labels/titles       # set new title to axis space height.
     #      lineMultiB["SP"]     <- 2.25               # fudge on bottom.
     #                 
     #      #cat("Stagger settings - lineAxisSizes:\n")
     #      #print(lineAxisSizes)
     #      
     #      #
     #      #  Done setting up labels and dual axis with sizes and spaces
     #      #
     #     
     #      #  Now handle the order (high and low.)
     #      #
     #      #  Check what happened in the last column to see where to start the staggering.
     #      #
     #      #  staggered  = false (def) - last column was low (also no staggered may have happened.) 
     #      #                   if staggering = false, ignore leave along, not doing stagger (not in this code)
     #      #                                 = true,  process (go high and process last label.
     #      #
     #      #               true        - last column was staggered and ended HIGH.
     #      #                   if staggering = false, ignore set staggered <- FALSE   (not in this code)
     #      #                                 = true,  process (go low and process last label.
     #      #
     #      #     
     #      
     #      # we are set to start low.
     #      
     #      if (!staggered) {     # staggered = FALSE (no stagger or ended low.) start high. 
     #         # last column had no stagger, no stagger done, or ends in low position.
     #         #   move to start in high position.
     #         # get updated information.
     #         s1            <-  seq(1,length(atLab),by=2)
     #         s2            <-  seq(2,length(atLab),by=2)
     #      # start high (ax1)
     #      } else {
     #         # start low
     #         s1            <-  seq(2,length(atLab),by=2)
     #         s2            <-  seq(1,length(atLab),by=2)
     #      }
     #      atLab1        <-  atLab[s1]
     #      atLab2        <-  atLab[s2]
     #      atRx1         <-  atRx[s1]
     #      atRx2         <-  atRx[s2]
     #      lAtRx1        <-  length(atRx1)
     #      lAtRx2        <-  length(atRx2)
     #      
     #      if (lAtRx1 != lAtRx2) {
     #         # if not the same length the pattern is AVAVA or VAVAV pattern.
     #         #   in the AVAVA case, staggered must have been FALSE to start high.
     #         #   in the VAVAV case, staggered must have been TRUE to start low.
     #         #   in either case, reverse staggered
     #         staggered <<- xor(staggered, TRUE)
     #     
     #      } else {
     #         # same length pair - AVAV or VAVA pattern.  Leave staggered set the same.
     #      }     
     #   }
     #}
     #
     #cat("Staggered - atRx1:",atRx1,"  atRx2:",atRx2,"  len(1):",length(atRx1),"  len(2):",length(atRx2),"\n")
     
     #cat("staggering:",staggering,"   staggered:",staggered,"  MakeStag:",MakeStag,"\n")
     
     #
     #####
     #cat("atLab1:",atLab1,"\n")
     #cat("atLab2:",atLab2,"\n")
     #cat("atRx1 :",atRx1 ,"\n")
     #cat("atRx2 :",atRx2,"\n")
     #cat("\n")
     #cat("lineAxisSizes:\n")
     #print(lineAxisSizes)
     #cat("lineTopSizes:\n")
     #print(lineTopSizes)
     #cat("lineBotSizes:\n")
     
     #print(lineBotSizes)
     
     #
     #  Step 7 - finish setting up the top and bottom labels. 
     #
     
     #  Top margin titles/axis
     lineSizesT        <- c(0,lineAxisSizes,lineTopSizes)    # combine axis and top titles spacings
     names(lineSizesT) <- c("N","Ax2","Ax1","SPT","AST","SP","L2","L1")
     lineSizesTM       <- lineSizesT * lineMultiT
     
     #cat("lineSizesT&TM:\n")
     #print(lineSizesT)
     #print(lineSizesTM)
     
     # calculate the positions of each and add offset.
     linePosT          <- cumsum(lineSizesTM) + 0.01        # get line position of each element
     names(linePosT)   <- c("Ax2","Ax1", "SPT", "AST", "SP", "L2", "L1")
     
     #cat("linePosT:\n")
     #print(linePosT)
     
     #   if overlaped but not staggered, linePosT  "Ax1" and "Ax2" should be the same.
     #
     #####
     
     #####
     #
     #  Bottom margin titles/axis
     #
     lineSizesB      <- c(0,lineAxisSizes,lineBotSizes)     # combine axis and bottom title spacings.
     names(lineSizesB) <- c("N","Ax2","Ax1","SPT","AST","SP","L3","L4")
     lineSizesBM     <- lineSizesB * lineMultiB
         
     #cat("lineSizesB&BM:\n")
     #print(lineSizesB)
     #print(lineSizesBM)
     
     # calculate the positions of each and add offset.
     linePosB        <- cumsum(lineSizesBM) + 0.01   # get line position of each elements
     names(linePosB) <- c("Ax2","Ax1", "SPT", "AST", "SP", "L3", "L4")
          
     #cat("linePosB:\n")
     #print(linePosB)
          
     titleLab3       <- linePosB["L3"]    # make any adjustments in the trailer code.
     titleLab4       <- linePosB["L4"]  
    
     #cat("lineDo:\n")
     #print(lineDo)
     
     #
     #####
     
     ######
     ##
     ##  Calculate the lastLab2Space to hand off to next column.
     ##
     ## if not staggered Labs - then calculate the space left 
     #
     #LabLastOverU   <- rightEdge - EndOfLabs[lAtRx]
     #LabLastOverI   <- LabLastOverU / xupi
     #lastLab2Space <<- LabLastOverI
     ##cat("Setting lastLab2Space & LabLastOverI",lastLab2Space,"\n")
     #
     ## if pos value - we have room.  neg - we need room.
     
     # 
     #  End of Xaxis processing.
     #
     ########
             
     ########
     #
     # Column Headers - printing
     #
     # Note: mgp(a,b,c) - a => position for axis labels,  b,c => position for axis values and line, 
     #       in mcex values. def = c(3,1,0)
     #    
     #
     
     #  Select panel and re-scale - 1st panel (top) to do title/labels and axis labels
     
     #cat("DX02-column headers printing - rx:",rx,"  ry:",ry," i:",1,"  j:",j,"\n")
     panelSelect(panels,1,j)
     
     x <- panelScale(rx,ry)
     par(xpd=T)

     # print in margin space above panel 1 of column.

     #
     #  column titles
     #     
     if (lineDo["L1"]) mtext(lab1[j],side=3,
                             line=linePosT["L1"], cex=lineTopSizes["L1"])
     
     if (lineDo["L2"]) mtext(lab2[j], side=3,
                             line=linePosT["L2"], cex=lineTopSizes["L2"])
    
     #
     # axis sub-title
     #
     if (lineDo["AST"])  {
         mtext(axisSubTitle, side=3, 
              line=linePosT["AST"], cex=lineAxisSizes["AST"])      # line 2 or 3
     }

     #    
     # column top axis(es)
     #
     if (lineDo["Ax1"]) {                                                             # line 1 or 2 (above axis # 2)
   
         #cat("Top-axis calls -   atLab1:",atLab1,"  atRx1:",atRx1,"\n")
         #cat("  mgp:linePosT['Ax1']:",linePosT["Ax1"],"\n",
         #    "  lineAxisSizes['Ax1']:",lineAxisSizes["Ax1"],"\n")
        
         axis(side=3,  tick=F, at=atRx1, labels=atLab1,
              mgp=c(3.2,linePosT["Ax1"],0), 
              cex.axis=lineSizesT["Ax1"] ) 
     }
     
     if (lineDo["Ax2"]) {                                                             # line 1
         #cat("Top-axis calls -   atLab2:",atLab2,"  atRx2:",atRx2,"\n")
         #cat("  mgp:linePosT['Ax2']:",linePosT["Ax2"],"\n",
         #    "  lineAxisSizes['Ax1']:",lineAxisSizes["Ax1"],"\n")
        
         axis(side=3,  tick=F, at=atRx2,  labels=atLab2, 
              mgp=c(3.2,linePosT["Ax2"],0), 
              cex.axis=lineAxisSizes["Ax1"])   # this is not an error, Ax2 is always printed the same size as Ax1
    
     }
     
     #
     ######## end of column header
    
     
     #####
     #
     # Column Trailers
     #
     # Select and Scale to bottom panel in column
     
     #cat("DX03-trailer column headers - numGrps-i:",numGrps," j:",j,"  numGrps:",numGrps,"  rx:",rx,"  ry:",ry,"\n")
    
     panelSelect(panels,numGrps,j)
     x <- panelScale(rx,ry)
     par(xpd=T)

     # print in margin space below bottom panel
     
     # padj in axis needed to make grid line label close
     
     #####
     #
     # Adjustment values to make bottom labels have the same space as the top labels.
     #   old method.
     #
     #botLAdj        <-  -0.05                         # label height adjustment  (title/reftext)
     #botAdj         <-  -lineSizesB["Ax1"]           #  ->> (-0.666667 or -0.5 ) (axis row height in lines)
     #botAxisAdj    <-  - 0.2 #-lineSizesB["Ax1"] * 0.3333  # - 0.05   #  1/2 * 0.8 of row height in lines
    
     #botAxisBase    <-  Title.Line.2x.pos - ( ( 1 - lineSizesB["Ax1"] ) * 0.6333333 )
     
     #cat("bottom title/labels-botLAdj:",botLAdj,"\n")
     #cat("     botAdj:",botAdj,"\n")
     #cat("     botAxisAdj:",botAxisAdj,"\n")
     #cat("     botAxisBase:",botAxisBase,"\n")
     #
     #
     #####
     
     #####
     #
     # new bottom margin line adjustment algorithm
     #
     desiredCex    <- lineAxisSizes["Ax1"]
     xPs           <- par("ps")    # get current system base point size being used.  Everything is based on this value.
     xHPsLU        <- strheight("00000",cex=1,units="user")
     xHDesPsLU     <- strheight("00000",cex=desiredCex,units="user")
     xDifHU        <- xHPsLU - xHDesPsLU       # different between system line and our line
     xBotAdj       <- xDifHU / xHPsLU          # ratio of dif (not used) and full line. % percent of line.
     
     botAxisBase   <- 0.15 - xBotAdj    # in lines.
     botAxisBAdj   <- botAxisBase  # + 0.05
     botLAdj       <- 0.05
     #cat("New Bottom - botAxisBase:",botAxisBase,"  botAxisBAdj:",botAxisBAdj,"  botLAdj:",botLAdj,"\n")
     
     # column bottom axis lines
     
     if (lineDo["Ax1"]) { 
        #cat("Bot-axis #1 - linePosB['Ax1']:",linePosB["Ax1"],"\n",
        #    "  lineAxisSizes['Ax1']:",lineAxisSizes["Ax1"],"\n",
        #    "  botAxisBase:",botAxisBase,"\n",
        #    "  botAxisBAdj:",botAxisBAdj,"\n")
        #cat("  atRx1:",atRx1,"  atLab1:",atLab1,"\n")
   
        axis(side=1, tick=F, at=atRx1, labels=atLab1, line=botAxisBAdj, 
                mgp=c(3.2, linePosB["Ax1"],0), 
                cex.axis=lineAxisSizes["Ax1"])
     }

     if (lineDo["Ax2"]) { 
        #cat("Bot-axis #2 - linePosB['Ax2']:",linePosB["Ax2"],"\n",
        #    "  lineSizesB['Ax1']:",lineSizesB["Ax1"],"\n",
        #    #"  botAxisAdj:",botAxisAdj,"\n")
        #    "  botAxisBase:",botAxisBase,"\n")
        #cat("  atRx2:",atRx2,"  atLab2:",atLab2,"\n")

        axis(side=1, tick=F, at=atRx2, labels=atLab2, line=botAxisBAdj, 
                mgp=c(3.2, linePosB["Ax2"],0), 
                cex.axis=lineAxisSizes["Ax1"])
     }
     
     # if axis sub-title
     if (lineDo["AST"]) {
        wAST <- linePosB["AST"] + botAxisBAdj
   
        #cat("BotAST - linePosB['AST']:",linePosB["AST"],"\n",
        #    "  lineAxisSizes['AST']:",lineAxisSizes["AST"],"\n",
        #    #"  botAxisAdj:",botAxisAdj,"\n")
        #    "  botAxisBase:",botAxisBase,"\n")
        #cat("  line=wAST:",wAST,"\n")
        
        mtext(axisSubTitle, side=1, line = wAST, 
                  cex=lineAxisSizes["AST"])
     }
     # ______Bottom Label/Title - Lab3 ______
     
     # bottom of column footnote (title)
     if (lineDo["L3"]) {
        titleLab3 <- linePosB["L3"] + botAxisBase - 0.05
        
        #cat("BotAxis # 3 - linePosB['L3']:",linePosB["L3"],"\n",
        #    "  botAxisBAdj:", botAxisBAdj, "\n",
        #    "  botAxisBase:", botAxisBase, "\n",
        #    #"  botLAdj   :",botLAdj,"\n",
        #    "  line=titleLab3:",titleLab3,"\n")
 
        mtext(side=1,lab3[j], line=titleLab3, cex=lineBotSizes["L3"])  # bottom labels.
     }  
     # _______Reference Value Legend
 
     titleLab4     <- linePosB["L4"]  + botAxisBase 
     
     #cat("reftxt:",reftxt,"  refval:",refval,"  lineDo[L4]:",lineDo["L4"],"\n")
     #cat("BotAxis # 4 (reftext) - linePosB['L4']:",linePosB["L4"],"\n",
     #    "     botAxisBase:",botAxisBase,"\n",
     #    #"     botAxisAdj:",botAxisAdj,"\n",
     #    #"     botLAdj:",botLAdj,"\n",
     #    "     line=titleLab4:",titleLab4,"\n")
       
       
     #  Handle special needs of the reftxt and it's line for a single column
     
     if (!is.na(refval)) { 
      
        if (is.between.r(refval,rx)) {  # refval must be in the range of the data. Otherwize - ignore.
  
           if (!is.na(reftxt) ) {
           
           
              #  Get y pos in user units to draw the line and text.
              # select panel done before this call.
         
              xpin          <- par("pin")                  # distances in inches
              xusr          <- par("usr")                  # distances in user scale (points)
              xmar          <- par("mar")
              xmai          <- par("mai")
              
              #fpc           <- 0.95                        # fudge adjustment 
              
              #cat("xpin:",xpin," xusr:",xusr," xmar:",xmar," xmai:",xmai,"\n")
           
              #
              #  Calculate X positions for the line and text in the margin. (units=user)
              #
           
              xCenterU      <- mean(xusr[1:2])             # center of the glyph column
              xWidthU       <- diff(xusr[1:2])             # unit width of glyph column => diff(rx) - user units
              xUnitsPerInch <- xWidthU/xpin[1]             # units / inch for x
              xHalfInchU    <- xUnitsPerInch * 0.5 #* fpc   # 1/2" of units
           
              #cat("  center of glyph-xCenterU:",xCenterU, "\n",
              #    "  width of glyph -xWidthU :",xWidthU, "\n",
              #    "            xUnitsPerInch :",xUnitsPerInch,"\n",
              #    "               xHalfInchU :",xHalfInchU,"\n")
              #
              #  line length will be whats left after taking away room for text or 1/2 inch 
              #
              
              xTxt          <- stringr::str_trim(reftxt)            # get refText and trim side blanks.
              
              # length of texts in units
              xTxtLenU      <- strwidth(xTxt,units="user", cex=lineSizesB["L4"]) #* fpc  # length text
              
              # 
              xHalfFreeLenU     <- ((xWidthU - xTxtLenU) / 2 ) #* fpc    # half space left for line 
              #cat("xTxtLenU:", xTxtLenU,"  half free avail-xHalfFreeLenU:",xHalfFreeLenU,"\n")
              
              xLineLenU <- xHalfFreeLenU
              
              #  see if room for half inche line, if not use shorter value.
              if (xLineLenU > xHalfInchU)  xLineLenU <- xHalfInchU    # get length of line to 1/2 inch
              
              # calculate start of line.
                            
              xLineStartu   <- xCenterU - (xLineLenU + xTxtLenU) / 2   # center - half (text length + line length)
              
              xTxtStartu    <- xLineStartu + xLineLenU
          
              #cat("xLineStartu:",xLineStartu," xTxtStartu:",xTxtStartu,"\n")
      
              #
              #  Calculate the Y positions for the line and text in the margin for the refText.
              #     line needs units=users,  text needs "lines"
              #
              xTitleLab4    <- titleLab4 # +  botLAdj      #  Text Line offset from Axis line.   
                
              #cat("ConvLineToUser call-xTitleLab4:",xTitleLab4,"\n")
              
              yTextPosu     <- ConvLineToUser(1, xTitleLab4)      # position text position in user units.
              yTextHu       <- strheight(xTxt, units="user", cex=lineSizesB["L4"])  # find height of text in user units.
              #cat("yTextPosu:",yTextPosu,"  yTextHu:",yTextHu,"\n")
              
              #  position of line based on Text position(user) - 60% of the text height.
              yLinePosu     <- yTextPosu - (yTextHu * 0.6)                # lines y coord. is 1/2 text height toward plot.
            
              #cat("Y Position for L4 - line(u):",yLinePosu,"  text(u):",yTextPosu,"   text(l) xTitleLab4:",xTitleLab4,"  titleLab4:",titleLab4,"\n")
              
              #cat("xTitleLab4:",xTitleLab4,"  titleLab4:", titleLab4,"\n",
              #     "   linePosB['L4']  :",linePosB["L4"],"\n",
              #     "   lineSizesB['L4']:",lineSizesB["L4"],"\n",
              #     "   botAxisAdj      :",botAxisAdj,"  botLAdj:",botLAdj,"\n")
              
              # way to find graphic length of string --> sw <- strwidth(reftxt,cex=Text.cex)
               
              # add text definition for legend.   (5/21/13 - added color to line)
              # draw line.    
              lines(c(xLineStartu, xTxtStartu), rep(yLinePosu, 2), 
                     lty=Ref.Val.lty, lwd=Ref.Val.lwd, col=iRef.Val.col)      # draw length line up to 1/2 inch.
              
              # mtext does not let you set the X position of the text, so the old text function must be used with x, y coordinates.
              
              text(xTxtStartu, y=yLinePosu, labels=xTxt, 
                      cex=lineBotSizes["L4"], col=iRef.Text.col, 
                      offset=0, adj=c(0,NA))                       # text starting at line end.

              #cat("Line%Start:", xLineStartu/xWidthu, "  Txt%Start:",xTxtStartu/xWidthu,"  titleLab4:", titleLab4,"\n")
 
           }
        }
     }
   
     #
     ##### end of trailer
     
     #cat("Returned staggered:",staggered,"\n") 
     
     return(list(atRx=atRx, rx=rx, ry=ry))   # return the set of tick points for grid lines. 
  }
  #
  ###

  ###
  #
  #   MapDrawer
  #
  MapDrawer <- function(wAreaVisBorders, wL2VisBorders, wRegVisBorders, wL3VisBorders, WorkList) {
     #
     #
     #
     
     wLen <- dim(WorkList)[1]   # get number of entries
     
     for (ind in c(1:wLen)) {
     
        wEntry <- WorkList[ind,]
        
        if (wEntry$Borders==1) {   # L2 borders
          #  Map background - Layer 2 borders   (regional areas  (US -> states))
            polygon(wL2VisBorders$x, wL2VisBorders$y,
                    density=-1, col=wEntry$Fill.col, border=FALSE)
            polygon(wL2VisBorders$x, wL2VisBorders$y,
                    density=0, col=wEntry$Line.col, lwd=wEntry$Line.lwd)
        }
        if (wEntry$Borders==2) {   # L1 colors

            polygon(wAreaVisBorders$x,wAreaVisBorders$y,
                    density=-1, col=wEntry$Fill.col, border=FALSE)
     
        }
        if (wEntry$Borders==3) {   # L1 borders
        
           #  setup each group of sub-areas and draw polygons.
           #    Not Referenced sub-areas  
           wVisBorders   <- wAreaVisBorders[wEntry$Selected,]
           polygon(wVisBorders$x,wVisBorders$y,
                    density=0, col= wEntry$Line.col, lwd=wEntry$Line.lwd)
        } 
         
        if (wEntry$Borders==4) {   # L3 borders 
             # Outline Country area (total area).
 
           polygon(wL3VisBorders$x, wL3VisBorders$y,
                    density=0, col=wEntry$Line.col, lwd=wEntry$Line.lwd)      # outside US boundary
        }
     }
  }

  #
  ###

  ###
  #
  #  MapPolySetup function - used by all areaMap glyphs to process the panel dimensions 
  #   and adjust the x and y ranges and scales for the particular map used.
  #
  #

  MapPolySetup <- function(mapType,wPanels,wAreaVisBorders,wL2VisBorders,wRegVisBorders, wL3VisBorders, DL3) {
     # entire area..  (what if subset is used.)
     # all but L3VisBorders
     
     if (DL3) {
        # all sets of boundaries
        rxpoly   <- range(wL3VisBorders$x,wRegVisBorders$x,wL2VisBorders$x,wAreaVisBorders$x,na.rm=TRUE)
        rypoly   <- range(wL3VisBorders$y,wRegVisBorders$y,wL2VisBorders$y,wAreaVisBorders$y,na.rm=TRUE) 
     } else {
        rxpoly   <- range(wRegVisBorders$x,wL2VisBorders$x,wAreaVisBorders$x,na.rm=TRUE)
        rypoly   <- range(wRegVisBorders$y,wL2VisBorders$y,wAreaVisBorders$y,na.rm=TRUE) 
     }
  
     rxadj    <- diff(rxpoly) * 0.02   # adjust x by + and - 2% of the size of the range
     rxVadj   <- c(-rxadj,rxadj)
     rxpoly   <- rxpoly + rxVadj
  
     ryadj    <- diff(rypoly) * 0.05   # adjust y by + and - 5% of the size of the range.
     ryVadj   <- c(-ryadj,ryadj) 
     rypoly   <- rypoly + ryVadj
  
     yxA      <- diff(rypoly) / diff(rxpoly)  # calculated aspect from MAP information.
     #cat("Map yxAspect:", yxA, "\n")
  
     #print(paste0("rxpoly:",paste0(rxpoly,collapse=" "),"   rypoly:",paste0(rypoly,collapse=" ")))
 
     # aspect ratio is y / x...
  
     # size of space in panel =
  
     panelW    <- diff(wPanels$coltabs[j+1,])
     panelH    <- diff(wPanels$rowtabs[2,])   # grap first row as model - All should be the same except median row
     #cat("Panel W:",panelW,"  H:",panelH,"\n")

     #cat("banner.max:",banner.max[mapType,"width"],"\n")
    
     rxDiff    <- diff(rxpoly)
     ryDiff    <- diff(rypoly)
     rxpoly2   <- rxpoly
     rypoly2   <- rypoly

     #
     #  Adjust rx and ry - rule: NEVER NEVER decrease rx or ry.
     #  if map Aspect (y/x) is lower then panel (h/w) then 
     #     example: 90/150 = 0.6   and   0.78/1.117 -> 0.698
     #         150 <> 90 * 1.117 / 0.78
     #         map in this space is  about 104/150  map will be taller then it should be.
     #         increase y range 
     #  if map Aspect (y/x) is high than panel (h/w) then 
     #     example: 90/150 = 0.6   and   0.66/1.117 -> 0.59
     #         map in this space is about  88/150   map will be wider then it should be
     #         increase x range.
     #
     #  One assumption is that the original panel width and height were laid out
     #  to accomodate the minimum/maximum height, aspect ratio, and title lengths.
     #  
     #  This is to adjust to fit the space.
     #  Objective:
     #            ryDiff         panelH
     #          ---------   =   --------     -->   rxDiff =? ryDiff * panelW / panelH
     #            rxDiff         panelW
     #
     wfx       <- ryDiff * panelW / panelH
     if (wfx > rxDiff) {   
        # change rx (expand)
        wfxd    <- abs(wfx - rxDiff)
        vfx     <- c(-wfxd/2,wfxd/2)
        rxpoly2 <- rxpoly + vfx
     } else {
        # change ry (expand)
        wfy     <- rxDiff * panelH / panelW
        wfyd    <- abs(wfy - ryDiff)                         # change needed.
        vfy     <- c(-wfyd/2, wfyd/2)
        rypoly2 <- rypoly + vfy
     }
     #cat("rxpoly2:",rxpoly2,"   rypoly2:",rypoly2,"\n")

     return(list(rxpoly2=rxpoly2, rypoly2=rypoly2))
  }
  #
  ###

  ###
  #
  #  Function to split numeric X,Y coordinate vectors based on NA. 
  #
  #  Return is a list of parts of the original vector up to the NA.
  #
  MMVSplit <- function(wX,Brks) {
   
     #print(Brks)
     wXa       <- wX
     wXa[Brks] <- NA
     wXs       <- split(wXa, cumsum(Brks))     # split up vector into smaller vectors in list
     wXz       <- sapply(wXs,  function(x) x[!is.na(x)])  # remove NAs 
   
     #print(wXz)
     return(wXz)
  }
  #
  ###

  ###
  #
  #  printPanelsParms - prints the associated parameter in creating a panel.
  #
  printPanelParms <- function(t) {
     print(t)
     cat("numGrps:",numGrps,"\n")
     cat("numCol :",numCol,"\n")
     cat("topMar :",topMar,"\n")
     cat("botMar :",botMar,"\n")
     cat("rowSize:",paste0(rowSize,collapse=" "),"\n")
     cat("rowSep :",paste0(rowSep,collapse=" "),"\n")
     cat("colSize:",paste0(colSize,collapse=" "),"\n")
     cat("colWidths",paste0(colWidths,collapse=" "),"\n")
     cat("colSep :",paste0(colSep,collapse=" "),"\n")
     cat("rSizeMx:",rowMaxH,"\n")
     cat("rSizeMn:",rowMinH,"\n")
     cat("rSizeMaj:",rowSizeMaj,"\n")
     cat("rMapCol:",PDMapCol,"\n")
     cat("\n")
  }
  #
  ###

  ###
  #
  #_________ function to pattern match alias names and return associated abbr.
  #
  SeerToAbbr <- function(xR,aNAI) {
     # xR   --> a vector of the registry names from SeerStat output (data)
     # aNAI --> a vector of abbr and alias values from the name table $Abbr and $Alias 
     ErrFnd  <- FALSE
   
     wReg    <- toupper(xR)
     wAbbr   <- rep(NA,length(wReg))
     xout1   <- sapply(c(1:length(aNAI$Alias)), function (x) grep(aNAI$Alias[x], wReg, ignore.case=TRUE))
     xout1a  <- unlist(xout1)
   
     xout2   <- !is.na(lapply(xout1, function(x) ifelse(length(x)==0,NA,x)))
     xout3   <- unlist( lapply( xout1, function(x) { if(length(x[])>1) { x } else { NA } } ) )
   
     if (any(!is.na(xout3))) {
        xout4   <- paste0(xout3[!is.na(xout3)], collapse=" ")
        xmsg    <- paste0("***MST-30 Registries in the data have duplicate name in rows:",xout4, "  Only one row per area is permitted.\n")
        ErrFnd  <- TRUE
        stopCnt()
        stop(xmsg, call.=FALSE)
     }
   
     if (!ErrFnd) {   # continue
   
        wAbbr[xout1a] <- aNAI$Abbr[xout2]
   
     }
   
     return(wAbbr)    # return list of abbreviates or NA if no match.
  }   

  #
  ###

  ###
  #
  # function to calculate and return scaling variable - ksc
  #
  # based on the value of xke =>  1 to 5.
  #  UPDATE to pass real height, and handle 1 to 6 properly.  This code assumes height used for 5.
  # 
     
  SetKsc <- function(xke) {

     C13  <- 0.33333
     if (xke == 1) {
        wKsc   <- 1
     } else {
        wKsc   <- (xke + C13)/(5 + C13) # scale value for the "y" element of the bar to keep uniformity
     }
     return(wKsc)
  }

  #
  ###
     
  #
  #
  #### end of micromap functions (glyphs and micromapST internal functions)
  #
  #
  ###########################
  ###########################

  #print("micromapST functions loaded")

  ################################################################################
  #
  #
  #   Continue startup - verification code.
  #
  #
  ################################################################################

  ################################################################################
  #
  #  Call Argument validation  
  #

   #  
   # Previously Checked:
   #
   #    bordDir
   #    bordGrp
   # 
   #    load border group   and the five data.frames
   #
   #    Start setting up .GlobalEnv variables.
   #
   #  1) statsDFrame -> present
   #  2) panelDesc   -> present
   # 
   #  3) statsDFrame -> get column names and number of columns; 
   #  4) statsDFrame & rowColName ->  locate row names for later linking.
   #  5) Compare row names and name table
   #  6) Check for duplicate statsDFrame rows
   #  7) Handle dataRegionsOnly call parameter - sub-map Setup
   #  8) Set values for regional or full mapping.
   #
   #  9) rowName
   #  
   #  Basic checks to make sure statsDFrame and panelDesc arguments are present and usable.
   #     More detailed checks done later.
   #
   
   StopFnd   <- FALSE

   #
   #  For statsDFrame and panelDesc - checks cover the single attributes of the variables,
   #  but do not handle a check for them being one element and NA.  Can grab the first element
   #  and test it, but it may be valid data.   Must verify the size and then if single, test for NA.
   # 
   
   #
   #_________ 1) statsDFrame (basic check) argument
   #
   #  check to see if the statsDFrame was provided and is the correct datatype

   if ( missing(statsDFrame) || is.null(statsDFrame) || !is.data.frame(statsDFrame) ) { 
       stopCnt()
       StopFnd   <- TRUE
       xmsg      <- paste0("***0101 CARG-DF First argument (statsDFrame) is missing or not a data.frame.")
       warning(xmsg, call. = FALSE)
   }
   
   #
   #_________ 2) panelDesc -  Basic initial check - Process the arguments
   #
   #  check to see if the panelDesc was provided and is the correct datatype.

   if ( missing(panelDesc) || is.null(panelDesc) || !is.data.frame(panelDesc) || !is.list(panelDesc) ) { 
       stopCnt()
       StopFnd   <- TRUE
       xmsg      <- paste0("***0111 CARG-PD The second argument, the panelDesc structure, is missing or not a data.frame or list.")
       warning(xmsg, call. = FALSE)
   }
   
   #

   if (StopFnd) {
      stopCnt()
      xmsg     <- paste0("***01Z0 CARG Key call arguments are missing, NULL, wrong type, or NA, Execution stopped.")
      stop(xmsg, call. = FALSE)
   }
   
   ### most of panelDesc is validated later.

   #print("statsDFrame and panelDesc variables are present and the correct type.")
   
   #
   #  Now get the column names of the statsDFrame and verify the match up of the rownames with 
   #   the border group names, abbreviations or IDs.
   #

   #_________ Get list of column name in statsDFrame for parameter verification

   wSFName      <- names(statsDFrame)        # get the column names from data.frame

   len_wSFName  <- length(wSFName)           # record the number of "named" rows in list (at front.)

   wSFNameList  <- c(wSFName,seq(from=1, to=len_wSFName))   # add valid row numbers to the list.

   numRows      <- nrow(statsDFrame)         # get number of rows in statsDFrame

   if (numRows == 1 && len_wSFName == 1) {  # we have a singular element variable
      # We only have one item in structure - is it a NA?
      if (is.na(statsDFrame[[1]][1])) {  # NA value was provided.
         stopCnt()
         xmsg     <- paste0("***01Z0 CARG Key call arguments are missing, NULL, wrong type, or NA, Execution stopped.")
         stop(xmsg, call. = FALSE)
      }
   }
  
   #
   #  wSFNameList now contains a list of the column names and column numbers 
   #      as character strings. This string will be used to verify any user 
   #      provided column names or character column numbers.
   #

   #
   # Start Accumulating the micromapST System variable list
   #
   mmSys        <- list(SFVarName = sDFName, SFNameL = wSFNameList, SFColNum = len_wSFName, SFRowNum = numRows)
   
   #print("mmSys")
   #print(mmSys)
   
   #
   #  Check to make sure user provide data frame has at least 1 row and at least 1 column.
   #
   if ( len_wSFName == 0 || numRows == 0 ) {

      xmsg      <- paste0("***0103 CARG-DF The ",sDFName," statsDFrame data.frame has no columns or rows. ")
      StopFnd   <- TRUE
      stopCnt()
      stop(xmsg, call. = FALSE)
   }

   #
   #  statsDFrame - data rows
   #
   #  headers or total area rate rows should not be included in data.format structure.. 
   #

   #
   #______________statsDFrame - data frame - verify row links/names______________
   #
   
   numRowsBG   <- nrow(areaNamesAbbrsIDs)      # get number of rows in name table
   
   #
   #  Must validate statsDFrame row names against the area list - process rowNamesCol to be able to proceed with 
   #  the link verification.
   #
   
   #
   #  Step 1 - find out where the row names for the sub-area are in the statsDFrame data.frame.
   #
   
   ###
   ###  If user provided a column with the sub-area "names", then we have to check to make
   ###  sure there are no duplicates in the statsDFrame data.frame.  If they were in the row.names, 
   ###  R already makes sure there are no duplicates.
   ###
   ###  Dont care what type of link it is at this point.
   ###
    
   #
   #_____________Check and Process the rowNamesCol call argument/parameter option___________________
   #
   
   StopFnd     <- FALSE
   ErrFnd      <- FALSE
   
   len_rowNamesCol <- length(rowNamesCol)
   rowNamesColx    <- rowNamesCol[[1]][1]   # get first item
   
   if ( missing(rowNamesCol) || is.null(rowNamesCol) || is.na(rowNamesColx) ) { 
  
      #  rowNamesCol is missing or not provided - no action - use the row.names on statsDFrame for the sub-area names.
      
      statsDFrame$RN <- rownames(statsDFrame)            # get a copy of the rownames.  (row.names)
      
      #  If no rowNamesCol provided, then we must assume the row names (sub-area names) are being
      #  provided in the row.names of the data.frame.  If the row.names were not assigned a sub-area 
      #  identifier (full name, abbr, alias, or ID) by the user, then the row.names will be 
      #  then we will just get "1", "2", ... as the row.names and they will not match anything.
      
      #  could be dangerous - later it may be best to STOP.
  
      #print("No rowNamesCol provided - must be in row.names")
  
   } else  {
   
      #  Have the rowNameCol call argument/parameter and a statsDFrame column name/number retrievve the sub-area links.
      
      # 
      if (len_rowNamesCol > 1) {
      
         # rowNamesCol can only be a vector of length 1. 
         warnCnt()
         ErrFnd      <- TRUE
         xmsg        <- paste0("***0173 CARG-RNC The rowNamesCol argument value must have a length = 1. Only first value used.")
         warning(xmsg, call. = FALSE)
         
         # we already did a [[1]][1] to get the first items in the variable.
         # we are just reporting on the length saved eariler.
      }
      
      # Look up the name and covert it into a column number - or - verify column number..
      if (!is.character(rowNamesCol) && !is.numeric(rowNamesCol) && !is.integer(rowNamesCol)) {
         
         # rowNamesCol is not the correct type of vector.   
         stopCnt()
         StopFnd    <- TRUE           # stop because user did specify, but its wrong.
         
         xmsg       <- paste0("***0172 CARG-RNC The rowNamesCol argument value must be a character or numeric value. ", "It was found to be: ",class(rowNamesCol),".")
         stop(xmsg, call. = FALSE)
      }
 
      litrowNamesCol = rowNamesCol    # Save the original literal value from rowNamesCol (could be number or name)
     
      rowNamesCol  <- CheckParmColx(litrowNamesCol, c('RNC','rowNamesCol'), wSFNameList, len_wSFName)   # see if value is good.
      #      if error, CheckParmColx issues the warning message and return 0.
        
      # got column number(s) if good. Multiple columns doesn't make sense. 
        
      if (!all(rowNamesCol>0)) {      # check to see if the value is good (>0 -> a valid column number)
      
         #  Bad column name or column number found. Error message was generated by CheckColx.
         
         stopCnt()
         StopFnd      <- TRUE         # again stop because user specified, but its wrong.
         xmsg         <- paste0("***01Z1 CARG Errors found in call arguments. Execution stopped.")
         stop(xmsg, call.=FALSE)
         #
         # Cant continue.  User provided rowNamesCol, so must have a valid column 
         # name/number and a valid list of links. If not, then looking at the row.names 
         # of the data.frame does not make sense.  Why would they specify a rowNameCol?
         #
      } 
      
      #
      #	 if problems are identified prior to this line, the package has stopped.
      #
      #  At this point, the rolColName exists and is a valid column name.
      #
      
      #cat("rowNamesCol is valid : ",rowNamesCol," - Now check for duplicates.","\n") 
      
      ###
      #
      #  get copy of column, convert column into row.names, but first
      #  need to check for duplicates before we do this.  Dont have to do this 
      #  check if sub-area names are in row.names of the data.frame.
      #
      
      dupNames   <- duplicated(statsDFrame[,rowNamesCol])
      dupRows    <- c(seq_along(dupNames))[dupNames]
      
      if (any(dupNames)) {
    
         StopFnd    <- TRUE
         stopCnt()
         xmsg       <- paste0("***0171 CARG-RNC The row names in column ",rowNamesCol, " of the ",sDFName," statsDFrame data frame contain duplicates. Only one row per sub-area is permitted. Duplicate rows are:", paste0(dupRows,collapse=","),".")
         stop(xmsg, call. = FALSE)
         
         #  possible work a round - later - is to delete the second occurance.
            
      }
      
      #
      #  No duplicate sub-area row names in the statsDFrame data provided by user and 
      #  column name or number is good -  move column to $RN  
      #
      
      #print("Moved to $RN")
      
      statsDFrame$RN <- statsDFrame[,rowNamesCol]
       
   }
   
   statsDFrame$rawRN      <- statsDFrame$RN           # save raw format of row name.
   statsDFrame$RN         <- toupper(statsDFrame$RN)  # upper case for comparisons.
   row.names(statsDFrame) <- statsDFrame$RN           # save in statsDFrame$RN as the row.names 
      
   #
   ###
   
   ###
   #
   #   At this point the sub-area names from the row.names on statsDFrame or 
   #   the sub-area names in a column of the statsDFrame have been added to the 
   #   internal statsDFrame data.frame in the $RN column.  If the values were
   #   checked for duplicates if provided in a data.frame column.
   #
   
   #
   # Next step is to validate the names against the programmed name list.
   #
   #    If provided in column (rowNamesCol), they are checked and moved to row.names.
   #    We only know they are unique.  Another check is needed to see if they match
   #    the area name/abbr/ID list.
   #
   ###

   ##____________statsDFrame rows OK to count

   #
   #   JP - Make sure the input data.frame is at least two columns - add one.  A single column data.frame
   #        acts differently then a two or more column data.frame under many operations.
   #
   #   JP - Dot code (at least) has problems with single column statsDFrame structures.
   #
   #   To protect code and any other areas that may have problems,
   #   quick fix is to append "0" column to the right of the provided data.frame.
   #   This forces the data.frame to be at least 2 columns.
   #
   
   numRows <- nrow(statsDFrame)
   Ex      <- rep(0,numRows)
   
   ADFrame <- cbind(statsDFrame,Ex)     # move to ADFrame and add Zero column.
        # a 1 column data.frame has a little different behavior the s 2 column data.frame

   #cat("Add 0 column to statsDFrame\n")

   #
   #   statsDFrame number of rows - validated.
   #

   #####
   #
   # Get general defaults   ->   colors and details
   #

   par(fin = par("din"))   # safety value to get moving.
   plot.new()

   #
   # ________________Load Colors and Details defaults_______________________________
   #

   #print("Calling micromapGSetDefaults")

   micromapGDefaults <- micromapGSetDefaults()  # get master list of variables and defaults

   #print("Got data.frame from micromapGSetDefaults")
   
   #####
   #
   #_________________colors _______________________________________
   #

   #  Must do after completing the details list processing
   #
   #  Verify "colors=" argument
   #
   #  Second purpose is to set the graphics colors not in the "colors" vector to grays or colors.
   #
   #  Read defaults into memory
   #

   #print("Validate colors")

   colFull      <- TRUE                  # control logical = TRUE doing Color, FALSE doing Greys
   NoErrs       <- TRUE
   doDotOutline <- FALSE
   
   mstColors     <- colors               # Multiple values none should be an NA.
   
   if ( missing(colors) || is.null(mstColors) || is.na(mstColors[[1]][1]) ) {
 
      mstColors  <- micromapGDefaults$colors    # use package defaults.

   } else {

      if (typeof(mstColors) == "character") {
    
         if (length(mstColors) != 24) {
      
            if (length(mstColors) == 12)  {  # check for the basic colors.
          
               # we have the basic 12 colors. Expand to the list of 24.
               colorlab      <- names(mstColors)
               TransColors   <- adjustcolor(mstColors,0.2)
               mtColors      <- c(mstColors, TransColors)
            
               if (!is.null(colorlab)) { names(mstColors) <- c(colorlab,paste0("l_",colorlab)) }
        
            } else {
      
               if (length(mstColors) == 1) {
                  wStr <- toupper(mstColors)
             
                  if ( wStr == "BW" || wStr == "GRAYS"  ||  wStr == "GREYS" ) {
              
                     #  set up the colors for printing in BW or Gray tones
                   
                     #  Get the main greys for the 6 colors (the middle 3-7/8 grays in the RColorBrewer scale.
                     #    and add the black for the median and a grey for the area highlight color.
                     xbw          <- RColorBrewer::brewer.pal(name="Greys",9)
                     greyColors   <- c(xbw[c(3:8)],"#000000","#E8E8E8")
                   
                     #  Build the transparent colors for the segmented bar charts.
                     TransColors  <- adjustcolor(greyColors,0.2)
                  
                     #  Set up the grey color vector as requested.
                     mstColors       <- c(greyColors,TransColors)
                   
                     #  Set up running parameters.
                     colFull          <- FALSE
                     Dot.Outline      <- TRUE
                     Dot.Conf.Outline <- TRUE
                     Dot.SE.Outline   <- TRUE
                     doDotOutline     <- TRUE  # outline dots in dot glyphs.
                  
                  } else {
                   
                     mstColors  <- micromapGDefaults$colors
                     warnCnt()
                     xmsg    <- paste0("***01K0 COLORS A invalid single value is provided for the colors argument. It must be 'BW', 'greys', or 'grays'. The argument is ignored.")
                     warning(xmsg,call.=FALSE)         
                  }
               } else {
               
                  warnCnt()
                  xmsg    <- paste0("***01K1 COLORS The colors vector has the incorrect number of elements. It must have 1 or 24 entries. ",length(mstColors)," provided.")
                  warning(xmsg,call.=FALSE)
               
               }
            }
         }
      } else {
 
         mstColors   <- micromapGDefaults$colors
         warnCnt()
         xmsg     <- "***01K2 COLORS The colors vector type is invalid.  It must be a character vector."
         warning(xmsg,call.=FALSE)

      }
   }

   assign("mstColors",as.character(mstColors))
   mstColorNames <- names(mstColors)

   rm(colors)
   #____ end of color check and adjustments.___
   #
  
   #
   #______________________Process details Defaults_________________________
   #

   #print("Validate details")
   
   #  Process defaults into the local variables as before.
   #  Setting the defaults into the system.  User provided overrides.
   
   wDetails <- micromapGDefaults$details     
   
   # dynamic assignment of defaults to individual variables in "micromapST"
   #  namespace.
   
   #print(wDetails)
   
   oldDefNam = "none"
   defNam = names(wDetails)
   
   for (i in 1:length(wDetails))
      {
        if (nchar(defNam[i]) <= 0) {
           warnCnt()
           xmsg     <- paste0("***01N3 DETS Zero length variable name found in the details list after the ", oldDefNam, " variable.")
           warning(xmsg,call.=FALSE)
        }
        oldDefNam    <- defNam[i]
        assign(defNam[i],wDetails[[i]])    # assign default values.
   
      }
   
   # All details names must be in the globalVariable call to be visible to CRAN checks.

   #  The valid details variable name list is the "defNam" from above and the detailsExtra list
   #    for the areaParms parameters.
   
   DetailNames    <- c(defNam,detailExtra)
   #print(DetailNames)
   
   #
   # The defaults have been moved to the individual variables.
   # Keep the list of names around to be to verify user supplied names.
   #

#
#________________ Process user provided details - merge into memory.
#

   # Now overlay with any values provided by user.

   #
   # dynamic assignment of detail data.frame to individual variables in the 
   #  "micromapST' namespace..
   #
   # Should I add code to verify names provided?
   #

   #print("Merge user details with default details.")
   
   #
   #  - validate user provided details before merging. details may contain multiple variables. 
   #

   numOverlaid <- 0
   
   if (!( missing(details) || is.null(details) )) {
     
      if (typeof(details) == "list") {
         
         nam       <- names(details)                 # parse the details list into variable that can be
         nam_match <- match(nam,defNam)
       
         for (i in 1:length(details)) {         #  referenced using the list's name.
             
             if (is.na(nam_match[i])) {
                
                # invalid variable name in details
                warnCnt()
                xmsg    <- paste0("***01N2 DETS Invalid details variable name: ",nam[i], " in the details list. Variable is IGNORED.")
                warning(xmsg,call.=FALSE)

             } else {
                # valid name
                numOverlaid <- numOverlaid + 1
                assign(nam[i],details[[i]])
                #print(paste0("details overlay of ",nam[i]," with ",details[i]))
             }
         }
      } else {
         stopCnt()
         xmsg    <- "***01N1 DETS The details parameter is not a list."
         stop(xmsg, call.=FALSE)
      }
   }
   
   #cat("envir=Id.Dot.pch:",find("Id.Dot.pch"),"\n")
   #cat("envir=topMar:",find("topMar"),"\n")
   
   #if (numOverlaid>0) {
   #  xmsg <- paste0("***0501 PANEL Number of parameters overlaid = ",numOverlaid)
   #  message(xmsg)
   #} 
   #
   #   Verify and adjust details variables
   #
   #cat("In micromapST - processing parameters.\n")
   #cat("envir=warnCnt:",find("warnCnt"),"\n")
   #cat("envir=staggered:",find("staggered"),"\n")
   #cat("envir=lastLab2Space:", find("lastLab2Space"), "\n")

   ####
   #
   #  Set in colors with BW or gray requested.   This resets it  - to Dot.Outline value?  OUCH!
   
   #doDotOutline <- Dot.Outline
   
   #
   ####

   
   ####
   #
   # Id.Dot.pch
   #
   #print("Validate Id.Dot.pch")
   
   if (!is.between.r(Id.Dot.pch,c(1,25))) {
       #  not an acceptable pch value 
       #cat("envir=Id.Dot.pch:", find("Id.Dot.pch"),"\n")
       
       Id.Dot.pch    <<-  22  # set to default

       warnCnt()
       xmsg    <- paste0("***01NA DETS The Id.Dot.pch variable can only be set to a range from 1 to 25.  Using the default of 22.")
       warning(xmsg,call.=FALSE)
   }  
   
   
   # 
   # This is the code the rcmd check could not detect the scope of the detail$ variables.
   #
   
   #
   #####

# Need to get ID width values before setting the panel defaults


#
#______________Function Call Argument Checks______________________
#


#------- Working variables for map and id glyphs.

#------- Start Getting widths of labels and titles to help setup column widths
#

    #
    #   This will have to be re-written to handle user provided labels and titles for the glyph columns.
    #

    medianBanner <-  Map.Median.text
    
    #cat("Calculating banners and column fixed widths.","\n")
    #print(medianBanner)
    #print(Map.Hdr1)
    #print(Map.Hdr2)
    #print(Id.Hdr1)
    #print(Id.Hdr1)
    
    #
    #   Map titles with symbols
    #
    
    sw = Map.Lab.Box.Width + 0.05 + 0.04 # square width and spaces on each side. (inches)
    #cat("Size of Box Symbols (guess) sw:",sw,"\n")
   
    # empty banner data.frame
    banner <- data.frame(H1=character(),H2=character(),H3=character(),M1=character(),stringsAsFactors=FALSE)
   
    #   add "Highlighed" titles for default.
    banner <- rbind(banner,t(c("","Highlighted",Map.Hdr2,medianBanner)))
   
    #   add headers for cumulative
    banner <- rbind(banner,t(c("Cumulative Maps",
                            paste0(Map.Hdr2,' Above Featured Rows'), 
                            paste0(Map.Hdr2,' Below Featured Rows'),
                            medianBanner) ) )
   
    #   add headers for median
    banner <- rbind(banner,t(c("Median Based Contours", 
                            paste0(Map.Hdr2,' Above the Median'),
                            paste0(Map.Hdr2,' Below the Median'),
                            medianBanner) ) )
   
    #   add headers for two ended (tail) 
    banner <- rbind(banner,t(c("",
                             "Two Ended Cumulative Maps",
                             paste0(Map.Hdr2," Highlighted"),
                             medianBanner) ) )
                             
    banner <- rbind(banner,t(c("",Id.Hdr1,Id.Hdr2,"") ) )
    
    
    bcn <- c("H1","H2","H3","M1")    # h1, h2, h3, median
    brn <- c("map","mapcum","mapmed","maptail","id")
    
    
    row.names(banner) <- brn
    colnames(banner)  <- bcn
    banner$H1 <- as.character(banner$H1)
    banner$H2 <- as.character(banner$H2)
    banner$H3 <- as.character(banner$H3)
    banner$M1 <- as.character(banner$M1)
    
    #cat("banner header data.frame:\n")
    #print(banner)
    
    #   .adj -> which lines in each header have symbols?
    banner.adj <- data.frame(H1=c(0,0,0,0,0),H2=c(0,sw,sw,0,0),H3=c(0,sw,sw,0,0),M1=c(0,0,0,0,0))
    row.names(banner.adj) <- brn
    
    banner.m <- c(1,1,1,0.8)    # text size multiplier for  H1, H2, H3, Med1
    banner.tc <- Text.cex * banner.m
    
    #cat("CEX for headers and median - banner.tc:",banner.tc,"\n")
    
    banner.w <- banner
    
    #  replace strings with width values for current font and Text.cex values.
    
    for (iH in c(1:4)) {
       for (iT in c(1:5))  {
          banner.w[iT,iH] <- strwidth(banner[iT,iH],units="inches",cex=banner.tc[iH])
       }
    }
    
    #
    
    banner.w <- as.data.frame(sapply(banner.w, function(x) as.numeric(x)))  # convert numeric.
    row.names(banner.w) <- brn
 
    #cat("widths in banners - banner.w:\n")
    #print(banner.w)
    
    banner.max <- as.data.frame(sapply(c(1:5), function(x)  max(banner.w[x,]+banner.adj[x,])))
    colnames(banner.max) <- "width"
    row.names(banner.max) <- brn

    #cat("maximum widths for each type of header - banner.max:\n")
    #print(banner.max)
   
    #  Make subroutine to be able to do again later.

    ID.Abbr.width       <- max(strwidth(ID.Abbr,units="inches",cex=(Id.Text.cex * Id.Cex.mod)))
    ID.Name.width       <- max(strwidth(ID.Name,units="inches",cex=(Id.Text.cex * Id.Cex.mod)))

    #cat("ID.Abbr.width:",ID.Abbr.width,"\n ")
    #cat("ID.Name.width:",ID.Name.width,"\n\n")

    Id.OverH <- Id.Dot.width*(Id.Dot.cexm * Id.Cex.mod) + Id.Space*2.5  # two spaces left and right of name.
   
    #cat("ID overhead (Id.Start, Dot.width, Space (box to letters), space (letter to edge):",Id.OverH,"\n")
    #cat("banner.max ID:",banner.max["id","width"],"   IDName:",Id.OverH+ID.Name.width,"  IDAbbr:",Id.OverH+ID.Abbr.width,"\n")
   
    #  width of ID glyph with border Group names/abbreviations

    Id.width    <- c(1.5,1)         # initialize
    Id.width[1] <- max((Id.OverH + ID.Name.width ),banner.max["id","width"])   # plus padding. FULLNAMES
    Id.width[2] <- max((Id.OverH + ID.Abbr.width ),banner.max["id","width"])   #    ABBREVIATIONS

    #cat("Id.width:",Id.width,"\n")
    #
    #  Build title lists for maps and get width for point size.
    #
   
    #cat("Map.Aspect:",Map.Aspect,"\n\n")

    #
    #print("Column Hdrs - Done")

#_____________Set up for Area Names and Abbreviation links.  
#
#_____________Borders to data Link ----  rowNames and rowNamesCol
#
#_____________Process rowNames option___________________
#
#

rowNames <- rowNames[[1]][1]

if ( missing(rowNames) || is.null(rowNames) || is.na(rowNames) )  {
   # no rowNames provided up front.  Set to default
   rowNames             <- "ab"
}

#cat("Validate rowNames : ", rowNames,"\n")

#__________________

   #
   #  Verify the rownames are valid and can be translated into abbrevation versions.
   #
   #
   # The user can enter abbr, full, alt_ab, alias, or ID with the data.
   # Which everone is picked, it must be the string in the data.frame and the panelData-data.frames to 
   # allow matching to the boundaries VisBorderr data.
   #
   # Each value is translated to the Key that is used to link the data to the 
   # key in the boundary data in areaVisBorders.
   #
   
   #  AD.link is the user value in the order of the data table.
   #
   #  areaIndex is in the order of the data table (AD.link) and points to the 
   #  matching entry in the name table, based on the proper match for the type of value.
   #

   #print("Clean up rownames in $RN")
   #cat("Border Group Name:",BordGrpName,"\n")
   
   statsDFrame$RN <- ClnStr(as.character(statsDFrame$RN))
   
   AD.link <- (as.character(statsDFrame$RN))   # get rownames.information (link to borders)  (all CAPS)
   
   ##### may be changed.
   
   #cat("Initial AD.link:",AD.link,"\n")
   
   if (BordGrpName == "USStatesBG") {
   
      ### If US States Patterns - look for the many ways Washington DC is possibly enter in the user data..                   ###
   
      #  Compare against common "DC" names and replace with "DC"
      if (rowNames == "full") {
         AD.Test <- toupper(AD.link)   # get capitalized version for the DC conversion.
         #  Build DC name table (all caps)
         DCnames = c("WASHINGTON, D. C.", "WASHINGTON D. C.", 
                     "WASHINGTON, D C",   "WASHINGTON D C",
                     "WASHINGTON, DC",    "WASHINGTON DC",       
                     "DISTRICT COLUMBIA", "DISTRICT OF COLUMBIA",
                     "DC", "D C", "D, C.","D.C","D C.","D.C.")
         # only clean up full names.         
         AD.link[!is.na(match(AD.Test,DCnames))] <- "DC"  ###  match short form in border group
      }
      
      #cat("Updated AD.link:",AD.link,"\n")
   }
   
   #
   if (rowNames == "alias" && enableAlias == FALSE) {
       stopCnt()
       StopFnd <- TRUE
       xmsg    <- paste0("***0191 CARG-RN rowNames='alias' is not supported for this bordGrp.")
       stop(xmsg, call.=FALSE)
   }
   if (rowNames == "seer" && BordGrpName != "USSeerBG") {
       stopCnt()
       StopFnd <- TRUE
       xmsg    <- paste0("***0192 CARG-RN rowNames='seer' is only supported for the 'USSeerBG' bordGrp.")
       stop(xmsg, call.=FALSE)
   }
   
   #  areaIndex pointer to Name Table is order of the user data.frame based on the rowNames parameter.
   
   IndexDFtoNT = switch(rowNames,  # find the correct list to match user provide links.
  
      # if "ab", use current name - get index
      "ab"    = {match(AD.link, areaNTAbbr)},
      
      # if "id", convert to index      
      "id"    = {match(as.integer(AD.link), as.integer(rlAreaNamesAbbrsIDs$ID))},
      
      # if "FIPS", convert to index (alias for "id")
      "FIPS"  = (match(as.integer(AD.link), as.integer(rlAreaNamesAbbrsIDs$ID))),
      
      # if "full" sub-area name, convert index
      "full"  = {match(AD.link, areaNTName)},
      
      # if "seer"  seer sub-area names from SeerStat (read and convert to index )
      "seer"  = {AliasToIndex(AD.link,rlAreaNamesAbbrsIDs$Alias)},
      
      # if "alias"  seer sub-area names from SeerStat (read and convert to index.)
      "alias" = {AliasToIndex(AD.link,rlAreaNamesAbbrsIDs$Alias)},
      
      # if "alt_ab" alternate name abbreviation used in data, convert to index.
      "alt_ab" = {match(AD.link, rlAreaNamesAbbrsIDs$Alt_Abbr)},
      
      #  No match..
      {
         stopCnt()
         StopFnd <- TRUE
         xmsg    <- paste0("***0190 CARG-RN Invalid rowNames call parameter value. The value must be 'ab', 'alt_ab', 'id', 'alias', or 'full'.")
         stop(xmsg, call.=FALSE)
      }
   )
   #
   #  IndexDFtoNT is index from caller's data.frame rows into the name table 
   #      (areaNamesAbbrsIDs) data.frame
   #
   #  By default, we will handle cases where statsDFrames does not contain 
   #       sub-areas in the border group.
   #

   callVL$rowNames      <- rowNames
   var  <- "callVarList"  
   wstr <- paste0("assign(var,callVL,envir=.GlobalEnv)")
   eval(parse(text=wstr))

   #cat("Initial IndexDFtoNT:",IndexDFtoNT,"\n")
   
   ######
   #
   #  Process ignoreNoMatches - the case where data is provided, but the is no row in the name table (and 
   #    therefore, no boundaries in the border group.
   #
   #  This also deals with data rows that don't match boundary information.
   #    a) all match - all data and all boundaries
   #    b) all data match boundaries (all data is matched, but not all boundaries are used.)
   #             b1) dataRegionsOnly option enabled - find regions and only do regions with data.
   #             b2) no regions - draw all.
   #    c) not all data matches boundaries (data without boundary,)
   #             c1) ignoreNoMatches = false  -> warning message and stop.
   #             c2) ignoreNoMatches = true   -> warning message, delete data rows, continue.
   #    d) no data matches in boundaries. (total miss match).  Warning and stop.
   #
   #
   #  Check and Implement delete last row (blank) as no match ignore option 
   #
   
   # set defaults for ignoreNoMatches call parameter
   
   if (is.null(ignoreNoMatches))  ignoreNoMatches = FALSE
   if (is.na(ignoreNoMatches))    ignoreNoMatches = FALSE
   #
   
   #cat("ignoreNoMatches : ",ignoreNoMatches,"\n")
   #cat("number of rows  : ",numRows,"\n")
   
   DFtoNTMissed      <- is.na(IndexDFtoNT)
   #cat("DFtoNTMissed:",DFtoNTMissed,"\n")
   #cat("any(DFtoNTMissed):",any(DFtoNTMissed),"\n")
   
   #
   #  areaUKey is list of abbreviation for each area.  If there is no match
   #  between the data links and areaNamesAbbrsIDs table, then it shows up as an NA.
   #
   if (any(DFtoNTMissed)) {
      # one or more of the data rows didn't match the name table
      
      #
      #  if ignoreNoMatches in data, strip rows that don't match name table.
      #
      BadList <- statsDFrame[DFtoNTMissed,"rawRN"]  # get list of rows that did not match.
      xmsg    <- paste0("***0106 CARG-DF The following rows in the ",sDFName," data.frame do not match any boundary name:")
      warning(xmsg,call.=FALSE)
      xmsg    <- paste0("***0107 ",paste0(BadList,collapse=", "))
      warning(xmsg,call.=FALSE)
      #
      if (ignoreNoMatches) {
         # ignore data rows that do not match match the name table.
         # remove row from data.frame
         xmsg  <- paste0("***0108 CARG-DF The rows not matched to boundaries will be removed and not mapped.")
         warning(xmsg,call.=FALSE)
         
         KeepList    <- !DFtoNTMissed              # get list of areas that don't match (T/F)- good entires = TRUE
         #cat("Good data rows:",paste0(KeepList,collapse=" "))
     
         # delete bad rows  (those not matching)        # Keep only rows that matched the name table.
         IndexDFtoNT <- IndexDFtoNT[KeepList]           # clean up index
         statsDFrame <- statsDFrame[KeepList,]          # clean up data frame
         AD.link     <- AD.link[KeepList]               # clean up AD.link 
      
         # if ignoreNoMatches set - this has removed the rows from the user's data table.
      } else {
         # stop if a missing match
         # at least one NA in list
         xmsg <- paste0("***0109 CARG-DF Data row names in the ",sDFName," data.frame must the boundary names in the name table. Call stopped.")
         stop(xmsg,call.=FALSE)
      }   
   }
   
   #cat("Adjusted data.frames - statsDFrame, AD.link, IndexDFtoNT:\n")
   #print(statsDFrame)
   #print(AD.link)
   #print(IndexDFtoNT)
   
   numRows <- length(IndexDFtoNT)      # update number of rows in data frame
   
   #cat("numRows:",numRows,"\n")
   
   #
   ####
   
   ####
   #
   #  grpPattern argument - default = NULL  (use calculated pattern)
   #
   
   #print("Validate - grpPattern")
   
   if (!( missing(grpPattern) || is.null(grpPattern) )) {
   
      # we have a user specifed grpPattern
      if (!is.numeric(grpPattern)) {   
         warnCnt()
         ErrFnd  <- TRUE
         xmsg    <- paste0("***01C0 CARG-GP The grpPattern call parameter must be an integer vector.  grpPattern ignored.")
         warning(xmsg, call.=FALSE)
         grpPattern <- NULL
      } else {
         xg <- sum(grpPattern)
         if (xg != numRows) {
            # grpPattern number of rows does not match the statsDFrame data.frame
            warnCnt()
            ErrFnd  <- TRUE
            xmsg    <- paste0("***01C1 CARG-GP The total number of rows in the grpPattern call parameter must be equal to the number of rows in the ", sDFName," data.frame.  grpPattern ignored.")
            warning(xmsg, call.=FALSE)
            grpPattern <- NULL
            
         } else {
            # check for correct group formats.
            #   No element greater than 5
            xg      <- max(grpPattern)
            if (xg > 5) {
               # grpPattern number of rows does not match the statsDFrame data.frame
               warnCnt()
               ErrFnd  <- TRUE
               xmsg    <- paste0("***01C2 CARG-GP Each value in grpPattern call parameter vector must be <= 5 (rows per group). A value of ",xg," was found.")
               warning(xmsg, call.=FALSE)
               grpPattern <- NULL
            
            } else {
               #   Rows descend in order to middle.
               xl    <- length(grpPattern)           # number of groups in grpPattern
               xlh   <- ceiling(xl/2)                # number of groups to median point.
               grpL  <- grpPattern[1:xlh]            # lower half groups
               grpU  <- grpPattern[(xl-xlh+1):xl]    # upper half groups
               if ( !all(grpL == sort(grpL,decreasing=TRUE)) || !all(grpU == sort(grpU)) ) {   # correction.
                  # if the sorted order of either half of the groups does not match the 
                  # pattern provided, warning and ignore the grpPattern.
                  warnCnt()
                  ErrFnd  <- TRUE
                  xmsg    <- paste0("***01C3 CARG-GP The grpPattern call parameter is not properly ordered. ", "The number of rows per group must be in desending order toward the median sub-area.")
                  warning(xmsg, call.=FALSE)
                  grpPattern <- NULL
               }
            }
         }
      }
   }
   
   #
   #####
  
   #####
   #
   #  regionsB argument  - default = FALSE.
   #
      
   #print("regionsB parameter Check.")
   def_regionsB    <- FALSE
   regionsBFlag    <- def_regionsB
   regionsB        <- regionsB[[1]][1]    
   
   if (! (is.null(RegVisBorders) || identical(RegVisBorders,L3VisBorders)) ) {
   
      # RegVisBorders boundary data.frame is present and different from L3.
   
      # validate parameter
      if ( is.null(regionsB) || is.na(regionsB) ) {
      
          #  argument is missing or not provided
          regionsB     <- def_regionsB
          regionsBFlag <- def_regionsB    # default
          #cat("regionsB support enabled - but no regionsB call parameter provided - regionsB set to FALSE.\n")
          
      } else {

          if ( !is.logical(regionsB) ) {
       
             ErrFnd      <- TRUE
             warnCnt()
             xmsg        <- "***01G0 CARG-RB The regionsB call argument is not a logical variable.  The default of FALSE will be used."
             warning(xmsg,call. = FALSE)
          
             regionsBFlag <- def_regionsB
             regionsB     <- def_regionsB
          } else {
             regionsBFlag <- regionsB
          }
      }
   }
      
   #cat("regionsBFlag parameter:",regionsBFlag,"  regionsB:",regionsB,"\n")
   #
   #####

   #####
   #
   #  dataRegionsOnly argument  - default = FALSE.
   #
   
   #print("dataRegionsOnly parameter Check.")
   def_dataRegionsOnly  <- FALSE
   dataRegionsOnlyFlag  <- def_dataRegionsOnly
   
   if ( aP_Regions ) {
   
      # border group supports regions (feature enabled)
      # validate parameter
      if ( is.null(dataRegionsOnly) || any(is.na(dataRegionsOnly)) ) {
      
          #  argument is missing or not provided
          dataRegionsOnly     <- def_dataRegionsOnly
          dataRegionsOnlyFlag <- def_dataRegionsOnly    # default
          #cat("regions support enabled - but no regions call parameter provided - regions set to TRUE.\n")
          
      } else {
          dataRegionsOnly <- dataRegionsOnly[[1]][1]
          if ( !is.logical(dataRegionsOnly) ) {
       
             ErrFnd      <- TRUE
             warnCnt()
             xmsg        <- "***01G5 CARG-DRO The dataRegionsOnly call argument is not a logical variable.  The default of FALSE will be used."
             warning(xmsg,call. = FALSE)
          
             dataRegionsOnlyFlag <- def_dataRegionsOnly
             dataRegionsOnly     <- def_dataRegionsOnly
          } else {
             dataRegionsOnlyFlag <- dataRegionsOnly
          }
      }
   }
   
   #cat("dataRegionsOnlyFlag parameter:",dataRegionsOnlyFlag,"  dataRegionsOnly:",dataRegionsOnly,"\n")
   
   #
   #  If duplicated rows exist, Notify user and stop.
   #
   #  Is this now a duplicate test to the previous test???    Yes it is.   (retire)
   #
   
   #print("check for duplicate statsDF rows - duplicate?")
   
   dupL <- duplicated(IndexDFtoNT)   # check for duplicate references to Name Table
   
   if (any(dupL)) {   
      # some of the matches are duplicates - not allowed.  One row per sub-area.
      DupList <- paste0(AD.link[dupL],collapse=", ")
      stopCnt()
      xmsg    <- paste0("***0104 CARG-DF There are duplicate entries in the statsDFrame data.frame.  Duplicate entries are ignored.\n",
                        "***0105 CARG-DF The duplicate rows are: ",DupList,"\n")
      stop(xmsg, call.=FALSE)
      rm(DupList)
   }
   rm(dupL)
   
   
   # one of the names provided abrv, alt_abrv, ID or full names are not valid 
   #  and did not match the data in the Name Table.  Can't link to any boundary data.

   # What link to use for boxplot and TS type data?
   
   #print("Get panelData Key.")
   
   panelDataKey <- switch(rowNames,
   
                         "ab"    =  areaNamesAbbrsIDs$Abbr[IndexDFtoNT],  
                         "full"  =  areaNamesAbbrsIDs$Name[IndexDFtoNT],
                         "id"    =  areaNamesAbbrsIDs$ID[IndexDFtoNT],
                         "alias" =  areaNamesAbbrsIDs$Abbr[IndexDFtoNT],
                         "seer"  =  areaNamesAbbrsIDs$Abbr[IndexDFtoNT],
                         "alt_ab"=  areaNamesAbbrsIDs$Alt_Abbr[IndexDFtoNT]
                         )

   #cat("panelDataKey:",panelDataKey,"\n")
   
   #  IndexDFtoNT is an index list to name/abbr/ID rows that match the 
   #       ADFrame rows -> position = ADFrame Row, value = abbr 

   #  Still need to check for duplicates.
   #
   #  Setup for IndexDFtoNT checks
   
   #  areaDatKey is the abbreviation in order of data.frame
   
   #
   #  statsDFrame$RN (AD.link) is the cleaned up strings initially used for link
   #  Should be able to re-use this field to link to any panelData structure.
   #

   #
   #  sub-areas to regions to Area processing 
   #
   #  Get list of sub areas in regions referenced by the data.j
   #  Set up used regions as the only spaces to map.
   #  Get list of all sub areas in the regions referenced.
   #
   
   areaNamesAbbrsIDs$NotUsed <- FALSE
   
   #  What does NT hold?
   
   #  List of all regions and L2 keys
   
   #print("Build regions lists from NT regID")   

   listAllL2         <- unique(areaNamesAbbrsIDs$L2_ID)
   #cat("listAllL2:",listAllL2,"\n")

   listAllRegions    <- unique(areaNamesAbbrsIDs$regID)
   #cat("listAllRegions:",listAllRegions,"\n")
   
   listAllAreas      <- areaNamesAbbrsIDs$Key
   #cat("listAllAreas:",listAllAreas,"\n")
   
   #cat("dataRegionsOnlyFlag:",dataRegionsOnlyFlag,"\n")
 
   if (dataRegionsOnlyFlag) {
      #  save L2_ID for each data row.
      statsDFrame$L2_ID <- areaNamesAbbrsIDs$L2_ID[IndexDFtoNT]     # put L2_ID into statsDFrame
   
      #  Get list of used L2 areas.
      listUsedL2        <- unique(statsDFrame$L2_ID)
      #cat("listUsedL2:",listUsedL2,"\n")
   
      #  What does the data show?
   
      #  data - regions and L2 key lists
   
      #  Pick up regID for each data row.
      statsDFrame$regID <- areaNamesAbbrsIDs$regID[IndexDFtoNT]     # put regID into statsDFrame
      #  Get list of used Regions
      listUsedRegions   <- unique(statsDFrame$regID)                # get list of regions used.  
      #cat("listUsedRegions:",listUsedRegions,"\n")

      areaRegMatch      <- match(areaNamesAbbrsIDs$regID,listUsedRegions)  # find all sub-areas in regions to be mapped
      areaRegKeep       <- !is.na(areaRegMatch)
      listUsedAreas     <- areaNamesAbbrsIDs[areaRegKeep,"Key"]
  
   } else {
   
      listUsedRegions <- listAllRegions
      listUsedL2      <- listAllL2
      listUsedAreas   <- listAllAreas
      #cat("regionsFlag=FALSE -> reset listUsed to listAll\n")
   }
   
   # Have all sub-areas in areaNamesAbbrsIDs (name table), areaVisBorders, L2VisBorders, and RegVisBorders.
   #    if used is less the total and regions set, reduce these tables to only the valid region.
   
   #cat("UsedRegions:",listUsedRegions,"\n")
   #cat("UsedL2     :",listUsedL2,"\n")
   #cat("UsedAreas  :",listUsedAreas,"\n")
 
   #cat("Overlays - L2:",Map.L2Borders,"  Reg:",Map.RegBorders,"  L3:",Map.L3Borders,"\n")

   if (length(listUsedRegions) != length(listAllRegions)) {
      # number of used regions is less that number of all regions in border group.
      #  This only happens if regions=TRUE and aP_Regions is TRUE.
      
      #  The rlXXXX tables must be sub-divided
      
      # get index to sub-areas in used regions (all aub-areas, not just was matched data.)
      #    mark not used name table entries
      SubAallR_Match      <- match(areaNamesAbbrsIDs$regID,listUsedRegions)   # find all subarea enters within regions used
      SubAallR_Good       <- !is.na(SubAallR_Match)   # list of valid name table entries.
      #cat("SubAallR_Good:",SubAallR_Good,"\n")
      
      # get keys to aub-areas in all subset of regions (no match against used regions list)
      areaNamesAbbrsIDs$NotUsed[!SubAallR_Good] <- TRUE             # set not used flag in all sub-area not in regions referenced
    
      # get list of sub-area keys in region
      listAllAreas        <- listNTKeysUsed <- areaNamesAbbrsIDs$Key[SubAallR_Good]   # get which sub-area keys are in region
      #cat("listAllAreas(in Reg.):",listAllAreas,"\n")
      
      # get keys to sub-areas that match data rows
      listUsedAreas       <- areaNamesAbbrsIDs$Key[IndexDFtoNT]     # get list of sub-areas in data (used)
      #cat("listUsedAreas:",listUsedAreas,"\n")
      
      #  sub-divide areaVisBorders
      
      rlAreaVM             <- match(rlAreaVisBorders$Key,listAllAreas)
      rlAreaKeep           <- !is.na(rlAreaVM)  # good rows to keep
      #cat("rlAreaKeep:",rlAreaKeep,"\n")
       
      rlAreaVisBorders     <- rlAreaVisBorders[rlAreaKeep,]
      #cat("rlAreaVisBorders:\n")
      #print(head(rlAreaVisBorders,50))
     
      #  sub-divide RegVisBorders
      
      rlRegVM             <- match(rlRegVisBorders$Key,listUsedRegions)
      rlRegKeep           <- !is.na(rlRegVM)  # good rows to keep
      #cat("rlRegKeep:",rlRegKeep,"\n")
      
      rlRegVisBorders     <- rlRegVisBorders[rlRegKeep,]
      #cat("rlRegVisBorders:\n")
      #print(head(rlRegVisBorders,50))
      
      if (!identical(RegVisBorders,L3VisBorders)) {
         # RegVisBorders is not equal to L3, so it has real boundaries in it.
         Map.RegBorders <- TRUE   # make sure Reg overlays are enabled.  We need them.
         regionsBFlag   <- TRUE   # print boundaries.
      }
      
      #  sub-divide L2VisBorders
      
      rlL2VM              <- match(rlL2VisBorders$Key,listUsedL2)
      rlL2Keep            <- !is.na(rlL2VM)
      #cat("rlL2Keep:",rlL2Keep,"\n")
      
      rlL2VisBorders      <- rlL2VisBorders[rlL2Keep,]
      #cat("rlL2VisBorders:\n")
      #print(head(rlL2VisBorders,50))
 
      #  Handle L3VisBorders 
      
      #  Turn off overlaying L3
      
      Map.L3Borders <- FALSE
      
      #  Report status
      
      #cat("Adjusted listUsed - area, Regions and listUsedL2:\n")
      #cat("UsedRegions:",listUsedRegions,"\n")
      #cat("UsedL2     :",listUsedL2,"\n")
      #cat("UsedAreas  :",listUsedAreas,"\n")
      
      #cat("AllAreas  :",listAllAreas,"\n") 
      #cat("Num data SAs:", length(listUsedAreas),"  Num NT SAs:",length(listAllAreas),"\n")
      #print("-end-")
      
   }  # End of regional VisBorder processing sub-dividing.
   
   #cat("Overlays - L2:",Map.L2Borders,"  Reg:",Map.RegBorders,"  L3:",Map.L3Borders,"\n")
   
   #print("Completed regions subsetting of boundary data.")

   #  Can't do much more until after the sortVar is handled.
   #
   
#   
#_______________plotNames option__________________
#
   # Get area names or abbreviations to plot_______________________

   #print("Validate plotNames.")
   def_plotNames <- "ab"
   plotNames     <- plotNames[[1]][1]

   #  Set the defaults if not present or NA
   
   if ( missing(plotNames) || is.null(plotNames) || is.na(plotNames) ) {
       plotNames = def_plotNames
   }

   # areaIDNames are used in the ID glyph as the literal for the area.
   #  Get list of names to use in glyph and the width required.

   # Default - abbreviations
   areaUAbbr   <- areaNamesAbbrsIDs$Abbr[IndexDFtoNT]
   areaUFull   <- areaNamesAbbrsIDs$Name[IndexDFtoNT]
   
   areaIDNames <- areaUAbbr
   IdW         <- Id.width[2]  # temp initialization  (abbreviation)
   
   #  Get width of ID gryphics column
   areaIDNames = switch(plotNames,
   
          "ab"  = {IdW <- Id.width[2]; areaUAbbr},    # set IdW to value and return vector of names.

          "full"= {IdW <- Id.width[1]; areaUFull},

          {  # no match

             plotNames = def_plotNames

             warnCnt()
             xmsg <- "***01B0 CARG-PN Invalid plotNames argument value.  The value must be 'ab' or 'full'. The default of 'ab' will be used."
             warning(xmsg,call.=FALSE)

          }
        )
   
   # areaIDNames are in statsDFrame order containing the ab or full name associated with the row in statsDFrame
   
   statsDFrame$IDNames <- as.character(areaIDNames)   # set IDNames for sorting and ID into statsDFrame
  
   IdColWidth <- max(banner.max["id","width"],IdW)
   #cat("ID column width to use - IdColWidth:",IdColWidth,"\n")
   
   #  now complete the default sort.
   
   #  statsDFrame$IDNames is in the order of the user data.  Not the Names/Abbr Table.
   
   #  areaIDNames is a list of name/abbr literals based on the plotNames specified and 
   #   the areaUIndex values.  The name or abbreviation values are pulled from the Name Table
   #   incase an alias or alt_abbreivation was used to link the data to boundaries.
   #
   #cat("areaIDNames:",areaIDNames,"\n")

#_______________title option (mstTitle)______________________
#

#print("title validation.")

mstTitle <- title      # can have multiple elements (1 or 2)

#  checks missing,, is character, length = 1 or 2.
if ( missing(title) || is.null(title) ) {
   # set to the default
   mstTitle <- c("")
}

if (length(mstTitle) < 1) {
   mstTitle <- c("")
   warnCnt()
   xmsg    <- "***01A2 CARG-TL The title parameter is empty. Recommend providing a title for the linked micromap."
   warning(xmsg,call.=FALSE)
}
if (length(mstTitle) == 1) { 
   if (is.na(mstTitle)) {
      # set to the default
      mstTitle <- c("")
  }
}
if ( ( typeof(mstTitle) != "character" ) || ( class(mstTitle) != "character") ) {
   mstTitle   <- as.character(unlist(mstTitle))
   warnCnt()
   xmsg    <- paste0("***01A1 CARG-TL The typeof/class of the title parameter is not character. ","Only character vectors are supported. The 'title' argument is ignored.")
   warning(xmsg,call.=FALSE)
}
if (length(mstTitle) > 2) {
   mstTitle   <- mstTitle[1:2]
   warnCnt()
   xmsg    <- paste0("***01A0 CARG-TL The title argument contains more than 2 items. ", "Only the first two will be used.")
   warning(xmsg,call.=FALSE)
}


#print("statsDFrame before sort")
#print(str(statsDFrame))

#_______________ascend option_____________________
#
#   default value is ascending.  (TRUE)

#print("Validate ascend")

ordDecr <- FALSE
ascend <- ascend[[1]][1]   # get first element.

   if (!( missing(ascend) || is.null(ascend) || is.na(ascend))) {
       if (is.logical(ascend)) {
          ordDecr <- !(unlist(ascend)[[1]])
       } else {
          warnCnt()
          xmsg    <- "***0186 CARG-AS The ascend parameter is not a logical variable.  Must be TRUE or FALSE."
          warning(xmsg,call.=FALSE)
       }
   }    

#_______________sortVar option____________________
#

#print("Validate sortVar")

# sort and store statsDFrame, areaID, and areaNames____________

   # rules for sortVar data columns.
   #    a) list of columns collected from sortVar parameter
   #    b) The numbers in the column are processed to trim blanks and eliminate "," from numbers
   #    c) The numbers in the column are converted to numeric.
   #    d) If the column does not have numbers, it is left as character and only blanks are trimed.
   # 

   # Set Default sort orders results
   
   ord       <- order(statsDFrame$IDNames, na.last=TRUE, decreasing=ordDecr)      # default is to sort in the sub-area names/abbr
   rankOrd   <- rank(sort(statsDFrame$IDNames),ties.method="min",na.last=TRUE)

   # ord and rankOrd are re-ordered (sorted) but point to the User data.frame.
   #    sorted order -> data.frame  (or areaUIndex) 
   #
   # data data.frame must be edited by now or sort will not work.
   #
   # names table must stay the same from now on.
   #
   
   # process sortVar
   sortVar <- sortVar[[1]][1]   # get first element.
   
   if ( missing(sortVar) || is.null(sortVar) || is.na(sortVar) ) {
     
      # if field omitted (null) sort use default values
      sortVar <- NULL
   
   } else  {
   
      # value/name provides - verify it.
      litsortVar <- sortVar
      sortVar    <- CheckParmColx(litsortVar,c('SORT','sortVar'),wSFNameList,len_wSFName)
           # column names and numbers are verified and converted to column numbers.
           # column 0 represents a no match, can't find.
 
      #print("sortVar returned by CheckParmColx")
      #print(sortVar)
  
      wSortVar <- sortVar[sortVar > 0]   # keep good column indexes
     
      if (length(wSortVar) > 0) {
         
         wSv      <- lapply(wSortVar, function(x) stringr::str_trim(statsDFrame[,x]))   # pull one or more rows and trim blanks
         wSv2     <- lapply(wSv, function(y) as.numeric(gsub(",","",y)))      # kill "," and convert to numeric
       
         wSv9Test <- lapply(wSv2, function(z) all(is.na(z)))                  # did data get converted to numeric?
         
         # check on the conversion to numeric - if fails, keep as character.
         wSv3 <- lapply(c(1:length(wSv9Test)), function(a) if(wSv9Test[[a]]) 
                         {
                            # TRUE - All entries are NA - most likely a character column
                            wSv[[a]]    # return original text version trimmed
                         } else {
                            wSv2[[a]]   # return numeric version 
                         }
                       )
      
         wSv3$na.last    <- TRUE               # set na.last = TRUE option
         wSv3$decreasing <- ordDecr            # set sort order
         
         ord             <- do.call(order,wSv3)
         rankOrd         <- rank(statsDFrame[,sortVar[1]],ties.method="min",na.last=TRUE)

      } else {
         # can't use sortVar - set to NULL      
         sortVar <- NULL   # no good values - NULL argument as if it was not present.
      }  
   } 
   #cat("sortVar - ord:",ord,"\n")
   #print(rankOrd)
  
#
#--------------Set up working vectors based on the sort
#
#  ord has the sorted order by ADFrame row numbers for indexing.
#
#  sortedOrd is the order of the statsDFrame data.frame 
#

   sortedOrd               <- ord                         # sorted display names (abbr or full)

   #print("sort completed.")
   #cat("sortedOrd:",sortedOrd,"\n")
 
#
#_______________SORT the data array as requested____________
#

   ###  are assigns needed in our mode?    Data area for all calls below...

   assign("dat",statsDFrame[sortedOrd,])                    # data fields    "dat" has sorted data frame of the statsDFrame
   #cat("dim(dat):",dim(dat),"\n")
   
   # 
   #  From now on, the "dat" structure is the primary data.frame containing the user's data.
   #
   IndexDattoNT           <- IndexDFtoNT[sortedOrd]    # index list from "dat" to Name table
   #cat("IndexDFtoNT:",IndexDFtoNT,"\n")
   #cat("IndexDattoNT:",IndexDattoNT,"\n")
   
   areaDatIDNames         <- areaIDNames[sortedOrd]
     
   #  IndexDattoNT is in data.frame order pointing to the name table
   
   areaDatKey             <- areaNTKey[IndexDattoNT]   # keys in order of the user data.
   areaDatAbbr            <- areaNTAbbr[IndexDattoNT]
   areaDatFull            <- areaNamesAbbrsIDs$Name[IndexDattoNT]
   areaDatID              <- areaNamesAbbrsIDs$ID[IndexDattoNT]
   areaDatAlt_Abbr        <- areaNamesAbbrsIDs$Alt_Abbr[IndexDattoNT]
   
   #cat("dim(dat):",dim(dat),"\n")
   #cat("length of areaID (areaDatKey): ",length(areaDatKey),"\n")
   #cat("areaID (areaDatKey)          : ",paste0(areaDatKey,collapse=" "),"\n")
   
   naADK       <- is.na(areaDatKey)
   #cat("areaDatKey-NA:",naADK,"\n")
   #cat("length(naADK):",length(naADK)," any(naADK):",any(naADK),"  all:",all(naADK)," sum:",sum(naADK),"\n")
   if (any(naADK)) {
     cat("bad areaDatKey:\n")
     print(dat[naADK,])
     print("SHOULD not get here.")
   }
   #print(dat)
   
   #cat("areaDatKey:",areaDatKey,"\n")
   
   #cat("row.names(dat):",row.names(dat),"\n")
   
   row.names(dat)         <- areaDatKey            # reset the row.names to the Key
 
   xDFrame                <- data.frame(Key=areaDatKey, Abbr=areaDatAbbr, 
                                 Full=areaDatFull, ID=areaDatID,
                                 IDNames=areaIDNames, Rank=rankOrd,
                                 Index=IndexDattoNT)
   
   #cat("xDFrame:\n")
   #print(xDFrame)
   
   # build index from name table to statsDFrame
   
   IndexNTtoDat           <- rep(NA,length(areaNamesAbbrsIDs$Key))
   
   for (ind in c(1:length(IndexDattoNT))) {
      IndexNTtoDat[IndexDattoNT[ind]] <- ind
   }
   
   #cat("IndexNTtoDat:",paste0(IndexNTtoDat,collapse=", "),"\n")
   
   #  IndexNTtoDat is in the name table order pointing to the data.frame.
   
   NotUsedList    <- is.na(IndexNTtoDat)
   NotUsedKeys    <- areaNTKey[NotUsedList]            # get list of unreferred sub-areas.
   NotUsedNames   <- areaNTName[NotUsedList]           # get list of names not referenced.
   
   #cat("NotUsedKeys>",paste0(NotUsedKeys,collapse=", "),"<\n")
   
   #if (any(NotUsedList)) {    # better message? 
   #   warnCnt()
   #   xmsg    <- paste0("***0102 CARG-DF The following sub-area(s) in the name table were not referenced in the user data.") 
   #   warning(xmsg,call.=FALSE)
   #   xmsg    <- paste0("***0102 CARG-DF  >",paste0(NotUsedNames, collapse=", "),"<")
   #   warning(xmsg,call.=FALSE)
   #}

   #cat("NotUsedKeys:",paste0(NotUsedKeys,collapse=", "),"\n")
   #cat("NotUsedList:\n")
   #print(NotUsedList)
   #cat("\n")
   
   assign("areaDatAbbr"     ,areaDatAbbr)        # area Abbr         "area Abbr" in order of the dat
   assign("areaDatID"       ,areaDatID)          # area ID           "area ID"   in order of the dat
   assign("areaDatFull"     ,areaDatFull)        # area Full         "area Full" in order of the dat
   assign("areaDatKey"      ,areaDatKey)         # area Key          "area Key"  in order of the dat
   assign("areaDatAlt_Abbr" ,areaDatAlt_Abbr)    # area Alt_Abbr     "area Alt_Abbr"  in order of the dat
   assign("areaIDNames"     ,areaIDNames[sortedOrd])  # area Display Names  "areaNames in order of the dat.
   assign("NotUsedKeys"     ,NotUsedKeys)        # area keys that were not referenced in the data.
   assign("NotUsedList"     ,NotUsedList)        # T/F list of not used sub-areas.

   assign("datOrder",sortedOrd)                # data order for use with panelData back to statsDFrame


#  Note:  sDFdat is the statsDFrame in sorted order.  All areaDatxxx are in the same sorted order.     
#

#print("done with Not Used Key List.")

#
#  Working references on VisBorders
#

#
#  axisScale
#
#   Default Call = NULL,  Default value = "e"   new extended
#
   #cat("axisScale>",axisScale,"<\n")

   #print("Validating axisScale:")
   
   axisScale <- axisScale[[1]][1]

   axisMethod = 0
   if (!(missing(axisScale) || is.null(axisScale) || is.na(axisScale))) {
      if (axisScale == "s") {
         # set up axis to use titled scaling  
         axisMethod   <- 2
      }
      if (axisScale == "sn") {
         # set up axis to use number scaling with suffix.
         axisMethod   <- 3
      }
      if (axisScale == "e") {
         axisMethod   <- 4
      }
      if (axisScale == "o") {
         # set up axis to use titled scaling  
         axisMethod   <- 1
      }
      if (axisMethod == 0) {
         # if still set, but bad value
         warnCnt()
         xmsg    <- paste0("***01D0 CARG-SC The axisScale argument set to ",axisScale,", must be set to 'o', 'e', 's', or 'sn'.  The default of 'e' will be used.")
         warning(xmsg,call.=FALSE)
         axisScale       <- "e"    # extended algorithm
         axisMethod      <- 4
      }
   } else {
      # parameter not present or set to NULL/NA
      axisScale       <- "e"    # extended algorithm
      axisMethod      <- 4
   }
   if (axisMethod == 0) {
      warnCnt()
      xmsg    <- "***01D1 CARG-SC The axisScale argument is Missing, NULL or NA. It must be set to 'o', 'e', 's', or 'sn'.  The default of 'e' will be used."
      warning(xmsg,call.=FALSE)
      axisScale       <- "e"    # extended algorithm
      axisMethod      <- 4
   }
   #cat("axisScale:",axisScale,"  axisMethod:",axisMethod,"\n")

#
#  staggerLab
#
#   Default Call = NULL,  Default value = FALSE  
#
   #print("Validating staggered:")
   
   staggerLab <- staggerLab[[1]][1]

   staggered  <<- FALSE    # start with a lower value.

   if (!( missing(staggerLab) || is.null(staggerLab) || is.na(staggerLab) )) {
      if (!is.logical(staggerLab)) {
         staggerLab <- FALSE
         warnCnt()
         xmsg       <- "***01E0 CARG-SL The staggerLab argument is not a logical value. Setting staggerLab to FALSE."
         warning(xmsg,call.=FALSE)
      }
   } else {
      # parameter not present or set to NULL/NA
      staggerLab    <- FALSE    # default = FALSE - don't stagger axis labels.
   }
   
   #cat("staggerLab:",staggerLab,"\n")

   #cat("staggered:",staggered,"\n")

#
######

######
#
#  maxAreasPerGrp
#
#   Default Call = NULL,  Default value = 5  
#
   #print("Validating maxAreasPerGrp:")
   maxAreasPerGrp <- maxAreasPerGrp[[1]][1]

   def_maxAreasPerGrp = 5    # start with a lower value.

   if (!( missing(maxAreasPerGrp) || is.null(maxAreasPerGrp) || is.na(maxAreasPerGrp) )) {
      
      maxAreas <- as.numeric(maxAreasPerGrp)   # convert to numeric and see if it is valid.
      if (is.na(maxAreasPerGrp)) {
         maxAreasPerGrp <- def_maxAreasPerGrp
         warnCnt()
         xmsg       <- "***01E4 CARG-SL The maxAreasPerGrp argument is not a numeric value. Setting to the default of 5,"
         warning(xmsg,call.=FALSE)
      } else {
         # have numeric value
         if(!( maxAreasPerGrp == 5 || maxAreasPerGrp == 6 )) {
           # not 5 or 6
           xmsg <- "***01E5 CARG-AL The maxAreaPerGrp call parameter is not 5 or 6. Value set to 5."
           warning(xmsg,call.=FALSE)
           maxAreasPerGrp = def_maxAreasPerGrp
         }
      }
   } else {
      # parameter not present or set to NULL/NA
      maxAreasPerGrp    <- def_maxAreasPerGrp    # default = 5 - don't stagger axis labels.
   }
   
   #cat("maxAreasPerGrp:",maxAreasPerGrp,"\n")

#
######

######
#
#  Now that the row names and any deletions have been done, then
#  panels can finally be setup.
#
numRows     <- nrow(dat)
#
######

#print("done call parameters - on to panelDesc..")

######

#_________________________ Get Panel Default Values ______________________

#   use details in memory - now that we have merged them with users.

micromapGPanelDefaults <- micromapGSetPanelDef(numRows,rowSizeMaj,rowSizeMin,rowSepGap, 5, grpPattern)

#__________________________ Save Panel Defaults to memory 
 
   #  get copy of panel defaults
   
   wPanelDet <- micromapGPanelDefaults
   
   #  copy to micromapST memory space.
   
   defNam = names(wPanelDet)
   for (i in 1:length(wPanelDet))
      {
        assign(defNam[i],wPanelDet[[i]])
      }

#

cGrpRatios <- c(1.333/5.333, 2.333/5.333, 3.333/5.333, 4.333/5.333, 5.333/5.333)

   
#
#####
   

#########
#
#  Call arguments are checkes - on to panelDesc
#
#
#########

ErrFnd  <- FALSE
StopFnd <- FALSE

#
#_________________ Check panel description content and formats _____________
#
#
# Since the panelDesc is a data.frame, it is a given the number of items in each
#  variable list is the same number.
#
# When we move to list of lists, this is no longer true, but we don't care.
#
#  If the objective is the list of list, then we can't do a full scan of each 
#  variable at this stage of the processing.
#

#______________Check for panelDesc$type validity______________

valid = c("map","mapcum","maptail","mapmedian",
          "rank","id","arrow","bar",
          "dot","dotse","dotconf","dotsignif",
          "ts","tsconf",
          "scatdot",
          "segbar","normbar","ctrbar",
          "boxplot")              # idDot and rank are not currently implemented

#____________________ List of expected and valid parameters in the panelDesc

PDParms <- c('type',
             'lab1','lab2','lab3','lab4',
             'col1','col2','col3', 'colSize',
             'rmin','rmax',
             'refVals','refTexts',
             'panelData',
             'adv'
            )

# get list of names/options in panelDesc 

PDUsed <- names(panelDesc)    # used by every glyph function to check for parameters

PDPmatch <- match(PDUsed,PDParms)    # is if all entries in panelDesc are valid

if (any(is.na(PDPmatch))) {
   #  one of more panelDesc parameters are bad
   stopCnt()
   StopFnd     <- TRUE
   #PDErrorList <- paste0(PDUsed[is.na(PDPmatch)],collapse=" ")
   xmsg        <- paste0("***0113 CARG-PD The following named lists in ",pDName," panelDesc data.frame are not valid: ",paste0(PDUsed[is.na(PDPmatch)],collapse=" "))
   warning(xmsg,call.=FALSE)
}

#___________________the panelDesc parameters (column names) are good _____
#
numTopHeaderRows <- 4.25    # start with 1-Titles, 2-lab & 1-X Axis two lines. (have to cover ID and Map headers)
numBotHeaderRows <- 1       # bottom 1-X axis lines.

#

if (axisScale=="s") {
   #  add 1/2 line for reduced size and sub-title on units.
   numTopHeaderRows <- numTopHeaderRows + 0.5
   numBotHeaderRows <- numBotHeaderRows + 0.5
}
if (staggerLab)  {
   # if staggerLab is specified (forces) add 0.25.  Will know until it too late if is dyn turned on.
   numTopHeaderRows <- numTopHeaderRows + 0.25
   numBotHeaderRows <- numBotHeaderRows + 0.25
}
if (length(mstTitle)>1)  numTopHeaderRows <- numTopHeaderRows + 1.25

#
#  May be able to do a better job - later - future enhancement
#

#
#________________type parameter
#


if (is.na(match('type',PDUsed))) {
   # Error 'type' parameter is not present
   stopCnt()
   StopFnd <- TRUE
   xmsg    <- paste0('***0114 CARG-PD The required "type" named list is missing in the ',pDName,' panelDesc data.frame.')
   warning(xmsg,call.=FALSE)
}

# get type vector as characters no factor, etc.

type = as.character(panelDesc$type) 

# test contents of type vector for validity
PDTmatch = match(type,valid)

if ( any( is.na(PDTmatch) ) ) {
    PDErrorList <- paste0(type[is.na(PDTmatch)],collapse=" ")
    StopFnd <- TRUE
    stopCnt()
    xmsg    <- paste0("***0115 CARG-PD The ",pDName," type named list contains one or more invalid glyph name(s): ",PDErrorList)
    stop(xmsg, call. = FALSE)
}

PDMap    <- (PDTmatch <= 4)                   # the first four are maps  (TRUE if columns is a Map).
xSeq     <- seq(1,length(PDMap),by=1)                    

PDMapCol <- xSeq[PDMap]                       # Get column number of maps

#print(paste0("Map columns=",PDMapCol))

#  Set up number of glyphs columns

numCol   <- nrow(panelDesc)    # number of glyphs columns 

numPDRow <- nrow(panelDesc)    # number of values in each parameter in panelDesc
numPDCol <- ncol(panelDesc)    # number of parameters present in panelDesc

#
#_________________panelDesc$labx____________________
#

blank    <- rep('',numCol)  # empty vector for labels
NAList   <- rep(NA,numCol)  # NA vector 
oneList  <- rep(1,numCol)   # numeric vector of all 1s.
zeroList <- rep(0,numCol)


# a NULL column cannot exist in a data.frame.  If the name is present, it exist!

# lab1
if (is.na(match('lab1',PDUsed))) { 
    lab1   <- blank 
} else {
    lab1   <- as.character(panelDesc$lab1)                 # convert to character
    xlna   <- is.na(lab1)                                  # find NA values in vector
    if (any(xlna))  lab1[xlna] <- ""                       # change NAs to ""
}

# lab2
if (is.na(match('lab2',PDUsed))) {
    lab2   <- blank 
} else {
    lab2   <- as.character(panelDesc$lab2)                 # convert to character
    xlna   <- is.na(lab2)                                  # find NA values in vector
    if (any(xlna))  lab2[xlna] <- ""                       # change NAs to ""
}
# lab3
if (is.na(match('lab3',PDUsed))) {
    lab3   <- blank 
} else {
    lab3   <- as.character(panelDesc$lab3)                 # convert to character
    xlna   <- is.na(lab3)                                  # find NA values in vector
    if (any(xlna))  lab3[xlna] <- ""                       # change NAs to ""
    numBotHeaderRows <- numBotHeaderRows + 1
}
# lab4
if (is.na(match('lab4',PDUsed))) {
    lab4   <- blank 
} else {
    lab4   <- as.character(panelDesc$lab4)                 # convert to character
    xlna   <- is.na(lab4)                                  # find NA values in vector
    if (any(xlna))  lab4[xlna] <- ""                       # change NAs to ""
}

#  All labels (1-4) are either text or "" entries.  Don't have to check for missing, NULL or NA.

#_________Save panelDesc Parameters in to namespace____________
#

   assign('lab1',lab1)
   assign('lab2',lab2)
   assign('lab3',lab3)
   assign('lab4',lab4)

#print(find("lab1"))  # print environment

# more panelDesc checks and setups after the function definitions.

#
#_______________________panelDesc$colx_____________________
#
#  Process -
#   1) check entire panelDesc variable vector and convert to numbers  "CheckCol"
#   2) In glyph check value and get data   "CheckPDCol"
#   3) check data vector for valid data    "CheckNum"
#

# number of columns based on the presence of Descriptions for Column

  # col1
  if (!is.na(match('col1',PDUsed))) {
     # col1 is present
     litcol1 <- as.character(panelDesc$col1)
     col1    <- CheckColx2(litcol1,"col1",1,panelDesc$type,wSFNameList,len_wSFName)
     x <-  (col1 == 0)
     #print(x)
     if (any(x,na.rm=TRUE)) {  StopFnd <- TRUE }
  } else {
     litcol1 <- NAList
     col1    <- NAList
  }
 #cat("col1:",paste0(col1,collapse=", "),">>",paste0(litcol1,collapse=", "),"\n")
  
  # col2
  if (!is.na(match('col2',PDUsed))) {
     # col2 is present
     litcol2 <- as.character(panelDesc$col2)
     col2    <- CheckColx2(litcol2,"col2",2,panelDesc$type,wSFNameList,len_wSFName)
     x <- (col2 == 0)
     #print(x)
     if (any(x,na.rm=TRUE)) {  StopFnd <- TRUE }
  } else {
     litcol2 <- NAList
     col2    <- NAList
  }
 #cat("col2:",paste0(col2,collapse=", "),">>",paste0(litcol2,collapse=", "),"\n")
  
  # col3
  if(!is.na(match('col3',PDUsed))) {
     # col3 is present 
     litcol3 <- as.character(panelDesc$col3)
     col3    <- CheckColx2(litcol3,"col3",3,panelDesc$type,wSFNameList,len_wSFName)
     x <- (col3 == 0)
     #print(x)
     if (any(x,na.rm=TRUE)) {  StopFnd <- TRUE }
  } else {
     litcol3 <- NAList
     col3    <- NAList
 }
#cat("col3:",paste0(col3,collapse=", "),">>",paste0(litcol3,collapse=", "),"\n")
  
#
#_____________panelDesc$rmin and rmax______________
#

   if (is.na(match('rmin',PDUsed))) rmin = NAList else
              rmin = as.numeric(panelDesc$rmin)

   if (is.na(match('rmax',PDUsed))) rmax = NAList else
              rmax = as.numeric(panelDesc$rmax)

#
#_____________panelDesc$refxxx________________
#

   if (!is.na(match('refVals',PDUsed))) {
      assign('lRefVals',as.numeric(panelDesc$refVals))
      # detail test in glyphs
   } else {
      assign('lRefVals',NAList)
   }
   # no check if RefVals are numeric. ????
   
   if (!is.na(match('refTexts',PDUsed))) {
      assign('lRefTexts',stringr::str_trim(panelDesc$refTexts))
      lRefTexts[lRefTexts == ""] <- NA  # convert blanks. 
      numBotHeaderRows <- numBotHeaderRows + 1
   } else {
      assign('lRefTexts',NAList)
   }
   # no check if RefTexts are character. ????
   
   #  
   #  Make adjustments for color or grays
   #

   if (colFull) {
     
      # set color values to work variables
      iRef.Val.col  <- Ref.Val.col
      iRef.Text.col <- Ref.Text.col
    
   } else {
  
      # set gray values to work variables
      iRef.Val.col  <- Ref.Val.BW.col
      iRef.Text.col <- Ref.Text.BW.col
   }
   
   

#_____________panelDesc$panelData_______________
#

#  if present is the typeof correct ?   - check within the glyph - it may be different.

   if (is.na(match('panelData',PDUsed))) { 
       wPanelData <- NAList 
   } else {
       wPanelData <- as.character(panelDesc$panelData)            # save pointer to panelD
   }    
   assign('panelData',wPanelData)
 
   rm(wPanelData)
       
   #
   #_________________-
   #
   #cat("Check on header row counts - top:",numTopHeaderRows,"  bot:",numBotHeaderRows,"\n")
   #cat("    top mar:",numTopHeaderRows * 0.2, "   bot mar:",numBotHeaderRows* 0.2,"\n")
   #cat("  compare to 1.1 and 0.5/0.75\n")
   
   #___panelDesc$colSize_________User specificed column width processing and checking

   # ____________________Column Size layout (initial)

   #  IdW set up in plotNames check

   numCol = length(type)    # get number of columns to support
  
   #cat("Building cparm table for run - Number of columns:",numCol,"\n")
   
   cparm   <- data.frame(cSize=numeric(0),lSep=numeric(0),rSep=numeric(0),rMinH=numeric(0),rMaxH=numeric(0))   # empty data.frame

   #  Build column width table based on the types of columns specified.
   
   for (j in 1:numCol) {
       # Test type of column to be built and call build routine.
      #cat("top of loop - type=",type[j],"\n")
      cparm2 =  switch(type[j],
            #  colSize, col width, left sep, right sep, row min, row max)
            "map"=      c(max(banner.max["map","width"],Map.Min.width),0,0,Map.MinH, Map.MaxH),            
            "mapcum"=   c(max(banner.max["mapcum","width"],Map.Min.width),0,0,Map.MinH, Map.MaxH),
            "maptail"=  c(max(banner.max["maptail","width"],Map.Min.width),0,0,Map.MinH, Map.MaxH),
            "mapmedian"=c(max(banner.max["mapmed","width"],Map.Min.width),0,0,Map.MinH, Map.MaxH),
            "id"=       c(IdColWidth,0,0,0,0),
            "dot"=      c(0,0,0,0,0),
            "dotse"=    c(0,0,0,0,0),
            "dotconf"=  c(0,0,0,0,0),
            "dotsignif"=c(0,0,0,0,0),
            "arrow"=    c(0,0,0,0,0),
            "bar"=      c(0,0,0,0,0),
            "boxplot"=  c(0,0,0,0,0),
            "ts" =      c(0,.175,0,0,0),
            "tsconf" =  c(0,.175,0,0,0),
            "scatdot" = c(0,.175,0,0,0),
            "segbar"  = c(0,0,0,0,0),
            "normbar" = c(0,0,0,0,0),
            "ctrbar"  = c(0,0,0,0,0),
            "rank"    = c(Rank.width,0,0,0,0),
            "nomatch" = c(0,0,0,0,0)
         )
      #cat("cparm2:",paste0(cparm2,collapse=", "),"\n")
      cparm <- rbind(cparm,cparm2)
     
   }
 
   # now have one row per column in the user panelDesc data.frame.
   
   colnames(cparm) <- c("cSize","lSep","rSep","rMinH","rMaxH")

   #cat("Column Sizing Table completed.\n") # dump table.
   #print(cparm)
   #cat("\n")
   
   # one row per column.

   borders = rep(borderSize,4)    # set borders widths to 0.5 inches

   ###  Add check of column type to table of miniumal or statics column widths.
   ###  Must have details lists processed to do this.
   ###  Recreate plotX as done in panelLayout

   # Pick up row height min and max from types used.
  
   rowMinH          <- max(cparm[,"rMinH"],rowSizeMn)     # Largest mininum for all glyphs involved and system minimum size (inches)
   rowMaxH          <- max(cparm[,"rMaxH"],rowSizeMx)     # Largest maximum for all glyphs involved
   
   #cat("rowMinH:",rowMinH,"   rowMaxH:",rowMaxH,"\n")
     
   #  Same formula as panelLayout

   xPlotWidth = (par("din")[1])-borders[1]-borders[2]-leftMar-rightMar   #  width in inches - (colSep, borders, left and right margins).
   
   #cat("xPlotWidth:", xPlotWidth,"\n")
   
   # done IdW = Id.width[1]                 # bigger value for full names  [2] is for abbreviations - default

#
#_____________panelDesc$colSize____________________
#

   colWidths      <- cparm[,1]              # get list of fixed width glyphs that have been requested (column # 1) from cparm.               
                                            # In this table, a value of zero is NO Fixed Width.
   colFlex        <- !(colWidths > 0)       # save list of glyphs that use don't have fixed widths - flexible values ( not maps and id )
   colNumID       <- c(1:length(colWidths))
   colGood        <- rep(TRUE,length(colWidths))
   
   #cat("colSize-Start colWidths:",colWidths," len:",length(colWidths),"  colFlex:",colFlex,"\n")
   
   DoColSize      <- FALSE
  
   # check for parameter?
   
   if (!is.na(match('colSize',PDUsed))) {
     # colSize is present
     DoColSize    <- TRUE              # colSize is present - do proportional space allocation.

     wColS       <- panelDesc$colSize
     #cat("Processing colSize parameter:",wColS,"  len:",length(wColS),"\n")

     if (length(wColS) != length(colWidths)) stop

     # check for NA's in colSize fields. - Error. Clear to NULL ""
     wColBad  <- is.na(wColS[colFlex])
     
     if (any(wColBad)) {
        # yes, invalid value by user.
        wColBadList <-colNumID[colFlex & is.na(wColS)]
        if (length(wColBad)<=0) stop
        warnCnt()
        xmsg       <- paste0("***01F1 CARG-CS The 'colSize' parameter in ",pDName," contains NA values in columns: ",paste0(wColBadList,collapse=","),". "," Values must be numeric and > 0.")
        warning(xmsg,call.=FALSE)
        
        colGood[wColBadList] <- FALSE    # mark problem column
     }
     
     #cat("1-wColS:",wColS,"  colGood:",colGood,"\n")
     
     # check for invalid fixed width fields in colSize - NA, "", " ", 0 -> OK.  Else - Bad and report.
     
     #  Set to NA all valid fixed width column values in the colSize vector.
     #     NA is valid
     wColS[!colFlex & wColS == 0  ]           <- NA   # 0 is valid
     wColS[!colFlex & stringr::str_trim(wColS) == "" ] <- NA   # "", " ", etc is valid
     #  What we have left is possible invalid entries.
     
     # if any fixed width column is not NA, problem
     if (any(!is.na(wColS[!colFlex]))) {
        # fixed width columns have characters or numeric or logical vlaues - OUCH!
        wColBadList <- wColS[!colFlex & !is.na(wColS)]   # get list of bad values.
        if (length(wColBad)<=0) stop  # check on programmer
        warnCnt()
        xmsg       <- paste0("***01F2 CARG-CS The 'colSize' parameter in ",pDName," has values for fixed width glyphs. Value(s): ",paste0(wColBadList,collapse=","),".  ", "Value(s) are ignored and set to NA.")
        warning(xmsg,call.=FALSE)
        #  at this point the fixed columns are NA or can be set to NA.
        wColS[!colFlex] <- NA  
     }
    
     #cat("2-wColS:",wColS,"  colFlex:",colFlex,"  colGood:",colGood,"\n")
     
     # Convert to numeric, if NA in colSize fields - eError report and set to NULL or "".
     #   Fixed Width Columns are NA, so we not work on flexible columns that can have values.
     
     suppressWarnings(numColS  <- as.numeric(wColS))   # make sure it's numeric.
     
     #  Any flex column that is not a number or can not be converted to number -> NA.
     #  also check for "Inf" values.   Will use as marker later.
     wColFG     <- colFlex & colGood
     wColSize   <- numColS[wColFG]
     wColNum    <- colNumID[wColFG]
     
     wColBad    <- is.na(numColS[wColFG])     
     
     if (any(wColBad)) {
 
        # have colSize value(s) that is not numeric or are "Inf".
        wColBadList   <- wColSize[wColBad]        # get list of bad entries.
        # invalid colSize entries, not numeric, could be character, logical, etc.
        warnCnt()
        xmsg        <- paste0("***01F3 CARG-CS The 'colSize' parameter in ",pDName," does not contain numeric values : ",paste0(wColBadList,collapse=","),".")
        warning(xmsg,call.=FALSE)
        #
        wColBadList          <- wColNum[wColBad]   # get index numbers
        colGood[wColBadList] <- FALSE 
     }
     
     #cat("3-wColS:",wColS," numColS:",numColS," colGood:",colGood,"\n")
          
     # colSize check range.
     wColFG       <- colFlex & colGood    # only range check good (so far) colSize values.
     wColSize     <- numColS[wColFG]      # list of values
     wColNum      <- colNumID[wColFG]     # indexes to vector
     
     # run the test.
     wColBad      <- ( wColSize <= 0 | wColSize > 200 )  # Only look at remaining good entries.
     
     if (any(wColBad)) {
        # colSize values out of acceptable range.
        wColBadList   <- wColS[wColNum[wColBad]]               # get list of bad entries
        # colSize entries are out of range <= 0 or > 200.
        warnCnt()
        xmsg          <- paste0("***01F4 CARG-CS The 'colSize' entries in ",pDName," are out of range ( <= 0 or > 200 ). Values: ", paste0(wColBadList,collapse=","), ".")
        warning(xmsg,call.=FALSE)
        colGood[wColNum[wColBad]] <- FALSE   # set all out of range values as no bad.
     }
     
     #cat("4-wColS:",wColS," numColS:",numColS," colGood:",colGood,"\n")
               
     numColS[!colGood] <- 0     # set bad values to zero.
     
     #cat("5-wColS:",wColS," numColS:",numColS,"\n")
     
     # Fix colSize columsn to Mean - "" columns in colFlex range.
         
     #  Get sum of valid colSize entries.   
     wColFG           <- colFlex & colGood
     sumFixCol        <- sum(colWidths)         # sum of fixed widths
     sumColSize       <- sum(numColS[wColFG])  # sum of values in user provided colSize 
               # bad values were set to zero.
     meanColSize      <- mean(numColS[wColFG]) # mean of values
     
     #cat("6-sumFix:",sumFixCol,"  sum colSize:",sumColSize," mean:",meanColSize,"\n")
     
     if (sumColSize == 0)             { DoColSize <- FALSE }   # sum of colSize = zero.
     if (all(!colGood[colFlex]))      { DoColSize <- FALSE }   # if all entries are bad - ignore colSize
     
     if (DoColSize) {
        # All flex columns must have a value
        #  replace bad values with mean of good values.
        repColS     <- colFlex & !colGood
        if (any(repColS)) {
           # we have come bad values to change to mean.
           wColBadList  <- wColS[repColS]  # get list of values being changed.
           
           warnCnt()
	   xmsg      <- paste0("***01F5 CARG-CS The reviewed 'colSize' parameter in ",pDName," has bad values (see above) and have been replaced by the mean of the good values: ", meanColSize,".  Bad Values:", paste0(wColBadList,collapse=","))
	   warning(xmsg,call.=FALSE)
           numColS[repColS] <- meanColSize
        }
        colSize     <- numColS                  # transfer back to colSize.
        litColSize  <- as.character(numColS)    # common starting point - either character or numeric.
        #cat("final colSize:",colSize,"\n")
     } else {
        warnCnt()
	xmsg      <- paste0("***01F6 CARG-CS The 'colSize' parameter in ",pDName," contains no useful information and will be ignored.")
	warning(xmsg,call.=FALSE)
	colSize    <- NAList
     }
   
   } else {
     # no parameter specified.
     colSize     <- NAList
     DoColSize   <- FALSE
   }
   #
   #  Only keep colSize entires for flexible glyphs
   #
   
   #cat("Finish pre-processing colSize -- DoColSize:",DoColSize,"  colSize:",paste0(colSize,collapse=", "),"\n")
   
   #cat("Starting column width calculations\n")
   
   #  colWidths      has column widths in inches or zero is not set yet. (initially fixed width columsn.)
   #  colFlex        has TRUE for columns that are width is undetermined.
   #  colSize        edited vector of relative ratio values for each column.
   
   # basic column separators (0 on edges, colSepGap for all internal)
   colSep       <- c(0,rep(colSepGap,numCol-1),0)

   # based on column type, add more space on left or right.  (cparm[,2] for left, cparm[,3] for right.) - Y Axis.  
   colSep[1:numCol]     <- colSep[1:numCol] + cparm[,2]          # add space on left of panel
   colSep[2:(numCol+1)] <- colSep[2:(numCol+1)] + cparm[,3]      # add space on right of panel
   #cat("colSep:",colSep,"\n")

   colSepSum     <- sum(colSep)               # total width used by separators
   
   xPlotWidthOrg <- xPlotWidth
   xPlotWidth    <- xPlotWidth - colSepSum    # space - subtract separator space = Available X width
   # available space.
   
   usedSpace     <- sum(colWidths)             # get amount of allocated space.
   freeSpace     <- xPlotWidth - usedSpace      # available space
   
   #cat("Setup-Space:",xPlotWidthOrg,"  colSepSum:",colSepSum,"  Avail:",xPlotWidth,"  freeSpace:",freeSpace," usedSpace:",usedSpace,"\n")
 
   if (DoColSize) {
     #cat("Doing colSize - colSize:",colSize,"  colWidths:",colWidths,"\n")
     if (length(colSize) <= 0) stop
     
     # Cycle 1 - calculate and adjust for minimum column widths
     
     sumColSize  <- sum(colSize,na.rm=TRUE)   # sum values
     wColSizeP   <- colSize/sumColSize        # get proportion.
     
     wColSize    <- wColSizeP * freeSpace     # calculate allocations.    
     wColMinE    <- (wColSize < colSizeMin)   # find too small columns.
     
     colWidths[wColMinE]  <- colSizeMin       # set low values to min. (if they exist)
     colSize[wColMinE]    <- 0                # remove low values from colSize calculation.
     
     #cat("C1-colSize:",colSize,"  wColSizeP:",wColSizeP,"  colWidths:",colWidths,"\n")

     # Cycle 2 - calculate (again) and adjust for maximum column widths
     
     usedSpace   <- sum(colWidths)
     freeSpace   <- xPlotWidth - usedSpace
     #cat("C2-usedSpace:",usedSpace,"  freeSpace:",freeSpace,"\n")
     
     sumColSize  <- sum(colSize,na.rm=TRUE)   # sum values
     wColSizeP   <- colSize/sumColSize        # get proportion.
     
     wColSize    <- wColSizeP * freeSpace     # calculate allocations.    
     
     wColMaxE    <- (wColSize > colSizeMax)
     #cat("C2-Max test - sumColSize:",sumColSize,"  wColSizeP:",wColSizeP,"  wColSize:",wColSize,"  wColMaxE:",wColMaxE,"\n")
     
     if (any(wColMaxE,na.rm=TRUE)) {
        # only do one more cycle if a value > max is found.
        
        colWidths[wColMaxE] <- colSizeMax      # set high values to max.
        colSize[wColMaxE]    <- 0               # remove high values from colSize calculation.
     
        #cat("C2-Max adj-colSize:",colSize,"  wColSizeP:",wColSizeP,"  colWidths:",colWidths,"\n")

        # Cycle 3 - if max adjustments - do it one more time.

        usedSpace   <- sum(colWidths)
        freeSpace   <- xPlotWidth - usedSpace
        #cat("C3-usedSpace:",usedSpace,"  freeSpace:",freeSpace,"\n")

        # Repeat for final values.
     
        sumColSize  <- sum(colSize,na.rm=TRUE)   # sum values
        wColSizeP   <- colSize/sumColSize        # get proportion.
     
        wColSize    <- wColSizeP * freeSpace     # calculate allocations.    
     }
     
     #  Last step - place the widths in to colWidths

     #  colSize columns hitting the minimum and maximum values have already been set in colWidths vector. 
     #  last calculation setup wColSize with the last columns.

     wColValFlag    <- (wColSize > 0 )           # list of values to merge into colWidths
     wColValFlag[is.na(wColValFlag)] <- FALSE    # NA are fixed columns, so make FALSE (no update)
     
     colWidths[wColValFlag] <- wColSize[wColValFlag]  # put values into wColWidths
  
   } else {
     # no colSize - do old way - equal subdivide.
     
     zeroCol    <- !(colWidths > 0)          # TRUE for any column with no width assigned. 
     numberCol  <- sum(zeroCol,na.rm=TRUE)   # get number of TRUEs = number of columns that need widths. (sum 1s)
     equalCol   <- freeSpace / numberCol     # get width of each column.
     #cat("Initial equalCol:",equalCol,"  FreeSpace:",freeSpace,"\n")
   
     if (equalCol > colSizeMax)  {  equalCol <- colSizeMax  }
   
     if (equalCol < colSizeMin) {
        warnCnt()
        ErrFnd <- TRUE
        xmsg    <- paste0("***0420 PANEL Calculated column widths is less than minimum ",colSizeMin," inches - too many columns specified.")
        warning(xmsg,call.=FALSE)
         
        if (equalCol < colSizeMin/2) {
           stopCnt()
           StopFnd <- TRUE
           xmsg    <- paste0("***0421 PANEL Column width is too small to be useful, Package stopped.")
           stop(xmsg,call.=FALSE)
        }
     }
     
     colWidths[zeroCol] <- equalCol
   }
   
   #cat("Final-colWidths:",colWidths,"\n")
   
   #
   savedColWidths <- colWidths    # save a copy of the column size parameters.
   savedColSep    <- colSep        # save a copy

   legfactor <- 1

   # add space if reference values provided.
   # JP-2010/07/23 0 change to refVals to be consistent.
   
   #cat("numTopHeaderRows:",numTopHeaderRows,"  numBotHeaderRows:",numBotHeaderRows,"\n")

   if(!is.null(panelDesc$refVals)){
      # if field present.

      if(any(!is.na(panelDesc$refVals))){

         # if value provided, provide room in the bottom margin for the reference test.
      
         botMar    <- botMarLegend

         # revisit calculation below to be more precise
         legfactor <- 9/(9-botMardif)    # ????
         
         #### Check on the use and need for "legfactor" in older code.
      }      
   }
   #cat("botMar:",botMar,"\n")
   
   #assign('legfactor',legfactor)  

   ########
   #
   #  Check for warnings or stops that should terminate the package/function
   #
   if (StopFnd) {
      xmsg    <- "***01Z9 CARG Errors have been found in parameters and data.  Please review program log and fix problems. Packaged stopped."
      stop(xmsg, call.=FALSE)
     
   }
   if (ErrFnd) {
      warnCnt()
      xmsg    <- "***01Z8 CARG Warnings have been found in the parameters and data.  Package continues, but results are unpredictable. Review log and fix errors."
      warning(xmsg, call.=FALSE)
   }   


   ########
   #
   #
   #  Process and calculate column spacing and offsets.
   #
   #
   # should not need to set this again.
   
   numCol   <- length(type)
   
   #####
   #
   # We build three panel layouts:
   #    1) layout of all glyphs panels (ngroups by ncols)
   #    2) layout of general blocks of glyphs (top, median, bottom groups) (3 or 2 by ncols)
   #    3) layout of page blocks (top, median, bottom groups) but only 1 column (3 o4 2 by 1)
   #
   #####
   
   #
   #  USStatesBG    set up - 50 or 51 rows -> 10 or 11 groups  - median - single element 
   #
   #  USSeerBG      set up - 9 to 20 rows -> 3(of 3,3,3) to 4(of 5,5,5,5) groups
   #
   #  KansasBG      set up - 105 rows -> 21 groups - median - 5 rows/group (11 groups)
   #
   #  NewYorkBG     set up - 62 rows -> 13 groups (5..5,4,4,4,5...5)   
   #
   #  MarylandBG    set up - 24 rows (counties + 1 city) -> 5 groups (5,5,4,5,5)
   #
   #  UtahBG        set up - 29 rows -> (5.5.4.1.4.5.5) -> 7 groups
   #
   #  ChinaBG       set up - 34 rows -> (5,5,5, <1,2,3,4> ,5,5,5) -> 7 groups
   #
   #  UKIrelandBG   set up - 218 rows -> (5,5,5,...,4,4,...,5,5,5)
   #
   #  SeoulKoreaBG  set up - 25 rows (districts) -> (5,5,5,5,5) -> 5 groups
   #
   #  AfricaBG      set up - 52 rows (countries) -> (5,5,5,5,5,2,5,5,5,5,5) -> 11 groups
   #
   #

   #printPanelsParms()
   # build layout for glyphs panels  (numGrps x ncol) (Individual)
   
   #cat("panelLayout - panels\n")
   
   assign("panels",panelLayout(
                        vnrow       = numGrps,               # num of Row/Groups
                        vncol       = numCol,                # num of columns
                        topMargin   = topMar,                # 0.95
                        bottomMargin= botMar,                # 0.5
                        leftMargin  = 0,                      
                        rightMargin = 0,
                        rowSep      = rowSep,                # vector
                        rowSize     = rowSize,               # vector
                        colSize     = colWidths,            # calculated column widths (inches)
                        colSep      = colSep,                # vector
                        rSizeMx     = rowMaxH,
                        rSizeMn     = rowMinH,
                        rSizeMaj    = rowSizeMaj,            # 7 rows per group/row
                        rMapCol     = PDMapCol,
                        disErr      = FALSE,
                        rDebug      = MST.Debug)
          )                                                  # c(.1,.1,.1) for 3

   
   #  Done above by "micromapSetPanelDef"
   #grounpedRowSize = details[["groupedRowSize"]]            # c(35,1.65,35) -> USStatesBG (51)
                                                             # c(7,7,7) or c(7,7,7,7) -> USSeerBG (9 -- 20)
                                                             # c(70,7,70) -> KansasBG (105)
                                                             # c(42,7,42) -> NewYorkBG (62)
   
   #groupedRowSep   = details[["groupedRowSep"]]             # c(0,0.1,0.1,0) or c(0,0.1,0)

 
   #print("panels;")
   #print(panels)
   
   
   #cat("medGrp:",medGrp,"\n")
      
   # Major panel group  title-top, panels, title-bottom  by columns (overlays panels)
   # section of panels (top(25), median(1), bottom(25) and "N" columns wide.
      
   ### generalize settings  - main panels (middle level)  (3 rows - "N" cols)
   ###    rows= title, glypics, footnotes  cols=one for each glyph
   
   panelBlocks    <- 2   # Number of blocks for an even number of group/Rows
   
   if (medGrp > 0) {
      panelBlocks <- 3   # Number of blocks for an odd number of group/Rows
   }

   #printPanelParms("panelGroup")
   
   #cat("panelLayout - panelGroup\n")
  
   # build layout for top, median(if present) and bottom cover panels (3 or 2 x numCol)
   assign("panelGroup",panelLayout(
                        vnrow        = panelBlocks,           #  2 or 3
                        vncol        = numCol,                #  numCols
                        topMargin    = topMar,
                        bottomMargin = botMar,
                        leftMargin   = 0,
                        rightMargin  = 0,
                        rowSize      = groupedRowSize,
                        rowSep       = groupedRowSep,
                        colSize      = colWidths,
                        colSep       = colSep,
                        rSizeMx      = rowMaxH,
                        rSizeMn      = rowMinH,
                        rSizeMaj     = rowSizeMaj,
                        rMapCol      = PDMapCol,
                        disErr       = TRUE,
                        rDebug       = MST.Debug)
        )
  
   #print("panelGroup:")
   #print(panelGroup)
  
   #cat("panelLayout - panelOne\n")
  
   # build layout for page (3 or 2  x 1)
   assign("panelOne",panelLayout(
                        vnrow        = panelBlocks,            #  2 or 3
                        vncol        = 1,                      #  1
                        topMargin    = topMar,
                        bottomMargin = botMar,
                        leftMargin   = 0,
                        rightMargin  = 0,
                        rowSize      = groupedRowSize,
                        rowSep       = groupedRowSep,
                        rSizeMx      = rowMaxH,
                        rSizeMn      = rowMinH,
                        rSizeMaj     = rowSizeMaj,
                        rMapCol      = PDMapCol,
                        disErr       = TRUE,
                        rDebug       = MST.Debug)
         )
  
   
   #print("panelOne:")
   #print(panelOne)
   
   #
   #  Variables that span glyphs
   #

   #staggered <- FALSE     # Flag to indicate where the current column should start staggering numbers
                        # FALSE = first label on line 1,   TRUE = first label on line 2.
                        # This value is set when staggered labels are proceed based on if the last value
                        # in the atRx1 is greater thatn atRx2 = TRUE then value is TRUE.


#####
# ____________________Main loop______________________________
#
#  Future of main loop.
#  This will change to do:  Setup, Page 1-Page Header, Glyph "n1" to "n2", and then the next page.
#
#####

#cat("Main Loop\n")

#  Build images of each column

   for (j in 1:numCol)  {
   
      #cat("Doing Type:",type[j],"\n")
   
      # Test type of column to be built and call build routine.
      switch(type[j],
         "map"=      rlAreaMap(j),
         "mapcum"=   rlAreaMapCum(j),
         "maptail"=  rlAreaMapTail(j),
         "mapmedian"=rlAreaMapMedian(j),
         "id"=       rlAreaID(j),
         "dot"=      rlAreaDot(j,      dSignif=FALSE),
         "dotse"=    rlAreaDotSe(j),
         "dotconf"=  rlAreaDotConf(j),
         "dotsignif"=rlAreaDot(j,      dSignif=TRUE),
         "arrow"=    rlAreaArrow(j),
         "bar"=      rlAreaBar(j),
         "boxplot"=  rlAreaBoxplot(j,  as.character(panelDesc$panelData[j]) ),
         "ts" =      rlAreaTSConf(j,   as.character(panelDesc$panelData[j]),  conf=FALSE),
         "tsconf" =  rlAreaTSConf(j,   as.character(panelDesc$panelData[j]),  conf=TRUE),
         "scatdot" = rlAreaScatDot(j),
         "segbar"  = rlAreaSegBar(j),
         "normbar" = rlAreaSegBar(j,   SBnorm=TRUE),
         "ctrbar"  = rlAreaCtrBar(j),
         "rank"    = rlAreaRank(j),
         "nomatch"
      )
      #cat("End of glyphs Call - lastSpace Lab2:",lastLab2Space,"  Lab3:", lastLab3Space,"\n")
   }
 
   # All columns are built and sitting in the panel.
   
   #####
   #
   # Fill in the top Page Titles
   #
   
   #cat("panelSelect - panelOne - margin='top'\n")
   
   panelSelect(panelOne,margin="top")    # full page top label area.
   x <- panelScale()

   if (length(mstTitle)==1){
       text(.5,.77,mstTitle,cex=Title.cex)
   } else {
       # only use the first two title character strings
       text(0.5, 0.9, mstTitle[1],cex=Title.cex)
       text(0.5, 0.65,mstTitle[2],cex=Title.cex)
   }
 
   #
   #####
 
   #####
   #
   # Time to report on the warnings and errors
   #
   
   message("End of micromapST processing.\n\n")
   
   warnNum <- get("i",envir=environment(warnCnt))  # get warnings counter
   if (warnNum > 0) {
      message(paste0(warnNum," warnings messages were logged.  Please review the run log and resolve any issues."))
   } else { 
      message("No warnings were logged.")
   }
   stopNum <- get("i",envir=environment(stopCnt))  # get stop message counter
   if (stopNum > 0) {
      message(paste0(stopNum," Stop messages were logged.  Please resolve issues and rerun."))
   } else {
      message("No stop messages were logged.")
   }
   if (( warnNum + stopNum ) > 0) {
      message("If warnings and error messages did not appear on your R console, please execute 'warnings()' to list them.\n")
   }
   message(" ")
  
   # change the following to call end of run report.  - set at start so R stops will be caught.
   
   #
   #####
   #x <- Sys.setlocale('LC_ALL',Saved_Locale)
   
   on.exit(print("micromapST Ends"))
   
} # end of micromapST Function

###  End of micromapST


####
#
#   .onLoad function - executed when the package is loaded initially.
#      builds a non-changeable micromapGDefault data.frame for use
#      as the default when colors and/or details are not specified.
#
#    Added by JP - Oct, 2012 - Setup permanent micromapGDefault data.frame for 
#          use as the default values on the call.
#
#    No longer required.
#
####

#.onLoad = function (libraryName, pkgName)
#
#   { 
#     #packageStartupMessage(".onLoad")
#     #packageStartupMessage(libraryName)
#     #packageStartupMessage(pkgName)
#     # generate default data.frame for micromapST.
#     #rlmicromapGDefaults <- micromapGSetDefaults()
#     #micromapGDefaults <<- rlmicromapGDefaults
#  
#    }
#

#  
####  
#
# End of load and variable initialization
#
####


######
####  ADD CHECK to make sure values are numeric when required.  (content of columns.)
####    Done for Arrow, the Dot set, Bar, SegBar/NormBar, CtrBar
####    Not yet for BoxPlot and TS.
######
