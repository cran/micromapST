######
#####
#
#  date: January 9, 2024
#
#  packages used by BuildBorderGroup function
#
#  base		bitwise functions; plot; load; and more.
#  tools	CRAN_check_xxxx; file_ext; file_path_sans_ext; file_exists; dir_exists
#  stringr	str_trim; str_split; str_to_upper; str_pad; str_locate; str_replace; str_replace_all; 
#               str_sub; str_to_lower; str_squish; str_remove; str_remove_all;
#  graphics	plot; plot.new; par
#  graphicsR	
#  RColorBrewer	brewer_pal
#  utils	read.csv; read.table; data; capture.output; globalVariables; object.size;
#               head; tail; str; write.csv; 
#  rmapshaper   ms_simplify  (SPDF and/or sf)
#  readxl	read_xls; read_xlsx
#  writexl	write_xlsx
#
#  sf		plot.sf; 
#               st_geometry; st_shift_longitude; st_is_longlat; 
#               st_transform; st_drop_geometry;  st_as_sf; st_as_sfc; 
#               st_union; st_difference; st_intersects; st_intersection; 
#               st_area; st_coordinates; st_bbox; st_centroid; 
#		st_crs; st_as_text; st_boundary; st_cast; 
#               st_is_valid; st_make_valid; st_read; st_write; 
#               sf_set_s2, st_buffer
#
#  spdep        poly2nb         # and supporting functions 
#
#####
  
#####
#
#   BuildBorderGroup  function to support micromapST.
#   Updated: 2021-1024,  2022-0316 (Testing and demo data), 2022-0920 (clean up and testing.)
#            2022-1001 to 2022-1205 (migrate to sf functions from retired functions).
#            2023-02xx to 2023-0724 (continue to update to sf, verify code, and test for 
#                release.)
#   In support of release 3.0.0.
#            2023-0724 to 2024-0103 continued updates to support book and correct problems.
#
#   This function takes a shapefile and name table (CSV or Excel format)
#   and creates a border group dataset ".RDA" of the geographic 
#   areas in the shape file for use as boundary data and location 
#   ids for micromapST. The micromapST package default border
#   group is a characterized US State map.  Several examples
#   of state county boundary, US Seer 18 Registry boundary, continent (Africa)
#   country (China, UK), and city (Seoul , S. Korea) are 
#   included in the micromapST package.
#
#   Several features of micromapST border groups are partially supported
#   by this build script:
#
#      1) Level 2 super-area boundaries.
#           Level 2 was originally intended to overprint existing or
#           selected boundaries that are already drawn with a slightly
#           heavier line to provide an acsent.  The level 2 
#           are only printed if there is a L2_ID entry in the 
#           name table, there is a L2VisBorders data.frame and the L2 enabled
#           is set to TRUE in the border group dataset.  
#
#           check the following????
#           If L2 boundaries differ from the area and regional 
#           boundaries, L2 is enabled and the VisBorders data.frame built. ???
#           Their super-areas can be specified in the Name Table provided
#           by the user, but must consist of aggregations of the areas.
#
#      2) If a map space is divided into regions (like to U. S. Census regions),
#           each region will be based on the boundaries of its areas.
#           When regional mapping is enabled, only the regions and their 
#           member areas boundaries are drawn when any of the areas in a region
#           have data.  If a region does not have any areas with data, then that
#           region and it member areas will not be drawn.
#           boundaries are drawn that contain data.
#           The name table provides the association of each area with 
#           its region.  When the border group is built, if the number of 
#           regions found in name table is greater than 1, the dataRegionsOnly 
#           flag in the areaParms data.frame is set to TRUE enabling the feature.
#           micromapST will be enabled in the border group dataset.
#           The user still must enable the feature when calling micromapST,
#           by setting dataRegionsOnly=TRUE in the micromapST call.
#
#      3) While the Level 2 boundaries do not have to be based on the area
#           boundaries, this is helpful. For the Regional mapping feature to 
#           work, each region should be based on area boundaries and have 
#           a one region to many area relationship.
#
#      4) The drawing order is:
#           Normally all areas, Level 2, Regional, and outline boundaries 
#           be are drawn. 
#           In one special case (dataRegionsOnly=TRUE and a regional boundaries
#           for multiple regions are included in the border group) the drawing
#           of the area, Level 2, regional, and the map outline boundaries) 
#           the drawing of these boundries will be limited to only regions 
#           contain at least one area with data.  The Level 2 boundaries drawn
#           must also fit within the active regions.  The map outline is 
#           is not useful and is not drawn.
#           The general order used by micromapST to draw a map is:
#           a) area colors (???)
#	    b) area boundaries (???)
#           c) level 2 boundaries
#           d) regional boundaries 
#           e) map outline (if needed)
#        
#       
#   Functions not supported and may require manual adjustments:
#      1) Automatic Characterization of the boundary data to ensure 
#           all areas are visible as clear or colored in the 
#           drawn micro map.  The areas must be manually adjusted
#           prior to calling this function. A simple tool is provided
#           for shifting, scaling, and rotating areas, but may not
#           provide enough manipulation to ensure the visibility of the
#           area using a GIS system or tool.   The BuildBorderGroup function
#           does provide a checkpoint of the name table and shape file data
#           to allow manual manipulation prior to building the final 
#           border group.
#      
#  Change List:
#  2019/08/28 - Initial code and documentation development.
#  2019/09/10 - Merged code from earlier builds for Kansas, Utah, China, etc.
#  2019/09/11 - Added Headers call parameter to provide header definitions for the 
#               map and id titles.
#  2019/09/20 - Finished rough packaging and sent to testers (GMU)
#  2019/09/21 - Found problem with regional and level 2 boundaries - tracked down
#               to bad code in modifying the name table headers to allow more 
#               flexibility - fixed 9/22.
#  2019/09/22 - Added plots for each VisBorder created.  
#             - Added titles to plots for clear understanding of the plots.
#  2021/09/29 - Restructured call parameters to include full set of Shape file name and directory;
#               NameTable name and directory, Border group name and directory,
#               Name of data section column containing the link to the initial NameTable,
#               Headers for the Map and ID glyph columns,
#               Percentage of shape file reduction (ReducePC),
#               debug flag.
#  2021/09/29 - Define and implement the call parameter validations and defaults.
#  2021/09/29 - adapt the test NameTable reading and validation.
#	      - add support for .CS, .xls, .xlsx, and .RDA file types.
#             - layout the defaults for the NameTable columns and interaction
#               to insure all columns contains usable data.
#             - add ability to construct abbreviations from names by using the first 4 chars and 
#               adding a numeric to make it unique. 
#             - add logic to ensure the ID numerics always contain leading zero.
#  2021/10/07 - Reorganized code to:  
#                  a) Common global functions, 
#                  b) Main call, 
#                  c) define internal functions,
#                  d) Validate provided call parameter (at least 80%), 
#                  e) Read, validate and process Name Table (from .csv, .xls, .xlsx, or .RDA images, 
#                  f) Build full Name Table  for use with the shape file processing and to validate 
#                     contents of columns, Backfill column information as needed.
#                  g) write out intermediate NameTable as check point, 
#                  h) Read Shapefile or accept SPDF from caller and validate.
#                  i) Check holes and validate completeness of the SPDFs geometry.
#                  j) Validate shape file (data section) and "ShapeLinkName", does shape file 
#                     SP image match the Name Table? - number of entires, - Link Names, 
#                  k) SP union to join all polygons of the same area under one reference and 
#                     assign row.names to SPDF to match NameTable rows (abbr), L2, or region lists. 
#                  l) Handle longitudinal wrapping of the area and preform any shifting or 
#                     scaling. (+over)
#                  m) transform projection into the default projection centered on the area (AEA) or 
#                     to the users projection with units, (no longer in longlat projection) 
#                  n) Save a copy of the SPDF to build the reg, L2 and L3 Vis Borders via SP UNION
#
#                  o) Test the area sizes in the area SPDF to determine if any area is much too small 
#                     to use as a micro map and notify the caller, 
#                     areas sq. m / Total sq. m vs percentage limit.
#
#                  p) scan the completed list and place any areas with "holes" first in the 
#                     list (are drawn first), (used SP plotting order) 
#                  q) Create VisBorders DF for areaVisBorders - decode the SP polygons to make the 
#                     area point arrays, simplify by rounding the x and Y values to -2 decimal points, 
#                     walk through the polygon points to eliminate any duplicate points
#                     caused by the value rounding.
#                  r) plot each area and verify proper boundaries, matching with neighbors, 
#                     and holes are handled properly.
#
#                  s) Repeat steps p through r and create L2VisBorders
#                  t) Repeat steps p through r and create L3VisBorders
#                  u) Repeat steps p through r and create RegVisBorders
#
#                  v) build areaParms table
#                  w) write all five (5) data .frames as one data set.
#                     (areaParms, areaVisBorder, RegVisBorders, L2VisBorder, L3VisBorder, areaNamesAbbrsIds)
#
#  2021/10/21 - Added capability to pass function a SPDF instead of filename and directory.
#             - Added feature to visualize the micromap in true size (1.5 x 1.5 max)
#             - Write out Name Table as xlsx, csv, and ascii formated files for documentation.
#
#  2022/03/16 - Added plot statements to track the visual of the maps 
#               as it passes through the creation and simplification processes
#               in the function. These are used in the papers on the 
#               build border group process.
#             - Cleaned up all of the error message coding and presentations.
#             - Created work .r file of the call sequence and parameters to recreate each border group .rda
#             - Added Michigan and Minnesota to the border group family.
#             - Built Michigan, Minnesota, Louisana, Mexico, Texas  Name Tables or pull out of the old border groups 
#               their name tables.
#
#  2022/03/18 - Added rotation to the area modifier commands.
#  
#  2022/03/19-04/04 
#             - Updated code to handle the proj4string using the slot(xxx, "proj4string") 
#               form with two components in the result:  projargs and comments.
#	      - Rewrote the code to validate the Xoffset, Yoffset, Scale and Rotate 
#               modifiers to an area.
#             - Dropped/disabled the "proj" (projection modification) per area. Not practical.
#             - Reviewed the MapLabel column and found micromapST does not use the 
#               column - not implemented. Instead it uses a section of code that is controlled
#               by the areaUSData flag in areaParm data.frame to enable.
#             - The validation of the MapLabel Name Table column was re-written and 
#               the three values contained in a string separated by commas, is parsed,
#               the values validated, and placed in three new Name Table columns: MapL (Label),
#               MapX (X value of coordinates), MapY (Y value coordinates.)
#             - Processing of the MapLabel coordinates and all of the area modifiers was
#               changed to handle these values in the same "coordinates" system as the  
#               original shapefile - so that would be long/lat or a projections +units.
#               The modifier values are not needed after the build of the border group but are
#               keep as a record of what was done or if the name table is reused to 
#               build another border group.
#             - The MapLabel coordinates (MapX and MapY) are used to create a 
#               SpatialPoints structure. If and when the map is transformed, 
#               the MapLabel SPDF is also transformed to make the label coordinated match 
#               the map.  micromapST will have to be modified to use these columns
#               instead of the code to place the labels.
#  2022/04/05 - Modified code for clgeo_Clean to handle the Proj4 without error.
#             - Attempted to resolve error "logging.info" function not found. Not
#               sure it is resolved, watching.
#             - Had to reorder the code logic to make sure information was available
#               when needed.  ShapeFile Link data, Key linking between Shapefile
#               and the NameTable, and more.
#             - Replaced (debug && <value> with bitwAnd(debug,<value>) to get the 
#               desired anding function.
#             - Recoded the L2 and Reg column logic to handle missing columns.
#               Now if the xxxName column is present but no xxxID column,
#               the Name column is used to find the groupings and place a representative
#               number (ID) in the xxxID column.  If the xxxID column is present but 
#               no xxxName column, a name is build from the xxxID column and placed in 
#               the xxxName column.  If neither are present, a sequence number is 
#               placed in the xxxID column (1 to number of rows) and a name is created
#               as mentioned above for the xxxName column.  The feature is only 
#               operational if the user provides the ID or Name for the L2 or Reg 
#               groups.
#             - The logic on the projection processing was updated: 1) The projection 
#               on an individual area is disabled and the column removed; 
#               2) If the proj4 string is supported in the call, it must be 
#               A NON-long/lat projection and will be modified to ensure the 
#               +units parameter is +units=m (the default.); 3) If the shapefile
#               does not contain a proj4string or wkt, it will be set to a long/lag
#               projection; 4) If the shapefile contains a non-long/lat 
#               projection it will be inspected and its +units= parameter 
#               changed to meter. The actual map transformation is done at the end 
#               of the process before the micromapST border group is created. 
#               5) The proj4 calling parameter will override the shapefiles projection.
#               6) If after processing the transformations for the Shapefile and/or
#               the proj4 calling parameter, the SPDF is still a long/lat projection,
#               the build function will create a AEA projection based on the maps 
#               centroid with a lat_1 and lat_2 lines at 25% and 75%;  
#               7) The coordinates used by the MapLabel feature are used to 
#               create a SpatialPoint structure.  
#               This structure is then transformed the same as the SpatialPolygons.
#             - Test runs have been completed on: USStates, Alaska, California,
#               Kansas, Kentucky ADD, Louisiana, Maryland, Michigan, Minnesota,
#               New York, Utah, Africa, China, Europe, India, Mexico, and Souel Korea.
#             - Fixes were applied to problems dealing with handling bad data and
#               missing name table columns.
#             - Enchance code to permit better matching of links between the Shapefile
#               and the NameTable. Change each to uppercase, trim blanks, added 
#               leading "0", and made all of the strings the same length.  This
#               look awkward for character strings, but help match numerical 
#               values better.
#             ? Should look into problem with column names being with and without
#               leading cap. letters.
#             - Modified the call parameter defaults to use the NameTableDir
#               value if the ShapeDir and/or the BorderGroupDir call parameters are
#               not provided.  This allows all key files to be keep in one directory.
#  2022-04-20 - Reorder logic to allow all key data and files to be "checkpointed"
#               to a directory.  A checkPointRestart option is added to allow the 
#               user to manually modify the shapefile (enlarging and moving any 
#               areas for a better micromap and then restart the buildbordergroup
#               process.  In many cases this will be required to ensure 
#               every area can be seen clearly when shaded in the micromap 
#               graphic column.  When a call to buildbordergroup is completed, 
#               the function will have left the checkpoint files in the 
#               bordergroupdir folder in a subfolder called "CheckPoint".
#               The user can modify the name table or the shapefile in any way
#               However, you can't change the linkage names in the name table
#               or the shape file, and you must save the modified name table
#               and shape file under the same names in the checkpoint folder.
#               Suggestion is to save these files under different names before
#               modifying. 
#
#               If "checkPoint=TRUE" is included in the original 
#               BuildBorderGroup function call, the function skips all 
#               parameter and table validations and reads back in the 
#               checkpointed data and produces the micromapST boundary data 
#               and control files for the border group.
#               Only minor check of the shape file is done to ensure its 
#               validity using rmapshaper.
#  2022-05-01 - Cleaned up logic, moved East/West hemisphere code into
#               a function to be used before any example maps are draw to
#               properly show how the maps will really look when the 
#               projection is long/lat.  This had to be done, since many of the 
#               Spatial function in R objected to long/lat coordinates > 180 or 
#               < -180.  So, the adjustment is made before drawing only.
#               Just before the SpatialPolygons are converted into the 
#               micromapST VisBorder format, the E/W Hemisphere Correction
#               adjustment logic is called one last time to make any needed
#               adjustments.  After this point, no Spatial functions are used
#               to process the boundary data as it is converted into vertex 
#               data.frames for micromapST.  This was discovered as a problem
#               later and how to be re-implemented.
#             - It is possible the shift, scale and rotate modifications can 
#               cause overlaping areas in the map. To resolve this problems,
#               the identification of neighbors for now overlaping area
#               must be know. This must be done before the areas are modified
#               for the existing neighbor functions will not work.
#               The neighbor identification is done after the polygons
#               are unioned under the areas identification as one "polygon"
#               in the SpatialPolygons structure. Once the data Section is re-created,
#               the neighbor relationships can be identified and lists of the 
#               Keys of the neighbors for each area can be added to the 
#               name table row. By converting the neighor logical number to 
#               the areas key value, as the map processing continues, the 
#               function will be able to always identify the neighbors by their
#               key tags (most of the time the Abbr). 
#             - Add functions to draw the maps for the SPDF data format and 
#               the VisBorder data format of the boundaries.
#             - Added a new sample Plots function to draw the shape file (SPDF) 
#               boundaries in the same manner as the PlotVis and a similar size 
#               that would be used in micromapST and show how the color shaping 
#               work appear.
#             - Corrected the PlotVis and PlotSPDF map drawing functions to 
#               ensure the maps have the correct aspect regardless of the size of
#               the ploting space.
#             - Setup debug flag to output sample maps about 1.5 x 2" at each level
#               of modification.  (debug=16)
#  2022-05-06 - Found neighor relationship keep finding errors in 
#               the geometry after unionSpatialPolygons and adjustments 
#               for the E/W Hemisphere (EWH) Correction logic (LL). Was finally able to 
#               establish the neigbhor relationships between areas after 
#               the union, and data Section rebuilding, but not doing any E/W Hemisphere 
#               adjustments. While the EWH adjustments are 
#               required before any modifications are made to an area, it can't
#               be done earlier as part of the process stream.  Several Spatial 
#               function did not like the extended
#               long/lat points above 180 or below -180 degrees and throw
#               errors and warnings. Changed logic to do the EWH adjustments 
#               just for the sample maps drawn after each major step, but 
#               no adjustments were made to the shape file during the 
#               main line processing.  Tried to do a temporary AEA 
#               projection for the sample maps to handle the IDL, but
#               it did not present the map as the user expected (long/lat projection).
#               The final solution was to do the EWH adjustment when a 
#               sample map was drawn, but delay the adjustment to the real
#               map until the last moment.
#             - Effectively in March, 2022, it was announced that 
#               rgdal, rgeos, and maptools were scheduled to be retired 
#               with no support by the end of 2023 (18 month from now.)
#               Recommendations are to use the sf/terra/stars and other 
#               packages.  Have stated reviewing the packages to determine 
#               the steps needed to remove dependency for these packages.
#               micromapST should not be affected, only the BuildBorderGroup
#               function.
#
#  2022-05-10 - EPSG code for Lat/Long in US  NAD83 = EPSG:4269
#                                    ESRI:102003 AEA us continental
#  2022-05-13 - Place the suppressWarnings() function around any R statment
#               that generates a rgdal, sp, or proj6 warning in regard
#               the proj4string usage inside a used package that we don't
#               have access to the code.  It is hoped this cleans up
#               the messages generated to the user and removes the 
#               confusing warnings we and they cannot control.
#  2022-05-14 - Updated - code and saved.
#  2022-06-25 - Cleaned up error and output messages to be usable to users.
#             - Added call parameter to specify the size of the Labels when used.
#               This variable is also added to the areaParms data.frame
#             - Updated logic to enable Region feature in micromapST when regID column exists.
#  2022-06-30 - Add Global address space - can't use micromapSTs
#  2022-08-31 - Made adjustments in all plot calls for SPDF structures to add sp:: to the calls.
#             - When problems are reported inserted gBuffer with 0 width to do the clean up of the 
#               structure.  Continued to expand this.
#             - Found the mai, mar parameters were being reset after a graphic file was being opened.
#               This caused the figure to be to large in the image.  Changed the placement of these
#               par settings to after the opens.  Also increased the mar to allow two lines for the 
#               title area and increased the size of the plot by 0.4 inches to accomodate the titles.
#               This seems to have removed all of the plot error messages that could not be traced
#               to any other source.
#             - Changed the debug flag implementation to guide the creation of intermediate plots of the 
#               map for documentation and visualizing how the map may look in the linked micromap.
#               The debug field is tried as a series of bits.  Each bit is assigned to an operation or 
#               extra function in the process.
#               Bit  1 =   1 - Used for line by line debugging.
#               Bit  2 =   2 - Outputs information to trace the process of the function.
#               Bit  3 =   4 - Display Information related to projection processing and variable.
#               Bit  4 =   8 - Plot intermediate Shape file and SPDF (not the same as 256 or 512).
#               Bit  5 =  16 - Display processing and variable related to the SPDF
#               Bit  6 =  32 - Display processing and variable related to the Name Table.
#               Bit  7 =  64 - Display internal variable on processing
#               Bit  8 = 128 - 0 = set output file type for the 512 option to PDF (default)
#                              1 = set output file type for the 512 option to PNG
#               Bit  9 = 256 - Generate a multiple plot graphic of the map in small format 
#                              with each plot having only 5 areas shaped.  Number of images = Areas/5 + 1
#               Bit 10 = 512 - Generate a 4" x 4" plot of the area at key states in the processing:
#                               RAW, After rmapshaping, After Name Table modifications, and 
#                               after transformation and converstion to the micromapST VisBorder
#                               format.
#               Bit 11 =1024 - Same as 512, but only generates plots for the RAW and Final images.
#               Bit 12 =2048 - Display the final BorderGroup layers on the screen:  Areas, Level 2,
#                              Regions, Map Outline (level 3).  Each is display in a separate window.
#               Bit 13 =4096 - NA
#               Bit 14 =8192 - Write to disk PDF file of multiple plots of the map from the areaVisBorder
#                              boundaries - one map per 5 colors as done for the 256 option.
#               Bit 15 =16384 - Future
#               Bit 16 =32768 - Future
#             - Changed the minimum map height to .5
#             - Resolved issues with multipel globalVariables calls.
#  2022-09-05 - Removed documentation on MapHdr call parameter.  Set default of Map.Hdr2 to "Areas".
#               Variable will still be set in the areaParms table in the border group.
#               Will still allow the map headers to be specified in the micromapST call.
#             - Changed the maptail colors to a pale yellow/green for above and a pale yellow/red
#               for below colors. 
#             - Tested regionsB and onlyDataRegions opens for border groups with 
#               regional boundary sets (like the US and the UK & Ireland border groups.)
#             - Corrected the location the "line" and refTxt are printed below the glyph.
#             - Verify all location ids are mapped to upper case for comparison to 
#               cover a lot of issues with the users data files and privately 
#               build border groups.
#             - Updated the version string from micromapST.Version.
#             - Modified the onlyDataRegions logic to turn on the regionsB option 
#               to replace the L3 outline of the map when only part of the map is draw.
#  2022-09-08 - Updated logic to enable regional feature when regID and regName 
#               columns are present.
#             - Updated logic to enable the Map.L2Borders feature when a valid 
#               L2 set of columns are present.
#             - Disabled MapHdr and made the default c("","Areas")
#  2022-09-20 - Correct typos in check point file names (,rda instead of .rda).
#             - Remove $area and other processing and test fields in the name table.
#  2022-09-22 - Added call parameter to allow the specification of a unique 
#               directory to write any intermediate plots or text output for the user.  
#               Required by CRAN to support any output via examples in the 
#               BuildBorderGroup documention.
#  2022-09-24 - Fixed the ability to pass the function directly the SPDF 
#               structure via the ShapeFile call parameter.
#             - Fixed the call parameter checks for MapHdr, IDHdr, ReducePC, 
#               debug, ShapeLinkName and NameTableLink to handle the 
#               limitations of is.null() and is.na() checks that produce
#               difference sizes of results.  This is now caught 
#               by CRAN pre-scans.
#  2022-09-27 - Released Version 2.0.1 to CRAN.
#  2022-10-03 - Based on discussion the effectiveness of the rmapshaper 
#               on the map at different ReducePC values and to make sure 
#               there are no coding issues, metrics have been added 
#               to document the SpatialPolygonsDataFrame outputed by 
#               rmapshaper.
#  2022-10-08 - V2.0.2 - Added key routines form MapColoring un-released 
#               package to replace the nacol function.  The nacol function 
#               used sample.int() in the allocation of color indexes.  
#               This created a situation where each run of 
#               BuildBorderGroup would create sample plots that used 
#               the colors differently.  Between two runs, the user 
#               could not compare the resulting maps.  The dsatur, 
#               getAM, getNeighbors, and getAmountColor function 
#               provide a very stable map coloring results.
#  2022-10-09? - V2.0.2 other changes for V2.0.2
#  2022-10-19 - Start conversion from sp/rgdal/rgeos to sf functions.
#  2022-10-25 - Continue conversion Work.
#  2022-11-06 - Continue conversion work.
#  2022-11-11 - Put in st_shift_longitude ONCE at the beginning and 
#               see if any function complains.  If no problems, do 
#               shift at begining and forget about it.
#  2022-11-23 - Continue conversion work to be completely sf for 
#               V2.2.0 release. It turned out to be about a 80% 
#               re-write in the spatial code sections.
#  2022-12-01 - Continue conversion work to sf for V2.2.0.
#               Decided this is such a major change in the code to 
#               use V3.0.0 instead of V2.2.0 replaced the entire 
#               spatial structure to VisBorders format logic.  
#               Removed all old code.
#             - The BuildBorderGroup function has been enhanced 
#               to accept an SPDF or sf data structure as the 
#               "ShapeFile" call parameter value. In this case, the 
#               ShapeFileDir is not required or checked.  If a SPDF
#               structure is provided, it is converted into a sf 
#               structure.  The structure must be a SPDF or sf
#               structure containing the data header to be able 
#               to link the contained polygons to the area entries 
#               in the name table.
#             - The BuildBorderGroup function can now accept a name 
#               table data.frame as the value for the NameTableFile 
#               call parameter. It must meet the requirements defined
#               for the 'csv' or 'spreadsheet' version of the name 
#               table.  In this case, the NameTableDir call parameter 
#               is not required and will be ignored and not checked.
#             - If the directories for the name table and shape file
#               are not used as outlined above, the name table 
#               directory can still be provided to use for the 
#               BorderGroupDir, or the BorderGroupDir MUST be 
#               provided.
#  2022-12-04 - Created the first packaging of V3.0.0 and was able 
#               to complete build, check and INSTALL.
#             - The micromapST examples completed with any failures, 
#               and produced clean linked micromap.
#             - Found the proj4string and projection logic were 
#               not completely right.  The first issue was "cat" 
#               display commands will not handle the list structure
#               of the new projection strings with $input and $wkt.  
#               Reviewed and corrected all code related to projection 
#               values.
#  2022-12-06 - Fix logic with projections including 'proj4'.
#  2022-12-10 - Add logic to disable sf_use_s2 for major modifications to the sf structure.
#               The sf structures are not always valid and must be checked and made valid.
#               Regretfully, st_is_valid does not uncover all of the issues or provide a cause.
#               Therefore, st_make_valid must be done between every major change done
#               done to the sf structure.  The st_make_valid function is not always successful
#               when executed with s2 enabled.  The recommended solution on the web is to 
#               do most of the repairs and major modifications with s2 disable.
#               Code has not been added to disable s2 usage at the begin
#               and to enable it at the end of the function.
#             - Note that the variable xxx$yyy and xxx['yyy'] do not return the same 
#               result.  If xxx is a list, then xxx$yyy returns a value and xxx['yyy'] returns 
#               a list.  Need to check the value returned for xxx[['yyy']]???
#             - One can obtain the proj.4 string for the wkt variable by requesting 
#               the $proj4string of a crs variable.  This causes the sf package to examine
#               the wkt and return the proj.4 string equivalent.   Very helpful.
#             - Still debugging how to clean up shape file structure that are not 100% 
#               valid. (see above.)
#  2022-12-12 - The st_shift_longitude is a very helpful function for 
#               handling Alaska and the US.  However, it does very 
#               poorly when trying to handle areas that statle the 
#               0 degree longitude.  It causes the reverse problem 
#               and created boundary line across the globe.  Logic 
#               has been added to determine if the map is spanning 
#               from -deg to +deg (us to ak.) or +deg to -deg 
#               across the greenwich meridian.  (delta > 180, - 
#               to +; < 180 + to -)
#             - It appears the st_shift_longitude function should 
#               not be used when it can cause geographs spanning 
#               the Prime Meridan are involved.  The result is the 
#               -degrees become + degrees and the map boundaries 
#               are spread across the entire map area from 0 to 360.
#               New logic is being added to 1) get the centroid 
#               of the map, 2) calculate the 180 degree point 
#               opposite to the centroid X value, 3) if the 
#               centroid X value is -, then we look for the range 
#               to be +deg to 180, 4) if the centroid X valie is +, 
#               then we look for the rnage to be -deg to -180, 
#               finally 5) determine the range from the -+180 meridian
#               to the opposite centroid X value.   If the st_bbox 
#               has an X min or max that falls within this range, 
#               then the shift function will be required. Since sf 
#               does not mind having the longitude shifted, we are 
#               trying to keep it enforced through out the processing.
#               However, some of the sf functions revert the 
#               longitude back to the -180 to +180 range.  Need the 
#               shift for any printing (keeps the map together) and 
#               when the name table modifications are made.
#  2022-12-23 - Fixed issue with polygons being reduced to a point or line.  This causes
#               the polygon function to not advance to the next col= element.
#             - Found the polygon function does not skip to the next color if the 
#               polygon is a point or line. They are not considered polygons, so the 
#               color is not steped forward.  The way we use the polygon function this means
#               the color steps on to the next polygon in the list, which may not be the one
#               we want to foreground color.  We paint all polygons everytime we draw a map.
#               The problem comes from the rounding done in the BuildVisBorder function.
#               A polygon with as many as 30+ points can be rounded to a single point depending
#               on the size of the polygon and the overall increments between points.  The 
#               old method of doing it to 500 meters no longer works. We considered justing
#               letting small shape disable, but on a small map, the rounding must at least be 
#               proportion to the map's size. Also, all polygons are important to the map.
#               While the black boundary like may wipe out these small polygons.  We must 
#               at a minimum find a method that makes sure polygons are never reduced to a polygon
#               with less than 4 points. No points (1 point), 2 points would be a degenerated point, 
#               No lines (3 points, start, middle, end), or a triangle (4 points, start, middle, 
#               middle, end).  Any smoothing(rounding) has to be the same for all points on the 
#               map.  Otherwise, neighboring polygons will not matchup on their shared 
#               boundaries.  The choose of whether to do it and what parameters to use, must 
#               be done at the start of the VisBorder build process and used from that point on.
#               The builds of the area, L2, Reg, L3 must use the same parameters.
#  2022-12-24 - Add redirect of the summary border group name table information to a text file
#               in the same folder as the border group output .rda file under the name of 
#               "<BGName>_Rep.txt".
#  2022-12-31 - Removed all debug printouts that were messing up the output for the user.
#  2023-01-02 - Add ability to capture the drivers used to read the initial shape file.  This could 
#               not be done when the shape file is passed as a binary structure to the function.
#               This information is used when the check point shape file is written to make sure
#               the check point file is the same format as the original shape file.
#             - Added option to specify the which format will be used to write the check 
#               point shape file.  It can be used to overwrite the known driver when 
#               the shapefile is read or to specify the format when the shape file is passed
#               to the function as a binary structure.
#             - The sf st_read function will not be limited to allow it to read 
#               any of the known boundary file formats.
#  2023-01-07 - add xLim and yLim (bbox) values against the native units and the transitioned
#               to help with parameter checking.  The tranformation units is saved to the 
#               areaParms data.frame. 
#  2023-01-08 - Removed the ShapeDriver call parameter.  It turns out the filename
#               and directory options required to make this work are too complicated.
#               Returning to allowing the Shapefile read to be anything they can 
#               get pass the st_read function with DSN = directory and LAYER = filename
#               or read the shape file in the users R script and pass the sf structure
#               to BuildBorderGroup via the ShapeFile call parameter.
#               As for the check point Shapefile, it will be written as ESRI Shapefile
#               only.  This will still allow the user to modify the boundaries as needed.
#  2023-01-19 - Updated BuildBorderGroup message documentation to include changes
#               and new messages.
#  2023-03-12 - Corrected missing MapAvgH variable used after doing a CheckPoint Restart.  Was 
#               not calculated from the MapMinH and MapMaxH.
#             - Changed the logic to create the BorderGroupDir path if it did not exist.
#  2023-04-20 - Completed initial conversion of BuildBorderGroup from the sp, spdep, maptools,
#               rgdal, rgeos packages that will be retiring the end of 2023 to the supported
#               spatial packages of sf and sfdep.  
#               Detailed testing has started to release this summer, 2023.
#             - rmapshaper package has changed how the calling parameters are handled and passed 
#               to other internal functions.  BuildBorderGroup function had to change the calling
#               order of the parameters to make the force_FC and sys parameter last in the calling 
#               list.
#  2023-07-15 - Discoveries related to use sf functions in this function:
#               1) Several function do not maintain the longitude shift applied to handle
#               the anti-meridian long/lat longitudes that cross the -+180 longitude line.
#               st_centroid, st_make_valid, and st_transform (as expected).
#               Most of the other function that don't seem to call st_make_valid internally
#               do not un-shift a shifted LongLat projection.  Programmer must watch out
#               for this condition.
#               2) Several functions generate a warning if the data section of the sf 
#               structure is present.  It's best to pass them an sfc structure so no 
#               messages are generated.
#               3) The longlat projection is not a true "PROJECTION".  Several functions
#               trigger a warning if the CRS is not a NA or LongLat type specification.  
#               st_shift_longitude (most be LongLat), 
#		4) The rmapshaper package and ms_simplified function has changed the way
#               the call parameters are handled.  Several are now passed through the "..."
#               mechanism. Initially the force_FC and sys parameters had to move to the 
#               end of the call list.  Now the force_FC parameter is not valid.  Since
#               both were just respecifications of the default values, they have
#               both been removed from the packages call to ms_simplify.
#               5) To guard against damaged geometric structures caused by several 
#               heavy processing functions, like ms_simplify, Neighbor finding, 
#               aggregate, desatr, and others, it is necessary to execute the "st_make_valid" 
#               function after each call to keep the structure health and avoid
#               errors and crashes in other R package functions.
#               6) Since there are two types of coordinates representation: rectangluar
#               and spherical, we attempted to initially run completely in spherical mode
#               allowing sf to use the s2 package routines.  However, it is critical that 
#               the package be able to handle all types of shapefiles we found in spherical
#               mode not all of the geometries were considered valid and some could
#               not be fixed using "st_make_valid" and the older package we used "CleanGeo"
#		was going to not depend on the retiring packages, but was going to remain 
#               focused on sp structures, we had to find a solution.  This may change
#               change later, but for now, we have disabled the use of the s2 routes and 
#               are focusing on servicing shapefiles build on the rectangular coordinates.
#               This seems to handle the spherical coordinates well enough for our 
#               purposes.  We do restore the default mode of "sf_set_s2=TRUE" at the end
#               of the program. 
#
#               Our goal remains to provide a comprehensive link-micromap package that 
#               allows users with a minimum of R scripting experience to quickly get 
#               charts created, and data analysis in as many different situations 
#               as possible, with quick execution to support multiple re-tries 
#               during their exploration of the data.  The package also produces 
#               very reasonable quality graphics that can be used for presentations and 
#               publications with little effort.
#
#  2023-07-16 - During the review of the package to ensure all functions are properly 
#               implemented using sf and sfdep, it was discovered the geometric average
#               centroid algorithm did not accurately identify an map's centroid. The
#               script has been updated to include an accurate method using st_union and 
#               st_centroid function.  
#             - rmapshaper has changed the processing of the call parameters again.  force_FC 
#               is now not a valid call parameter.  Since the call included them and set 
#               the parameters to their default values, both parameters have been removed. 
#               This is in hope the default values and features provided by the ms_simplify 
#               function don't change in the future.
#  2023-07-27 - Add call parameter to instruct function to combine ShapeDir and ShapeFile into
#               the DSN string and not use the Layer option on the st_read function call.  This
#               feature is required to be able to read non-ESRI shapefiles and maybe several other
#               formats.
#             - Logic has been added to attempt to st_read the shapefile using two setups:  dsn as 
#               the full filename and dsn= as the directory and layer= as the filebase.  Esri drivers
#               use the dsn and layer while other drivers use the dsn only.  If the Esri approach failes
#               then the single filename as the dsn= will be tried.  If both fail, an error will be 
#               sent to the user.
#  2023-07-30 - Found incorrect code in the validity check for the NameTableFile parameter. Replaces
#               length function with nchar to find out how many characters where in a filename.
#             - Most of the simple validation routines did not catch parameters set to "".  
#               The code was updated to str_trim the parameter, and check for NA, not a character variable,
#               and having a value of "" or " ".  This closed a lot of invalid paramters.
#  2023-08-08 - It appears the sfdep package does not add any value to micromapST that spdep can 
#               provide.  Swapped out the sfdep::st_continguity function call for the spdep::poly2nb
#               function call.
#  2023-11-09 - Corrected logic in checking the ShapeFile to NameTable links and if numeric properly
#               replacing them with the default name of "NAME".
#             - Corrected the MapHdr, ReducePC, IDHdr, MapMinH, MapMaxH call parameter checks
#               to check for "missing" as well as null and na.  For na, any() added as insurance
#               incase a vector or list back it through the validation checks.
#  2023-11-28 - Masked additional messages and warnings from st_intersects and st_intersection
#               functions from the users. 
#  2023-11-28 - Attempted to filter all incoming "Name", "Abbr", "ID", "Alt-Abbr", "Alias",
#               information in the Name Table with the same information in the Shape file
#               and name table.  If all are filtered the same (Name Table at time of being 
#               built and synced with the polygons data) and the user's location ID 
#               information data at the time of importing it for micromapST use, then the
#               it will be the best environment to ensure location ids will match almost
#               99.999% of the time.  Process: Upper Case, no internal punctuation (.-=/ etc.),
#               no internal blanks (only one), and trim external blanks.
#  2024-01-06 - Suppressed warning and messages around rmapshaper, aggregation, and file.remove.
#               Merge new section from 3.0.3 back into 3.0.2 as the base.
#               Rewrote the documentation section on the BuildBorderGroup debug call parameter.
#  2024-01-07 - Several error messages and warning started appearing in the basic tests related
#               to the validation of the MapL column; not a character type column and the length 
#               of the strings were over 3 characters when they were actually 2.  It was found
#               that read.csv and possibily other reading function, will treat any quote in the 
#               original data as a quote and not remove it.  Thus a "AK" field in the .csv file
#               will be placed in the MapL field as "AK" with a length of 4 characters.  The quotes
#               are considered part of the value.  The use of noquote complicates the process 
#               by changing the class of the MapL vector, so it is no longer a character vector.
#               It also does not actually remove the quotes, they re-appear when the string is 
#               converted back into a character variable. The problem is resolve by manually 
#               removing the quotes in the MapL data.
#  2024-01-09 - Correcting column validation when characters and vectors are involved.  Lists are
#               vectors!  If any structure is treated by str_trim, it becomes a character vector. OUCH!
#               Package will work, but may need a lot of adjustments later.
#  2024-01-09 - Several user suggested using the following commands to reliably 
#               get a single boundary around a map:  st_union or aggregate, st_boundary,
#               and st_     . The st_union returns a single area, so it's not
#               useful.  The st_combine also returns a single area.  That leaves
#               aggregate for building Level 2 and Regional boundary sets.  It was
#               hoped st_union could do the Level 3 merger of all areas into one,
#               but caused problems and could not handle interior slivers or gaps
#               between areas.  st_buffer helped for a while, but would not scale
#               and did modify the L3 shape.
#
# 
#  Operation Sequence:
#
#       1) Load support libraries
#               sf, sfdep, stringer, tools, utils, graphics,
#               readxl, and writexl 
#	2) Verify all call parameters (pathes and ranges)
#       3) If checkPointReStart = TRUE is specified on the function call, 
#          validate only the critial call parameters to allow the function 
#          to restart the processing using the checkpointed data files and 
#          a possibly modified shapefile and jump to step 19 below.
#
#       4) Read (if needed) shape file and convert into sf spatial structure.
#
#	5) Read (if needed) the initial name table and validate 
#          the presents of the required columns and their content.
#       6) Convert old MapLabel columns into MapL, MapX, and MapY columns. (retired)
#       7) Validate the MapX, MapY, and MapL name table columns values.
#
#       8) Validate and clean boundary spatial data for all areas with sf tools 
#       9) Collect the projection information and determine what transformations
#          may be needed before the micromapST border group is created.
#          (Provide LongLat CRS when none is present, ensure the end projection
#          will be in meters, and if no end projection is provided, create
#          a AEA for the map to provide the best presentation.
#       10) Used the sf tool "st_shift_longitude" to handle the +-180 degree 
#          longitude issue.  However, several sf "st_" function revert the 
#          coordinates back to standard lon/lat during processing.  
#       11) Process LINK fields in the Name Table and Shape file build a 
#          linkage between the two and create and assign a single "Key" per 
#          per area to be used in the micromapST boundaries data.frames.
#       12) Simplify spatial data with rmapshaper. Reduce the geometries keeping between
#          2 to 0.05 percent of the original geometry.
#       13) Aggregated the polygons for same basic area under one entry per 
#          area and combine data information. (sf multipolygon)
#          After this aggregation (union), the structure contains only one 
#          entry or row per basic area (name table row).
#       14) Perform any requested name table modification:  shift, scale, rotate.  
#          With sf, this is now a single algebric formula using matrix 
#          math.  Gone are the 2 pages of code.
#       15) Perform transformation on map and MapX and MapY points.
#          Used the data in the name table, created a set of points, 
#          then transformed.
#       16) Build areaParm parameter data.frame
#       17) Clean up Name Table - remove work columns
#       18) Checkpointed the Name Table and Shapefile in ESRI Shapefile format
#           to disk for possible manual processing (enlarging of unseeable areas).
#
#       19) Reload for checkpoint files and continue processing.  
#           (Or restart process on request for checkPointReStart).
#       20) Plot to validation boundary data images if requested (debug=1024).
#	21) Using the area sf structure, make copies and aggregate to form
#           the Level 2, Regional and L3 boundaries sf structure.
#       22) Convert each of the 4 boundary sf structures into VisBorders format 
#           data.frames for micromapST.  In the process round the vector values
#           based on the size of the transformed map and remove any duplicate 
#           vector now created.
#       23) Bundle the 4 VisBorders data.frames (area, L2, Regional, L3), 
#           the name table, and areaParms data.frame into the single border 
#           group dataset and write to a compressed .rda file.
#
#  This routine accepts standard ESRI shape file format boundary data,
#  a user provided name table and creates a Boundary Group in the format
#  required by the micromapST package.  micromapST uses the R "polygon" 
#  drawing function and needs the data to be in a simple data.frame of 
#  the X, Y points, Key for the area, indicator is the polygon is a hole,
#  and NA,NA values for the final X, Y point in a polygon.
#
#  Author:  James Pearson, StatNet Consulting, Gaithersburg, MD 20877
#  Updated  January 3, 2024
#
#  Version:  1.1.0 (with package V2.0.2) updated nacol
#
#  Version:  2.0.X (replaced nacol with DSatur and minor fixes.) 
#
#  Version:  2.9.0 (completed the conversion of BuildBorderGroup function from 
#                  sp, spdep, maptools, rgdal, rgeos dependencies to sf and sfdep.)
#            Removed references to any functions in sp, spdef, maptools, rgdal, and 
#            rgeos packages.  These packages are being retired in October, 2023 and 
#            the sf and sfdep package will be used to handle spatial objects in the 
#            sf, sfc, and sfg structure formats.  This change is required to remain
#            on CRAN for distribution.
#
#  Version:  3.0.0 Tested release of 2.9.0
#
#  Version:  3.0.1 Correction of some call parameter checks. 
#
#  Version:  3.0.2 Correct as.Date call; correct logic to properly 
#                  fill-in map colors when a single median area is present and 
#                  not mapped;  tighten up several call parameter checks to make
#                  sure internal R error don't occur and confuse the user.
#
#  Version   3.0.2 updated - corrected BuildBorderGroup processing of sf structures to get 
#                  VisBorder Files.  Changed from st_union to aggregate and added st_boundary
#                  to processing for L3 boundaries.  
#                  Attempt to fix bad geometries that leave slivers in 
#                  geometry - no successful.  Must be careful when making manual adjustments.
#                  Added the origin= option to the as.Date function call to support
#                  old R releases.
#                  Removed test output plots when modifying an area.  USstBG had 4 extra 
#                  plots.
#                  Updated documentation to only include one description of the border
#                  group data.frames (data sets) in bordGrp chapter.
#                  
#                  
#
#
#  functions used from sf:         sf, st_as_sf, st_as_sfc, st_geometry, st_drop_geometry,
#                                  st_union, aggregate, st_make_valid, st_is_valid, st_buffer,
#                                  st_transform, st_shift_longitude, st_read, st_write
#                                  st_crs, st_coordinates, 
#
#  functions used from stringr:    str_trim, str_split, str_replace, str_replace_all,
#                                  str_sub, str_squish, str_pad, 
#
#  functions used from spdep:      poly2nb
#
#  functions used from graphics:   plot line, arrows, polygon, axis, text, boxplot, mtext,
#                                  points, plot.new, strheight, strwidth, par
#
#
#####

#####
#
#  Initialize the definition of Global variable outside of the BuildBorderGroup function name space.
#
   utils::globalVariables(c(
	"warnCnt",         "stopCnt",        "errCnt",          "callVL",        "callVarNames",
	"ShapeFile",       "ShapeFileDir",   "ShapeFilePath",   "ShapeLinkName",
	"NameTableFile",   "NameTableDir",   "NameTablePath",   "NameExt",       "NameTableLink",
	"BorderGroupName", "BorderGroupDir", "BorderGroupPath", 
	"debug",           "proj4",          "ReducePC",       
	"MapHdr",          "MapMinH",        "MapMaxH",         "IDHdr",        
	"LabelCex",        "convertPROJ4",
	"VisForm",         "VisForm2",       "RScale",   
	"WorkSf01",        "WorkSf01Proj4",
	"SfPCkpt",         'SfCkpt'          
      ),add=TRUE)

#
#  End of Global Functions
#
#####
#######
#########


#########
#######
#####
#
#  Main Code Call - Version 3.0.2 - 2024-01-03
#

BuildBorderGroup <- function(
			   # Base filename of Shape file without extensions or SPDF or sf data structure.
			ShapeFile       = NULL,		
			   # Directory containing Shape file 
                        ShapeFileDir    = NULL,	
                           # Variable in spatial data containing link to NameTable
                        ShapeLinkName   = NULL,	
                           # The filename with extension of the NameTable or name table data.frame 
                        NameTableFile   = NULL,	
                           # Directory containing the NameTable (.CVS, .xlsx, .xls, or .RDA)
                        NameTableDir    = NULL,	
                           # The column in the NameTable to use to link the sf polygons to the Name Table.
                        NameTableLink   = NULL,   
                           # Name of the Border Group (BordGrp)
			BorderGroupName = NULL, 	
			   # Directory to write the Border Group data set into. (optional, default=NameTableDir)
			BorderGroupDir  = NULL,	
			   # One or two header lines for the Map Glyph column 
			   #       (Max 16 characters each) (optional)
			MapHdr          = NULL,		
			   # Minimum Height for micromap drawing (inches) (optional)
			MapMinH         = NULL,    
			   # Maximum Height for micromap drawing (inches) (optional)
			MapMaxH         = NULL,      
			   # One of two header lines for the ID Glyph column (optional)
			   #      (Max 12 characters each)
			IDHdr           = NULL,	 
			   # cex value for the Map Labels (optional)
			LabelCex        = NULL,           
			   # The percentage of vertex to be kept by rmapshaper (optional)
			ReducePC        = 1.25,       #  in % to be kept
			   # Callers requested micromap image final projection (optional) 
			   #     (projection string with $input and $wkt not sp::CRS)
			proj4           = NULL,		
	                   # Will only do a calculated AEA transformation if the 
	                   # shape file projection is not a long/lat projection and 
	                   # and proj4 is provided and not a long/lat projection.
	                   #
	                   # default = FALSE, True for restart.
			checkPointReStart = NULL,       #  FALSE - normal run, TRUE - start at check point
			   # Debug flag (control bits) = 0 to 65535.
			debug           = 0		
                   ) 
   {
   
   #
   #  Future Enhancements:
   #   a) Allow the shape file to be read without a name table being 
   #      specified.  If the caller can specify at least one variable
   #      or more in the shape file (or spatial structure) to be used
   #      as the \"Name\", \"Abbr\" and-or \"ID\" columns that would have 
   #      been in the name table.   It should 
   #      be possible to pull One or more name tables columns from the 
   #      shape file header data.  Syntax may be like:  
   #      SFNameTable=data.frame(Name=\"<var1>\",Abbr=\"<var2>\", ID=\"<var3>\")
   #      The structure becomes a pseudo name table and is filled with 
   #      the variables data from the shape file.  If one column does 
   #      not have data in the shape file header, then the \"<varx>\" 
   #      will be NA.  The name table will be initially filled in with 
   #      all rows from the shape file and finally duplicates will be 
   #      removed after the unionSpatialPolygons function pulls all 
   #      polygons for one area under one \"Polygons\" element. The 
   #      NameTableFile, NameTableDir and NameTableLink parameters will 
   #      not be needed.  The ShapeFileDir will become the default 
   #      directory for the BorderGroupDir.  If not provided, then the 
   #      BorderGroupDir MUST be provided.
   #
   #   b) The BuildBorderGroup function can aggregate areas for the user.  
   #      If they provide a \"AreaGroup=\" call parameter that is a vector 
   #      equal in length to the sf structure being processed and uses values 
   #      from the \"Abbr\" name table column to indicate what polygons 
   #      (and areas) belong to the new aggregated area that will match 
   #      the name table \"Abbr\" field, then the BuildBorderGroup function 
   #      will use the \"AreaGroup=\" vector when the unionSpatialPolygons 
   #      function is performed.  In this case, the ShapeLinkName and 
   #      NameTableLink call parameters are not used.  The length of 
   #      the vector must be the same as the length of the SPDF. 
   #      The values in the AreaGroup vector will match the values 
   #      in the \"Abbr\" column in the name table provided.
   #      This would support a situation like the Kentucky ADD aggregation 
   #      without extra coding or steps.  Plots of the map before (raw) 
   #      and after the aggregation will be available via debug 
   #      flag to allow the user to check the aggregation operation.
   #      For each area need:  1) list of components, 2) name of super-area.
   #
   #   c) The name table information is currently outputted to the console 
   #      at the end of the function process will have an option to 
   #      direct the report to a disk file instead of the console.  
   #      The option may look like: Report=<filename>.  The report would 
   #      be written to the BorderGroupDir as are the other intermediate files.
   #
   #   d) The intermediate files will not by default be written to the 
   #      BorderGroupDir as they are today. They can be requested by 
   #      use of the \"WriteWorkFiles=TRUE/FALSE\" or something similar.  
   #      This will cut down on the amount of disk clutter generated 
   #      by the function.  Check point files will still be written.
   #      Note: The builder can retreive all of these files from the 
   #      bordergroup dataset.
   #
   #   e) The value of the function on return will be set to the full path 
   #      and filename of the location storing the bordGrp dataset.   
   #      e.g., "c:/projects/micromapST/bordgrps/ArizonaBG.rda"    
   #      Using this string, the user could easily read in the border 
   #      group and inspect or use the 6 data.frame stored in the 
   #      border group.
   #
   #   f) The output messages will be cleaned to do a simple report of 
   #      the run parameters in a clean manner instead of what seems 
   #      like random run of messages.  This would be followed by a 
   #      simplified work flow and ending summary.
   #      Warnings and Errors will continue to reported when noticed.
   #
   #   g) The package will be updated to include one example of 
   #      BuildBorderGroup function execution.
   #
   #   h) Work will continue to complete the guidance in the error/warning 
   #      messages in the documentation.  Messages will also be reviewed 
   #      to improve the communications.
   #
   #####
   #
   # Call Parameters Definitions:
   #
   #   ShapeFile	- a character string (1 element in vector) of the name of the 
   #                      Shape file or a binary R SPDF or sf spatial structure.  
   #                      If the value is the name of the shape file filename, the file
   #                      must exist in the ShapeFileDir. The shape file is read by the 
   #                      the function into an sf structure.  The shapefile name should not 
   #                      have any extensions included.  If ".shp", ".shx", ".dpf", and-or 
   #                      ".prj" extensions are present the extension will be removed for the 
   #			  st_read layer parameter. Generally, no file extension should
   #			  be included.
   #                      The ShapeFileDir parameter is combined with the ShapeFile 
   #                      parameter and an extension of ".shp" to test if the shape 
   #                      file exists. The value cannot be NULL or NA_character_.
   #
   #                      If the value is a SPDF (SpatialPolygonsDataFrame) or sf 
   #                      (simple features) spatial data structure of the boundary data, 
   #                      then it is process as if it was just read in by the function.   
   #                      If it is an SPDF structure it will be converted to an sf 
   #                      structure.  SP, sfc and sfg structures are not supported
   #                      since the shape file must be linked to the name table 
   #                      through its data variables.
   #                      
   #                      This parameter is required and has no default values.
   #
   #   ShapeFileDir     - a character string (1 element in vector) - is the 
   #                      directory name containing the shape file.  It should 
   #                      not end in a slash, but if one if present it will be 
   #                      removed.  The directory is used as the dsn= value in 
   #                      the st_read call to read the shape file.  Only the 
   #                      first element is used. The default value for this 
   #                      parameter is the name table directory and then the 
   #                      current working directory as a last resort.
   #                      The existence of the directory is checked.
   #
   #                      The ShapeFileDir parameter is only used if the 
   #                      ShapeFile parameter is used to pass the function 
   #                      the name of a shapefile.  Otherwise, it is ignored 
   #                      and not checked.
   # 
   #                      If this parameter is required and has a value of NA, 
   #                      then the current working directory will be used.
   #
   #                      If this parameter is omitted (NULL), then the value 
   #                      provided in the Name Table Directory will be used.
   #
   #   ShapeLinkName    - a character string (1 element in vector) - is the 
   #                      name of the column (or variable) in the shape files 
   #                      (SPDF or sf) structure's data section that will be 
   #                      used as a link of all of the polygons and multipolygons
   #                      in the structure to the area's name table.  The values 
   #                      are matched to the name table's \variable{Link} column 
   #                      values.  There may be multiple polygons/multipolygons 
   #                      in the shape file for an area. The number of unique 
   #                      values (links) in the sf must match the number of 
   #                      entries in the Name Table.  Only the first element is used.  
   #                      After preprocessing the shapefile link values are stored 
   #                      in the spatial structure in the \variable{X__Link} 
   #                      variable or column. The default ShapeLinkName value 
   #                      is 'NAME'.
   #
   #   NameTableDir     - a character string (1 element in vector) - is the 
   #                      directory name containing the callers provided 
   #                      NameTable .CVS, .xlsx, .xls, or .RDA file.  
   #                      This file contains the NameTable to be used with 
   #                      the shape file. If it is not completely filled out, 
   #                      the function will complete the table and write an 
   #                      intermediate .RDA copy for later re-use or for review.  
   #                      The existence of the directory is checked.
   #
   #   NameTableFile    - a character string (1 element in vector) - is the name 
   #                      of the initial or working image of the NameTable 
   #                      provided by the caller to provide different types 
   #                      of labels for each area in the micro map boundary data. 
   #                      The values of NULL and NA_character_ can not be used.  
   #                      The NameTableName is combined with the NameTableDir 
   #                      to create the complete path to the file. The path 
   #                      is tested to verify it exists before attempting to 
   #                      open the file.
   #
   #                      The data provide by the micromapST caller can 
   #                      use a Name, Abbr, ID, Alt_Abbr or Alias identifier 
   #                      to match the data with the name table row of a 
   #                      specific area. (See the Name Table Requirements for 
   #                      more information.)
   #                       
   #                      As of v3.0.0, the name table data.frame can be passed 
   #                      directly to the function using the NameTableFile 
   #                      call parameter by NameTableFile=NTable  
   #                      where NTable is a data.frame of a valid name table.
   #
   #   NameTableLink    - a character string (1 element in a vector) - This 
   #                      string specifies which column in the name table will 
   #                      be used to link the SPDF information with the 
   #                      NameTable.  The default is \variable{Link}, but 
   #                      it could be the Name, Abbr, or ID fields.
   #
   #   BorderGroupName  - a character string (1 element in a vector) - This 
   #                      string provides the name of the border group.  
   #                      It is used to label the border group inside 
   #                      its data set and as the name of the final border 
   #                      group data set file. It is recommended the name be 
   #                      kept short and end with 'BG' to help provide quick 
   #                      identification of the border group file.  Only the 
   #                      first value is used. This value has no defaults, 
   #                      NULL and NA_character_ cannot be used.
   #
   #   BorderGroupDir   - a character string (1 element in a vector) - this 
   #                      string identifies the directory where the final 
   #                      version of the border group will be written.  
   #                      Any intermediate data sets will also be written 
   #                      to this directory.  Only the first value is used.
   #                      The default value is the current working directory 
   #                      if value is NA. If value is missing, the Name 
   #                      Table Directory value is used.
   #
   #   MapHdr           - is a character vector with 1 or two elements - c(a,b) - the 
   #                      character vector provides the header labels for the Map glyph 
   #                      column containing the micro map drawings for the areas.
   #                      It is recommended the headers be no longer than 16 characters 
   #                      or the the usual width of the micro map. If the strings are 
   #                      too long, they will be truncated.  The border group builder 
   #                      can specify 1 or two header labels for the map glyph.  
   #                      For Example:  MapHdr=c('Header1','Header2')
   #                      If no values are provided for this parameter the c('Areas') 
   #                      will be used.
   #
   #   MapMinH          - is a numeric value used to specify the minimum amount of 
   #                      space to allocate in the output graphic for the micromap 
   #                      drawing in the row group.  The default is 0.5 inches.
   #
   #   MapMaxH          - is a numeric value used to specify the maximum amount of 
   #                      space to allocate in the output graphic for the linked 
   #                      micromap drawing in the row group.  The default is 1.75 inches.
   #                      Note: The actual value used for the height of the 
   #                      micromap will vary between the specified minimum and 
   #                      maximum depending on the number of areas being
   #                      drawn on the graphic.
   #
   #   IDHdr            - is a character vector with 1 or two elements - the 
   #                      character vector provides the header labels for the 
   #                      ID glyph column containing the area names.  The names 
   #                      can be the full name ('Name') or the abbreviation ('Abbr') 
   #                      for the area.  It is recommended the headers be no longer 
   #                      than 12 characters or the the maximum width of the area 
   #                      names.  If the strings are too long, they will be truncated.  
   #                      The border group builder can specify 1 or 2 header labels 
   #                      for the ID glyph. For Example:  IDHdr=c('U.S.','States')
   #                      If no values are provided for this parameter the 
   #                      BorderGroupName will be used.
   #
   #   LabelCex         - a numeric value to be used as the cex value for the 
   #                      Map Label text when Labels are used.  The default is .25.
   #
   #   ReducePC         - is a numeric value (1 element in a vectex) - the 
   #                      numeric is the goal reduction by rmapshaper to 
   #                      the shape file. The range of the value is from 
   #                      0.0001 to 100 percent. A value of 1.5 indicates 
   #                      rmapshaper will keep 1.5% of the original vectex
   #                      in the map areas. The default value is 1.25%.
   #
   #                      
   #
   #   proj4           -  (optional) can be the character string equal to the 
   #                      $input proj4 string or the $wkt string in the st_crs results
   #                      or a list containing the $input and #wkt strings.  
   #                      The value must be processible by the st_crs function without
   #                      generating an error.  
   #
   #                      The map is processed as a long/lat projection or 
   #                      the projection specified in the shape file. 
   #                      Once the simplification, smoothing and modifications
   #                      are completed, the map will be transformed using the 
   #                      provided proj4 value.  The input is tested and 
   #                      converted to a full $input and $wkt version of the 
   #                      the projection in compliance with the proj6 project.
   #
   #                      The proj4 protection value can not be a longitude/latitude
   #                      projection because of its distorted size of each area and will 
   #                      be ignored.
   #
   #                      If the is still a longitude/latitude projection after all 
   #                      processing is completed, an Albers Equal Area (aea) project
   #                      will be created around its centroid with lat_1 and lat_2 
   #                      value half way between the bottom or top of the 
   #                      map and the center lat_0 value.  
   #              
   #                      If the projection provided in the proj4 call parameter
   #                      or a non-long/lat projection in the shape file 
   #                      does not have a +units=m parameter setting the units to 
   #                      meters, the projection will be modified to change 
   #                      the units to meters and the map will be transformed
   #                      at the end of the processing.
   #
   #                      It is suggested, the caller use the border group 
   #                      without this call parameters and seeing if it meets 
   #                      their needs before specifying it.
   #
   #                      An example of a full projection description is:
   #                    
   #            > LLproj <- sf::st_crs("+proj=lonlat +datum=NAD83 +ellipse=WGS84 +no_def")
   #            > LLproj
   #            Coordinate Reference System:
   #              User input: +proj=lonlat +datum=NAD83 +ellipse=WGS84 +no_def 
   #              wkt:
   #            GEOGCRS["unknown",
   #                DATUM["North American Datum 1983",
   #                    ELLIPSOID["GRS 1980",6378137,298.257222101,
   #                        LENGTHUNIT["metre",1]],
   #                    ID["EPSG",6269]],
   #                PRIMEM["Greenwich",0,
   #                    ANGLEUNIT["degree",0.0174532925199433],
   #                    ID["EPSG",8901]],
   #                CS[ellipsoidal,2],
   #                    AXIS["longitude",east,
   #                        ORDER[1],
   #                        ANGLEUNIT["degree",0.0174532925199433,
   #                            ID["EPSG",9122]]],
   #                    AXIS["latitude",north,
   #                        ORDER[2],
   #                        ANGLEUNIT["degree",0.0174532925199433,
   #                            ID["EPSG",9122]]]]
   #            > 
   #
   #
   #   checkPointReStart - (optional) a logical value - this value is normally FALSE  
   #                      to indicate the function is creating a new border group.
   #                      In the process, checkpoint images of key 
   #                      data are written to a 'checkpoint' directory in
   #                      the bordergroup target directory for troubleshooting 
   #                      and to allow the user to make one last chance to do any 
   #                      manual modifications to the shape file of the 
   #                      boundaries.  Manual changes may be required in cases 
   #                      where the areas need to be enlarge so any colored shading 
   #                      of the area will be visible to the border group 
   #                      users.  When the checkPoint call parameter is 
   #                      present and set to TRUE, the function read back 
   #                      in the checkpointed files and continues the process
   #                      to create the dataset for micromapST boundaries.
   #
   #   debug            - is a numeric value (1 element in a vector) - this is 
   #                      a debug value represents a series of bits in an integer
   #                      value.  Each bit represents a debug function with in 
   #                      the function. a value of 1 is bit 1 of the integer,
   #                      a value of 2 is bit 2 of the integer, a value of 4
   #                      is bit 3 of the integer and so on.  A debug value of 6
   #                      would activate the debug activities for bit 2 and 3.
   #                      The debug value can range from 0 to 65535 using a 16 bit 
   #                      integer.  
   #                      The default value is FALSE or 0.  The Values of 
   #                  	1   = running stand-a-long not as a call (manual debugging only), 
   #			2   = trace program flow, 
   #			4   = display projection processing and variable,
   #          	    	8   = Plot intermediate shape files and sf,
   #                    16  = sf processing and Variables, 
   #			32  = Name Table processing and validation, 
   #			64  = Display internal variuables related to sf/SPDF 
   #                          and Shape File, 
   # 			128 = Selection of 0:PDF or 1:PNG output format for 
   #                          intermediate plots. 
   #                  	256 = Write to a PDF a set of map images at each stage 
   #                          of processing, 
   #   			512 = Write to a file (PDF or PNG, bit 8) a single map 
   #                          image at each stage of processing, 
   #                  	1024 = Write a single map image (PDF or PNG, bit 8) of 
   #                           the final areas and map, 
   #                    2048 = Display the final VisBorders images (areas, L2, 
   #                           Regions, L3) on the screen at the end of processing, 
   #			4096 = NA,
   #			8192 = Write a PDF file containing multiple samples 
   #                           of the map scales to the size the may appear in 
   #                           a linked micromap.
   #
   #                    The major stage of the BuildBorderGroup process are:
   #                       1) Acquired shape file spatial data (Raw unprocessed image)
   #                       2) After simplification by rmapshaper
   #                       3) After modifications by parameters provided in the Name Table
   #                       4) After transformation of geometry
   #                       5) After rounding, duplicate point removal and conversion to the 
   #                          VisBorders format for micromapST.
   #
   #  Name Table Requirements:
   #
   #  The name table can be constructed using a test editor or spreadsheet program.  
   #  The output should be saved as a .csv, .xls or .xlsx formatted file.  
   #  The first row must contain the names of each column. At least two columns 
   #  must be present: the 'link' and at least one of the following 
   #  'Name', 'Abbr', or 'ID'. It is recommended to include all three if possible
   #  to make the border group the most usable.
   #  The values in the 'link' column must match the values in 'ShapeLinkName' 
   #  attribute/variable in the sf/SPDF structure.  The number of areas 
   #  (rows in the name table) should match the number of areas in the shape 
   #  file, SPDF, or sf structure provided.  
   #  The values in the 'Name', 'Abbr' and 'ID' columns should be the most 
   #  commonly accepted values for the full name of the area, the common 
   #  abbreviation for the area and a numerical ID for the area.
   #
   #  The name table can also contain special identifiers for each area:  
   #  'Alt-Abbr' and 'Alias'.  The 'Alt-Abbr' column is use to provide 
   #  an alternate abbreviation for the areas when there are more then 
   #  one commonly accepted abbreviation.  For example an international 
   #  abbreviation and a local abbreviation.  The 'Alias' identifier is 
   #  used with a wildcard match against the identifier provided in the 
   #  data tables.  The 'Alias' character strings must match some part 
   #  of the identifer provide and must also be unique in the name table.  
   #  The 'Alias' feature help use data produced by other programs and 
   #  websites that do not use standard identifiers.
   #
   #      The values in each column must be unique.
   #      The use definition of each of these rows are: 
   #         - Name (full name length name of an area), 
   #         - Abbr (an commonly accepted abbreviation for the area), or 
   #         - ID   (a numeric ID for the area).
   #      It is highly recommended that all three columns be included in the NameTable.
   #
   #      Additional columns may be specified:  
   #         - Alt_Abbr - an alternate abbreviation that can be when a 
   #              second abbreviation is needed (also commonly accepted);
   #         - Alias - a character string used to match data to the NameTable 
   #              using a regular expression match of '*<value>*' (this is useful 
   #              when the source of the data does not provide a clear area name 
   #              or abbreviation, but a unique string can be used to match the 
   #              name in the data); 
   #
   #      The initial NameTable must contain a 'Link' column containing the values 
   #      used to link the NameTable row to an area in the SPDF.
   #
   #      Other information and values contained in the NameTable to support other
   #      features are:
   #         - L2_ID - a value to identify which Level 2 space the area belongs 
   #              (a matching L2_ID_Name should also be provided), and  
   #         - Reg_ID - a value to identify which region in the geological space the 
   #              area belongs (a matching Reg_Name should also be provided), 
   #              this supports the micromapST feature of only matching regions within 
   #              a BordGrp in the micro map drawing.  
   #
   #      More information on the NameTable s provided later.
   #
   #  Checkpoint restart:  After the map simplification, name table modifications, 
   #   projection transformation are made and Name Table has been completed, 
   #   copies of the Name Table as .rda and .csv files and the map spatial boundary
   #   data as an Esri Shapefile and .rda file are written out for inspection.
   #   If more adjustments need to be made to the Shape File, the shape file
   #   can be read into GIS software and further changes made. Do not modify
   #   the data contained in the shape file or its structure.  Return the 
   #   shape file to its original location.
   #   To restart the processing, the Name Table .RDA and the Shape file are 
   #   retreived from the CheckPoint space in the BorderGroupDir directory.  
   #   The fact that they are both read from the 'CheckPoint' directory and the 
   #   Name Table data.frame can be read from an .rda file indicates this 
   #   is a checkpoint restart of the process. 
   #   Once the minimal information is verified, the data is read in and 
   #   processing is continued.  A copy of the areaParm data.frame is also 
   #   written to the checkpoint to save handling the areaParm data again.  
   #   The Name Table directory points to the area containing the checkpoint 
   #   subdirectory.  The Checkpoint call parameter tell the function to add 
   #   the 'checkpoint' directory to the path, read the three files, and 
   #   then pickup at the end part of the process.
   #
   #####
   #######
   #########
   
   
   #########
   #######
   #####
   #
   #   create error and stop counters functions - must be in .GlobalEnv 
   #     so the panelXXXX functions can use them.
   #
   var   <- 'errCnt'
   wstr  <- paste0('assign(var,NewCounter(),envir=.GlobalEnv)')
   eval(parse(text=wstr))
   
   var   <- 'stopCnt'
   wstr  <- paste0('assign(var,NewCounter(),envir=.GlobalEnv)')
   eval(parse(text=wstr))
   #
   
   ####
   # Initialize colors from external function (GetMColors)
   # (uses code in the globals module)
   #
   xColors  <- GetMColors()     # pick up colors from globals module.
   mcolors  <- xColors$mcolors
      
   options(warn=1)   # enable warning at the time of occurance
   
   #######  398x
   #
   #  Common Functions
   #
   
   #####
   #
   #   errCntMsg - Send message as Warning, count error, return TRUE
   #
   errCntMsg  <- function(msg) {
       errCnt()
       warning(msg, call.=FALSE)
       return(TRUE)
   }
   #
   ######
   
   ######
   #
   #   stopMsg - send message as Stop, count stop error, return TRUE
   #       What is the difference from stopCntMsg???
   #
   stopMsg  <- function(msg) {
       stop(msg, call.=FALSE)
       return(TRUE)
   }
   #
   ######
   
   ######
   #
   #   stopCntMsg - send message as warning, count stop error, return TRUE
   #
   stopCntMsg <- function(msg) {    
      stopCnt()
      stop(msg, call.=FALSE)
      return(TRUE)
   }
   #
   ######
   
   #####
   #
   #   rot function to rotate the map points in a matrix 'a' radians about the centroid.
   #     This function uses matrix algebra in concert with the sf structure.
   #
   
   rot <- function(a) matrix(c(cos(a),sin(a),-sin(a),cos(a)),2,2) # 2x2 - a is radian
   
   #
   #####
   
   #####
   #
   #  aggFun - a function to inspect the data.frame columns and determine
   #    an appropriate aggregation method - copy or sum.
   #
   aggFun <- function(z) { if (methods::is(z[1],"character")) {
                              # it's a character - just copy it.
                              z[1]
                           } else {
                              # it's a number - sum it.
                              sum(as.numeric(z))
                           }
                        }
   #
   #####
   
   #####
   #
   #   isBetween 
   #     x is the value to test
   #     l and r are the left (lower) and right (higher) values to check
   
   isBetween <- function(x,l,r){
          if (x > r)  return (FALSE)
          if (x < l)  return (FALSE)
          return(TRUE)
   }
   
   #
   #####

   #####
   #
   #  function to convert PROJ4 string into CRS format, catch any errors and warnings, report them
   #  and return CRS to caller.  (Done-10/10)
   #
   
   convertPROJ4 <- function (x) {
   
      #  function is designed to convert a proj4 string into CRS format
      #  and catch any errors or warnings.
      #
      #   x - user provided projection as:
      #        1) projection description string (similar to proj4 string 
      #           or the st_crs $input string.)
      #        2) the projection comments string matching the st_crs
      #           $wkt string.
      #        3) a list consisting of both the $input and $wkt strings
      #           as defined in the st_crs function.
      #
      #   If x is the wkt string, then it is seen as a character string.
      #   It is processed as an $input string - generating a crs with 
      #   the input and wkt entries the same.  If either input or wkt 
      #   contains errors as st_crs attempt to create the wkt, it is 
      #   flagged and we report FALSE.
      #
      #   If x is found to be 'crs' class variable, the st_crs function
      #   does nothing.  The only test for the crs, is to do a st_as_text
      #   function against the $wkt. If it is bad, I can catch an error/warning.
      #   But the result is always a "character" string.
      #
      #   value = is a list of two items.
      #        If the proj4 string and/or wkt string are correct
      #        then the first item is the $input of the crs and the second item 
      #        is the $wkt of the crs.  If the calling value was a "crs" variable,
      #        and the text is correct, then the value is the crs.
      #        If there is an error or warning, then the first
      #        item is the word "WARNING" or "ERROR" and the second item
      #        is the warning/error message from sf.  If a character string is 
      #        is not present (NA, 0, ""), then a NA_character_ is returned.
      #
      #cat(\'convertPROJ4 routine \n')
      z <- list("ERROR",'Empty')
      znull <- NULL
  
      if (is.na(x))  return(z)
  
      if (!methods::is(x,"crs"))   {
         # value is not a crs class value.
         if (!methods::is(x,"character")) {
            # not a character vector - error
            ErrFnd      <- errCntMsg(paste0("***3868 The proj4 value character vector is not a valid projection.\n",
                                            "        Must be a value acceptable to st_crs(). proj4 is ignored.\n"))
            return(znull)     
         } else {
            x  <-  stringr::str_trim(x)
            if (x == "" )  return(znull)
            if (x == "NA") return(znull)
            if (x == "0")  return(znull)
            # have a character string 
            #cat("Value is a character string with content.\n")
            # character vector - OK try the convert
            
            #cat("TryCatch - st_crs on string\n")
            suppressWarnings(   # we want the error - not on consule but in "res".
             res <- tryCatch( 
         
                {  y <- sf::st_crs(x) },     # returns the crs or error.
                    warning = function (war) { 
                              #print(paste0("My Warning: ",war))
                              z  <- list('ERROR', war)
                              return(z)
                             }, 
                    error   = function (err) {
                              #print(paste0("My Error:  ",err))
                              z <- list('ERROR', err)
                              return(z)
                             }, 
                    finally = { }
             )
            )
            # if no warning or error, res is the crs value.
         }
      } else {
        #  it appears to be a crs class variable .
        #cat("x is crs class.  TryCatch - st_as_text.\n")
        res <- x
        suppressWarnings(
         res <- tryCatch(
            { y <- sf::st_as_text(x) },    # This checks the wkt only. returns wkt not input no crs.
               warning = function(war) {
                   #print(paste0("st_as_text Warning:",war))
                   z <- list('ERROR', war)
                   return(z)
                 },
               error   = function(err) {
                   #print(paste0("st_as_text Error:", err))
                   z <- list('ERROR', err)
                   return(z)
                 },
              finally= { }
           )
        )  
        if (res[1]!='ERROR') res = x     
      }
      #class(res)
      #print(res)
       
      # it should be a crs or it is invalid
      #cat("I got to the last half.. convertPROJ4 code 1544 \n")
      return(res)
      
     }  # end of convertPROJ4 function
   #
   #
   #####

   #####
   #
   #  AEAProjection - build AEA proj4 string for the ShapeFile image passed to it.
   #    The AEA projection will be based on the centroid of the map in the ShapeFile,
   #    with lon_0 and lat_0 set to the centroid, and lat_1 and lat_2 set to 1/4 the 
   #    distance below and above the lat_0.  The +units is set to meters.
   #
   #   Input: sf structure
   #
   #   Output: proj4string for the created AEA projection
   #   UPDATE to use sf structures
   #
  AEAProjection <- function(wsf) { 
        #
        #  Calculate AEA projection
        #    wsf is now an sf structure (everything)
        #
        #  The basic AEA for the continental US is:
        #
        #   Projection       = Alber equal area (aea)  => simpleconic
        #   Lat Parallel 1   = 33
        #   Lat Parallel 2   = 45
        #   Origin of Lat    = 39              # for US, center line is 39, move up and down 6 degrees. 
        #   central Meridian = -96   (96W)
        #
        #  sf_use_s2 must be disabled because this function uses st_make_valid.
        #
        # To fabricate a aea projection:
        #  Set the center based on the shape files centroid.  
        #  If shape file is long/lat, then centroid should be in long/lat.
        #  If shape file is long/lat, then proj4string should be present or NULL and 
        #  I can use the lab point for the entire shape file to get the center.
        #  Set the lat_1 and lat_2 values 1/4 distance from the top and bottom of the map height. 
        #
        #  Based on research in to how the lat_1 and lat_2 are chosen, their 
        #  location is based on the size of the region covered.  Generally, the factor is between 
        #  4 and 6.  The approximation of K=4 is used.  That divides the height of the 
        #  map into 1/4th. The lat_0 is the center of the height.  The lat_1 and lat_2 are 
        #  positioned 1/4 from the bottom and 1/4th from the top.  The area is equally divided in 1/4th.
        #
        #cat("AEAProjection: Shape file now must be projected onto the right AEA \n",
        #    "based on the centroid of the space. This is done when the user has \n",
        #    "not specified a projection or the shape file has no projects.\n")
        #
        # Research in to the position of the long and lat values
        # K = 7  large extent in Longitude (E-W)(Wide - W/H < 0.75)
        #   = 5  large extent in Latitude  (N-S)(Tall - W/H > 1.5)
        #   = 4  circular or elliptical         (W/H = 1)
        #   = 3  square (1:1)                   (W/H = 1)
        #
        # Using K = 4 is a compromise to obtain the Lat1 and Lat2 for our projection.
        #
        #  ADD Code to change K from 4 - 5 - 7 range based on aspect ratio.  FUTURE
        #   find center of the areas.	    
        #
        #  To get the centroid of the entire map, you must first st_union all of areas.
        #  Even if the map has been shifted, this algorithm does care. We are only dealing 
        #  with the centroid, which get convert back to normal coord and the centroid.
        #
        #  Since many of the sf function revert the longitude back to normal from shifted,
        #  this function is not dependent on the shift and really does not want the shift
        #  to be in effect.   The first st_union and st_make_valid are also used to revert.
        #
	#cat("Entered AEAProjection Function Code 1615 .\n")
	wsfc <- sf::st_geometry(wsf)
        
        suppressMessages(
           FullMap <- sf::st_make_valid(sf::st_union(wsfc))     # now one area
        )
        # need because of trying st_centroid on LL
        #cat("Find centroid of full map Code 1622 \n")
        suppressWarnings(
            FullMapCtr    <- sf::st_coordinates(sf::st_centroid(FullMap))
        )
        #cat("Full Map centroid:",FullMapCtr,"\n")
        xBBox      <- sf::st_bbox(FullMap)                 # get bbox of the full map.
	
	latHeight  <- diff(xBBox[c(2,4)])
        latQuarter <- latHeight/4
        # WARNING: if lon/lat, the above function revert the st_shift_longitude function.
        # cat("AEAProjection: Center of map - long/lat:",FullMapCtr[1]," ",FullMapCtr[2],".  1/4 height:",latQuarter,"\n")
        
        #
       	# Build new AEA projections.
        #  ### need logic to handle smaller match.
        #
        # Find center Long of area  (lon_0)
        #
        
        wLong0 = round(FullMapCtr[1],3)    # centroids X coordinate in LL, rounded to 3
        if (wLong0 < 0) {
           wLong0 <- as.character(paste0(abs(wLong0),"w"))
        }
        #  Latitudes (3)  (lat_0 - middle)
        wLat0  = round(FullMapCtr[2],3)   # get center latitude - round to three decimal places.
        
        #   lat_1 - lower
        wLat1     <-  round(wLat0 - latQuarter,3)  # is the lower lat half way between middle and bottom edge      
        
        #   lat_2 - upper
        wLat2     <-  round(wLat0 + latQuarter,3)  # is the upper lat half way between middle and top edge
       
        #  calculated AEA projection for transform.
        AEAProj4  <- paste0("+proj=aea +units=m +datum=NAD83 +lat_1=",wLat1," +lat_2=",wLat2,
                            " +lat_0=",wLat0," +lon_0=",wLong0," +no_defs")
        #cat("Calculated AEA projection for transform\n",
        #    "       ",AEAProj4,"\n")
        return(AEAProj4)     
    }    # Done 10/10    DONE
 
 
   ######
   #
   #  BuildVisBorder - converts a sf structures of any region or sub-division into a VisBorders
   #       format.
   #
   #   Call Parameters; Sf structure, TypeVis (for documentation and messages) 
   #   Updated to sf functions - 2202-1201
   #   Assumption:  By the time we get here, the Shape file units will be meters.  
   #   So, any rounding will always be to the 10s.   Rounding of Degrees will not be handled.
   #
   
   BuildVisBorder <- function(wsf,  TypeVis) {
      #
      #    wsf     - simple features of the shape file.
      #            The sf must be supplimented by the union of polygons under each
      #            area. The name table is tied to the shape file data via
      #            a 'Key' string in the Name table and Shapefile/Visborder data.
      #            The 'Key' is loaded into the row.names in the name table and sf structure.
      #            To insure both tables have the 'Key' it is stored in the 
      #            Name Table $Key and Shape File $X__Key fields.
      #            If the row.names are not converted properly, the $X__Key attribute will 
      #            be used to recreate them.
      #
      #            The plot order is maintained within the sf structure and not a separate 
      #            structure like the sp structures.  The assumption is the order of the 
      #            area/polygons in the structure are in proper order.
      #
      #  TypeVis   a string identifying the type of Vis File being converted.  Like
      #            "area", "Level 2", etc.
      #
      #   At the end of the function, the matrix must be converted into a data.frame and 
      #   numerical values converted from character to numeric.  The column labels must
      #   must also be changed from "X" and "Y" to "x" and "y".
      #
      #   The assumption is the sf structure has already been aggregated into one area per row in sf structure.
      #   The row.names are set to the "Key" value for the areas that matches the 
      #   name table.
      # 
      #
      #cat("BuildVisBorder Function - for ",TypeVis," file. Code 1702 \n")
      
      #
      # This function takes a geometry and converts it into a VisBorder structure and returns
      # the VisBorder image.
      #
      #print(wsf, n=60)
      wsfG    <- sf::st_geometry(wsf)     # get rid of data.
  
      # Build data.frame to return the VisBorders format.
      VisForm <- data.frame(x=numeric(0), y=numeric(0), 
                        L1=integer(0), L2=integer(0), L3=integer(0), 
                        hold=logical(0), ID=integer(0), Key=character(0)
                       )
      RScale    <- -1            # round to tens for meters.  10m or 33 ft. 
      xR        <- range(sf::st_bbox(wsf)[c(1,3)])     # range for all
      if (abs(xR[2]) <= 360) RScale <<- 4   # if x,y are long/lat (-360 to 360), the round to .00001 (4)
      # new logic.
      
      
      #
      #  Scaling changes to be done...
      #
      #  Estimate the number of increments you need across the map.
      #  Say 5000 units.  Take the range of X values and divide by 5000.
      #  Now round the point values to an increment value calculated above.
      #
      #  xRange extended to provide buffer to side of mapping space. (1%)
      #  xRange/units = delta per unit.  
      #  Round by;   Value/deltaunit - as.integer - * deltaunit
      #  Integer is not rights... must have rounding to approximatesly the 
      #  deltaunit value.  So, 0.06 units -> round to near it. otherwize 
      #  the integer does overrounding on top of the .06 unit multiple.
      #
      
      RNList     <- row.names(wsf)            # Get row.names from wsf
      NumAreas   <- length(RNList)            # number of Row.Names (areas)
      #print(NumAreas)
      
      #cat("looping through the ",NumAreas," geometry rows to get coordinates.\n")
      
      # Main loop to traverse the rows of the sf and then within the rows. Step through list of areas = numerically
      xx <- lapply(seq(1:NumAreas), function(nz) {
                 #  nz is the row number of the geometry
                 RN      <- RNList[nz]           # get Key for this area.
                 #cat("RN:",RN,"  index:",nz,"\n")
                 z       <- wsfG[[nz]] # pull off row and get st_geometry?
                 tc      <- class(z)[2]          # get class to help make decisions.
                 m       <- sf::st_coordinates(z) # get coordinates.
                    # may not be needed because of the cast to multipolygon....
                 if (tc == "POLYGON") {          # adjust coordinates to make them look like MULTIPOLYGON structure
                    x    <- colnames(m)
                    # adjust columns in coordinates
                    m    <- cbind(m,m[,'L2'])    # new L3
                    m[,'L2'] <- as.numeric(1)    # reset L2 to 1
                    colnames(m) <- c(x,"L3")     # change column names to L1, L2, L3...  now it looks like multipolygon.
                 }
                 m       <- as.data.frame(m)     # extended coordinates to include break points, hole indicator, and Key.
                 m[,"X"] <- round(m[,"X"],RScale)   # round vector values
                 m[,"Y"] <- round(m[,"Y"],RScale)   # round vector values
                 
                 # m matrix has 5 columns:  X, Y, L1, L2, L3.
                 bmark   <- paste0(as.character(m[,"L3"]),as.character(m[,"L2"]),as.character( m[,"L1"]))  # form unique ID (character based)
                              # one row per row in data.frame
                 bmarku  <- unique(bmark)        # find the unique set.
                 xm      <- match(bmark,bmarku)  # id column made of L3,L2,L1.. from coordinates. index to unique id.
                 xh      <- (m[,'L1'] > 1)       # if L1>1, then this polygon is a hole.  One per row.
                  
                 m2      <- NULL
                 m2      <- data.frame(hole=as.logical(xh),ID=as.numeric(xm))  # columns 1, 2
                 m2$Key  <- RN                   # add column  3  - $Key
                 m3      <- cbind(m,m2)          # add the three new columns (hole, id, key) - now 8 columns.
                 
                 #  Break up the polygons in the structures and add NAs at the end of the vector list.
                 #  Then add the polygon with Key, and Hole info to the results data.frame
                 lapply( split(m3[,1:8],m3[,7]) , function(z) { 
                             # z is the matrix to the split point (change of id)
                                   z    <- as.data.frame(z)
                                   lenz <- dim(z)[1]
                                   IDx  <- z[lenz,7]   # get list of IDs in this split.  ???
                                   #   must use z matrix values because of the splits to get hole and id.
                                   zAdd <- data.frame(X=NA,Y=NA,L1=0,L2=0,L3=0,hole=z[1,6],ID=z[1,7],Key=RN)
                                   #print(zAdd)
                                   z9 <- rbind(z,zAdd)  # And end of polygon records for "polygon" draw function.
                                   lenz9 <- dim(z9)[1]
                                   if (lenz9 < 4) {
                                      cat("***3A22 Invalid polygon found in ",RN," area #:",nz," id:",z[1,7],"\n")
                                      print(z9)
                                   }
                                  #     x  y L1, L2, L3, hole, ID, Key  (verified)
                                   VisForm <<- rbind(VisForm,z9)
                                })
                 
             }
          )  
      #cat("Dim of VisForm:",dim(VisForm),"\n")
      
      VisForm2        <- VisForm
      VBnames         <- names(VisForm2)
      #cat("VBForm column names:",VBnames,"\n")
      
      #str(VisForm2)
      #  Clean up the column names (x,y) # convert uppercase with lowercase
      VisFnames       <- names(VisForm2)
      xm              <- match("X",VisFnames)   # replace "X" with "x"
      VisFnames[xm]   <- "x"
      xm              <- match("Y",VisFnames)   # replace "Y" with "y"
      VisFnames[xm]   <- "y"
      names(VisForm2) <- VisFnames              # replace column name list.
     
      #  Clean up the x and y as numerics not characters - impact range function
      VisForm2$x      <- as.numeric(VisForm2$x)
      VisForm2$y      <- as.numeric(VisForm2$y)
      VisForm2$hole   <- as.logical(VisForm2$hole)
      VisForm2$ID     <- as.numeric(VisForm2$ID)
      
      VisForm2$L1     <- as.numeric(VisForm2$L1)    # or set to NULL
      VisForm2$L2     <- as.numeric(VisForm2$L2)
      VisForm2$L3     <- as.numeric(VisForm2$L3)
      #                                               and any others not needed.
      #str(VisForm2)
      
      #cat("Now to remove duplicates. code 1823 \n")
      
      VisForm3        <- as.data.frame(RemoveDups(VisForm2, TypeVis))
      Lft             <- dim(VisForm3)[1]
      row.names(VisForm3) <- seq(1:Lft)
      
     
      return(VisForm3)    # return VisBorders structure to caller.
            
   }  
      
   #
   #
   #####
   
   ######
   #
   #  RemoveDups - Remove duplicate points in a VisBorder matrix x,y point structure.
   #    The VisBorder matrix structure has 4 columns a this point in the processing:
   #      x, y, hole, Key
   # 
   #  The method looks for duplicate point that may have appeared due to the 
   #  rounding of all of the boundary data points by walking through the point list
   #  and removing any neighboring points that are the same.  It is assumed any 
   #  neighboring polygons that share the same points will also have the point in 
   #  their list removed. NA, NA coordinates are processed like any other point set.
   #  When two neighboring point sets are identical, one is erased. 
   #
   #  No changes for sf vs. sp   DONE
   
   RemoveDups <- function(VisB, TypeVis) {
      #
      #  VisB is a VisBorder matrix 0f x, y values (and hole, key) for any set of 
      #  spaces or areas.  This function identify and remove any duplicate x,y points 
      #  in a matrix.  A duplicate is defined as two point next to each other that 
      #  are the same.  Points are separated by a different point, an NA point, or 
      #  the begining of a new polygon.  The NA point should mark all of these 
      #  transitions. The matrix should have 4 columns:  X, Y, holes, Key.
      #  The VisB matrix must have row.names set for each row equal to the number 
      #  of the row.
      #
      #  It is assumped the vector values have already been rounded when converted 
      #  to the matrix x,y.
      #
      
      #
      #  polygons are being reduced to 1 point (first and last combined.)
      #  If one point, kill polygon.  One point polygons don't use up a col=...
      #  Polygon must have more that 3 points (First, middle, last) or it is 
      #  no more than a line.
      #
      #  Need new rounding algorithm.  Then modify remove dups to handle < 4 points per polygon.
      #
      
      VisB2 <- VisB
      
      #  Get the number of points in the matrix
      lenVis    <- dim(VisB)[1]   # Get number of rows in the matrix/data.frame
      
      #  Initial the first "previous" x,y points.
      oldX      <- VisB[1,"x"]
      oldY      <- VisB[1,"y"]
      oldVal    <- 1

      KeepList <- rep(TRUE,lenVis)    # initialize the Keep list to all TRUE.
      
      #  Scan the rest of the points to the end of the data.frame  
      #  One keeplist variable per point. Mark any points that are duplicated 
      #  to be removed later.
      
      ind <- 2
      while (ind < lenVis) {  # for does not allow us to alter the index. Must use while.
      
         if ( !is.na(VisB[ind,"x"]) && !is.na(VisB[ind,"y"]) ) {
            # we have coordinates.
            if ((oldX == VisB[ind,"x"]) && (oldY == VisB[ind,"y"])) {
   
               #  both equal  - we have a match of non-NA oxy points  
               #   Set keepInd = FALSE to delete it.
               KeepList[ind] <- FALSE        # indicate this point should be deleted.
   
               #cat("Marking ",row.names(VisB[ind,])," at ",ind," as duplicate to ",oldVal,
               #     " ",oldX,"=",VisB[ind,"x"]," & ",oldY,"=",VisB[ind,"y"]," type:",TypeVis,"\n")
               # move on to check next x,y point set.   
            }  #  Dup or no dup advance the saved items.
            
            oldX   <- VisB[ind,"x"]   # save new old values.
            oldY   <- VisB[ind,"y"]
            oldVal <- ind
         } else {
         
            #cat("Found NA coord at ",ind,":",row.names(VisB[ind,])," signalling new polygon.\n")
            #cat(" Resetting checks to value+1 to skip next polygon. Old set to ",ind+1," Current set to ",ind+2,".\n")
            oldX   <- VisB[ind+1,"x"]  # save the next one after the NA.
            oldY   <- VisB[ind+1,"y"]
            oldVal <- ind + 1 
            ind    <- ind + 1        # set ind to the value after the NA which is the new saved value.
         }
         # Move Forward.
         ind       <- ind + 1        # step to next point in the polygon.
      }
      # Done with scan.  KeepList tell me what entries to keep.
     
      if (any(!KeepList)) {
         # we have a vector of points to delete.
         DupList    <- row.names(VisB)[!KeepList]  # get the row names for any row/vector to be deleted.
         # Report the list of points to be deleted. The row.names should be the same as the row index #.
         #cat("Duplicate vectors to be removed in ",TypeVis," file are:\n",paste0(DupList,collapse=", ",sep=""),"\n")
         VisB2      <- VisB[KeepList,]   # keep the good entries 
         #  Report on the accomplishment
         lenVis2    <- dim(VisB2)[1]     
         #cat("RemoveDups Completed-Original number of vectors was: ",lenVis," Current length is: ",lenVis2,"\n")
      }
      return(VisB2)
   }
   #
   ######
   
   
   #####
   #
   #  MapColoring functions
   #
   #  DSatur is a graph coloring algorithm pu forward by Daniel Brelaz in 1979.  Similarly 
   #  to greedy coloring algorithm, DSatur colors the vectices of a graph one after another,
   #  adding a previously usused color when needed.  Once a new vectex has been colored, 
   #  the algorithm determines which of the remaining uncolored vertices has the highest
   #  number of colors in its neighborhood and colors this vertex next.  Brelaz defines 
   #  this number as the degree of saturation of a given vertex.  The contraction of the term 
   #  degree of saturation forms the name of the algorithm. DSatur is a heuristic graph
   #  coloring algorithm, yet produces exact results for bipartite, cycle and wheel graphs.
   #  DSatur has also been referred to as saturation LF in the literature.
   #
   #  reference(Brelaz, Daniel (1979-04-01) "New methods to color the vertices of a graph"
   #  Communications of ACM 22 (4) 251-256, doi: 10.1145/359101  ISSN 0001-0782.
   #
   #
  
   #   Convert sf to matrix 
   #
   # st_getAM  function (getAM in sf)  using sf functions to interface with geos.
   #
   st_getAM <- function(xsfc)
     {
      requireNamespace("sf")
      xsfc <- sf::st_make_valid(xsfc)  # make sure the sfc is valid.  shift is gone.
      #cat("Entered st_getAM.\n")
      
      suppressWarnings(
         suppressMessages(
            SFnbmat <- sf::st_intersects(xsfc, sparse=FALSE)
         )
      )
      #print(SFnbmat)
      
      #cat("length of x:",length(xsfc),"\n")
      
      for (i in 1:length(xsfc)) {
         for (j in 1:length(xsfc)) {
            if (SFnbmat[i, j]) {
      
               Xi <- xsfc[i]   # get feature at i
      
               Xj <- xsfc[j]   # get feature at j
      
               suppressWarnings(
                  suppressMessages(
                     stIntersection <- sf::st_intersection(Xi, Xj)   # check each intersection
                  )
               )
               if (length(stIntersection[[1]]) > 0 ) { Res <- TRUE } else { Res <- FALSE }
               
               SFnbmat[i, j] <- Res
                 
            }
         }
      }
     
      diag(SFnbmat) <- FALSE
      #print(SFnbmat)
      return(SFnbmat)
   }
   #
   # Since none of the rest of the logic deals with geometrics.  The SF or SP versions of getAM
   # would provide the basics for both answer sets.
   #
  
   #
   #  getNeighbors
   #
   
   ## Get neighboring verteces
   getNeighbors <- function(adj_mat, node_index) 
   {
     nb         <- which(adj_mat[node_index,])
     nb         <- nb[!(nb==node_index)]
     return(nb)
   }
   
   #
   #  getAmountColor
   #
   ## Count occurrences of color in given nodes
   getAmountColor <- function(node_indexes, color_number, coloring) 
   {
     node_colors  <- coloring[node_indexes]
     return(sum(node_colors==color_number))
   }
   
   #
   #  dsatur
   #
   dsatur <- function (x, coloring = NULL)   # D.Brelaz (1979) ACM
   {    #   x - adj matrix for the areas.
       #cat("Enter Dsatur function Code 2038 \n")
       adj_mat       <- x                 # get the adjaceny matrix
       #cat("dim of adj_mat:",dim(adj_mat),"\n")
       #cat("class of adj_mat:",class(adj_mat),"\n")
       diag(adj_mat) <- FALSE             # set diag of adj matrix to "FALSE" - Neighbor to self.
       degrees       <- list()  # empty list.

       #   coloring - vector of colors to use.
       if (is.null(coloring)) {
           # If not list of colors.
           color_counter      <- 1          # index to current color = 1
           saturation_degrees <- rep(0, nrow(adj_mat))
           coloring           <- rep(0, nrow(adj_mat))
           uncolored_vertices <- 1:nrow(adj_mat)
           index_maximum_degree <- 0
           maximum_degree     <- 0
           
           for (index_node in 1:nrow(adj_mat)) {
               
               degrees[[length(degrees) + 1]] <- c(sum(adj_mat[index_node, ]), index_node)
               if ((degrees[[index_node]])[1] > maximum_degree) {
                   maximum_degree <- (degrees[[index_node]])[1]
                   index_maximum_degree <- index_node
               }
           }
           #cat("calling getNeighbors Code 2063 .\n")
           neighbors = getNeighbors(adj_mat, index_maximum_degree)
           
           for (index_neighbor in neighbors) {
               saturation_degrees[index_neighbor] <- saturation_degrees[index_neighbor] + 1
           }
           
           coloring[index_maximum_degree] <- color_counter
           uncolored_vertices <- uncolored_vertices[-index_maximum_degree]
           
       }   else {
           # have coloring vector for processing.
           color_counter      <- max(coloring)
           saturation_degrees <- rep(0, nrow(adj_mat))
           uncolored_vertices <- 1:nrow(adj_mat)
           uncolored_vertices <- uncolored_vertices[coloring == 0]
           
           for (index_node in 1:nrow(adj_mat)) {
               degrees[[length(degrees) + 1]] <- c(sum(adj_mat[index_node, ]), index_node)
               index_neighbors  <- getNeighbors(adj_mat, index_node)
               index_saturation <- 0
               
               for (number_color in 1:color_counter) {
                   if (getAmountColor(index_neighbors, number_color, coloring) > 0) {
                     index_saturation <- index_saturation + 1
                   }
               }
               saturation_degrees[index_node] <- index_saturation
           }
       }
       
       while (length(uncolored_vertices) > 0) {
       
           maximum_satur_degree = -1
       
           for (index in uncolored_vertices) {
               if (saturation_degrees[index] > maximum_satur_degree) {
                   maximum_satur_degree = saturation_degrees[index]
               }
           }
           indexes_maximum_satur_degree <- c()
           
           for (index in uncolored_vertices) {
              if (saturation_degrees[index] == maximum_satur_degree) {
                   indexes_maximum_satur_degree <- c(indexes_maximum_satur_degree, index)
              }
           }
           coloring_index = indexes_maximum_satur_degree[1]
           
           if (length(indexes_maximum_satur_degree) > 1) {
               maximum_degree = -1
               for (index in indexes_maximum_satur_degree) {
                   degree <- (degrees[[index]])[1]
                   node_index <- (degrees[[index]])[2]
                   
                   if (degree > maximum_degree) {
                      coloring_index = node_index
                      maximum_degree = degree
                   }
               }
           }
           node_index_neighbors = getNeighbors(adj_mat, coloring_index)
           
           for (number_color in 1:(color_counter)) {
               if (getAmountColor(node_index_neighbors, number_color, coloring) == 0) {
                   coloring[coloring_index] = number_color
                   break
               }
           }
           
           if (coloring[coloring_index] == 0) {
               color_counter <- color_counter + 1
               coloring[coloring_index] = color_counter
           }
           
           uncolored_vertices <- uncolored_vertices[!(uncolored_vertices == coloring_index)]
           for (index_neighbor in node_index_neighbors) {
               subneighbors = getNeighbors(adj_mat, index_neighbor)
               if (getAmountColor(subneighbors, coloring[coloring_index], coloring) == 1) {
                   saturation_degrees[index_neighbor] <- saturation_degrees[index_neighbor] + 1
               }
           }
       }
       
       return(coloring)
   }

   #
   #   getColoring - setup for the dsatur function.   (FIX)
   #
   
   getColoring  <- function (x) 
       #  x is the input information on the Spatial structure
       #
       #  x = matrix then it is the adjacency matrix of the area in the SpatialPolygons or sf
       #  x = SpatialPolygons or SpatialPolygonsDataFrame (new - simple feature)
       #
       #  The SP structure is converted into an adjacency matrix using getAM (get Adjacency Matrix)
       #  At this time the simple features structures is just being added to this code to 
       #  create a sf structure convertion to an adjacency matrix under st_getAM.
       #
    {
       if (methods::is(x, "matrix")) {   # have adjacency matrix for all areas.
           #cat("processing matrix\n")
           adj_mat <- x
           diag(adj_mat) <- FALSE
       } else 
           if (methods::is(x, "SpatialPolygons")) {                  ##### *****
               #cat("processing SP\n")
               xsf     <- sf::st_as_sfc(x)         # convert SPDF or SP
               adj_mat <- st_getAM(xsf)            # convert SP to sf to matrix,
           } else {
               if (methods::is(x,"sfc")) {
                  #cat("processing sfc\n")
                  adj_mat <- st_getAM(x)      # convert polygon sfc into matrix.               
               } else {
                  stop("First value must be an adjacency matrix, SpatialPolygons* object or a simple feature geometry column (sfc).")
               }
           }
           #cat("calling dsatur.\n")
           #print(adj_mat)
           coloring   <- dsatur(adj_mat)
           #cat("dsatur returned - coloring:\n")
           #print(coloring)
           return(coloring)
   }

   #
   #
   #####
   
   #####
   #
   #  SamplePrts_sf - one lattic style - multiple small maps with 5 areas
   #  colored each time and one scaled small image map with 5 areas colored.
   #
   #  This routine is called at the three major points in the function where the 
   #  map was modified: Raw-after the initial reading of the shape file;
   #  rmapshaper-after the map has been simplified by rmapshaper; name table  
   #  modifications-after the map has been modified as requested in the Name  
   #  Table Xoffset, Yoffset, Scale, Rotate, and ModOrder parameters. 
   #  The fourth sample point is based on the VisBorders formated 
   #  data at the end of the run.
   #  Used to graphically show how the map areas will looked shaped.
   #  The type of maps draw are dependent on debug bits 256 and 512.
   #
   #  debug bit 256 tells this routine to do a multiple map printout at about the same 
   #  size as the micromap may appear and rotate through the 5 colors to show if any 
   #  areas would no be visiable on the linked micromap.   
   #
   #  debug bit 512 tells this function to draw about a 4" x 4" version of the map with all areas 
   #  colored in.  This is done at each major modification point.
   #
   #  debug bit 1024 tells this function to draw about a 4" x 4" version of the map with all areas
   #  colored in.  This is the same map created for debug bit 512 but instead of 4 maps, only the 
   #  last version of the map is drawn (after vectors are converted to VisBorders format.)
   #
   #  This print sample routine must handle two situations:
   #   a) the SPDF/sf contains polygons not grouped by area. Thus
   #      the number of polygons is much larger than the number of areas.
   #      However, the polygons should be grouped by area and colored.
   #   b) The SPDF/sf has been unioned and all of the polygons are held 
   #      under the entry for each area.
   #   c) correct for long/lat issues crossing the 180 longitude to make a more realistic 
   #      map.
   #  Color are assigned by area identifiers.
   #
   #  Adjust code to handle 5 areas and less.
   #
   
   SamplePrts_sf <- function(PPsf, PPTitle, PPMfrow, debug, NTKey, MAvgH=NULL, NCol = 5) {
      
      # PPsf    = Maps sf structure.   Only use the geometry.
      # PPTitle = string used for the title of the graphic and as part of the output filename.
      #           PPTitle is used for the title and output filename of the sample plots.
      #           The PPTitle string is editted before used as part of the file name
      #           by replacing the " " (spaces) with "_" (underscores).
      # PPMfrow = Parameter for the Mfrow graphics call for the multiple map display 
      #           for debug=256 bit
      # NTKey   = Name Table $ Key column. The Key is the link between the name table 
      #           single row to the multiple polygons in the Shapefile.
      #           This is used to make sure all of the polygons related to the same area are
      #           colored the same and colored at the same time.
      #           This is required to be able to handle the situation that all polygons associated
      #           with a given area have not been combined into a single multipolygon (row) in the 
      #           spatial structure.
      # MAvgH   = size of the average map range, attempt to estimate the Group/Row height that 
      #           may be encountered.  (Initial attempt to print the small sizes failed.  More
      #           coding required, but not at this time. Changed the size to about 4 x 4.)
      #
      #
      
      #cat("Entered SamplePrts_sf...\n")
          
      # Remove Titles from plots.
      
      if (length(NTKey) < 2) {
         xmsg <- paste0("***3997 The number of areas in the map is 1 or less. A border group\n",
                        "        can not be made.\n")
         StopFlag <- stopCntMsg(xmsg)
         stop()
      }
      
      PPsfGeo <- sf::st_geometry(PPsf)   # get sfc geometry.
      
      # Edit title string to replace " " with "_" for filenames.
      FTitle <- gsub(" ","_",PPTitle)
      
      NNN          <- 5   # number of colors and areas per group/row.
      if (NCol != 5) {  NNN <- 6   }
 
      PKeyList         <- PPsf$X__Key            # use keys to pull polygons together for an area.
                   # one entry in list per geometry row in sf
      #print(PKeyList)
      
      PKeyListSize    <- length(PKeyList)   # length of sfc / sf (number of geometries)
      
      UniPKeyList     <- unique(PKeyList)   # list of unique area identifiers (Keys)
      UniPKeyListSize <- length(UniPKeyList) # length of unique area Keys list.
      
      if(NNN > UniPKeyListSize ) NNN <- UniPKeyListSize   # keep NNN at or below the number of areas.
           # NNN is normally 5, but could be down to 2
      
      xNNN             <- UniPKeyListSize - NNN   # used by both map plotters. for DE or RI = 0 (maps with < 6 areas)
      
      # if NNN <= 5/6, then xNNN will start being zero.
      #cat("UniPKey:",UniPKeyListSize," NNN:",NNN,"  xNNN:", xNNN,"\n")  
      
      # if number of colors is not 5, then use 6
      BaseColors  <- NULL
      BaseColors  <- c(BaseColors,mcolors[1:NNN])      # basic set of "x" colors.
      BlankColors <- c(rep(NA,NNN))                    # blank set of "x" colors.
  
      if (missing(MAvgH) || is.null(MAvgH)) {  MAvgH = 1.3  }
 
      # Match sf/sfc geometry rows to a name table area via Key
      Poly2Area        <- match(PKeyList,NTKey)  # match each polygon to a NTable entry.
               # multiple rows/polygons can be associated with one Name Table entry.
       
      # initialize NTKeyCol table.  this table maps the colors to the polygon LINKS or KEYS
      NTKeyCol        <- data.frame(Key=NTKey,Col=NA)  
              # NTKey should match sf Key on row/geometry/polygon.
    
      ###########
      
      if (bitwAnd(debug,256) != 0) {
         # draw a set of maps (about 9 to 12 per page) to see the relative size of the micromaps.
         # The colors are rotated through 5 at a time to allow the user to check the 
         # visibility of each areas
         
         NumPanels        <- as.integer((length(NTKey)-1)/NNN + 1)       # calculate number of panels.
         #cat("SamplePrt_sf - bit 256 - Number of areas:",UniPKeyListSize,"  Number of Panels:",NumPanels,"\n")
         
         # This printout is only done to PDF.   Build PDF filename and title.
         PDFTest          <- paste0(BGPathName," Test Chart - ",PPTitle,".pdf")
         Title            <- paste0("Test Chart - ",PPTitle)
         #cat("SamplePrt -> ",PDFTest,"\n")
         
         # open PDF file for output 
         grDevices::pdf(PDFTest,width=10.5,height=7.75)   # open destination PDF file.
         Sys.setFileTime(PDFTest,Sys.time())
         # set up for multiple images.  outside and inside margins.
         par(mai=c(0.125,0.125,0.125,0.125))  # 1/8" around - inside margins
         par(mar=c(1,1,2,1))                  # in lines
         par(oma=c(.5,.5,.5,.5))              # outside margins = 0.5"
                  
         par(mfrow=PPMfrow)   # setup to provide about the same space as a micromap
         #cat("PPMfrow:", PPMfrow,"\n")
         
         # initialize the color pattern (one per area) (first 5 areas.)
         VColors       <- c(BaseColors, rep(NA,xNNN))    # moving vector(areas) as we draw.
    
         #cat("xNNN:",xNNN,"  NumPanels:",NumPanels,"  length of VColors:",length(VColors),"\n")
         
         # plot "n" number of maps (number of areas/5 maps) in a matrix of windows.
         for (inx in c(seq(1,NumPanels))) {   # find the polygons for each area via the Key.
     
            NTKeyCol$Col  <- VColors     # fill list if the colors (5 good, rest blank) 
            ColList       <- NTKeyCol[Poly2Area,"Col"]  # get color per polygon
               
            # now match the polygon list KeyCol list and pick up the color
            xm            <- !is.na(NTKeyCol$Col)   # find areas with no color
            
            NTKeyLeg     <- NTKeyCol$Key[xm]    # get key and col for legend.
            NTKeyLegCol  <- NTKeyCol$Col[xm]
         
            #  Plot one map - 5 colors/areas   
            plot(PPsfGeo, col=ColList, lwd=0.2, asp=1)     # sf plot.
            par(new=TRUE)  # for the next plot.
            #graphics::title(main=Title)
            
            graphics::legend("right", NTKeyLeg, text.col = NTKeyLegCol, 
                   cex=0.9, bty="n", pch=NA, xpd=TRUE, inset=-0.05)
         
            #  slide colors over for the next set.
            #  Put 5 blank colors infront of list and then keep only the right number.
            VColors      <- c(BlankColors,VColors)[1:UniPKeyListSize]
         }
         
         # end of page, close file.
         x <- grDevices::dev.off()            # close the PDF
         
      }  #  done with panels of multiple scaled maps with shapings.
      
      #############
      
      #  the debug=256 code seems to work.
      
      #  Generate single image of map pdf or png based on debug = 128
      
      #  its the debug=512 that doesn't...
      if (bitwAnd(debug,512) != 0) {

         #cat("SamplePrt - 512 bits ...\n")
         
         # Starting point for mcolors.
      	 # This printout is done to png or pdf
         # The image were also hard to work with.  So, they were all scaled up to about 4 x 4.
      	 PngH         <- 4                       # y = 4"
      	 PngW         <- 4                       # x = 4 / (y/x)
      	 PngH         <- PngH + 0.4    # To make room for title add .4 inches (2 lines)
      	 
         # objective is to print one map boundary image at approximate the correct scale.
            	   
         # initialize the color pattern (one per area)
         VColors       <- c(BaseColors, rep(NA,xNNN))  # moving vector(areas) as we draw.
         NTKeyCol$Col <- VColors
        
         PCol          <- NTKeyCol[Poly2Area,"Col"]   # one entry and color per area (regardless of number of polygons.) 
                                                       # sample coloring are assigned by neighbors of unioned areas.
              
         #  we have two debug flag numbers.  512 prints these images 
         #  at each processing step of the sf, 1024 only prints the first and 
         #  FINISHED images.  If 1024 is set, 512 is asserted.
         
         OutTestSm    <- paste0(BGPathName,"_SM_",FTitle,OType)
         #cat("output file PDF or PNG: ",OutTestSm,"\n")
         
         if ( OType == ".png" ) {
            grDevices::png(OutTestSm,res=300,width=PngW, height=PngH , units="in")
         } else {
            grDevices::pdf(OutTestSm, width=PngW, height=PngH)
         }
         Sys.setFileTime(OutTestSm,Sys.time())
         # setup margins to get the biggest image.    
         par(mai=c(0,0,0,0))
         par(mar=c(0,0,2,0))  # one line for title.
         par(omi=c(0,0,0,0))
         par(oma=c(0,0,0,0))

         plot(PPsfGeo, col=PCol, lwd=0.05, asp=1)
         #graphics::title(main=PPTitle,cex.main=0.75)
        
         x <- grDevices::dev.off()
      }
  
   }
   #
   #####


   #
   #  End of Common Functions
   #
   #####
   #######
   #########
   
   
   #########
   #######
   #####
   #
   #   Part 1.0 - BuildBorderGroup Initial Call Parameter check logic.
   #     validates the color and order.
   #
   ##### 310x
   
   ErrorFlag <- FALSE
   StopFlag  <- FALSE
   
   #####
   #
   #  Quick check out of the debug parameter.   DEBUG
   #
   def_debug <- 0
   if (is.null(debug) || missing(debug)) {
       debug <- def_debug
   } else {
       if (!methods::is(debug,"numeric")) {
          ErrFnd <- errCntMsg(paste0("***3100 debug call parameter is not a numeric value. The default value\n",
                         "        of ",def_debug," will be used.\n"))
          debug <- def_debug
       }
   }
   
   #cat("DEBUG set to :",debug,"\n")
   #
   #####
   
   # if not debugging - pull formals from the function calls.
   
   if (bitwAnd(debug,1) == 0) {
      # standard parameter front
      
      #####
      #
      #  Save call parameter values for warning and error messages, 
      #  not content, name of variables.
      #
      #  Cant do this in a function because the environment and 
      #  frames will change.
      #
      # Get list of call parameters - the formals - for the function 
      #   and default values. (as defined).
      frml         <- formals()              
      # Get the name of the parameters  (as we validate the parameter, 
      #   we will back file the defaults.
      frmlNames    <- names(formals())            
      
      # Get the names and values used on the current call.
      callVar      <- as.list(match.call())[-1]   
      # Get the names of the used call parameters
      callVarNames <- names(callVar)              
      
      # merge the formals parameter list with the parameter 
      # list used at the time of the micromapST call with user 
      # set values.
      
      # Seed the call variable list with the formals and default values
      callVL       <- frml                        
      # copy the values used in the call .
      callVL[callVarNames] <- callVar[callVarNames]  
    
   } else {
      #
      #  debug = 1
      #
      
      # Fake call - list of parameters and defaults.
      frml <- list(ShapeFile=NULL, ShapeFileDir=NULL, 
                   ShapeLinkName=NULL, 
                   NameTableFile=NULL, 
                   NameTableDir=NULL, NameTableLink = NULL,
                   BorderGroupName=NULL, BorderGroupDir=NULL,
                   MapHdr=NULL, MapMinH=NULL, MapMaxH=NULL, 
                   IDHdr=NULL, ReducePC=NULL, proj4=NULL, 
                   checkPointReStart=NULL, debug=debug)
      #
      # set call variable list (callVL) to the defined list and defaults.
      callVL       <- frml    
      
      # merge defaults with values set at time of call.
      if (!is.null(ShapeFile))         callVL$ShapeFile       <- ShapeFile
      if (!is.null(ShapeFileDir))      callVL$ShapeFileDir    <- ShapeFileDir
      if (!is.null(ShapeLinkName))     callVL$ShapeLinkName   <- ShapeLinkName
      if (!is.null(NameTableFile))     callVL$NameTableFile   <- NameTableFile
      if (!is.null(NameTableDir))      callVL$NameTableDir    <- NameTableDir
      if (!is.null(NameTableLink))     callVL$NameTableLink   <- NameTableLink
      if (!is.null(BorderGroupName))   callVL$BorderGroupName <- BorderGroupName
      if (!is.null(BorderGroupDir))    callVL$BorderGroupDir  <- BorderGroupDir
      if (!is.null(MapHdr))            callVL$MapHdr          <- MapHdr
      if (!is.null(MapMinH))           callVL$MapMinH         <- MapMinH
      if (!is.null(MapMaxH))           callVL$MapMaxH         <- MapMaxH
      if (!is.null(LabelCex))          callVL$LabelCex        <- LabelCex
      if (!is.null(IDHdr))             callVL$IDHdr           <- IDHdr
      if (!is.null(ReducePC))          callVL$ReducePC        <- ReducePC
      if (!is.null(checkPointReStart)) callVL$checkPointReStart <- checkPointReStart
      if (!is.null(proj4))             callVL$proj4           <- proj4
          
      callVarNames    <- names(callVL)    # get list of all names.
      #killlist       <- c("proj4","ReducePC")
      #callVarNames   <- callVarNames[is.na(match(callVarNames,killlist))]   # emulating only values in call.
   }  
   for (ivar in names(callVL)) {
      #  Extract the variables from list and then from parameters
      # build cm to assign value to Global Variable.
      wstr <- paste0("assign(ivar,callVL$",ivar,",envir=.GlobalEnv)")  
      eval(parse(text=wstr))   
   }
   
   parmNames <- callVarNames
   
   ##### 310x
   #
   # Initialize Variables  0.1
   #
  
   ReqCParms    = c("ShapeFile", 
                    "NameTableFile", "BorderGroupName") 
                    
   ReqCkptParms = c("NameTableDir", "BorderGroupName")
   
   #  MapHdr is now an optional call parameter.
   
   # List of required call parameters
   # If no directories are specified, we use the current working directory.
   
   #
   #  Set up project 4 strings  -  Original if none is present in the ShapeFile.
   #
   OrigProj      <- "+proj=longlat +datum=NAD83 +ellipse=WGS84 +no_defs"
   #
   
   DoUserProj4   <- FALSE  # user provided proj4 for final projection
   DoModProj4    <- FALSE  # have modified shapefile proj4 with meters 
                           #(only if shapefile projection was modified to Meters.)
   DoBldAEAProj  <- FALSE  # ShapeFile is LL, no proj4, need to build AEA projection
   ShpProjLL     <- FALSE  # Indicator of LL projection in ShapeFile, or none and set to LL.
   Across180     <- FALSE  # Indicates the LL projection is across the 180 meridian (-xx to +xx)
 
   ModProj4      <- NA     # modified ShapeFile Proj4 modified to meters
 
   OType         <- ".pdf"
   if (bitwAnd(debug,128) !=0)  OType  <- ".png"
   
   if (bitwAnd(debug,4) != 0)              # debug = 4 - print out projection variables.
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
             "  DoUserProj4:",DoUserProj4,
             "  DoModProj4:",DoModProj4,
             "  DoBldAEAProj:",DoBldAEAProj,"\n")
   
   #
   #  New logic to find center of map for ProjCRS update.
   #
   #  Get height in units, from bottom 1/4 up Lat1, up 1/4 to center = Rig Lat, up 1/4 to Lat 2.
   #  Center E to W is Central Meridian
   #
   
   #
   # Call parameters and specified parameters are now saved in a structure (Named List)
   #     real test is if they have NULL or NA values.
   #
   #####
   
    
   #####  311x
   #
   #  Part 1.1 - checkPointReStart call parameter and logic wrapper.
   #
   #  If checkPointReStart is TRUE. then the critical call parameters to check first
   #  are the checkPointReStart, NameTableDir (foundation for checkpoint folder), and
   #  BorderGroupName (part of the checkpoint file name in folder).
   #  Then we can continue or bypass the other checks.
   #
   #  Required for normal run:  
   #       NameTableDir, BorderGroupName, 
   #       
   #    Optional:
   #       ShapeFile, NameTableFile
   #       ShapeFileDir, ShapeLinkName, 
   #       NameTableLink, NameTableDir, 
   #       BorderGroupDir, 
   #       ReducePC, debug, proj4, 
   #       MapMinH, MapMaxH,  MapHdr, IDHdr
   #
   #  Required for checkPointReStart run:
   #       NameTableDir*, BorderGroupName*, checkPointReStart=TRUE
   #
   #    Optional:
   #       BorderGroupDir, debug
   #
   #	Not Used:
   #       ShapeFile, ShapeFileDir, ShapeLinkName,
   #       NameTableFile, NameTableLink, 
   #       MapHdr, IDHdr, MapMinH, MapMaxH, proj4, ReducePC
   #
   #       The areaParms contains most of the information if a checkpoint restart is 
   #       preformed and the values are samed under the border group name.
   #       The re-projection of the shape file has already been done.
   #     * must be the same value as used in original run.
   #
   
   ##### 312x
   #
   #  Part 1.2 - validate checkPointReStart
   #
   #  Check for checkPointReStart call parameter
   
   if (missing(checkPointReStart) || is.null(checkPointReStart)) {
      # checkPointReStart call parameter is missing, set to default
      checkPointReStart <- FALSE
   } else {
      checkPointReStart <- checkPointReStart[[1]][1]  # get first value
      if (!methods::is(checkPointReStart,"logical")) {
         # not a logical variable
         xmsg     <- "***3120 The checkPointReStart call parameter is not a logical value.\n"
         StopFlag <- stopCntMsg(xmsg)
      } else {
         if (checkPointReStart) {
            if (bitwAnd(debug,2) != 0) {     # debug = 2 then display progress print outs.
               cat("***3122 Check Point Restart has been requested. \n",
                   "        Check point files will be read from folder :\n",
                   "        ",NameTableDir,"/Checkpoint","  directory.\n")
            }
         }
       }
   }
   #
   # P.S. The directories could be "" representing the current working directory.
   #
   #####
   
   #####  313x
   # 
   #  Part 1.3 - Require calling parameters  (normal or restart)  
   #
   #
   if (checkPointReStart) {
      #  checkPointReStart Mode
      xReqCParms <- ReqCkptParms
   } else {
      #  Normal Run mode.
      xReqCParms <- ReqCParms
   }
   
   #  Test for the required call parameters?
   cm           <- match(xReqCParms, parmNames)
   # test if any missing.
   cmna         <- is.na(cm)   # find missing ones.   NA indicate missing required parameter
  
   if (any(cmna)) { #      We have missing required parameters  - throw error and stop
  
      missingRegList <- xReqCParms[cmna]   # get list of missing required call parameters
      StopFlag       <- stopCntMsg(paste0("***3130 Required call parameters are missing : \n",
                                          "        ",paste0(missingRegList,collapse=", ")," - execution stopped.\n"))
   }
   
   #
   #####
   #
   if (StopFlag) {
      stop(paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
                  "        Please fix problem(s) and retry.\n"))
   }
   #
   #####
   
   #
   #  Changing strategy.  
   #  a) Process all of the parmeters, 
   #  b) if Dir parms are present, indicate DirValid.
   #  c) At the end of the processing, check which are present and which are not.
   #  d) Then backfill filenames with the directories.  
   #
   #  Now that the ShapeFile and NameTable can be passed directly in the parameters, 
   #  their directory parameters are not needed and will not be checked.
   #  So, BorderGroupDir may be required if no other directory (name table 
   #  and Shape file are present.  
   #  
   #  It allows the function to go through the filelist and backfill any of the 
   #  directories required with what is provided.  It also removes the check
   #  for data table passing.
   #  Priority of Directory usage if no provided is:
   #  1) Name Table Dir, 2) Shape File Dir, and 3) Border Group Dir.  
   #  NameTable and ShapeFile are always input and required.  Border Group 
   #  is always output of the routine.
   #
      
   #####  314x
   #
   #  Part 1.4 - NameTable Dir (#1) 
   #                (required may be the backup dir for BorderGroupDir)
   #
   NTDirValid <- FALSE
   NTDir      <- NULL
    
   if ( missing(NameTableDir) || is.null(NameTableDir)  ) {
       # make sure its NULL
       NameTableDir <- NULL    # no directory provided. Use current working directory
       StopFlag     <- stopCntMsg(paste0("***3141 NameTableDir call parameter is missing or NULL.\n",
                                         "        Correct and re-run.\n"))
   } else {
       if (length(NameTableDir) != 1) {
          NameTableDir <- NameTableDir[[1]][1]   # get the first value.
       }
       NameTableDir <- stringr::str_trim(NameTableDir)                   # trim spaces.
       if ( is.na(NameTableDir) || !methods::is(NameTableDir,"character") ||
            nchar(NameTableDir)<=0 ) {
          #   value is NA or not a character string.
          StopFlag   <- stopCntMsg(paste0("***3142 NameTableDir call parameter is an 'NA', Empty or not\n",
                                          "        a character string. Correct and re-run.\n",
                                          "        Value is : ",NameTableDir,"\n"))
       } else {         
          # validate the directory exists and is reference.
          NameTableDir <- stringr::str_trim(NameTableDir)                   # trim spaces.
          x  <- stringr::str_sub(NameTableDir,-1,-1)  # get last character of path
          if (x=="/" || x=="\\") {
             # last character is a slash. Trim it off.
             NameTableDir  <- stringr::str_sub(NameTableDir,1,-2)
             NTDirValid    <- TRUE
          }
          if (!dir.exists(NameTableDir))  {
             # NameTableDir path does not exist.
             StopFlag   <- stopCntMsg(paste0("***3143 NameTableDir value specified does not exist.\n",
                                             "        Value is : ",NameTableDir,"\n"))
          }  else {
             # have a good NameTable directory
             NTDir      <- paste0(NameTableDir,"/")  # add slash to NTDir.
             NTDirValid <- TRUE
          }
          #  We should have a good NameTable Directory or a StopFlag.
       }
   }
   
   #cat("NTDir:",NTDir," 2768 \n")
   callVL$NameTableDir <- NameTableDir
   callVL$NTDir        <- NTDir		    # with /
   callVL$NTDirValid   <- NTDirValid
   #
   #####

   #####   315x
   #
   #  Part 1.5 - BorderGroup Name and Directory & Restart Directory.
   #
   BGDirValid <- FALSE
   
   if (missing(BorderGroupDir) || is.null(BorderGroupDir) ) {
      BorderGroupDir <- NameTableDir
      BGDir          <- paste0(NameTableDir,"/")     # with /
   } else {
      BorderGroupDir <- BorderGroupDir[[1]][1]  # get single item
      BorderGroupDir <- stringr::str_trim(BorderGroupDir)
      if ( is.na(BorderGroupDir) || !methods::is(BorderGroupDir,"character") || 
            nchar(BorderGroupDir) <= 0) {
         # BorderGroupDir set to NA, '' or not a character value. - use working directory
         cat(paste0("***3156 BorderGroup directory specified in the call parameter is NA, \n",
                    "        Empty or not a character vector. The parameter will be\n",
                    "        ignored and the NameTable directory used.\n",
                    "        Value is : ",BorderGroupDir,"\n") )
         BorderGroupDir <- NULL    # no directory provided.
         BGDir          <- NULL
      } else {
         # validate the directory exists and is reference.
         BorderGroupDir <- stringr::str_trim(BorderGroupDir)                   # trim spaces.
         # remove trailing "/" if present
         x  <- stringr::str_sub(BorderGroupDir,-1,-1)
         if (x=="/" || x=="\\") {
            BorderGroupDir  <- stringr::str_sub(BorderGroupDir,1,-2)
            BGDir           <- paste0(BorderGroupDir,"/")	 # with /
            BGDirValid <- TRUE
         }
      
         if (!dir.exists(BorderGroupDir))  {
            # BorderGroupDir path does not exist.
            #StopFlag  <- stopCntMsg(paste0("***3150 BorderGroup directory specified in the call parameter does not exist. Value=",BorderGroupDir,"\n"))
            # Create the path ---  This may become optional later.
            cat(paste0("***3150 BorderGroup directory specified in the call parameter \n",
                       "        does not exist. It will be created.\n",
                       "        Value is : ",BorderGroupDir,"\n"))
            dir.create(BorderGroupDir)
            BGDir      <- paste0(BorderGroupDir,"/") # with /
            BGDirValid <- TRUE
            
         } else {
    
            # have a good directory 
            BGDir <- paste0(BorderGroupDir,"/")	# with /
            BGDirValid <- TRUE
         }
      }
   }      
   
   #cat("BGDir:",BGDir," 2742 \n")
   callVL$BorderGroupDir <- BorderGroupDir
   callVL$BGDir          <- BGDir		  # with /
   callVL$BGDirValid     <- BGDirValid
   
   #
   #  BorderGroupName
   #
   
   if ( missing(BorderGroupName) || is.null(BorderGroupName) ) {
      StopFlag <- stopCntMsg(paste0("***3152 The required BorderGroupName call parameter is missing."))
   } else {
      BorderGroupName   <- BorderGroupName[[1]][1]
      BorderGroupName <- stringr::str_trim(BorderGroupName)
      if (!methods::is(BorderGroupName,"character") || is.na(BorderGroupName) || 
           nchar(BorderGroupName) <= 0 ) {
         StopFlag <- stopCntMsg(paste0("***3154 BorderGroupName is a 'NA', Empty or is not a character string.\n",
                                       "        Value is : ",BorderGroupName,"\n"))
      }
   }
   
   # ***** change to go after NTDir only if missing from BorderGroupDir.
   # ***** Do we need code to check and strip extension form Group Name???
      
   # strip BG from the end of the Border Group Name.
   if (stringr::str_sub(BorderGroupName,-2,-1) == "BG") {
      BGBase <- stringr::str_sub(BorderGroupName,1,-3)
   } else {
      BGBase <- BorderGroupName
   }
   
   #cat("BGBase:",BGBase,"  2754 \n")
   callVL$BGBase       <- BGBase      # Name without BG as the ending.
      
   BGFile              <- paste0(BGBase,"BG.rda")
   #cat("BGFile:",BGFile,"  2758 \n")
   callVL$BGFile       <- BGFile
   
   BorderGroupPath     <- paste0(BGDir,BGFile)
   
   BGPathName          <- paste0(BGDir,BGBase)
  
   #cat("BGPathName:",BGPathName,"  2765 \n")
   
   callVL$BorderGroupName <- BorderGroupName
   callVL$BorderGroupPath <- BorderGroupPath
   
   cat("Border Group will be written to:",BorderGroupPath,"\n")
   
   if (StopFlag) {
      stop(paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
                  "        Please fix problem(s) and retry.\n"))
   } 
  
   #
   #####
   #######
   #########
   
   
   #########
   #######
   #####
   #
   #
   
   ## Check for normal path or checkpoint restart
   
   #cat("checkPointReStart:",checkPointReStart,"  2798 \n")
   
   if (!checkPointReStart) {   # normal validation of call parameters.
   
      #######
      #####
      #
      #  NORMAL PATH
      #
      #  Start validating and processing the calling parameters.
      #
       
      ##### 320x
      #
      #  Part 2.0 - Name Table filename.   Build NameTablePath
      #
      NTPassed           <- FALSE
      def_NameTableType  <- 1          #   default .csv
      def_NameExt        <- ".csv"     #   default extension
      NameTablePath      <- ""
      
      #cat("NTDir:",NTDir," 2933 \n")
      
      if (missing(NameTableFile) || is.null(NameTableFile) ) {
         StopFlag   <- stopCntMsg(paste0("***3202 NameTableFile parameter has not been provided. Execution Stopped.\n"))
      } else {
         # Name Table may have been passed as binary structure (data.frame)
         if (methods::is(NameTableFile,"data.frame")) {
            # the value passed in NameTableFile is a data.frame.
            # treat as a read name table.
            # cat("NT passed as data.frame 2942 \n")
            # print(NameTableFile)
            NTable   <- NameTableFile
            NTPassed <- TRUE
         } else {
            #cat("NT testing for non-character type 2890 \n")
            if (methods::is(NameTableFile,"tbl")    || methods::is(NameTableFile,"list") || 
                methods::is(NameTableFile,"tbl_df") || methods::is(NameTableFile,"matrix") ||  
                methods::is(NameTableFile,"array")  || methods::is(NameTableFile,"numeric")) {
                # all of the above are not allowed.
                xmsg <- paste0("***3204 The name table passed to function on NameTableFile parameter\n",
                               "        is not a valid variable type:",paste0(class(NameTableFile),collapse=", ",sep=""),"\n")
                StopFlag <- stopCntMsg(xmsg)
            } else {    
               # assume it is a character string, take only the first element.
               #cat("NameTableFile:",NameTableFile," class:",class(NameTableFile),"\n")
               
               if ( is.na(NameTableFile) ) {
                  # the value passed is 'NA'
                  StopFlag <- stopCntMsg(paste0("***3206 The NameTableFile parameter is set to 'NA', requires a valid\n",
                                                "        file name or structure.\n"))
               } else {
                  # validate the directory exists and is reference.
                  NameTableFile <- stringr::str_trim(NameTableFile)        # trim spaces.
                  if (nchar(NameTableFile) <= 0) { 
                     # empty parameters  like "" or " ", etc.
                     xmsg     <- paste0("***3207 The NameTableFile parameter is empty. A name table structure\n",
                                        "        or file name must be provided.\n")
                     StopFlag <- stopCntMsg(xmsg)
                  } else {
                     # strip off extention and see what type of file it may be. Also check for .RDA exists.
                     fnSplit       <- stringr::str_split(NameTableFile,"[.]")[[1]]   # split up user provided name.
                     # print(fnSplit)
                     NameTableFileBase <- fnSplit[1]    # base filename only
                     ##cat("FileBase:",NameTableFileBase,"\n")

                     if (nchar(NameTableFileBase) <= 0) {
                        # no filename base...  No characters
                        xmsg <- paste0("***3207 The NameTableFile parameter is empty. A name table structure\n",
                                       "        or file name must be provided.\n")
                        StopFlag <- stopCntMsg(xmsg)
                     } else {
                        if (nchar(fnSplit[2])<=0 || is.na(fnSplit[2])) {
                           # if no extension (missing) - then add .csv
                           ErrorFlag      <- errCntMsg(paste0("***3208 File extension on name table filename is missing.\n",
                                                              "        Assuming .csv type.\n"))
                           NameExt        <- def_NameExt         #  "csv"
                           NameTableFile  <- paste0(NameTableFileBase,".",NameExt)    # make it a CSV file
                           NameTableType  <- def_NameTableType   # 1 - (CSV)
                           NameTablePath  <- paste0(NameTableDir,"/",NameTableFile)
                           #cat("no extension - NameTableFile:",NameTableFile,"  NameTablePath:",NameTablePath,"\n")
                           
                        } else {
                           # if extension is present - must be .csv, .xls, .xlsx, or .RDA
                           #cat("Found file ext of ",fnSplit[2],".\n")
                           NameExt     <- stringr::str_to_upper(fnSplit[2])  # make uppercase so only have to match uppercase versions.
                   
                           NTExtList   <- c("CSV","XLS","XLSX","RDA","RDATA")
                           xm          <- match(NameExt,NTExtList)
             
                           if (all(is.na(xm))) {   # nor VALID TYPE OF FILE - ERROR AND STOP
                              # error - extension must be .csv, .xls, .xlsx, .RDA or .RData.
                              StopFlag <- stopCntMsg(paste0("***3209 The NameTable file is not a .csv, Excel, or R .RDA format.\n"))
                           
                           } else {
                              # have valid match, XM is type (position in table above)
                              NameTableType <- xm
                              #  cat("NameTableType:",xm,"\n")
                              #  extent in NameExt for later use.
                              #  NameExt has validated file extension
                              #  NameTableFileBase has the filename base
                              #  NameTableType is the type of file (1=CSV, 2&3=Speadsheet, 4=RDA)
                              NameTablePath <- paste0(NameTableDir,"/",NameTableFile)
                           }
                           #cat("rebuild 2958 - Path:",NameTablePath,"\n")
                        }#
                     }
                  }
               }
            } ##
         }
      }
      #
      #####
      
      if (StopFlag) {stop()}
      
      ##### 321x
      #
      #   Section 2.1 - NameTableLink call parameter
      #
      def_NameTableLink <- "Link"   # not used.
      
      if (missing(NameTableLink) || is.null(NameTableLink)) {
         NameTableLink <- def_NameTableLink # if not there assign the default
      } else {
         # NameTableLink can now be a multiple or singluar length variable.
         NameTableLink <- NameTableLink[[1]][1]   # get singular version
         NameTableLink <- stringr::str_trim(NameTableLink) # trim blanks
         
         if (!methods::is(NameTableLink,"character") || is.na(NameTableLink) || 
              nchar(NameTableLink) <= 0 ) {
            # NameTableLink is not a character value, is empty ('' or ' '), or NA
            xmsg <- paste0("***3212 The NameTableLink call parameter is an 'NA', Empty \n",
                           "        ('' or ' ') or not a character string. Fix and rerun.\n")
            stopCntMsg(xmsg)
         #} else {
            # Using "Link" or user provided.
            # Can't check if it exist until later.
         }
      }
      #
      #####
      
      cat("***3215 Name Table Link column is:",NameTableLink,".  \n")

      ######  322x messages - 322x - Binary Shape Files
      #
      #  Part 2.2 - ShapeFile - simple character string, filename with no 
      #        extensions or sf structure for the shapefile.  Directory 
      #        should be provided in ShapeFileDir parameter if filename.
      #        Used with readOGR shapefile read, as layer= parameter.
      #
      WorkSf01      <- NULL
      BinaryPassed  <- FALSE
      ShapeFilePath <- NULL
      ShapeFileExt  <- NULL
      ShapeDirValid <- FALSE
      
      if (is.null(ShapeFile) || missing(ShapeFile) ) {
      
         StopFlag <- stopCntMsg(paste0("***3220 ShapeFile parameter has not been provided or is NA.\n"))
           
      } else {
         # see if character string or binary structure (SPDF or sf).
         # The caller could have called and passed the function an SPDF or a sf structure.
         # We are trying to avoid the SPDF structure, 
 
         if (any(is.na(ShapeFile))) {
            StopFlag <- stopCntMsg( paste0("***3220 ShapeFile parameter has not been provided or is NA. ") )
         } else {
            if (!methods::is(ShapeFile,"character")) {
               # not a character string (filename).   Assume it's binary
               #cat("ShapeFile parameter is not characters, check for sf or SPDF structure.\n")
         
               #  Binary Spatial Structure
               
               #  The Binary structure must be either SPDF or a full sf with data.
               #   SP and sfc and sfg structures are not supported.
         
               if (methods::is(ShapeFile,"sf")) {
                  # sf structure with header and all geometric rows
                  #cat("***3221 ShapeFile is a sf structure passed as a call parameter.\n")
                  BinaryPassed <- TRUE
                  WorkSf01     <- ShapeFile
               
               } else {
                  # check for SPDF structures
                  if (methods::is(ShapeFile,"SpatialPolygonsDataFrame")) {
                  
                     #cat("***3222 ShapeFile is a SPDF structure passed as a call parameter.\n")
                     # it is either a SPDF.  In either case convert to sf.
                  
                     ShapeFile     <- sf::st_as_sf(ShapeFile)   # convert
                     BinaryPassed  <- TRUE 
                     WorkSf01      <- ShapeFile
                     
                  } else {   # - not missing or NULL, not NA, not binary or character???
                     StopFlag <- stopCntMsg( paste0("***3223 The ShapeFile call parameter is being used to pass a full \n",
                                                    "spatial structure to the function.\n",
                                                    "        However, the structure must be a SPDF or a sf class. The data was:",class(ShapeFile),"\n") )
             
                  }
               }
            } else {  # is character not binary  -> it must be a filename!!!
               BinaryPassed <- FALSE
               ShapeFile    <- ShapeFile[[1]][1]
               #  Character file names ---  error number 323x error numbers
               # validate the directory exists and is reference
               ShapeFile    <- stringr::str_trim(ShapeFile)           # trim spaces.
               ShapeFileExt <- ""
               if (nchar(ShapeFile) <= 0 || is.na(ShapeFile) || !methods::is(ShapeFile,"character") ) {
                   # ShapeFile is NA, Empty(""), or not a character string
                   StopFlag <- stopCntMsg( paste0("***3227 The ShapeFile call parameter is an NA, empty('' or ' '),\n",
                                                  "        or not a character string. The filename of the Shapefile\n",
                                                  "        must be specified. \n"))                                                 
               }
               #cat("ShapeFile Name:",ShapeFile,"\n")
         
               # handle the problem callers may include extension.
               x <- tools::file_ext(ShapeFile)     # ext last 4 characters of shape file name.
               if (x != "") {
                  if (x == "shp" || x == "shx" || x == "dbf" || x == "prj") { # for ESRI.
                     # extension matches one of the shape file extensions.
                     #  try to find shape file with and without ".shp" extension.
                     ShapeFile     <- tools::file_path_sans_ext(ShapeFile)
                     ShapeFileExt  <- x
                     # strip the extension, but will need one for later file check.
                  }
               } else {
                  #  Have no extension - save a "shp" for later name validation
                  ShapeFileExt <- "shp"
               }
               #
               # If ShapeFile is pointing to a file, then must process the ShapeFileDir 
               # call parameter to find out.  Otherwise, it is ignored.
               #
               ###
                
               ###  322x+
               #
               #  ShapeFileDir - Where the shape file is located. (only if this is a filename and not structure)
               #
               #  Need a default:  1) if NameTableDir present use it.
               #                   2) if not, used BorderGroupDir
               #                   3) if that is not present, set to NULL for the working directory
               #
               def_ShapeDir <- NULL	   # if not present and NULL, use the NameTable Directory
               
               #
               if (missing(ShapeFileDir) || is.null(ShapeFileDir)) {
                  # no directory specified see if NameTableDir exist and use it.
                  
                  if (nchar(NameTableDir) > 0) {
                     # have NameTableDir
                     ShapeFileDir    <- NameTableDir                        # no ShapeFileDir provided, use Name Table Dir.
                  } else {
                     # no NameTableDir - try using BorderGroupDir or binary structure.
                     if (nchar(BorderGroupDir) > 0 ) { 
                        # have border group Directory - use it.
                        ShapeFileDir <- BorderGroupDir
                     } else {
                        # where is the shape file?  Only place left is the working group.
                        ShapeFileDir <- getwd()   # get working directory name
                     }
                     # how to handle no ShapeFileDir with a binary image?  Has no meaning.  The logic says we get here
                     # because we are following the trail to a directory/filename, not binary.
                  }
                  # back filled with other directories
               }
               
               # Have a ShapeFileDir.. 
               ShapeFileDir <- ShapeFileDir[[1]][1]   # pick up only the first value.
               ShapeFileDir <- stringr::str_trim(ShapeFileDir) # trim blanks
               
               if ( is.na(ShapeFileDir) || nchar(ShapeFileDir) <= 0 || !methods::is(ShapeFileDir,"character") ) {
                  # the Shape Directory is NA, empty ("" or " ") or not a character string,  force to use working directory
                  ShapeFileDir  <- NULL            # no directory provided use working directory
                  ShapeFilePath <- ShapeFile
               } else {
                  # validate the directory exists and is reference.
                  ShapeDirValid <- TRUE
                  
                  if (!dir.exists(ShapeFileDir))  {	  ##   msg 3224
                     # ShapeFileDir path does not exist.
                     StopFlag <- stopCntMsg(paste0("***3224 ShapeFileDir directory specified does not exist.\n",
                                                   "        Value is ",ShapeFileDir,"\n"))
                     # stop processing.
                  } else {
                     #  Have a valid existing ShapeFile directory
                     if (!is.null(ShapeFileDir)) {      # real directory.
                        x  <- stringr::str_sub(ShapeFileDir,-1,-1) # strip possible trailing / or \\
                        if (x=="/" || x=="\\") {
                           ShapeFileDir  <- stringr::str_sub(ShapeFileDir,1,-2)
                        } # end of cleaning up directory
                   
                        #  Have valid name and dir -> build ShapeFile path   Dir without / or \\
              			       ### how can this happen???
                        # no directory specified. or supplied, use working directory.
                        # build full pathnames
                        ShapeFilePath <- paste0(ShapeFileDir,"/",ShapeFile)  # path with no ext.
                     } else {
                        ShapeFilePath <- ShapeFile
                     }
                     ShapeDirValid <- TRUE
                     # now that the extensions have been stripped, add it back for the exist test.
                  } # end of directory and path build 
               }
               # put extension back on for file test.
               xS <- paste0(ShapeFilePath,".",ShapeFileExt)  # put full filename back together.
               
               if (!file.exists(xS))    {    ### msg 3225
                  # Shapeliest (dir and name) does not exist.
                  StopFlag   <- stopCntMsg(paste0("***3225 Shape file (dir & name) does not exist. Value=",xS))
               }  # end of existance test for full path.
                
               callVL$ShapeFileDir   <- ShapeFileDir
               callVL$ShapeFilePath  <- ShapeFilePath
               callVL$ShapeFileExt   <- ShapeFileExt
     
               # error or ready to read file.
               #cat("ShapeFileDir:",ShapeFileDir,"  ShapeFilePath:",ShapeFilePath,"  Ext:",ShapeFileExt," \n")
               
            } # error or have binary or BINARYPASSED = FALSE
         } # NA check
      } #  end Missing and NULL check
      callVL$ShapeFile      <- ShapeFile
      callVL$BinaryPassed   <- BinaryPassed
      # have shape file or error.
      
      
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      } 
      
      # end of ShapeFile check
      
      callVL$ShapeDirValid  <- ShapeDirValid
      #cat("ShapeDirValid:",ShapeDirValid,"\n")
 
      #
      #  end Shape file name/directory or SPDF(sf)
      #
      ######
     
      #  At this point, we have a full pathname and file name to a shape file that exist - or - 
      #  The binary image of a spatial structure.   BinaryPassed signals which exists.
      #  If it is a filename, then it is read later and checked.
      
      ######


      ######   323x  - Shape Link name
      #
      #  Part 2.3 - ShapeLinkName
      #
      #  Only need if processing a ShapeFile - file or binary.  Restart - no.
      #
      def_ShapeLinkName <- "NAME"
      ShapeLinkName <- ShapeLinkName[[1]][1]
      
      if (missing(ShapeLinkName) || is.null(ShapeLinkName) ) { 
         ShapeLinkName = def_ShapeLinkName   # the default.
         ErrorFlag  <- errCntMsg(paste0("***3230 The ShapeLinkName call parameter is missing. The default\n",
                                        "        value of 'NAME' will be used.\n"))
      } else {
         # check length
         # ShapeLinkName is present
         if (length(ShapeLinkName) != 1) {
            ShapeLinkName <- ShapeLinkName[1]  # use only first value
         }
         # Length ShapeLinkName is now 1
         if (is.na(ShapeLinkName)) {
            # The value of ShapeLinkName is NA, replace with default value
            ShapeLinkName <- def_ShapeLinkName
            ErrorFlag <- errCntMsg(paste0("***3232 ShapeLinkName value is 'NA'. The default of 'NAME' will be used.\n"))
         } else {
            if (!methods::is(ShapeLinkName,"character")) {
               ShapeLinkName <- def_ShapeLinkName  # replace with default value.
               ErrorFlag  <- errCntMsg(paste0("***3234 ShapeLinkName is not a character string. Value is ",ShapeLinkName,".\n",
                                              "        The default of 'NAME' will be used.\n"))
            }
         }
      }
       
      callVL$ShapeLinkName <- ShapeLinkName
      cat("Shape file data.frame Link column name is ",ShapeLinkName,".\n")
      
      #  End of Shape file
      #
      #######
      
      if (StopFlag) {
   	stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	            "        Please fix problem(s) and retry.\n"))
      }
     
      #
      #  Done Checking the require parameters 
      #
      ######
      
     
      ###### 324x  - MapHdr (1,2)
      #
      #  Part 2.4 - MapHdr  
      # 
      def_MapHdr   <- c("","Areas")
      def_MapHdr_v <- TRUE   # TRUE = set to def, FALSE = user provided base.
      #
     
      if (missing(MapHdr) || is.null(MapHdr) ){
         MapHdr    <- def_MapHdr
         
      } else {
         if (!methods::is(MapHdr,"character")) {
            # MapHdr strings are not characters
            ErrorFlag <- errCntMsg(paste0("***3240 The MapHdr parameter does not contain character strings for\n",
                                          "        use as the column headers. The MapHdr will be ignored.\n"))
            MapHdr       <- def_MapHdr   
            def_MapHdr_v <- TRUE
         } else {
            # MapHdr is characters, check length
            if (!methods::is(MapHdr,"vector")) {
               errCntMsg(paste0("***3242 The MapHdr parameter must be a simple vector type.\n",
                                "        MapHdr is ignored.\n"))
               ErrorFlag <- FALSE
               MapHdr       <- def_MapHdr   
               def_MapHdr_v <- TRUE
            } else {
         
               if (length(MapHdr) == 1) {
                  if (!is.na(MapHdr)) {
                     # character = length = 1
                     MapHdr       <- c("",MapHdr)
                     def_MapHdr_v <- FALSE
                  }
               } else {
                  if (length(MapHdr) > 2) {
                     ErrorFlag <- errCntMsg(paste0("***3244 The MapHdr parameter has zero or more than 2 elements. \n",
                                                   "       Only the first 2 will be used.\n"))
                     MapHdr       <- MapHdr[1:2]
                     def_MapHdr_v <- FALSE
                  } else {
                     # We have MapHdr - length of 2 and character.  
                     if (max(nchar(MapHdr)) > 16) {
                        warning(paste0("***3246 It is suggested the max length of the MapHdr strings be\n",
                                       "        16 characters.\n"),call.=FALSE)
                     }
                     def_MapHdr_v    <- FALSE
                  }
               }
            }
         }   
         # empty parameter - it now optional, fill with default of c("","Areas")  MapHdr[1] is not currently used.
         if (def_MapHdr_v) MapHdr <- def_MapHdr  # if still TRUE, set MapHdr to def value.
         # Optional - NOW
      } 
      callVL$MapHdr <- MapHdr
      if (def_MapHdr_v == FALSE) cat("MapHdr Header Labels used : ",paste0(MapHdr,collapse=", ",sep=""),"\n") 
      #
      ######
      
      ###### 325x  (1)
      #
      #  Part 2.5.1 - MapMinH - minimum height for the micromap
      #
      def_MapMinH <- 1.0
      MapMinMin   <- 0.4
      MapMinMax   <- 2.5
      #
      MapMinH <- MapMinH[[1]][1]
      
      if (missing(MapMinH) || is.null(MapMinH) || any(is.na(MapMinH))) {
         # empty parameter - use default 
         MapMinH         <- def_MapMinH
      } else {
         if (!methods::is(MapMinH,"numeric")) {
            # MapMinH strings are not characters
            ErrorFlag    <- errCntMsg(paste0("***3251 The MapMinH parameter does not contain numeric value.\n",
                                             "        Default Value is used.\n"))
            MapMinH      <- def_MapMinH    # Use default of a def_MapMinH
         } else {
            # Numeric -> Pick only the first element of MapMinH
            MapMinH      <- MapMinH[[1]][1]
            # Check to make sure its within range.
            if (MapMinH < MapMinMin || MapMinH > MapMinMax ) {
               ErrorFlag <- errCntMsg(paste0("***3252 The MapMinH minimum height value is out of range\n",
                                             "        (0.4 to 2.5 inch). The default will be used.\n"))
               MapMinH   <- def_MapMinH
            }
         }     
      }
      callVL$MapMinH <- MapMinH
      #
      ######
      
      ######  325x  (2)
      #
      #  Part 2.5.2 - MapMaxH - Maximum height for the micromap
      #
      def_MapMaxH <- 1.75
      MapMaxMin   <- 1
      MapMaxMax   <- 2.5
      #
      MapMaxH <- MapMaxH[[1]][1]
      
      if (missing(MapMaxH) || is.null(MapMaxH) || any(is.na(MapMaxH))) {
         # empty parameter - use default 
         MapMaxH         <- def_MapMaxH
      } else {
         if (!methods::is(MapMaxH,"numeric")) {
            # MapMaxH strings are not characters
            ErrorFlag    <- errCntMsg(paste0("***3254 The MapMaxH parameter does not contain numeric value.\n",
                                             "        Default Value is used.\n"))
            MapMaxH      <- def_MapMaxH    # Use default of a def_MapMaxH
         } else {
            # Pick up only the first element of the variable
            MapMaxH      <- MapMaxH[[1]][1]
            # Make sure within range.
            if (MapMaxH > MapMaxMax || MapMaxH < MapMaxMin ) {
               ErrorFlag <- errCntMsg(paste0("***3255 The MapMaxH maximum height value is out of range \n",
                                             "        (1 to 2.5 inches). The default will be used.\n"))
               MapMaxH   <- def_MapMaxH
            }
         }
      }
      callVL$MapMaxH <- MapMaxH
      
      ######  Check values, then make sure Min < Max
      # 
      #  Part 2.5.3 - check min and max values
      #
      if (MapMinH > MapMaxH) {
        ErrorFlag <- errCntMsg(paste0("***3257 The MapMinH value must be less than the MapMaxH value.\n",
                                      "        Will swap values.\n"))
        x         <- MapMinH
        MapMaxH   <- MapMinH
        MapMinH   <- x
      }
      MapAvgH   <- mean(c(MapMinH,MapMaxH))
      #
      ######
      
      cat("The Maps minimum and maximum height will be:",MapMinH," & ",MapMaxH,"\n")
      
      if (StopFlag) {
         stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
                     "        Please fix problem(s) and retry.\n"))
      }
         
      ######  326x
      #
      #  Part 2.6 - IDHdr
      #
      def_IDHdr <- c(BGBase,"Areas")   # use the MapHdr as the default
      #
      
      if (missing(IDHdr) || is.null(IDHdr) || (length(IDHdr)==1 && any(is.na(IDHdr))) ) {
         # empty parameter - use the default values.)
         IDHdr     <- def_IDHdr
      } else {
         if (!methods::is(IDHdr,"character")) {
            # IDHdr strings are not characters
            ErrorFlag <- errCntMsg(paste0("***3261 The IDHdr parameter does not contain character strings\n",
                                          "        for use as the column headers.\n"))
            IDHdr     <- def_IDHdr
         } else {
            # IDHdr is characters, check length
            if (!methods::is(IDHdr,"vector")) {
               ErrorFlag <- errCntMsg(paste0("***3262 The IDHdr parameter must be a simple vector type.\n"))
               IDHdr       <- def_IDHdr
               
            } else {
               if (length(IDHdr) > 2) {
                  IDHdr <- IDHdr[1:2]   # keep only the first two elements
                  ErrorFlag <- errCntMsg(paste0("***3264 The IDHdr parameter has more than 2 elements. Only the\n",
                                                "        first 2 will be used.\n"))
               } else {
                  if (max(nchar(IDHdr)) > 12) {
                    warning(paste0("***3266 It is suggested the max length of the IDHdr strings\n",
                                   "        be 12 characters.\n"), call.=FALSE)
                  }
               }
            }
          }
      }
      callVL$IDHdr <- IDHdr
      
      cat("IDHdr header labels:",paste0(IDHdr,collapse=", ",sep=""),"\n")
      #
      ######
      
      ######   327x
      #
      #  Part 2.7 - Reduce PC    (Range:  .01 to 100 percent)
      #    It is not a decimal, but a percent ranging from 0.01 to 100 %
      #
      def_ReducePC   <- 1.25     # % Value is remaining vectors precentage of original.
      #
      ReducePC <- ReducePC[[1]][1]
      
      if (missing(ReducePC) || is.null(ReducePC) || any(is.na(ReducePC))) {
         # empty parameter - use default of 1.25 % keep value
         ReducePC   <- def_ReducePC
      } else {
         ReducePC <- as.numeric(ReducePC)   # if not numeric, will become NA
         if (is.na(ReducePC)) {   # after conversion, if = NA, its was not numeric to start.
            # ReducePC value must be a numeric
            ErrorFlag <- errCntMsg(paste0("***3272 The ReducePC parameter must be a numeric value. The default\n",
                                          "        of 1.25 % will be used.\n"))
            ReducePC  <- def_ReducePC
         } else {
            # ReducePC is a numeric, check length
            if (!methods::is(ReducePC,"vector") ) {
               # never happen...
               ErrorFlag   <- errCntMsg(paste0("***3274 The ReducePC parameter is not simple vector. The default\n",
                                               "        value of 1.25 % will be used.\n"))
               ReducePC    <- def_ReducePC 
            } else {
               if (length(ReducePC) > 1) {
                  #  never happen...
                  ReducePC    <- ReducePC[1]
                  ErrorFlag   <- errCntMsg(paste0("***3276 The ReducePC parameter has more than one value. Only the first\n",
                                                  "        value will be used.\n"))
               } else {
                  if (ReducePC < .0001 || ReducePC >= 100) {  # out of range  (percentage to keep - .0001 TO 100 %)
                       ErrorFlag   <- errCntMsg(paste0("***3278 The value of ReducePC is out of range (0.0001 to 100 %).\n",
                                                       "        The default value of 1.25 % will be used.\n"))
                       ReducePC  <- def_ReducePC
                  }
                  #  The ms_minize call parameter is in ratio from 0 to 1, need to convert
               }
            }
         }
      }
      callVL$ReducePC <- ReducePC
      cat("Map simplification keep value used is:",ReducePC," %. \n")
      #
      ######
      
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
        
      ######   328x
      #
      #  Part 2.8 - LabelCex (Range:  .01 to 10 )
      #
      def_LabelCex   <- .4    # Value is remaining vectors precentage of original.
      #
      LabelCex <- LabelCex[[1]][1]
      
      if (missing(LabelCex) || is.null(LabelCex) || any(is.na(LabelCex))) {
         # empty parameter - use default 
         LabelCex   <- def_LabelCex
      } else {
         LabelCex <- as.numeric(LabelCex)   # if not numeric, will become NA
         if (is.na(LabelCex)) {   # after conversion, if = NA, its was not numeric to start.
            # LabelCex value must be a numeric
            ErrorFlag <- errCntMsg(paste0("***3282 The LabelCex parameter must be a numeric value. The default\n",
                                          "        of 0.25 will be used.\n"))
            LabelCex  <- def_LabelCex
         } else {
            # LabelCex is a numeric, check length
            if (!methods::is(LabelCex,"vector") ) {
               ErrorFlag   <- errCntMsg(paste0("***3284 The LabelCex parameter must be a simple vector. The default\n",
                                               "         value of 0.25 will be used.\n"))
               LabelCex    <- def_LabelCex
            } else {
               if (length(LabelCex) > 1) {
                  LabelCex    <- LabelCex[1]
                  ErrorFlag   <- errCntMsg(paste0("***3286 The LabelCex parameter has more than one value.\n",
                                                  "        Only the first value will be used.\n"))
               } else {
                  if (LabelCex < .05 || LabelCex > 10) {  # out of range  (percentage to keep)
                       ErrorFlag   <- errCntMsg(paste0("***3288 The value of LabelCex is out of range (0.05 to 10). \n",
                                                       "        The default value of 0.25 will be used.\n"))
                       LabelCex    <- def_LabelCex
                  }
               }
            }
         }
      }
      callVL$LabelCex <- LabelCex
      cat("The font multiplier for the map labels is set to ",LabelCex," through the LabelCex call parameter.\n")
      #
      ######
      
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
      #
      ######
            
      
      ######  330x
      #
      #  Part 3.0 - proj4 or proj parameter - micromap projection.  For all of the map.
      #
      #    Done in step (setup 2.2, executed in 6.0)
      #    This parameter can be a PROJ4, PROJ5, or PROJ6 formatted projection.
      #    It can consist of simple string or the structured WKT layout.
      #
      #    Formats: <string>      should convert to Proj4 and WKT structure
      #             <slots with string & WKT structure>
      #
      #  **** New policy: The caller should transform the shapefile to the projection 
      #       they want to use in the micromap maps, then save the shapefile for 
      #       reloading by this function (could make changes to allow the shapefile/SP
      #       data.frame to be passed directly to the function instead of reading it in.
      #       Proj4 is only used when caller provides a long/lat projected shapefile or
      #       one with no proj4string (is.na) and want to override the default
      #       transform based on AEA and the calculated Albers parallels and central median
      #       and center latitude.  
      #

      #
      #  There is no default value for the proj4 parameter.  If it is NULL or NA, it stays
      #  that way until its time to test the projection of the shapefile.  If none Long/Lat,
      #  the proj4 parameter is checked.  If none if provided then the default AEA
      #  projection with calculated parameters is created and used.
      #
      #  Standard normal long / lat projection for the world.
      
      OrigProj <- "+proj=longlat +datum=NAD83 +ellipse=WGS84 +no_defs"   # if needed to fill the geometry.
      
      #
      #  When converted to full projection and wkt, results in the above string and 
      #  the following CRS.
      OrigCRS <- sf::st_crs(OrigProj)    # not sure OrigCRS is needed.

      #
      #  For an example of the full $input and $wkt Coordinate Reference System structure
      #  see section on 'proj4' call parameter above:
      #
      #  In the input format, a long/lat project shows up as "NULL", 
      #    "", "NA" or input="+proj=longlat ..."
      #  In the wkt format, it is not clear if a long/lat projection 
      #    has any specific unique value.
      #
      
      #  The LL tracking flag will be set based on the "input" string in the st_crs value.
      ShpProjLL     <- FALSE  # The shapefile is already a LL projection, 
      # or has no projection - force to LL.
      
      #
      #  The following flags track all of the changes to the projection 
      #  in the map to be processed.
        
      #  general projection flags:
      DoUserProj4   <- FALSE  # user provided proj4 for final projection
      DoModProj4    <- FALSE  # projection in shape file needs to be modified to be meters 
      # (Would like to get all projections in to meter units.
      
      DoBldAEAProj  <- FALSE  # ShapeFile is LL or no projection, 
      #    need to build AEA projection for final transformation
      
      ModProj4      <- NA    # modified ShapeFile projection modified to meters
      #
      #  Projection Status:
      #
      #  Override the default map projection with the users projection..
      #  The projection string is provided in proj4 format and must 
      #  be convert able by CRS to a usable projection.  It must also be 
      #  reversible back to the proj4 string as a validation.
      #
      #  The transformation is done right before printing the maps.
      #  The projection of the maps is returned to the caller.
      #
      #  If present, proj4 call parameter is inspected for LL. If found, not 
      #     permitted and ignored - set to proj4=NULL.  (DoUserProj4<-NULL)
      #  If present, proj4 checked for being non-LL, string inspected.  If +units is not 
      #     meters, set to meters and flaged for transformation. (DoUserProj4) with mod.
      #  If no proj4 -> shapefile projection exist and not LL, inspect for +units = meters. 
      #     Build projection to change to meters, after modifications. (DoModProj4)
      #  If proj4 is missing and shapefile was LL or none, then indicage AEAproj needed.
      #     (DoBldAEAProj)
      #
      #  Before the final projection is done:
      #     a) do shift, scale and rotate adjustments in original units
      #     b) Build AEA projection is needed.
      #     c) Do final projection 
      #         if no proj4 - ModProj4 or AEAProj
      #         if proj4 - proj4
      #
      #     Modifications are done is the original projection and units.  FACT.
      #
      #     Even if they are all AEA, the lat and lon parameter place the area in 
      #     different locations with different 0,0 centroid.  Meters are therefore
      #     different.
      #
      #   enhancement - do scaling with cartograms.
      #   
      #   Upgrade to sf - going with the idea, that the proj4 is just a string and it is 
      #   handled when it is assigned.
      #   proj4 could not have two elements on a list (input and wkt)
      #
      #   hidden in crs class variables are other attrs: units_gdal, IsVertical, 
      #   Wkt, Name, proj4string, epsg, xy, ud_unit, b, units, beyond input and wkt. 
      #
      #   Wkt string usually start with PROJCRS[  or GEOGCRS[ or BASEGEOGCRS[ 
      #
      #   Steps  1) determine what we have proj4, wkt, crs
      #          2) validate and convert into crs
      #              a) proj4(input) to crs - easy
      #              b) wkt to crs - harder (via wkt to proj4, back to crs, and rebuild)
      #              c) crs validate with st_as_text - should the above get run throught this check?
      #     
      #          3) Test for long/lat (flag)
      #          4) Test for meters - if not modify (km) in proj4 parameter and shapefile.
      #          5) a)proj4, b)modfied shapefile, c)AEA
      #
      #  Secrets behind the st_crs or any crs class variable...
      #       $input - the initial string used by st_crs.  Not necessarily the proj4 or proj6
      #       $wkt   - the output wkt value used by the packages.
      #       $ud_unit - the units of the wkt
      #       $proj4string - the proj4 equivalent of the wkt  (generated on the fly)
      #       $epsg  - the epsg code number if known for the wkt
      #     parameters for sfc st_crs(, parameter=xxx)
      #       $SemiMajor, $SemiMinor, $InvFlatterning, $IsGeographic, $units_gdal, $IsVertical,
      #       $WktPretty, $Wkt, $Name, $proj4string, $epsg, $yx, and $ud_unit.  
      #
      #    The assignment must be done as $ or the list class is also transferred.
      #
      #  recommend using epsg numbers
      #
      #  Working with the projections (st_crs) should not involve S2, so we don't have to disable it.
      #
      DoUserProj4 <- FALSE
      Wproj4      <- sf::st_crs(NA)    # crs
   
      if (missing(proj4) || is.null(proj4)) {
         xmsg     <- paste0("***3301 No projection provided in the shapefile or the \n",
                            "        proj4 call parameter, will be set to a\n",
                            "        Long/Lat projection.\n")
         cat(xmsg)
         proj4    <- NA      # proj4 string equivalent and INPUT
      
      } else {
         #  a proj4 call parameter was provided.
    
         Wproj4 <- NA  # output results of checks.

         if (any(is.na(proj4)) ) {
            # proj4 string can not be NA, "" or not a character string.
            cat("***3302 The proj4 call parameter set to NA. \n",
                "        The parameter will be ignored.\n")
            proj4    <- NA
         } else {
            if (!methods::is(proj4,"crs")) {    # test for crs class.
               # not a crs class 
               if (!methods::is(proj4,"character")) {    # test for single variable character string
                  # not crs class or character class.
                  xmsg <- paste0("***3304 The proj4 call parameter is not a valid character string or\n",
                                 "        'crs' structure for a projection. The proj4 parameter will\n",
                                 "        be ignored. The default AEA projection will be used in needed.\n")
                  ErrorFlag <- errCntMsg(xmsg)
                  proj4     <- NA
               } else {

                  proj4 <- proj4[[1]][1]      # Get only first value.
                  proj4 <- stringr::str_trim(proj4)   # Trim spaces.
         
                  #  Have character string. Is it the $input or $wkt part?
                  #  If wkt starts with PROJCRS[, GEOGCRS[, or BASEGEOGCRS[ or other.  
                  #   Best guess.

                  if (proj4 == "" || is.na(proj4) ) {
                     # NA or empty field - force to NA
                     proj4 <- NA
                  } else {
                     # have text in string.  STRING.
                     p7  = stringr::str_sub(proj4,1,7)
                     p8  = stringr::str_sub(proj4,1,8)
                     p13 = stringr::str_sub(proj4,1,13) 
                     p12 = stringr::str_sub(proj4,1,12)
                     p14 = stringr::str_sub(proj4,1,14)
                     p15 = stringr::str_sub(proj4,1,15)
                     if (p7  == "ENGCRS["      ||
                         p7  == "GEOGCS["      || p7  == "PROJCS["      || p7  == "VERTCS["      ||
                         p8  == "GEODCRS["     || p8  == "GEOGCRS["     || p8  == "VERTCRS["     || 
                         p8  == "PROJCRS["     || p8  == "TIMECRS["     ||
                         p12 == "BASEGEOGCRS[" || p12 == "GEODETICCRS[" || p12 == "COMPOUNDCRS[" ||
                         p12 == "VERTICALCRS[" || p13 == "PROJECTEDCRS["||
                         p14 == "PARAMETRICCRS[" || 
                         p15 == "ENGINEERINGCRS[" 
                         # 
                         # sf::st_crs()$units_gdal, sf::st_crs()$proj4string
                        ) # if a valid CRS type.
                     {
                        # based on header characters - it looks like wkt format.
                        # try to insert into $wkt field of crs with NA $input.
                        testcrs        <- NULL
                        #   ensure the order in the crs class
                        testcrs[[1]]   <- NA
                        testcrs[[2]]   <- proj4
                        #   ensure the lists are named properly
                        names(testcrs) <- c("input","wkt")
                        class(testcrs) <- "crs"          # set class.
                        #   Since the input (proj4) is empty, get the proj4 based on the wkt
                        wP4            <- sf::st_crs(testcrs)$proj4string   
                        testcrs$input  <- wP4     # place the proj.4 image in $input
                        Wproj4         <- convertPROJ4(testcrs)      # see if it's good.

                        DoUserProj4    <- TRUE                  # for now we have a user provided proj4.
                        #
                        #   wkt to testcrs, st_crs(testcrs)$proj4string -> newcrs <- st_crs(newInput)
                        #          fills in both input and wkt to match.
                        #   if proj4 (proj4string) modified, then new wkt updates.
                        #
                        #   use st_crs(variable)$ud_unit to get units of crs.  units(xxx)$numerator to 
                        #   get key letters 'm', 'km', '0' (degree), 
                        #
                     } else {
                        # just a string - assume $ input
                        Wproj4         <- convertPROJ4(proj4)   # attempt a $input (proj4string) conversion.
                        # Take proj4 string parameter and check for valid proj4string type variable. 
                        DoUserProj4    <- TRUE
                     }
                     #  get updated proj4 out of "good" crs later.
                  }
               }
            } else {
               # have crs class variable
               Wproj4 <- convertPROJ4(proj4)  # check proj4.

               #  Have a crs structure
               # can only test with st_as_text
            } # end of processing by char or crs
         }  # end if check for NA
      } # end of check for missing or NULL
      
      #  proj4 - stays the same to this point. It is a vector not list.
      #
      #  at this point we have produced or found an error in a string or crs.
      #  proj4 as $input as input, converted to Wproj4 full crs, checked.
      #  proj4 as $wkt   as input, converted to Wproj4 full crs, checked.
      #  proj4 as crs    as input, copied to Wproj4 and checked.
      #  ERRORS reflected in Wproj4 -> [[1]] -> "ERROR"
      #
      #  proj4 is either $input of a valid crs or full warning/error message. 
      #  Wproj4 is always a list of two parts:  $input and $wkt or "ERROR:" and "message".
      #   If the converstion is correct WProj4 is class crs.
      #  Wproj4$proj4string -> proj4 style string, It is generated from wkt.
      #
      #  It is assumed the proj4 as a $input string can be converted back to crs.
      #
      # two stage check
      # What were the results of the checks (look in WProj4)
      #
      if (!is.na(proj4)) {
         # we have a result or valid crs.
         #cat("Wproj4:\n")
         #print(Wproj4)
         
         if (Wproj4[[1]] == "ERROR") {
            # error has occured, not valid.
            # Error found and reported by attempt conversion to CRS.
            xmsg        <- paste0("***3305 Invalid proj4 parameter value provided. \n",
                                  "        Parameter will be ignored.\n")
            ErrorFlag   <- errCntMsg(xmsg)
            proj4       <- NULL
         } else {
            # No error found - pass forward as valid crs.
            # WProj4 contain $input and $wkt values (the crs)
            # Conversion to CRS - looks good.
            proj4       <- Wproj4$proj4string   # better value
            DoUserProj4 <- TRUE
         }
      }
      #  to test for degrees or meters, etc.  the units on $ud_units is class(units) and 
      #  will tell you what it is.   Then the question is how to change it.
      #
      #  At this point - proj4 is the  proj4string image of the $wkt string.
      #                  Wproj4 is the full CRS.  ***
      #
      
      ## two stage check
          
      Save_proj4 <- proj4
      CPproj4    <- NULL
      
      # we are just working on the proj4 to be later used.
      # Step 3 is it longlat? and is it meters?
      if (!is.na(proj4)) {
         # If proj4 calling parameter will not NULL, check to make sure
         # check the +units= keyword to make sure the results will be in meters.
        
         if (is.na(stringr::str_locate(proj4,"\\+proj=longlat")[1]) || !sf::st_is_longlat(proj4)) {    
            # It is not longlat, Is the +units=m or not?
            
            if (is.na(stringr::str_locate(proj4,"\\+units=m |\\+units=m$"))[1]) {  
               # Not longlat and +units not set to meters.
                     
               xmsg <- paste0("***3308 The proj4 call parameter does not have +units=m,\n",
                              "        changing string to meters.\n")
               errCntMsg(xmsg)
               
               matchstr    <- "\\+units=[[:alpha:]]+"     # RegExp string to find +units=???.
                     
               #  find and replace any "+units=<>" string with "+units=m " 
               proj4       <- stringr::str_replace(proj4,matchstr,"\\+units=m ")
               CPproj4     <- sf::st_crs(proj4)   # get the new proj4/wkt
               DoUserProj4 <- TRUE
               cat("New proj4 call parameter is: ",CPproj4$proj4string,"\n")
            }
         } else {
            #  The projection is long/lat...
            xmsg        <- paste0("***3306 The proj4 call parameter specifies a long/lat projection.\n",
                                  "        proj4: ",proj4,"\n",
                                  "        The final projection can't be a longlat projection. \n",
                                  "        An AEA projection will be used, if shapefile does not\n",
                                  "        have a non-long/lat projection.\n")
            ErrorFlag   <- errCntMsg(xmsg)
            proj4       <- NULL
            CPproj4     <- NULL
            DoUserProj4 <- FALSE
         } 
      }
         
      #cat("proj4 results - proj4:",proj4,".  Code 3886 \n")
        
      #  Note if +proj=longlat, there is no units... Only if not LONGLAT
      #  In wkt world its GEOGCRS, DATUM, ELLIPSOID, LENGTHUNIT["metre",1]d
      #     LENGTHUNIT[\"metre\",1] go for this anywhere.  
      #
       
      callVL$proj4      <- proj4
      callVL$CPproj4    <- CPproj4
      callVL$DoUserProj4<- DoUserProj4
       
      # if LL, can't be meters, kill proj4.  Mark for AEA calculation.
      # if not LL, could be meters - check units.  if meters - OK
      #     if not meters - ???   (if km, cm, mm - scaling to meters.)
      #     if miles, or other linear distrances - scale. 
      #     only if ll, can't do anything.
       
       
      if (!is.na(proj4)) cat("Caller requested projection transformation at end of processing sf structure to:\n",proj4,"\n")  
      
      if (StopFlag) {
         stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
                     "        Please fix problem(s) and retry.\n"))
      }
      #
      #  At some the ShapeFile proj4string and the proj4 parameter string will be 
      #    adjusted to make sure +unit= is set to meters.
      #
      #####
      #######
      #########
     
        
      #########
      #######
      #####    33xx
      #
      #   Early read of the ShapeFile.  Need some parameters to handle questions
      #   processing the Name Table.
      #
      #   Starting to work with spatial data.   Disable sf_use_s2(FALSE)
      
      suppressMessages(
         sf_use_s2(FALSE)   # see if warning or error/??
      )

      #######    331x   (Code: 3982)
      #
      #  Part 3.1 - Setup to process ShapeFile and find shapefile 
      #     for areas to use.   (Read file)
      #
      #     Example, find the shapefile containing the counties for a 
      #     State or areas of the border group geographical space. 
      #     All areas must be in the same shapefile. If not, the 
      #     shapefiles must be combined into one shapefile or SPDF 
      #     prior to calling this function.
      #
      #     If there are areas in the shapefile that are not in the 
      #     name table, the caller will be notified that the areas 
      #     in the SPDF will be dropped from the border group.   
      #     No name table row, no boundary data will be kept.
      #
            
      ShpProjLL     <- FALSE  # not LL
      DoBldAEAProj  <- FALSE
      DoModProj4    <- FALSE
      ShapeDriver   <- "ESRI Shapefile"  # default value if nothing found or binary

      if (!BinaryPassed) {
         # Filename passed in the function call
         #   ShapeFile common extensions:   This assumes ESRI Shapefile, but others??
         SFExtList    <- c("shp","shx","dbf", "prj")
         #
         SFDir        <- ShapeFileDir
         # extension on file
         SFExt        <- tools::file_ext(ShapeFile)              
         xm5          <- any(SFExt == SFExtList)
         if (xm5) {
            # without any extension
            SFName    <- tools::file_path_sans_ext(ShapeFile)   
         } else {
            SFName    <- ShapeFile
         }
         #
         #  Reading shapefile from:
         #
         cat("***3311 Reading shape file from\n",
             "        dir(DSN): ",SFDir,"\n",
             "        file(Layer):",SFName,"\n")
         #
         #  Read shapefile into sf structure.
         #   capture the output of the st_read call to get the name of the driver used.
         #
         #  This logic has been upgraded to try the dsn/layer approach, if it fails try the dsn
         #
         ReadFlag = 0      # unknown...
         xText <- utils::capture.output(
            xres1 <- try(WorkSf01 <- sf::st_read(dsn=SFDir, layer=SFName,    # removed quiet= can't capture no output.
                                      stringsAsFactors=FALSE,
                                      check_ring_dir=TRUE,   # check and correct, CCW-Area, CW-hole
                                      as_tibble = FALSE)
                        )
            )
            #print(xres1)
         if (inherits(xres1, "try-error")) {
            cat("Reading Shapefile using dsn/layer - failed.\n")
            # first method failed.
            xText <- utils::capture.output(
              xres2 <- try(WorkSf01 <- sf::st_read(dsn=paste0(SFDir,"/", SFName),    # removed quiet= can't capture no output.
                                        stringsAsFactors=FALSE,
                                        check_ring_dir=TRUE,   # check and correct, CCW-Area, CW-hole
                                        as_tibble = FALSE)
                          )
              )
            if (inherits(xres2, "try-error")) {
               cat("     - using dsn only setup failed. Unable to read shapefile.\n")
               xmsg <- paste0("***3312 The sf::st_read can not import the shapefile as specified. \n",
                              "        The errors reported were :\n",
                              xres1,"\n",
                              xres2,"\n")
               StopFlag <- stopCntMsg(xmsg)
            } else {
               ReadFlag = 1 # indicate dsn setup used.
            }
            #print(xres2)
         } else {
            ReadFlag = 2   # indicate dsn/layer setup used.
         }
         # print(xText)
         #  the "using driver" may not be in the first row of text.
         #  nice to know, but can't use.  Too complicated to figure out how to write.
         lenText <- length(xText)   # array of length of each line in the output
         
         # search for driver name  "using driver" is always at the end of the text line in one of the first lines.
         SReadDriver <- NA
         ic = 1
         while (is.na(SReadDriver) && ic <=lenText) {
            # no driver found and text line available
            # search for "using driver", find begining and end of following string.
            SReadDriver <- stringr::str_sub(stringr::str_trim(noquote(    # trim blanks and remove quotes.
                                 stringr::str_sub(xText[ic],
                                      # look for "using driver" lead and if exists get the end position of the "using driver" string.
                                      stringr::str_locate(xText[ic],"using driver")[1,"end"]+1,-1)
                                      # add one to end position and extract (str_sub) the driver name to end of line.
                               )),2,-2)
                               # trim the resulting string and get rid of quotes.  Then save as SReadDriver.
                     # if no "using driver" field exists, NA is returned and saved, move on to the next string.
            ic = ic + 1
         }
         # got the name of the driver used to read the shape file into the function.
         if (!is.na(SReadDriver)) {
            cat("***3314 Spatial Driver found in boundary data file read was ",SReadDriver,".\n")
         } else {
            SReadDriver = "ESRI Shapefile"
         }
         
         #  as an sf structure there is no @data slot.  All of the attrs are $ including the 
         #  geometry.  The geometry is a sfc_polygon or sfc_multipolygon with one entry per polygon(s).
         #  The number of entries must equal the number of items in each attr array.
         #
      } else {
         cat("***3315 The shape file structure was passed to the function in the call.\n")
         ReadFlag    <- -1                 # -1 = binary image passed in call parameter
         WorkSf01    <- ShapeFile
         SReadDriver <- "ESRI Shapefile"   # force driver name.
         
         #  What type of binary image is it ?
        
         if (methods::is(WorkSf01,"SpatialPolygons")) {
            # SP or SPDF
            WorkSf01 <- sf::st_as_sf(WorkSf01)  # convert SPDF and SP to sf
        
         } else {
            
            # now check for sf types of binary code 
            if (!methods::is(WorkSf01,"sf")) {
               # not full sf structure
               xmsg <- paste0("***3317 The spatial structure passed to the function via the\n",
                              "        ShapeFile parameter is not a SPDF or a sf full structure.\n",
                              "        Please correct and try again.\n")
               StopFlag <- stopCntMsg(xmsg)
            }
            # We want to accept any of these structures as the value of the parameter.
            # Any of these should be converted to sf - here.
         }
         #
         #  Error Checking of binary.
         #
      
      }
      #  have a shape file in sf format.
      if (StopFlag) {
         stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
                     "        Please fix problem(s) and retry.\n"))
      }
      #
      #####
      #
      StopFlag      <- FALSE
      ErrorFlag     <- FALSE
      #
      #####
      
      
      #####  332x  (0-4)
      #
      #  Part 3.2 - Inspect and set projection in shapefile.
      #
            
      WorkSf01_a    <- WorkSf01
      WorkSf01      <- sf::st_make_valid(WorkSf01)   # added 1/3/24
      WorkSfc01     <- sf::st_geometry(WorkSf01)
      
      WorkSf01BBox  <- sf::st_bbox(WorkSf01)   # watch out different format not matrix
      MapBox        <- WorkSf01BBox
      xLim          <- as.numeric(MapBox)[c(1,3)]
      yLim          <- as.numeric(MapBox)[c(2,4)]
       
      WorkSf01Proj4 <- NULL     # proj4string image
      SfCrs         <- sf::st_crs(WorkSf01)    # get projection (user and wkt) (FULL)
      Sfproj4       <- SfCrs$proj4string       # get old proj4
      #print(Sfproj4)     # could be empty or NA or NULL
      
      if (is.null(Sfproj4) || any(is.na(Sfproj4)) || any(Sfproj4 == "")) {
         # if shape file crs (proj4) is empty, NA or NULL 
         # no projection supplied in ShapeFile (sf POLYGONS)
  
         #cat("***3320 The projection field in the shapefile is empty, set to \n",
         #    "      ",OrigProj,"\n")
         
         # Set to generic longlat projection.         
         sf::st_crs(WorkSf01)<- OrigProj            # set empty projection to def L/L
         SfCrs               <- sf::st_crs(WorkSf01)# get current projection string
         Sfproj4             <- SfCrs$proj4string   
         WorkSf01Proj4       <- Sfproj4
         
         ShpProjLL           <- TRUE     # indicate shapefile is longlat
         DoBldAEAProj        <- TRUE     # indicate most likely will need an AEA project built.
         
      } else {
         # not empty or NA.
         #cat("class of Sfproj4:",class(Sfproj4),"\n")   # st_crs is not empty.
         WorkSf01Proj4       <- SfCrs$proj4string      # must assign with $ or else you transfer class.
         # shape file has crs..
         
      }
      WorkSf01Proj4   <- as.character(WorkSf01Proj4)
      SfCrs$input     <- WorkSf01Proj4
      
      #cat("projection check: \n")
      #print(Sfproj4)                 # class crs
      
      #
      #  In this section it was found W <- SFproj4$proj4string transfered the value 
      #  as character and nothing else.  
      #  while W <- SFproj4["proj4string"] transfered the value and the class of 'list'
      #  because it also transfered the name.  
      #
      
      if (bitwAnd(debug,64) != 0) 
          {
            cat("class(WorkSf01Proj4):",class(WorkSf01Proj4),"\n")
            cat("WorkSf01_projection: ",WorkSf01Proj4,"\n")
          }
      # may have a projection string set in the shapefile (SfCrs & Sfproj4),
      # and the proj4 string type in WorkSf01Proj4.  
      
      #####
      #
      #  Need to make sure geometry is valid
      
      
      #####
      #
      # Is it long/lat and what units if not Long/Lat?
      #
      # Is the st_shift_longitude needed?   Crosses the anti-meridian.
      #
      
      LLShift <- FALSE     # no shift required.
      
      # check # 1 is the proj4 long/lat 
      if (is.na(stringr::str_locate(WorkSf01Proj4,"\\+proj=longlat"))[1]) {  
         # if found, START/END are values.  is.na comes up FALSE
         # if not found, START/END are "NA" and is.na = TRUE.
         # the st_crs(sf)$input is not a longlat projection
         #     alternate is to test using st_is_longlat() - later..
         ShpProjLL                    <- FALSE
         DoBldAEAProj                 <- FALSE
         ModProj4                     <- NA
            
      } else {
         # if found, START/END are values.  is.na comes up FALSE
         # the projection in the shapefile is long/lat
         ShpProjLL                    <- TRUE
         DoBldAEAProj                 <- TRUE
         ModProj4                     <- NA

         #  Determine if shift can be used.
         #  Get bbox of map  [MapBox]
         # xbbox                        <- sf::st_bbox(WorkSf01)
         # Get centroid of map
         #    Take centroid of all areas and sum x,y
         #cat("get centroid for shift evaluation.\n")
         #cat("Proj4String:",sf::st_crs(WorkSf01)$proj4string,"\n")
         
         WorkSfc01  <- sf::st_geometry(WorkSf01)   # sfc, but LL still throws a warning.
         suppressWarnings(   
            suppressMessages(
               MapCtr <- sf::st_centroid(sf::st_union(WorkSfc01))   # converted to sfc or sfg
                    # must have valid geometry
            )
         )
         xctr     <- sf::st_coordinates(MapCtr)   # restore xctr to work with original code.
         #cat("Map's Center:",xctr,"\n")
         
         if (all(c(sign(xctr[1])==sign(xLim[1]),sign(xctr[1])==sign(xLim[2]),
                 sign(xLim[1])==sign(xLim[2])))) {
            # If the signs of the X box values and X centroid are the same, then 
            # all of the points are in the same hemisphere - no need for shift.
            LLShift   <- FALSE
         } else {
            # the extreme longitudes or centroid are in different hemsipheres.  
            # cross the 180 or 0 meridian
         
            if (sign(xctr[1])==-1) {
               # sign of X at the centroid is negative -  West Hemisphere.
               # therefore xctr[1]+180 degree is the opposite point and one limit.
               Xopp <- xctr[1] + 180  # a point 180 degrees to the east of the centroid.
               X180 <- 180            # the 180 or East side of the anti-meridian.
                   # if any point(bbox) is between these two limits, then shifting is REQUIRED.
               
               #cat("LL Range:",Xopp,"  ",X180,"   ctr:",xctr[1],"  ",xLim,"\n")
               #  is the X min or max within this range. Pos "+n" to 180 
               #  The point lands in the area between 180 degrees (E) and ctr-180 (or somewhere
               #  between about 45 and 180 degrees.
               
               if (isBetween(xLim[1],Xopp,X180)) {
                  # within range
                  LLShift = TRUE
               }
               if (isBetween(xLim[2],Xopp,X180)) {
                  # within range
                  LLShift = TRUE
               }
               # example - Alaska:  X bbox has -179 to 179 min and max.  Since
               # the difference between these values is about 358, it spans the anti-meridian.
               # The USA centroid X=-96 degrees.  So, the test range would be 180 to 84 degrees.
               # The points in the eastern hemsphere are all positive and would fall in that range.
                              
            } else {
               # sign of X at the centroid is +  East Hemisphere.
               
               Xopp <- xctr[1] - 180    # a point 180 degree west of the centroid.
               X180 <- -180             # the -180 or west side of the meridian
               
               #cat("LL Range:",X180,"  ",Xopp,"   ctr:",xctr[1],"  ",xLim,"\n")
               #  is the X min or max within this range. Pos -180 to "-n" 
               #  if any point is between these two limits, then shifting is REQUIRED.
               #  the point lands in the area between -180 degrees (W) and ctr+180 (or
               #  somewhere between -45 and -180 degrees
               
               
               if (isBetween(xLim[1],X180,Xopp)) {
                  # within range
                  LLShift = TRUE
               }
               if (isBetween(xLim[2],X180,Xopp)) {
                  # within range
                  LLShift = TRUE
               }
            }
         }   
      }
      #if(LLShift) cat("LLShift flag indicating the st_shift_longitude function will be used.\n")
      ###
      
      if (bitwAnd(debug,4) != 0) cat("Proj Flags -ShpProjLL:",ShpProjLL, "  DoUserProj4:",DoUserProj4,
                                     "  DoModProj4:",DoModProj4,         "  DoBldAEAProj:",DoBldAEAProj,"\n")
      #
      #####
            
      ##### 332x  (5-9)
      #
      #  Part 3.2 - Handle Link column in Shape File ; put it away.
      #
      cat("***3325 Checking Shape Link Name Column:",ShapeLinkName,"\n")
      
      WorkSf01Names <- names(WorkSf01)  # get names of shape file data columns
      if (bitwAnd(debug,64) != 0) 
         cat("Shape file data variable names:",paste0(WorkSf01Names,collapse=", ",sep=""),"\n")
      
      #   Make sure "LinkName" exists in the data.frame
      if (!any(ShapeLinkName == WorkSf01Names)) {     # is ShapeLinkName is valid?
         # no = not valid name
         StopFlag <- stopCntMsg(paste0("***3326 The ShapeLinkName provided: ",ShapeLinkName,"\n",
                                       "        does not exist in the shape file data.\n"))
      } else {
         # column present
         #cat("***3327 Shape file link variable name is valid, values will be \n",
         #    "        cleaned up and stored namesin variable X__Link.\n")
   
         shpLinkData               <- stringr::str_squish(as.character(WorkSf01[[ShapeLinkName]]))
	 WorkSf01[[ShapeLinkName]] <- shpLinkData
	 WorkSf01[["X__Link"]]     <- shpLinkData
      }
      
      #
      if (StopFlag) {
   	 xmsg <- paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
   	                "        Please fix problem(s) and retry.\n")
         stop(xmsg)
      }
      #
      #####
      #
          
      if (bitwAnd(debug,8) != 0){
         WorkSfc01 <- sf::st_geometry(WorkSf01)
         OutPutP   <- paste0(BGPathName, "BBG_Raw_shapefile_image.pdf")
         grDevices::pdf(OutPutP,width=10,height=7)
         Sys.setFileTime(OutPutP,Sys.time())
         plot(WorkSfc01,main='',lwd=0.2,asp=1,key.pos=NULL)     # *** CHANGe
         graphics::title("Shape file - Original raw data")
         x <- grDevices::dev.off()
      }  # now done via Sample Prts
      
      #  fill empty projection in shape file
      
      #  The SP, SPDF, and sf are in WorkSf01 is RAW no modifications as a sf structure.
      
      #cat("End of section 3.2 - Code 4364 \n")
      #####
      #######
      #########
      
      
      #########
      #######
      #####
      #
      #  BuildBorderGroup - Read and validate NameTable (90%)
      #
      #  Ready to start processing and build the border group from the 
      #  shape file
      #
      
      #cat("***3500 Name Table Link column is:",NameTableLink,"\n")
 
      ##### 35xx and 36xx
      #
      #  Part 5 - read excel or .csv file and verify it. (columns and data)
      #              Set up of variables
      #
      #  The Name Table starter file must be an .axils, .axils, or an .csv 
      #  formatted file. Once the Name Table is fully built, it will be 
      #  re saved as an .xlsx formatted excel spreadsheet file.  
      #  The Link column and at least one of the following must be 
      #  present in the Name Table spreadsheet:  Name, Abbr, or ID
      #  
      #  The Name Table starter file must have the following columns 
      #  and heading:
      #
      #  "ID" - a numeric reference to the area (example, 
      #    US FIPS codes) This value needs to entered in the 
      #    spreadsheet, CSV file as a character string with leading 
      #    zero.  If leading zeros are not present, they are added 
      #    to make all of the ID values the same length.
      #
      #  "Name" - a character field containing the full length name 
      #    of the area (example, county name = "Jefferson") as would 
      #    be used by a user of their border boundary data set.
      #
      #  "Abbr" - a character field containing the most common 
      #    abbreviation for the area. The abbreviation should be no 
      #    more than 6 characters if at all possible.  The abbreviation 
      #    should be a widely used and accepted abbreviation for the area. 
      #    For Example: the abbreviations for the US states are two 
      #    character abbreviations assigned by the US Postal Service:   
      #    Kansas => KS.  The fewer characters the better.  
      #
      #  "Link" - a character field used to link the Name Table row 
      #    entires to the shape file areas through the "ShapeLinkName" 
      #    column name in the Shape data.frame.  This link is 
      #    also used to group the multiple polygons of an area
      #    under one "polygons" structure (or multipolygons) in 
      #    the spatial structure.
      #
      #    - a character string used to link the Name Table rows
      #    to the "polygon" entries for the area in the 
      #    Spatial Polygons structure created from the shape file 
      #    and simplified and generalized.  The "ShapeLinkName" call 
      #    parameter provides the name of a column in the sf 
      #    data.frame created when the shape file was read into 
      #    the sf data.frame.  The "Link" values in the 
      #    Name Table in each row (representing an area) must match 
      #    the values in the sf$<ShapeLinkName> field.  
      #    Since there may be multiple polygons in the SPDF for 
      #    each area, the important attribute is there are the same 
      #    number of rows in the Name Table as there are unique strings 
      #    in the shape file SPDF identified in the 
      #    sf$<ShapeLinkName> column.
      #
      #  Optional field (columns) for area look up.
      #
      #  Alt_Abbr (Alternate Abbreviation) - a character field containing an alternate 
      #    abbreviation for the area.  In some cases there is not a 
      #    single accepted abbreviation for the areas.  To be able to 
      #    make the border group more usable, an alternate abbreviation 
      #    can be used in the data.  However, the program must be told 
      #    to use the alternative abbreviation instead of the "Name", 
      #    "Abbr", or "ID".  This is done through the rowNames call 
      #    parameter by setting it to "Alt_Ab".  The Name Table must 
      #    have an "Alt_Abbr" column with the values of the second 
      #    set of abbreviations. If you want an alternate abbreviation 
      #    to be available, include it in the Name Table under the 
      #    column name of "Alt_Abbr".  If not column is provided by 
      #    the user, the package will copy the "Abbr" column to the 
      #    "Alt_Abbr" column.  The values in the alternate abbreviation
      #    column should be 6 characters or less, similar to the abbr. 
      #    field.
      #   
      #    We are looking into adding code to automatically match 
      #    the rowNameCol values against the "Abbr", "Name", "ID", 
      #    "Alt_Abbr", and "Alias" Name Table columns to find the 
      #    best fit and report the finding to the user.
      #
      #    When the NameTable is filled out, if there are no 
      #    "Alt_Abbr" values provided, the data in the "Abbr" column 
      #    is copied to the "Alt_Abbr" column.
      #
      #  Alias - a character string that can be used in a pattern match
      #    against the area name string in the data structure. In come 
      #    cases the source of the data table does not output a short 
      #    or reasonable abbreviation or name.  In this case, if each 
      #    area can be tagged by a unique string in the area label
      #    in the data, the "Alias" feature can be used to tie the 
      #    data to an area.  This is a special feature of the 
      #    Name Table and micromapST.  If the starter Name Table 
      #    contains character strings in the "Alias" column, the data 
      #    will be included in the final NameTable data.frame.  To use 
      #    the "Alias" feature, set the calling parameter "rowNames" 
      #    to "Alias".
      # 
      #  Once the Name Table is mostly completed, the function moved 
      #  on to loading the shape file, validating it, cleaning it up 
      #  (cleangeo), Simplifying the polygons to able 0.75% of the 
      #  original size and complexity, and then joins the polygons 
      #  for each area under one "polygon" slot in the SPDF.
      #  
      #  Name Table "Key" column value is assigned each areas' 
      #  collection of polygons in the SPDF/sf and eventually the 
      #  sf row.name.  
      #  A "Key" value can be provided by the caller in the 
      #  Name Table.  It will be used as the "Key" value for the Name Table
      #  and the shape file and Visborder data (later).
      #  If none is provided, the "Abbr" value or one of the following values: 
      #  "ID", "Alt_Abbr", and finally the "Name" column. 
      #  The row.names of the Name Table will be used to set
      #  the "Key" values in all table and the border data for lookups.
      #
      #  It is recommended the NameTable have values for each type 
      #  of reference to an area (Name, Abbr, ID, and Link.) If any column 
      #  is missing, the function attempts to backfill the information 
      #  from the other columns to create a unique value for 
      #  the missing field. The most used references columns are the "Abbr"
      #  and "ID" columns.
      #
      #  Process of backfilling column values when not present:
      #     The process of backfilling missing location identifier 
      #     columns with the "Abbr" column. If it is not present, 
      #     the function looks to see if any of there columns are 
      #     present and uses their value as the "Name", "Abbr", 
      #     "Alt_Abbr", "Alias", "ID", 
      #
      #  If the shape file contains more areas then listed 
      #     in the Name Table, the extra areas will be ignored 
      #     and processing continued.  A list of the omitted areas 
      #     will be outputted to notify the caller.
      #
      #  The micromapST "rowNames" call variable specifies which 
      #  column in the NameTable will be used to match the data 
      #  rows to the geographic areas in the boundary group.  
      #  The rowNames values are "full" (Name), "AB" (Abbr), 
      #  "alt_ab" (Alt-Abbr), "ID" (ID field), and "alias" 
      #  (alias wildcard matching).
      #
      #  If a data row does not match an entry in the NameTable, 
      #  it can not be represented in the linked micro map.  
      #  Any area that does not have data is colored "WHITE" 
      #  to show no data present.
      #
      #  In the creation of the boundary group, the number of 
      #  rows (lines) in the NameTable must equal the number of 
      #  areas in the SpatialPolygonsDataFrame saved in the 
      #  next steps.
      #
      #  The following fields are optional in the name table and 
      #  are used to provide additional functions.  If the feature 
      #  is desired, the following columns must be set by the user.
      #  This routine will not try to create there columns, but will
      #  check their validity.
      #
      # Layer 2 Feature:
      #
      #  The Layer 2 feature, allows the builder to overprint the map with
      #  with a little wider boundary line for areas within a Layer 2 space.
      #  In the US border groups, the Layer 2 boundaries are the state areas.
      #  In this case, the Layer 2 feature just accentuates the state boundaries.
      #  
      #  If a border group has administrative districts that are not regional 
      #  spaces, the Layer 2 feature can be used to outline the administrative
      #  spaces on top of the states, districts or province areas contained in 
      #  the administrative district.
      #
      #  It is recommended that any Layer 2 spaces and their boundaries
      #  be contained in region spaces if the region feature and DataRegionsOnly option
      #  is planned to be used.
      #
      #  To implement Layer 2 boundary overlaying, fill in the L2_ID and L2_ID_Name
      #  columns in the name table.  
      #
      #  L2_ID - numerical ID of one Layer 2 group of areas in the map. 
      #
      #  L2_ID_Name - a character name of one of the Layer 2 group of areas in the map.
      #          It represents the L2_ID value.
      #
      #  The L2 grouping of the areas can consist of one area per L2 group
      #  up to 2 L2 groups per border group.  One L2 group per map is not
      #  allow or useful. The value of the ID is only used to collect and combine the 
      #  boundary data for the L2 group.  Either the L2_ID column or the L2_ID_Name
      #  columns can be used for this purpose. The areas identified by the L2_ID and 
      #  L2_ID_Name should be the same.  Both columns are carried in the 
      #  Name Table for possible future reference.  At this time only the L2_ID
      #  column is used.  If the L2_ID column is missing, the information in the 
      #  L2_ID_Name column will be used to construct the L2_ID column.
      #  
      #  When the builder provides the layer 2 information, the Map.L2Borders flag 
      #  will be set to TRUE to indicate layer 2 information and VisBorder boundary
      #  data is included in the border group.
      #
      # Regions Feature:
      #
      # The Regions feature along with "dataRegionsOnly" feature make use of the 
      # the regID or regName columns in the name table.
      # The "regions" call parameter when set to TRUE, requests that micromapST
      # draw the region boundaries on the micromap.  The region boundaries are
      # drawn with a little heavier line then the standard boundary line.  
      #
      # If the "dataRegionsOnly" call parameter is set to TRUE, then micromapST
      # will only draw regional areas where the user has provided data for an 
      # area within the region.  This aspect of the regional boundary data provides
      # a means of including multiple groups of areas in a border group instead 
      # of having multiple border groups.  For example, The UKIrelandBG border group
      # included with this package has regions of England, Wales, Scotland,
      # Northern Ireland, and the republic of Ireland included in one border group.
      # When the call parameter of "dataRegionsOnly" is set to TRUE, if the data
      # supplied in the micromapST function call only contains entries for areas 
      # in Scotland, then only the Scotland counties will be drawn.  The counties
      # for Wales, England, and the Irelands will be suppressed.
      #
      # The builder can enable this feature by including in the Name Table 
      # either the regID or the regName columns.  Like the Layer 2 boundaries,
      # the regID is the column used by the function. If it is missing, the 
      # function will construct it based on the information provided in the 
      # regName column.  If either column is present, there must be more than 1 
      # region specified.  Having one region per area can be done, but is not very
      # useful.
      #
      #  regID - numeric ID of the geographic region a collection of areas are a member.
      #
      #  regName - character string of the name of the region identified by 
      #    regID.  Again it represents a region a collection of areas are
      #    a member.  The regID and regName list must represent the same collections
      #    of areas in the map.
      #
      #  In the US, states can belong to one of 4 census regions.  Each area
      #  must belong to only one region, identified by regID and/or regName. 
      #  When "regionsB=TRUE" is set in the call, micromapST package will 
      #  draw the regional boundaries with the heavier line over the map.
      #  If the "dataRegionsOnly=TRUE" is set in the call, micromapST will
      #  only draw areas in regions where the user has provided rows of data
      #  in the data data.frame (statsDFrame).
      #
      #  When one of the regional columns are present in the name table, 
      #  The aP_Regions flag in the areaParms data.frame is set to TRUE to 
      #  enable the regions features and indicate the regVisBorders boundary
      #  data is included in the border group.
      # 
      #
      #  When inspected, the following columns were added to complete 
      #  the NameTable:
      #      Key and Link
      #
      #  The "Key" is always set to the content of the "Abbr" field.  
      #  If the "Abbr" field is empty, the "Name" field will be used. 
      #  If the "Name" field is empty, the "ID" field will be used.  
      #
      #  The "Link" field is set to values that allow the function 
      #  to associate each Name Table row with an area in the Shape file.
      #  Once the shape file and the name table are matched up, the Shape file
      #  and Name Table "Link" fields are replaced with a "Key" field containing
      #  the selected key.  In this manner the "Link" values don't have to be 
      #  easy for the micromapST user to remember.  The "Key" values should be.
      #
      #  A good tool for editing and building the NameTable prior to running 
      #  this function is a spreadsheet. The finished worksheet representing
      #  the NameTable must be the first worksheet in the spreadsheet, the 
      #  first row must be the accepted column names, and no rows should be
      #  blank (no data and does not represent an area.) The
      #  spreadsheet can be saved as an .CS, .xls, or xlsx file.
      #
      #  Name Table Modifications
      #
      #  The Name Table also includes a few special columns used in the construction of the 
      #  Border Group but are not used in micromapST.  There fields provide information to 
      #  shift, scale and rotate the location of an area within the map.  
      #  In the U. S. border groups, these parameters were used to size and move Alaska, 
      #  Hawaii, and the District of Columbia and to re-size Rhode Island to make it more
      #  visible.  These columns will not be added to the Name Table, 
      #  but if they are present, they will be validated and used to modify 
      #  the polygons of the associated area.
      #
      #   Xoffset, Yoffset - provide the shift values to move an area to a different location
      #       in the map space.  Since the shift process is done after the map is transformed 
      #       to the AEA projection or the caller has already changed the projection, the units
      #       are in meters.   For Xoffset, a neg value move the area to the left and pos 
      #       value to the right. For Yoffset, a neg value moves the area down and a pos value
      #       up.
      #
      #   Scale  - Scale applys a percentage increase or decrease in size to both the X and Y axis. 
      #       Scaling is done by first converting the coordinates from their normal centroid (xc, yc)
      #       to a normalized 0,0 center. The coordinates are scale in relation to the center of 0,0 and 
      #       then restored to their original centroid of xc, yc.
      #       The values are in percentages.  Value of 100% is no change.
      #       Values < 100% are reduction in size.  Values > 100% are enlargement of area size.
      #       The default is NA.  The valid range is from 0.5% to 200%.
      #
      #   Rotate - The number of degrees to rotate an area, from -360 to 360.  The default is 0.
      #       The rotation is done around the centroid of the area.  After rotating, the area may be 
      #       scaled or moved.
      #
      #  The following optional columns are supported to help manipulate 
      #  the boundary data for an area. The offset moves are applied after 
      #  the transformation of the original SPDF/sf, so its units should 
      #  be in meters.
      #
      #     Xoffset - (-) moves X values to the left and 
      #               (+) to the right 
      #     Yoffset - (-) moves Y values to down and (+) up
      #     Scale   - scaling in percentage (0.01% to 1000%) done around 
      #               the centroid of the area. For example, scaling Alaska 
      #               down by 50% and moving the state from the NW of 
      #               the continental US to just below California to make 
      #               the overall map more compact. The default is 100% or no change.
      #     Rotate  - the number of degree CW to rotate the area.  The valid
      #               range is -360 (CCW) to 360 (CW)  The default is 0 degree rotation.
      #     ModOrder -The modification order to execute the name table 
      #               modifications.  In some cases, the order can 
      #               effect the outcome of the re-combining of the area polygons.
      #               The valid range is 0 to "n".  (not implemented)
      #
      #  
      #  Map Labeling
      #
      #  Since it is possible to relocate an area far enough away from its normally 
      #  recognizable location, three more columns have been added to the name table
      #  to allow the border group builder to specify a very short label and the X, Y
      #  coordinates to draw the label on the map.  In the U.S. border group, labels were 
      #  added for "AK", "DC", and "HI".  No label was used for "RI".  
      #
      #  The name table columns used for this labeling are: "MapL", "MapX", and "MapY" 
      #  for the text label (please keep to a few letters), and the X and Y coordinates.  
      #  The X and Y coordinates should be in the coordinates system used by the 
      #  original shape file. When the areas are shifted, scaled, or rotated, 
      #  these X,Y coordinates are not modified.  The builder should run the 
      #  function and then assess the correct location for the label. 
      #
      #  The "MapL", "MapX", and "MapY" columns may be created by the 
      #  user. The older "MapLabel" name table column has been retired, but 
      #  will be translated into the new columns if seen. This feature is used in 
      #  rare cases, when a label is required for a moved area. 
      #  The labels are only drawn on the first micromap in the graphic.
      #
      #  The old format of the defunct "MapLabel" field is Label,X,Y in quotes, where L is the label
      #  and X and Y are the numerical values for the labels draw coordinates. 
      #  After processing, the "MapLabel" field is deleted leaving the "MapL", "MapX",
      #  and "MapY" fields.
      #
      #  MapLabel - (retired) for each area, MapLabel provides a vector of three values: 
      #    Label and x and y coordinates. When present in the NameTable, micromapST
      #    will draw a label on the first micro map at the oxy coordinates to help
      #    identify moved areas. In the U. S. States border group, extra area labels  
      #    are used for Alaska, Hawaii and District of Columbia, since they were 
      #    moved. The oxy values must be in the same units and orientation as the 
      #    oxy points in the final border group, meters. This options should only be 
      #    used less that 3 times in a border group.
      #    This field should normally be NA_character_. 
      #
      #    The MapLabel field is being retired and replaced by three field
      #    (columns). The present MapLabel triple field is now processed into 
      #    the newer "MapL", "MapX", and "MapY" fields. New implementations 
      #    should use only the three columns. The "MapL" field contains the 
      #    text of the label to be drawn at the "MapX" and "MapY" coordinates.
      #    The coordinates for the label are the same coordinates system used 
      #    by the original shape file.  The X and Y values must take into 
      #    account any shifts, scaling or rotation planed for the area 
      #    being labeled.
      #
      #    If the map coordinates are transformed from the original coordinates,
      #    the label ccordinates (MapX, MapY) also be transformed using a 
      #    SpatialPoints structure.  The original proj4 will be copied to 
      #    the structure. It will then be transformed as did the original map.  
      #    The new coordinates will be then
      #    be returned to the Name Table MapX and MapY field.
      #
      #        MapL  - character, 2 character label for the offset area 
      #                 in the map.
      #        MapX & MapY - numeric x and y coordinates to draw the MapL text.
      #    
      #    The MapLabel information can be provided in the Name Table 
      #    using the "MapL", "MapX", and "MapY" columns. The use of the 
      #    MapLabel column is being retired.  Once the MapL, MapX, and MapY
      #    columns are constructed, the MapLabel column is deleted.
      #
   
     
      #  Any extra columns (fields) in the spreadsheet will be ignored as the nametable
      #  is verified and completed.
      #
      #  To be able to match the most input Name Tables with the Shape file, 
      #  All of the column names in the original Name Table are converted to 
      #  upper case.  Then matched against a list of the column names in upper case.
      #  The matches are returned to the column names with the proper upper and 
      #  lower case letter to match the rest of the software.
      #
      #  Required Columns in csv / xlsw file  or passed as DF (NameTable)
      #
      
      ##### 350x
      #
      #  Variable Setup
      #
      #cat("Name Table Variable Setup - Code 4717 \n")
      UserLinkCol <- c(NameTableLink)           # columns that must be present
      LinkCol   <- c("Link")
      OneCol    <- c("Abbr","Name","ID")        # columns that at least one must be present
      OptCol1   <- c("Alt_Abbr")                # extension to the OneCol list.
      OptCol3   <- c("Alias")
      OptCol2   <- c("L2_ID","L2_ID_Name","regID","regName")  # optional columns to be added.  L2 and Regional Info.
      ManCol    <- c("Xoffset","Yoffset","Scale","Rotate")
      ManOCol   <- c("ModOrder")
      #ProjCol  <- c("Proj","Proj4")                    # optional columns used for adjustments.
      ProjCol   <- NULL
      LabCol    <- c("MapLabel")
      Lab2Col   <- c("MapL","MapX","MapY")
      TotCol    <- unique(c("Key", OneCol, OptCol1, OptCol2, OptCol3, ManCol, ManOCol, 
                            ProjCol, LabCol, LinkCol, UserLinkCol, Lab2Col, "DoAdj"))  # combined list of all columns kept in the NameTable.
      
      MustCol   <- unique(c(LinkCol,OneCol,OptCol1))     # Link, Name, Abbr, ID columns  Plus
      KeyCol    <- unique(c(LinkCol,"Abbr","Alt_Abbr","ID","Name","Alias"))   # possible fields for the Key and in order of selection.
      AltCol    <- c("Link","Name","Abbr","ID","Alt_Abbr","Alias","L2_ID_Name", "regID","regName",
                     "Xoffset","Yoffset","Scale","Rotate","ModOrder","Proj","MapLabel","MapL","MapX","MapY",
                     "Key")
      AltColCaps <- stringr::str_to_upper(AltCol)  # get list of column names in upper case.
      
      StopFlag   <- FALSE
      ErrorFlag  <- FALSE
      
      ######  351x
      #
      # Part 5.1 - read nametable .csv and validate   (or excel spreadsheet)
      #     The type of the NameTable file is determined in the call parameter check.
      #
       
      # select correct code to read name table
      
      #cat("NTPassed:",NTPassed,"\n")
      
      if (!NTPassed) {
      
         #cat("Opening Name Table @ Type:",NameTableType,"  Path:",NameTablePath,"\n")
         
         if (!file.exists(NameTablePath)) {
            # Name File does not exist.
            xmsg <- paste0("***3511 The Name Table Path provided to read the Name Table\n",
                           "        does not exist.\n",
                           "        Value is ",NameTablePath,"\n")           
            StopFlag <- stopCntMsg(xmsg)
            
         } else {
           
            NTable <- NULL
            NTable <- switch(NameTableType,
                         utils::read.csv(NameTablePath),     # .csv
                         readxl::read_xls(NameTablePath),    # .xls    
                         readxl::read_xlsx(NameTablePath),   # .xlsm 
                         load(NameTablePath)                 # .rda
                        )
      
            cat("***3510 The Name Table was read from:",NameTablePath,"\n")
         
            # NameTable stored as .rda file.
      
            if (NameTableType == 4) {  #  a .rda file has been loaded and may contain multiple data.frame.
               # the value of NTable is a list of objects loaded from the load.
               xm <- match("areaNamesAbbrsIDs",NTable)
               if (is.na(xm)) {
                  # a "areaNamesAbbrsIDs" name table was not included in load.
                  # If only one data.frame was loaded, try to use it.
                  if (length(NTable) == 1) {
                     xDfName <- NTable[[1]][1]    # get name of the table.
                     NTable  <- get(xDfName)      # get name of DF read
                     if (!methods::is(NTable,"data.frame")) {
                           StopFlag <- stopCntMsg("***3512 The NameTable in the .rda file is not a data.frame.\n",
                                                  "        Please correct and retry.\n")
                     }
                    # We have good data.frame to use as the NameTable
                  } else {
                     StopFlag <- stopCntMsg(paste0("***3514 There are more than one data.frame in the .rda file provided\n",
                                                   "        for the NameTable. Provide only one data.frame table\n",
                                                   "        in the .rda and retry.\n"))
                  }
                  #  StopFlag == TRUE of we have a data.frame to use as the name table.
               
               } else {
                  #   the areaNamesAbbrsIDs data frame was in the data set loaded - use it.
                  NTable <- areaNamesAbbrsIDs   #  Make he viable old areaNamesAbbrsIDs date frame the working Name Table.
               } 
               # The name table (if present) has been placed in the NTable data.frame
               # Finished with setup up .rds data frame as Name Table.
            }
            # All readings of the Name Table are equal and unknown at this time - equally.
         }  # End of existing path check
      }   
      # if NTPassed, NTable is already loaded with the passed data.frame
      if (StopFlag) {
          xmsg <- paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
                         "        Please fix problem(s) and retry.\n")
          stop(xmsg, call.=FALSE)
         
      }
      StopFlag = FALSE
      #cat("End of Name Table Read - Code 4819 \n")
     
      ##### 352x
      #
      #  Part 5.2 - Validate Name Table 
      #
      #   Check upper case and other variations - Do uppercase, then find match in array and 
      #    get correct upper/lower version for the code.
      #
      
      NTable      <- as.data.frame(NTable,stringsAsFactors=FALSE)   # its now a data.frame
      NTNames     <- names(NTable)
      NTable02    <- NTable
      
      #  Convert column names to the proper upper and lower case
      #  step 1 - make all of them upper case
      NTNamesOrig <- NTNames
      NTNames     <- stringr::str_to_upper(NTNames)
      
      #  step 2 - match them against the AltColCaps list.
      xm          <- match(NTNames,AltColCaps)
      xmna        <- is.na(xm)  # no match
      #  step 3 - replace any matches in the original list.  (no match stay as is.)
      NTNames     <- AltCol[xm]  # update enties that matched.
      NTNames[xmna] <- NTNamesOrig[xmna]   # restore entires that did not match
      
      if (bitwAnd(debug,64) != 0) { 
         cat("Updated Name Table column names: \n")  #  Code 4917 \n")
         print(data.frame(o=NTNamesOrig, n=NTNames))   # if one is wrong it remains upper cast.
      }
      
      names(NTable) <- NTNames  #  up date column names.
      #cat("Name Table Columns:",NTNames,"\n")
      
      if (length(NTNames) <= 0) {
         xmsg <- paste0("***3521 The Name Table has no columns of data.")
         StopFlag <- stopCntMsg(xmsg)
      }
      if (dim(NTable)[1] <= 0) {
         xmsg <- paste0("***3522 The Name Table has no rows or areas.")
         StopFlag <- stopCntMsg(xmsg)
      }
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
      NTable0 <- NTable
      
      #
      #  Must haves:   "Link"  (or one provided by user) and 
      #      at least one "Location Identifier" - "Name", "Abbr", "ID"
      #      Optional ties to data are "Alt-Abr" should only be used 
      #      if "Abbr" is filled and an alternate is needed.
      #      "Alias" is a special identifier to be used as needed.
      #
      
      ##### 353x
      #
      #  Part 5.3 is the Link column still in the table?  (required columns)
      #
      # check "link" column name (provided by user or default)
      #cat("Part 5.3 - Code 4880 \n")
      
      NTable03    <- NTable
      
      NTableLink  <- rep(NA,dim(NTable)[1]) 
      # build empty Link table column.
      #cat("Empty NTlink Table = length:",length(NTableLink),"\n")
      
      xm          <- any(NameTableLink == NTNames)     # is the name in the list.  def="Link"
      if (!xm) {  # error link not present.
         xmsg        <- paste0("***3532 The column specified in the NameTableLink calling parameter\n",
                               "        does not exist in the loaded Name Table.\n")
         StopFlag    <- stopCntMsg(xmsg)               # can't do this test until now.
      } else {
         NTableLink  <- NTable[,NameTableLink]         # get copy of link data
         NTableLink  <- stringr::str_trim(NTableLink)  # clean up.
         # move data to the link column name
         NTable$Link <- NTableLink                     # not a risk of over writing - This is our column.
      }
      
      NTNames       <- names(NTable)
      #NameTableLink <- "Link"   # kill off old information.    Old column no longer required
      
      #cat("Checking name table for required minimum columns.\n")
      
      ###### 354x 
      #
      #  Part 5.4 - In forming the name table, the shapefiles have 
      #      data columns that may have clues as to which could be 
      #      used for each of these. The caller can use this 
      #      information there research to build the name table.
      #
      #  The Caller must provide a name table that has at last 
      #  a Name, Abbr, or ID column.
      #   
        
      #cat("Checking name table to make sure at least one column is Name, Abbr, or ID.\n")
      xmPres    <- sum(OneCol %in% NTNames)
        
      if (xmPres==0) {
         # The key loc ids were not found in the Name Table column list.
        
         xmsg <- paste0("***3542 At least one of the following columns must be present in the\n",
                        "        NameTable file:", paste0(OneCol,collapse=", "),"\n")
         warning(xmsg, call.=FALSE)
         xmsg2 <- "  Please correct the spreadsheet and try again."
         stop(xmsg2,call.=FALSE)
         StopFlag <- TRUE
      }
      
      ###### 355x
      #
      #  Part 5.5 - Remove any columns not in our acceptable list  (can delete the user named link - NOW!
      #
      #cat("Part 5.5 - Code 4934 \n")
      
      xmM          <- TotCol %in% NTNames          # identify good columns
      KeepList     <- TotCol[xmM]
      DelList      <- NTNames[!(NTNames %in% TotCol)]
      if (length(DelList) > 0 ) {
         cat("***3550 The following columns are not needed and will be deleted\n",
             "        from the Name Table :\n")
         print(DelList)
         NTable    <- NTable[,KeepList]           # Clean up what we cant handle
      } 
      
      NTNames      <- names(NTable)
       
      ###### 356x
      #
      #         Columns that should have values and no duplicates.
      #
      #  Part 5.6  - Clean up the data in each column.  Included Link column
      #            Check for NA or "" Values
      #            Check for duplicate Values
      #
      #cat("Part 5.6 - Code 4956 \n")
      
      NoDupCol   <- c(LinkCol, OneCol, OptCol1, OptCol3)   # list of columns that cant have duplicate entries.
      NoDupList  <- NTNames[NTNames %in% NoDupCol]
      #cat("Name Table - The following location id columns will be validated:",paste0(NoDupList,collapse=", ",sep=""),"\n")
      
      for ( inx in NoDupList )  {
         # check for duplicates.  No format check.  Should not be NA or "".
         #cat("Checking Name Table NoDup Column:",inx,"\n")
         Wrk       <- NTable[,inx]   # get data
         Wrk       <- stringr::str_trim(Wrk)
         UniWrk    <- unique(Wrk)
         if (length(Wrk) != length(UniWrk))  {
            # duplicates in list.
            xmsg       <- paste0("***3562 The ",inx," column contains duplicate entries. \n",
                                 "        Correct and retry.\n")
            StopFlag   <- stopCntMsg(xmsg)
         }
         
         if (any(is.na(Wrk)) | any("" == Wrk)) {
            xmsg       <- paste0("***3564 The ",inx," column contains NA or blank values\n",
                                 "        that are not allowed.\n")
            StopFlag <- stopCntMsg(xmsg)
         }
      }
      
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
      #
      ######
      
      ######
      #
      #  Now work on cleaning up the contents of the columns and back fill 
      #  important columns 
      # 
      xm0  <- any("Name" == NTNames)   # is the "Name" column present?
      if (xm0) {   # yes it is
         NTable$Name <- stringr::str_replace_all(NTable$Name,"[[:punct:]]", "")   # remove punctuation from the Name field.
      }
      # this must also be added to filter the row names on the user supplied data.frame
      ###### 357x
      #
      #  Part 5.7 - Backfill to create "Abbr" -> "Name" ->  "ID"  (Location IDs)
      #
      
      #cat("Part 5.7 - Name Table Backfill:  If Abbr is missing, can use Name, ID, Alt-Abbr or Alias VALUES.\n")
      
      ######  355x & 357x
      #
      #  Part 5.7.1 - Abbr - field (# 1)
      #
      xm1     <-  any("Abbr" == NTNames)
      if (!xm1) {
         warning(paste0("***3572 The Abbr column in the Name Table is not included. \n",
                        "        Will attempt to backfill the Abbr from other information.\n"),call.=FALSE)
         #  What can we use as a replacement
         
         xm1    <- any("Alt_Abbr" ==  NTNames)   # is the alternate abbr column present ???
         if (xm1) {
            # Use Alt_Abbr for Abbr
            #cat("Used Alt-Abbr for Abbr.\n")
            NTable$Abbr <- NTable$Alt_Abbr
         } else {         
            # the Alt_Abbr column is not present.
            xm1     <- any("Name" == NTNames)
         	 if (xm1) {
               # Use Name for Abbr
               #cat("Used Name for Abbr.\n")
               NTable$Abbr <- NTable$Name
            } else {            
               # the Name column is not present.
         	    xm1     <- any("ID" == NTNames)
         	    if (xm1) {
                  # Use ID for Abbr	      
         	       #cat("Used ID for Abbr.\n")
         	       NTable$Abbr <- NTable$ID
         	    } else {
                  # ID field is not present
         	       xm1    <- any("Link" == NTNames)
         	       if (xm1) {
                     # Use Link for Addr
         	          #cat("Used Link for Abbr.\n")
         	          NTable$Abbr <- NTable$Link  # $Link may not be Shape$Link
         	       } else {
                     # Link field is not present 	     
         	          xm1    <- any("Alias" == NTNames)
         	          if (xm1) {
         	             # use the Alias as the Abbr
         	             #cat("Used Alias for Abbr.\n")
         	             NTable$Abbr <- NTable$Alias
         	          } else { 
         	             # the Alias column is not present. 
         	             # Nothing left to backfill with.
         	             # This can not happen.  STOP
         	             xmsg <- paste0("***3576 None of the columns needed are present. The Link and\n",
         	                            "        one of the Name, Abbr, and ID column should have been there.\n",
         	                            "        This should never happen with the previous checks.\n")
         	             stop(xmsg,call.=FALSE)
         	          }
         	       }
         	    }
            }
         }
      } else {
        # Abbr field is present - no backfill needed.
        if (bitwAnd(debug,32) != 0) 
           cat("***3573 The Name Table Abbr field is persent-no backfill required.\n")
      } 
      #   Add check for length of abbreviation
      
      xAbbrMax <- max(nchar(NTable$Abbr,keepNA=TRUE))   # get maximum length of Abbr values
      if (xAbbrMax > 6) {
         xmsg <- paste0("***3574 Some of the Name Table Abbr values are longer than\n",
                        "        6 characters. It is recommended the Abbr values be keep short.\n")
         warning(xmsg, call.=FALSE)
      }
      
      #  Abbr field is now setup no matter what.
      
      ######  357x
      #
      #  Part 5.7.2 - Name
      #  
      #cat("Part 5.7.2 - Name Table Backfill:  If Name is missing, can use Abbr, ID, Alt-Abbr or Alias VALUES.\n")
      
      xm1    <- any("Name" == NTNames)
      if (!xm1) {
          # The Name column is not present.
          #cat("no Name field present - attempting to find a usable substitution.\n")
          # Name is not present  - find a backfill
          
          xm2    <- any("Abbr" == NTNames)
          if (xm2) {
             #  Use Abbr for Name
             #cat("Used Abbr for Name.\n")
             NTable$Name <- NTable$Abbr
          } else {
             # Abbr is not present - keep looking
             xm2   <- any("Alt_Abbr" == NTNames)
             if (xm2) {
                # Use Alt_Abbr for Name
                #cat("Used Alt_Abbr for Name.\n")
                NTable$Name <- NTable$Alt_Abbr
             } else {
                # Alt_Abbr is not present - keep looking
                xm2    <- any("ID" == NTNames)
                if (xm2) {
                   # Use ID for Name
                   #cat("Used ID for Name.\n")
                   NTable$Name <- NTable$ID
                } else {                
                   # ID is not present
                   xm2   <- any("Alias" == NTNames)
                   if (xm2) {
                      # Use Alias for Name.
                      #cat("Used Alias for Name.\n")
                      NTable$Name <- as.character(NTable$Alias)    
                   } else {
                      # no Alias field is present
                      xmsg <- paste0("***3576 None of the columns needed are present.  \n",
         	                     "       The Link and one of the Name, Abbr, and ID column \n",
         	                     "       should have been there. This should never happen\n",
         	                     "       with the previous checks.\n")
         	           stop(xmsg,call.=FALSE)
         	        } # end Alias 
                } # end ID
             } # end Alt_Abbr
          } # end Abbr
      #} else {
         #cat("Name Table Name field if present.\n")
      } # end of Name - it exists
      
      #
      #  At this point, Name and Abbr should have valid data 
      #   or was backfilled.
      #
        
      ###### 357x
      #
      #  Part 5.7.3 - backfill Alt_Abbr 
      #
      #cat("Part 5.7.3 - backfill Alt_Abbr \n")
      
      xm2          <- any("Alt_Abbr" == NTNames)
      if (!xm2) {
         # no Alt_Abbr Present, but Abbr is (by now)!
          
         ####### Backfill from Abbr.  If Abbr was not present, 
         #  it was backfilled from name.
      
         #cat("Name Table Alt_Abbr column is not present,",
         #  " will backfilling Alt_Abbr with Abbr.\n")
         
         NTable$Alt_Abbr <- NTable$Abbr # backfill with Abbr
      }
      
      #  Reset NameTable name lists - could have added one.
      NTNames      <- names(NTable)  # refresh the name list
      
      ###### 357x
      #
      #  Part 5.7.4 - backfill Alias
      #cat("Part 5.7.4 - backfill Alias \n")
      
      xm2          <- any("Alias" == NTNames)
      if (!xm2) {
         # no Alias, Backfill from Name
         
         #cat("Name Table Alias column is not present,",
         #   " will backfilling Alias with Name values.\n")
         
         NTable$Alias <- NTable$Name
      }
         
      #
      #  Abbr and Name columns either existed or is now backfilled from
      #    1) Abbr, 2) Name, 3) ID, 4) Alt_Abbr, or 5) Alias.
      #  All exist and are filled user provided info, or backfilled from 
      #  other fields.
      #
      #  Reset NameTable name lists - could have added one.
      #
      NTNames   <- names(NTable)  # refresh the name list
      NumNTable <- dim(NTable)[1]
      #
      #####
      
      ###### 358x
      #
      #  Part 5.8 ID column
      #
      #cat("***3582 Checking ID column in the name table to make sure the values\n",
      #    "        are numeric with leading zeros.\n")
      
      xm1    <- any("ID" == NTNames) 
      if (!xm1) {
         # ID field is not present.  Create the ID column using 
         #  a numerical sequence 1 to n.
         #  Since it must be a numeric field, must fill it in with a 
         #  sequence of numbers.
         xmsg  <- paste0("***3584 The ID column is not present. A numerical sequence number has\n",
                         "        been used to fill the column.\n")
         warning(xmsg,call.=FALSE)
       
         xlen      <- dim(NTable)[1]
         NTable$ID <- (seq(101,101+xlen-1))
      } else {
         # Test that then are numeric
         NTable$ID <- as.numeric(NTable$ID)    # convert to numeric.
         if (any(is.na(NTable$ID))) {
            # some numeric values did not convert.
            # get list of invalid ID
            xmna      <- is.na(NTable$ID)      
            # get row.names for the IDs with problems.
            BadList   <- NTable$Name[xmna]     
            ErrorFlag <- 
               errCntMsg(paste0("***3586 The ID data column is not all numeric values.\n",
                                "        Values will be assigned.\n"))
            # get maximum value to start a extension list.
            xMax      <- max(NTable$ID,na.rm=TRUE)   
            #
            xmTF      <- is.na(NTable$ID)  # get list of bad rows.
            xmNum     <- sum(xmTF)
            NTable$ID[xmTF] <- seq(xMax+10,xMax+10+xmNum)
         }
      }
      
      # ensure all off the IDs are numeric and have the same number of 
      # digits (with leading zeros.)  (Option??) 
      # convert numeric to character
      NTable$ID    <- as.character(NTable$ID)
      # find the max number of characters in the IDs
      NTable$ID    <- stringr::str_pad(NTable$ID,max(nchar(NTable$ID)),
                            'left','0')  
      
      #  Reset NameTable name lists - could have added one.
      NTNames      <- names(NTable)  # refresh the name list
      #
      #####
      
      ##### 359x
      #
      #  Part 5.9 - validate and fill in L2_ID and L2_ID_Name 
      #
      #  L2_ID  variable (level 2 boundaries)
      # 
      #cat("Clean up L2_ID and L2_ID_Name columns if needed.\n")
      #cat("Part 5.9 - L2_ID and Name.\n")
      
      L2Feature = FALSE    # disabled - make me turn it on.
      
      xm2          <- any("L2_ID" == NTNames)
      if (!xm2) {
         # no L2_ID present   - check for L2_ID_Name
         
         xm3       <- any("L2_ID_Name" == NTNames)
         if (!xm3) {
            #  L2_ID not present and L2_ID_Name is not present 
            
            L2Feature = FALSE
            
            #  Create entries one area per L2 area.  Don't build L2 columns.
            #NTable$L2_ID      <- seq(from=1,to=NumNTable)
            #NTable$L2_ID_Name <- paste0("L2_",NTable$L2_ID)
            # do not create two new columns.  Feature Off.
            #
         } else {
            # L2_ID is not present, but L2_ID_Name is.
            NTable$L2_ID_Name <- stringr::str_trim(NTable$L2_ID_Name)
            if (any(is.na(NTable$L2_ID_Name)) | any(NTable$L2_ID_Name=="")) {
               xmsg <-paste0("***3590 Name Table L2_ID_Name column contains row(s) with NAs or '' \n",
                             "        values.  The L2 feature is disabled.\n")
               ErrorFlag <- errCntMsg(xmsg)  # not all of the L2_ID_Names are present. ERROR
               L2Feature <- FALSE
            } else {
               # have a full list of L2_ID_Names
               # get unique list.
               uL2_ID_Name       <- 
                   unique(sort(stringr::str_trim(NTable$L2_ID_Name)))
               # get an ID value
               tempL2_ID         <- match(NTable$L2_ID_Name,uL2_ID_Name)
               # create L2_ID
               NTable$L2_ID      <- tempL2_ID                              
               # use L2_ID_Name for any pattern and repeats. Then copy 
               # that numerically into L2_ID
               L2Feature         <- TRUE
            }
         }
      } else {
         # L2_ID is present, but no L2_ID_Name column
         NTable$L2_ID  <- stringr::str_trim(NTable$L2_ID)
         # we got all we need.
         if (any(is.na(NTable$L2_ID)) | any(NTable$L2_ID == "")) {
            # have empty or na rows in L2_ID
            xmsg <- paste0("***3592 Name Table L2_ID column contains row(s) with NAs or '' \n",
                           "        values.  The L2 feature is disabled.\n")
            ErrorFlag <- errCntMsg(xmsg)
            L2Feature = FALSE
         } else {
            # Have good set of L2_IDs.
            L2Feature   <- TRUE
            xm3   <- any("L2_ID_Name" == NTNames)
            if (!xm3) {
               # no L2_ID_name   
               # NTable$L2_ID_Name <- paste0("L2_",NTable$L2_ID)  # don't create L2_ID_Name
               x <- 1   # don't create new column.
            } else {
               # Both exist - leave them along.
               NTable$L2_ID_Name <- stringr::str_trim(NTable$L2_ID_Name)
               # are they the same pattern?  We go by L2_ID anyway - no need to check.
            }
            #  Check for only 1 L2_ID - not allowed.
            L2Num  <- length(NTable$L2_ID)
            UL2Num <- length(unique(NTable$L2_ID))
            if (UL2Num == L2Num || UL2Num == 1 ) {
               L2Feature = FALSE   # turn it off.
            }
         } 
      }  
      #  Reset NameTable name lists - could have added one.
      NTNames <- names(NTable)  # refresh the name list

      ###### 359x
      #
      #   Part 5.9 - Validate RegID and RegName
      #
      #cat("Clean up regID and regName columns.\n")
      RegFeature = FALSE
      
      xm7 <- any("regID" == NTNames)
      if (!xm7) {
         # no regID present
         xm8 <- any("regName" == NTNames)
         if (!xm8) {
            # no regName and regID are present  - one region per area.
            #NTable$regID   <- seq(from=1, to=NumNTable)
            #NTable$regName <- paste0("RegN",NTable$regID)
            RegFeature      <- FALSE       # disabled.  don't create new columns
            #
         } else {
            # regNames are present, create regID
            NTable$regName <- stringr::str_trim(NTable$regName)
            if (any(is.na(NTable$regName)) || any(NTable$regName == "")) {
               # have missing values in the regNames
               xmsg <- paste0("***3596 Name Table regName column contains row(s) with NAs or '' values.\n",
                              "        Regions feature is disabled.\n")
	       ErrorStop  <- errCntMsg(xmsg)
               RegFeature <- FALSE  #  can't do the region feature
            } else {
               # no regID present but the regName column is and it is complete.
               uRegName          <- 
                      unique(sort(stringr::str_trim(NTable$regName)))
               tempRegID         <- match(NTable$regName, uRegName)
               NTable$regID      <- tempRegID
               # use regName to set the pattern for the regions
               RegFeature        <- TRUE
            }
         }
      } else {
         # regID present. 
         NTable$regID <- stringr::str_trim(NTable$regID)
         # regID are present.
         if (any(is.na(NTable$regID)) || any(NTable$regID == "")) {
            # have missing values regIDs.
            xmsg       <- paste0("***3598 Name Table regID column contains row(s) with NAs or '' values.\n",
                                 "        Regions feature is disabled.\n")
	    ErrorStop  <- errCntMsg(xmsg)
            RegFeature <- FALSE   # disable RegFeature
	 } else {
            xm7 <- any("regName" == NTNames)
            if (!xm7) {
               # but no regName  - choose to not create regNames column, not needed.
               #NTable$regName <- paste0("RegN",NTable$regID)  # not done.
               x <- 1  # don't create more columns.
            } else {
               # both columns are present - nothing to do..
               NTable$regName <- stringr::str_trim(NTable$regName)  # clean up column
            }
            RegFeature = TRUE  # good regID list - enable 
         }   
      } 
      #  Reset NameTable name lists - could have added one.
      NTNames <- names(NTable)  # refresh the name list
      #print(NTable[,c("L2_ID","L2_ID_Name","regID","regName")])
      #
      #####
      
      ##### 3599
      #
      #  Do we activate the regional feature?
      #  Must have more than one region id in list.
      #  If the number of unique region IDs = 1 or the number of 
      #  region IDs equals the length of the name table, then
      #  disable the RegFeature - There are no regions to speak of.
      #
      if (RegFeature)  {
         # the regional feature has been enabled, see if this is valid.
         RegNum    <- length(NTable$regID)          # number of entries in name table
         x         <- length(unique(NTable$regID))  # number of unique regIDs.
         if (x == 1 || x == RegNum) {
            # only one region or the number of regions = number of rows(areas) in name table
            RegFeature   <-  FALSE
            xmsg <- paste0("***3599 The Region Feature has been disabled. The number of\n",
                           "        regions defined is as either 1 or is equal to\n",
                           "        the number of areas.\n")
         }
      }
      #
      #####

      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above. Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
      
      #####
      #
      #  Assorted columns
      #
             
      ##### 361x   #####
      #
      #  Part 6.1 - Validate Key and Set Correctly.
      #
      #  Key
         
      xm9    <- any("Key" == NTNames)
      if (!xm9) {
         # no Key field provided.
         #cat("Backfilling Key with Abbr.\n")
         NTable$Key <- NTable$Abbr   # use name field or backfill
      }
      NTNames <- names(NTable)  # refresh the name list
      row.names(NTable) <- NTable$Key
      
      if (bitwAnd(debug, 4) != 0) {
         cat("Proj Flags - ShpProjLL:",ShpProjLL, "  DoUserProj4:",DoUserProj4,
              "  DoModProj4:",DoModProj4,  "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
      #
      #####
     
      #####
      #
      #    Adjust Name, Abbr, Alt_Abbr, ID, and Key values.
      #
      
      #
      #####
      #cat("Code 5436 - Name Table after Name, Abbr, ID, L2 and Rg columns checked.\n")
      #print(NTable)
      
        
         
      ##### 362x
      #
      #  Part 6.2 - Modification Column Checks and builds 
      #           (Valid Parmeters) MUST WAIT.
      #
      # Other Column has a range associated with it.  The table is 
      # based on the units in the original ShapeFile. Can't depend 
      # on ShpProjLL at this moment.
      #
      # Should we allow these columns to be miss cased and forced to 
      # UPPER case to process.
      #

      WorkSf01BBox  <- sf::st_bbox(WorkSf01)   # linear vector X,Y,X,Y
        
      ManCol    <- c("Xoffset","Yoffset","Scale","Rotate")
      
      if (diff(WorkSf01BBox[c(1,3)]) <= 360 ) {    # difference in X values (long)
         #  best guess is it is longlat.
         ModCols <- data.frame(n=ManCol, 
              low = c(-360, -360,0.0001,-360), 
              high= c(360,360,10,360), 
              def = c(0,0,1,0))
      } else {
         # Otherwise it is meters, km, etc.
         ModCols <- data.frame(n=ManCol, 
              low = c(-24000000, -24000000,0.0001,-360), 
              high= c(24000000,24000000,10,360), 
              def = c(0,0,1,0))
      } 
      row.names(ModCols) <- ModCols$n
      
      NTable     <- as.data.frame(NTable,stringsAsFactors=FALSE)
      NTNames    <- names(NTable)
      #cat("Prior to column check - 5536\n")
      #print(NTable)

      # As a coding safety measure, if a modification column does 
      # not exist, one will be created and filled with NA.
      
      xm         <- !(ManCol %in% NTNames)    # which modify columns don't exist
    
      # Fill in default values for missing columns.
      for (ntc in ManCol[xm]) {
          def  <- ModCols[ntc,"def"]
          #cat("column:",ntc,"  def:",def,"\n")
          NTable[,ntc] <- def
      }     
    
      xm         <- !xm
      
      # list of columns to check-The others were just added.
          
      Def_ModVal     <- rep(FALSE,dim(NTable)[1])
      # One per row.  Each valid operation forces a TRUE.
      NTable$DoAdj   <- FALSE   # All of the rows. 
      
      NTableRN       <- row.names(NTable)   # list of rows to check
      #1 get name of row to work on.  Cycle through rows.
      for (inxRN in NTableRN) { 
         AllDoMod  <- FALSE    # initially - no work to do for this row.
         # check to see if NTable row is set to the defaults 
         # - no need to check further.
     
         for (inx in ManCol) { 
            #   Check each value for row for this modifier.
            DoMod       <- FALSE            
            ModDef      <- ModCols[inx,"def"]    
            
            WrkVal <- NTable[inxRN,inx]  # a value to inspect.
            if (is.na(WrkVal)) { 
                 WrkVal <- ModDef     # NA - next value Set Def.
                 next
               }
            if (WrkVal == "") {
                 WrkVal = ModDef
                 next      # "" - next value
               }
            if (WrkVal == ModDef) {
                 next  # default - next value
               }
            # values of NA and "" are set to the defaults.
            
            #cat("Checking Name Table Manipulation Row-Column:",inxRN,
            #    " - ",inx,"\n")
            # cycle through each value for each row
            
            # set up for the specific test for this column  # warning - st_shift
            LLim        <- ModCols[inx,"low"]    
            HLim        <- ModCols[inx,"high"]
                      
            #cat("Mod Table:",inx," value test-Low:",LLim,", High:",HLim,
            #    ", def=",ModDef,".\n")
            # it is not one of the no action values, check it for real.
            
            WrkVal      <- stringr::str_trim(WrkVal)  # Clean up value
            WrkVal2     <- as.numeric(WrkVal)
            #cat("inxRN:",inxRN,"  inx:",inx,"  WrkVal:",WrkVal2,"\n")
 
            if (is.na(WrkVal2)) { #3 it is not a numeric value.
               xmsg <- paste0("***3622 The Name Table in the ",inxRN," area row and in the \n",
                              "        ",inx," column is not numeric. \n",
                              "        It has a bad value of: ",WrkVal,"\n",
                              "        Value set to the default. Fix and retry.\n")
               ErrorFlag <- errCntMsg(xmsg)
               WrkVal2   <- ModDef
            } else { #3
               # good numeric - now check range.
               # Range Check.
               if (WrkVal2 > HLim || WrkVal2 < LLim) { #4
                  xmsg     <- 
                     paste0("***3624 Data in row : ",inxRN," for ",inx," parameter\n",
                            "        is ",WrkVal2," and out of range. (",formatC(LLim,format="f",width=4)," to ",formatC(HLim,format="f",width=4),")\n")
                  StopFlag <- stopCntMsg(xmsg)
               } else { #4
                  # good number 
                  DoMod <- TRUE   # good value
               } #4
            } #3
            
            if (DoMod) { #5
               AllDoMod <- ( AllDoMod || DoMod )
            } #5
         } #2  Default or Validate the row?.
         NTable[inxRN,"DoAdj"] <- AllDoMod
      } #1 loop through each row (area)
      
      #
      NTNames  <- names(NTable) ###  Code: 5511 - Point a
      
      #
      #####
      
      if (StopFlag) {
   	 stop(paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
   	             "        Please fix problem(s) and retry.\n"))
      }
      
      #
      #####
      
      NTable3 <- NTable
      NTNames <- names(NTable)
       
      ##### 363x  (0-4)
      #
      #  Part 6.3 - MapLabel validation - two formats
      #      MapLabel="AK,1,2"   or MapL="AK", MapX=1, MapY=2
      #
      #      Note -MapL should really not have the string quoted.
      #      But since it could be, we will remove any quotes.
      #
      #  Check for the MapL,MapX,and MapY format and columns first.
      #
      
      #
      #  Part 6.3.1 - MapL, MapX, and MapY Name Table columns
      #
      #  If MapL is present with valid MapX and MapY, ignore 
      #  MapLabel column - retired.
      #
      #  Setup up for MapL, MapX, MapY, conversion of MapLabel 
      #  and then validation
      #
      MapLData <- FALSE     # no MapL data, may be MapLabel data.
      NTable1  <- NTable
             
      nrNT     <- dim(NTable)[1]
      
      xMapL    <- rep(NA,nrNT)
      xMapX    <- xMapL
      xMapY    <- xMapL
      
      #
      #  Do we have MapL columns?
      #
      im8      <- any("MapL" == NTNames)        # is MapL column present
      
      if (im8) {
         # MapL present, validate MapL, MapX, and MapY.  
         # If good, ignore MapLabel.
         # remove head and tail white space.
         MapL           <- stringr::str_trim(NTable$MapL)   # get a copy for processing
         MapL[MapL==""] <- NA_character_    # no label set to NA
         MapL1          <- str_remove_all(MapL,'\"')
         MapL           <- str_remove_all(MapL1,"\'")
         NTable$MapL    <- MapL
         MapLNA         <- rep(NA_character_,length(NTable$MapL))
         
         if (all(is.na(MapL) | MapL=="")) {
            # MapL column is empty  - don't have to check MapX and MapY   
            # - go check for MapLabel.
            MapL <- MapLNA  #  set all to <NA>
         } else {
            # Validate the MapL  - at least one is present
            # methods::is(NTable$MapL,"character") is against the entire column, not entry.
            
            xmNoNA    <- !is.na(NTable$MapL)          # not an <NA>  * (old MapLs) have label?
            if (!methods::is(NTable$MapL,"character")) {  # the column is character..  step 1
              # column not character - logic or numeric
              xmsg        <- paste0("***3630 The MapL column in the name table is not character data.\n",
                             "        Labeling will not be done.\n") 
              ErrorFlag   <- errCntMsg(xmsg)
              NTable$MapL <- MapLNA
            } else {
              # possible labels are in rows xmNoNA = TRUE
              
              im9 <- (any("MapX" == NTNames) && any("MapY" == NTNames) ) 
              if (!im9) {    # do we have MapX and MapY columns - no one or the other is missing.
                 # one of the companion columns is missing.
                 xmsg   <- paste0("***3631 If the MapL column is present with label(s), then MapX and\n",
                                  "        MapY must be present.  One or the other is missing.\n",
                                  "        Labeling is not done.\n")
                 ErrorFlag    <- errCntMsg(xmsg)
                 NTable$MapX  <- NA
                 NTable$MapY  <- NA
                 MapL         <- MapLNA   # fix it later.
              } else {
                 # have both needed x,y coordinates columns
                 #suppressWarnings(
                    NTable$MapX  <- as.numeric(NTable$MapX)
                 #)
                 #suppressWarnings(
                    NTable$MapY  <- as.numeric(NTable$MapY)
                 #)
                 if (all(is.na(NTable[xmNoNA,"MapX"]))) {
                    # the MapX values are all missing or invalid.
                    xmsg <- paste0("***3632 MapL value is present and there are no valid MapX coordinates.\n",
                                   "        Labeling will not be done.")
                    ErrorFlag <- errCntMsg(xmsg)
                 } else {
                    # we have MapX
                    if (all(is.na(NTable[xmNoNA,"MapY"]))) {
                       # The MapY values are all missing
                       xmsg <- paste0("***3633 MapL is present and there are no valid MapY coordinates.\n",
                                      "        Labeling will not be done.\n")
                       ErrorFlag <- errCntMsg(xmsg)
                    } else {
                       # We do have MapX and MapY
                       # step through the values for final validation
                       # xmMap has TRUE set for each row with MapL
                       MapList  <- seq(1,length(NTable$MapL))[xmNoNA]   # get list of rows with MapL labels.
                       # MapList should be list of index numbers of name table rows.
                       #cat("MapList:",MapList,"\n")
                        
                       for (inx in MapList) { 
                          # Step through list of row index and check it out
                          xxMapL <- NTable[inx,"MapL"]
                          #cat("MapL:",xxMapL," len:",nchar(xxMapL),"\n")
                          
                          # label for error messages
                          NTN <- NTable[inx,"Name"]        # get name field for message.
                          
                          if (nchar(xxMapL) > 3) {
                             # too long a label > 3 chars
                             xmsg <- 
                                paste0("***3634 The MapL label - ",xxMapL," -  for area ",NTN," should be 3 or less\n",
                                       "        characters to be usable.\n")
                                ErrorFlag <- errCntMsg(xmsg)
                          } else {
                             # build table of just the MapL labels.
                             MapLData <- TRUE   # have good MapL, X, and Y.
                          }
                       }
                       # it is all stored in the Name Table - validate later.
                    }
                }
              }
            }
         }
         #NTable$MapL <- MapL   # restore MapL values to Name Table  (Done above)
      }
      #
      #  End of MapL, MapX, and MapY processing
      #  Have information in NTable data.frame
      #
      #####
    
      #cat("Status of MapLData:",MapLData,"\n")
      ##### 363x  (5-7)
      #
      #  Part 6.3.2 - MapLabel - and break it up into three fields 
      #        (MapL, MapX, MapY)
      #
      #  The format for the MapLabel Name Table entry is a character 
      #  string containing three character values separated by commas.
      #  Example:   "AK,3,5"  where the first field in the label to 
      #    be drawn (text) and the second and third fields are the 
      #    x and y coordinates to drawn the label.
      #
      #  If MapL was present and valid, the MapLabel call parameter 
      #  will be ignored in favor of the MapL, MapX, and MapY 
      #  columns in the Name Table.
      #
      #  The x,y coordinates used to draw the label must be in the 
      #  same coordinates system and units as the original shape file.  
      #  The points are used to create a SpatialPoints structure.
      #  If the maps is transformed, the points for the Map labels 
      #  are also transformed and NTable is updated.
      #  
      
      if (!MapLData) {
         # if TRUE, then we already built MapL, X, and Y. Skip MapLabel processing.
         #
         # No MapL, MapX, MapY columns present.  Thus no MapLData 
         # processed.  Can have one or the other, but not both.
           
         # is the MapLabel column present?  
         xm8 <-  any("MapLabel" == NTNames)  
     
         # Clear out result columns.
         NTable$MapL     <- NA
         NTable$MapX     <- NA
         NTable$MapY     <- NA
                   
         if (!xm8) {
            # no MapLabel field (column)  create the column and 
            # fill with NA
            
            #cat("no MapLabel, Do not build column\n")
            NTable$MapLabel <- NULL  # make sure it is removed.
            # no MapLabel parameters in Name Table..
        
         } else {
            # Column is present - check for any content
            
            # clean up all strings in column
            NTable$MapLabel <- stringr::str_trim(NTable$MapLabel)  
            
            if (all(is.na(NTable$MapLabel)) || 
                       all(NTable$MapLabel == "")) {
               # column is empty - skip processing it.
               x <- 1
               #cat("***3635 No MapLabel content - processing skipped.\n")
               
            } else {
               # there should be some entries to validate - usually 
               # no more than 6 or so.
               
               for (inxRN in row.names(NTable)) {           
                  # Scan each Name Table row.
                  # check for format of each entry.  if a true 
                  # MapLabel entry, move data to MapL, MapX, MapY.
                  
                  # a copy of the single value
                  WrkVal <- NTable[inxRN,"MapLabel"] 
                 
                  #  Check for the default - no action values.
                  if (is.na(WrkVal)) next   # MapLabel = NA
                  if (WrkVal=="")  {
                      WrkVal = NA
                      next   # MapLabel = ""
                  }
                  # Parse the MapLabel - three components.
                  
                  # parse first field nchar > 1 - use only the first value
                  WrkValV   <- WrkVal[[1]][1]      
                  #cat("WrkValV:",WrkValV,"\n")
                  
                  # one element / split into many
                  WrkValV   <- 
                      stringr::str_split(gsub("\"","",WrkValV),",")[[1]]
                  
                  # Now check for number of items and type
                  #cat("WrkValV - split:",
                  #   paste0(WrkValV,collapse=", ",sep=""),"\n")
                 
                  if (methods::is(WrkValV,"vector") && length(WrkValV)==3 ) {   
                     # Yes, value must be a vector of three items
                     # Check each out the three values, none can be 
                     # blank or NA
                     WrkValV <- stringr::str_trim(WrkValV)
                     if (any(is.na(WrkValV)) || any(WrkValV == "")) {
                        # Empty entry one of the values is blank or NA
                        xmsg <- paste0("***3636 Some of the items in the MapLabel entry for ",inxRN,"\n",
                                       "        are NA or blanks. Will be ignored.\n")
                        ErrorFlag <- errCntMsg(xmsg)
                     } else {
                        # all entries contain data.
	             
                        NTable[inxRN,"MapL"] <- 
                           noquote(as.character(WrkValV[1]))
                        NTable[inxRN,"MapX"] <- 
                           as.numeric(as.character(WrkValV[2]))
                        NTable[inxRN,"MapY"] <- 
                           as.numeric(as.character(WrkValV[3]))
                        MapLData <- TRUE
                        # Validate it later.
                     }
                  } else {
                     # the variable is not a vector and does 
                     # not have 3 elements.  Something is missing.
                     xmsg <- paste0(
                             "***3637 The MapLabel value for ",inxRN," is not valid.\n",
                             "        Must be a character string with three values separated\n",
                             "        by commas. The value is ignored.\n")
                     ErrorFlag <- errCntMsg(xmsg)               
                  }
               }  # end of for loop
            }
            #NTable$MapLabel <- NULL    # MapLabel converted, delete.
         } 
         NTNames <- names(NTable)  # refresh the name list
      }   
      # Finished with MapLabel and MapL, MapX, and MapY initial 
      #   processing and validation.
      #
      #####
      
      ##### 363x  (8-9, 40)
      #
      #  Part 6.3.3 - data converted into MapL/MapX/MapY format.
      #    Now validate the data format and ranges.
      #
      #  If we found any information under MapLabel or MapL, 
      #  then we need to validate it NOW
      #
      NTable4 <- NTable
            
      if (MapLData) { 
         # Validate the MapL, MapX, and MapY information from
         # MapL columns or resulting from the MapLabel column.
         
         # find all rows with MapL information
         xmMap    <- !is.na(NTable$MapL)          

         # get the access names (abbr) for entries
         MapRN    <- row.names(NTable)
         MapRN    <- MapRN[xmMap]        
         #  multiplier
         MapMul   <- matrix(c(.66,.66,1.5,1.5),ncol=2)   # AGAINST the bbox numbers
         # increase the values.
         MapBox2  <- MapBox * MapMul      # MapBox is from WorkSf01 when it was loaded.
                 
         for (inxRN in MapRN) {
            # the variables for this area
            xMapL <- NTable[inxRN,"MapL"]
            xMapX <- NTable[inxRN,"MapX"]
            xMapY <- NTable[inxRN,"MapY"]
            #  
            #  Inspect
            #
            if (is.na(xMapL) || (xMapL == "")) {
               # Blank value
               next
            }
            if (nchar(xMapL) > 3) {
               ErrorFlag <- errCntMsg(paste0("***3638 The label value in the MapLabel entry for ",inxRN,"\n",
                                             "        is > 3 char.  Only first 2 characters will be used.\n"))
               xlen      <- nchar(xMapL)
               if (xlen > 3) xlen = 3
               xMapL     <- stringr::str_sub(xMapL,1,xlen)
            }
            if ( is.na(xMapX) || is.na(xMapY) || (xMapX=="") || (xMapY=="") ) {
               ErrorFlag <- errCntMsg(paste0("***3639 One of the coordinates in the MapLabel entry for ",inxRN,"\n",
                                             "        is/are not a number. ",xMapX," or ",xMapY,"\n"))
               next
            }
            # Check Range of X,Y coordinates.
            
            if (xMapX < MapBox2[1,1] || xMapX > MapBox2[1,2] || 
                    xMapY < MapBox2[2,1] || xMapY > MapBox2[2,2] ) {               
               # out or range - any of the TRUE - out of range              
               ErrorFlag <- errCntMsg(paste0("***3640 One of the MapLabel coordinates for ",
                          inxRN," are out of range. Entry ignored.\n"))               
            } else {               
               # Every thing is fine with this entry.
               NTable[inxRN,"MapL"] <- xMapL    # put back in Name Table               
               NTable[inxRN,"MapX"] <- xMapX               
               NTable[inxRN,"MapY"] <- xMapY               
               MapLData <- TRUE               
            } 
         }
      }
      #  End of map label processing and conversion.
      #
      NTNames <- names(NTable)  # refresh the name list
      
      #cat("Complete Name Table - ",NTNames,"  Code 5916 \n")
      #print(NTable)
         
      #
      #####
      #######
      #########
      #cat("End of Name Table Processing - Code 5923 \n")
      #  End of Name Table Processing and setup.
      ############

      if (StopFlag) {
         stop(paste0("***3999 Errors have been found and noted above.  Execution stopped.\n",
                     "        Please fix problem(s) and retry.\n"))
      }

      #cat("NTable after MapLabel check\n")
      #print(NTable)   
      
      #########
      #######
      #####
      #
      #  BuildBorderGroup_Part 7 <- Shape file has been read, but not inspected or cleaned up.
      #    
      #
      
      if (bitwAnd(debug,64) != 0) cat("ShapeFile Current Proj4 CRS:",
            as.character(SfCrs$input)," ",as.character(SfCrs$wkt),"\n")    #     String
           
      #####  
      #
      #  Later this code will be turned into a function
      #
      #  Part 7 - process, simplify shapefile and get initial
      #      data for nametable
      #
      #    Build SFdsn and SFlayer to specify the path to the shapefile
      #    and the layer to be loaded.
      #
      #     If an area contains multiple polygons, then there may be multiple "polygon" 
      #     class elements for a single area. The function will do a union of the 
      #     polygons in an area under one "Polygons" structure. To reform a SPDF,
      #     the @data part of the SPDF must be aggregated from the "Polygon"s that 
      #     were combined.  However, the critical information can be copied and a 
      #     true aggregation of the area, parimeter, etc. is not required.
      #
      #     Future Feature Add code to handle .zip version of shapefile.
      #
      #    A character string name of the area covered by the 
      #    boundary group.
      #
      #     Note 1: Read ShapeFile - was moved to just behind
      #     the completion of the call parameter validation.
      #     This was done to have the ShapeFile data section available (drop geometry)
      #     for the Name Table content validate and building the 
      #     "links" and "keys". 
      #  
      #     If the shapefile projection was empty, longlat projection will be inserted.
      #     If a projection is present in the shapefile, it is checked to see if 
      #       it is a long/lat projection.
      #     If the projection is long/lat the ShpProjLL and DoBldAEAProj flags
      #       are set.  
      #     Otherwise the shapefile does not have a LL projection and the flags
      #       are set to FALSE.
      #     WorkSf01Proj4 carries the st_crs(WorkSf01)$input
      #
      
      ##### 371x  
      #
      #   Part 7.1 - Handle processing of Shape File projection, see if we will be 
      #              re-projecting or not.
      #
      #  If empty, set to generic long/lat projection. Check the proj4 to see if
      #  user set it to a long/lat or other projection and set flags.
      #  If user set proj4 to non-long/lat, we check for +units=m.  If not,
      #  a new projection is built to change it.
      #
      #  Processing based on shape file's projection.
      #
      
      ModProj4       <- NA          # none created at this time.
      DoModProj4     <- FALSE
      
      # WorkSf01Proj4 has already been retreived from the initial load 
      # of the shapefile and if empty back filled with a basic long/lat 
      # projection string. 
      # 
      # ShpProjLL - was set up when shapefile opened.  
      # If the shapefile has:
      #    a) no projections - forced LL proj and set flag.
      #    b) projection present with long/lat - set flag
      #    c) projection present and is not long/lat - reset flag.
      # If user provided proj4 call parameter is Long/lat, it is ignore 
      # with error message.
      #
      # Check for +units=m if non-long lat.
      
      #cat("WorkSf01Proj4:", WorkSf01Proj4,"  ShpProjLL:",ShpProjLL,"\n")
      if (!ShpProjLL) {
         #cat("Check non-long/lat projection in shapefile to see if its +units are meters.\n")
         
         # Is not longlat, need to check for +units=m or not?
         # If no +units= the default is "m".
         if (!is.na(stringr::str_locate(WorkSf01Proj4,"\\+units="))[[1]]) {
            # +units= is in the projection
            if (is.na(stringr::str_locate(WorkSf01Proj4,"\\+units=m |\\+units=m$"))[[1]]) {  
               # +units not set to meters.
               cat("***3711 The projection provided in the Shape File does not have\n",
                   "        +units=m, will modify and setup for re-projection to change to meters.\n")
               CurProj     <- WorkSf01Proj4    # get copy of projection string
               
               # find the +units=??? and replace with +units=m.
               matchstr    <- "\\+units=[[:alpha:]]+"     # RegExp string to find +units=???.
               ModProj4    <- stringr::str_replace(CurProj,matchstr,"\\+units=m ")   # find start and end and do replacement
            
               cat("New Projection for ShapeFile to be applied later:",ModProj4,"\n")
               #  ModProj4 contains copy of the upgraded projection from the shape file.               
               DoModProj4 <- TRUE   
               
               # Should not do transform until after modifications....
            
            } else {
               #cat("***3712 Found +units=m in projection string of non-longlat projection\n",
               #    "        in the shape file. \n")
               # If ShapeFile projection is not Long/Lat and has +units=m, no projection later
               x <- 1
            
            } # end of adjustment for +units=
         }
      }  # end of non-longlat checking.
      
      #
      #  When the proj4 call parameter is inspected and found to be good,
      #    DoUserProj4 is set to TRUE.  If the proj4 string does not contain
      #    +units=m, it is edited and changed.
      #
      #  If call parameter proj4 is a longlat protection, a warning is thrown
      #    and the parameter is ignored.  Having the final projection LL does not
      #    make any sense.
      #
      #  If the shapefiles projections is non-long/lat and +units=m, then ModProj4 will be
      #    set to NA and DoModProj4 to FALSE to correct the +units.
      #
      #  Since all adjustments are in native units, no transformation can be done until
      #    after the adjustments are made.
      #
      #  If shapefile default or original proj4 is longlat, then both ShpProjLL and 
      #    DoBldAEAProj are set to TRUE. 
      #
      #  If DoUserProj4 is set, then DoModProj4 and ModProj4 value will be ignored.
      #    They would be overriden by the users proj4 request.
      #
               
      WorkSf02     <- WorkSf01        # advance the sf for the next phase of this process.
      WorkSf02_a   <- WorkSf01
      WorkSf02Data <- sf::st_drop_geometry(WorkSf02) # save for later if need to restore.
      
      #
      #####
      
      ###
      ## Start Here Cleaning Polygons
      ###
      
      #cat("***3720 Cleaning up polygons in spatial structure.\n")
      
      #####  3720  (new)
      #
      #  Inspect and clean up shape file using sf tools.
      #
      #  sf_use_s2 has to be disabled to repair and process the polygons.
    
      # Clean up geometry with sf..
      xStatus <- sf::st_is_valid(WorkSf02)   # one true per polygon
      if (any(!xStatus)) {
         # something is wrong
         BadStatus  <- !(xStatus == TRUE)   # find FALSE and NAs
         cat("***3722 Shape File contains invalid polygons.  The indexes and associate reasons are :\n")
         xp1        <- WorkSf02[BadStatus,]
         xp2        <- sf::st_is_valid(WorkSf02[BadStatus,], reason=TRUE) # get reasons for invalid polygons.
         xp1$reason <- xp2
         print(xp1)
         
         cat("        Attempting to fix.\n")
         WorkSf02   <- sf::st_make_valid(WorkSf02)  # ahould fix them.
         xStatus2   <- sf::st_is_valid(WorkSf02)
         if (any(!xStatus2)) {
            cat("The following polygons still have a problem. Processing stopped.\n")
            BadStatus2  <- !(xStatus2 == TRUE)
            xp1a        <- WorkSf02[BadStatus,]
            xp2a        <- sf::st_is_valid(WorkSf02[BadStatus2,], reason=TRUE)
            xp1a$reason <- xp2a
            print(xp1a)
            stop()
         } else {
           cat("        All issues fixed.\n")
         }
      }   
      # to ensure shape file is good.
      
      if (bitwAnd(debug,8) != 0) {
         WorkSfc02    <- sf::st_geometry(WorkSf02)
         OutPutP      <- paste0(BGPathName,"Shape_File_sf_Cleaned_Up_image.pdf")
         grDevices::pdf(OutPutP,width=10, height=7)
         Sys.setFileTime(OutPutP,Sys.time())
         
         plot(WorkSfc02,main='',lwd=0.2,asp=1,key.pos=NULL)   # **** CHANGE
         graphics::title("Shape File after sf cleaned")
         x            <- grDevices::dev.off()
      }
      
      ##  Part 7.2 - BuildBorderGroup - inspect and clean the SPDF/sf 
      ##            polygon structure
      ##
      ##      Execute Cleangeo on the shapefile.   - Retired
      ##
  
      ##
      ##  WorkSf03 now has the geospatial data points.
      ##
      WorkSf03   <- WorkSf02
      WorkSf03_a <- WorkSf02
      
      ######
      ########
      ##########
      
      #########
      #######
      #####
      #
      #  We have the basic NTable and Shape file @data information (sf::st_drop_geometry)
      #  Time to link the two tables. It will be a one to many link
      #  at this time.
      #
      ##### 
      #
      #  Step 7.4 - Get DATA and KEY from WorkSf03.
      #
                
      ##### 374x
      #
      #  Step 7.4 - Check shape file - link data to Name Table.
      #     Validate against NameTable.
      #     and set keys in ShapeFile to kill LINK
      #
      #   Should be done after union to least area entries and after 
      #   keys are selected in the Name Table. This is after the 
      #   polygon union.
      #
      #cat("Part 7.4 - Code 6166 - shape file LINK \n")

      #
      #  7.4 - Set Up Shape Link and Name Table Link for match:
      #
      #  Whatever the links are - numberic or character.  It is best to 
      #  pad all of the links to the same length with leading "0"s.  
      #  So trim, get max length and pad with "0". It works best 
      #  with numbers, and should not really impact character strings.
      #  Just don't modify any column but true "Link" columns.
      #  If a user specifies a "name" or "Abbr" column, copy it to 
      #  link first.  Same goes for the ShapeFile@data
      #
      # Build the matching strings from EACH link column.   
      #    (always paded to the left with "0" in case its FIPS codes.)
      #
      #  The SPDF link name is valid, get the data.   (We are assuming 
      #    the link is a numeric and need leading 0 padding.)
      #  Pull data from user provided name (or default), place in 
      #    the X__Link column, and prep for the match.
      #
      
      #####
      #
      #  Part 7.4 - Sort & Setup NameTable by "Name" field and order by "Name"
      #
         
      # Name Table is re-ordered bsaed on "name" field.
      xst          <- order(NTable$Name)
      NTable       <- NTable[xst,]
      row.names(NTable) <- NTable$Key
                      
      NTable       <- as.data.frame(NTable,sringsAsFactors=FALSE)
      NTNames      <- names(NTable) # reset Name Table column names.
    
      #####
      #
      #  Get the Shape File "Link' info to create link string for matching.
      #
      # WorkSf's data DF   - get shape file link variable
      
      WorkSf03$X__Link     <- stringr::str_replace_all(WorkSf03$X__Link,"[[:punct:]]","")
      WorkSf03$X__Link     <- stringr::str_squish(WorkSf03$X__Link)
      WorkSf03$X_Link      <- WorkSf03$X__Link
      WorkSf03Link         <- WorkSf03$X__Link
      maxSf03              <- max(nchar(WorkSf03$X__Link))
      #cat("WorkSf03 - Link, maxChar:",maxSf03,"\n")
      #print(WorkSf03$X__Link)
 
      #
      #  Get the name table "Link" info to create link string for matching.
      #
      # Name Table link
      #  Move Name Table "link" coiumn info to NTable$Link column.
      #  The name table link is now in the correct spot.
      NTableLink           <- stringr::str_replace_all(NTable$Link,"[[:punct:]]","")
      NTableLink           <- stringr::str_squish(NTableLink)
      maxNT                <- max(nchar(NTableLink))
      #cat("NTable - Link, maxChar:",maxNT,"\n")
      #print(NTableLink)

      maxBoth              <- max(maxSf03,maxNT)
    
      #  Now create the strings for matching the Shape File and Name Table
      #
      # Trim trailing and leading blanks, pad to left with "0" to make all of the strings
      #  the same length, and make all of the strings uppercase to maximize the possibility
      #  of a reasonable match.
      #
      WorkSf03LMatch       <- WorkSf03Link
      WorkSf03LMatch       <- stringr::str_pad(WorkSf03LMatch,maxBoth,side="left",pad="0")
      WorkSf03LMatch       <- stringr::str_to_upper(WorkSf03LMatch)
      WorkSf03$X__Link     <- WorkSf03LMatch   # save in sf
      #print("WorkSf L Match")
      #print((WorkSf03LMatch))
      
      NTableLMatch         <- NTableLink
      NTableLMatch         <- stringr::str_pad(NTableLMatch,maxBoth,side="left",pad="0")
      NTableLMatch         <- stringr::str_to_upper(NTableLMatch)
      NTable$Link          <- NTableLMatch     # save in Name Table
      #cat("NameTable L Match \n")
      #print((NTableLMatch))
   
      #
      #####

      
      ##### 375x
      #
      #  7.5 - Compare ShapeFile links to the Name Table links
      #
      #cat("***3750 Comparing shape file to name table links \n")
      xm          <- match(WorkSf03LMatch, NTableLMatch) # check is all of the areas in the sf have name table rows?
       
      MS_Sf_m     <- is.na(xm)      #  NA values for any ShapeFile Link that does not have a Name Table entry.
          
      if (any(MS_Sf_m)) {
         # have entries in ShapeFile that are not in NTable.
         
         if (bitwAnd(debug,16) != 0) {
            print(NTable)
            print(WorkSf03)
         
            print(MS_Sf_m)
            LL <- WorkSf03$X__Link[MS_Sf_m]
            print(LL)
         }
         
         #
         #  Delete extra areas or polygons that don't have a Name Table entry.
         #
         MS_Sf_List  <- WorkSf03Link[MS_Sf_m]     # get name (link) list of Shape File areas not in Name Table.
         MS_Sf_ListU <- unique(sort(MS_Sf_List))
         ErrorFlag   <- errCntMsg(paste0("***3752 The following Shape File areas are not in Name Table:\n",
	                                 "        ",paste0(MS_Sf_ListU, collapse=", ", sep=""),"\n",
	                                 "        The areas will be dropped."))
         
         MS_Sf_List2 <- WorkSf03$X__Link[MS_Sf_m] # list of non-matched links
         ShpDelList  <- (WorkSf03$X__Link %in% MS_Sf_List2)    # find which polygons get deleted. (could be multiple polygons)
         # Do the deletes from the WorkSf03 sf
         
         WorkSf03    <- WorkSf03[!ShpDelList,]    # keep the rest.
         
         # Clean up the match list for deleted polygon/area.
         WorkSf03LMatch <- WorkSf03LMatch[!ShpDelList]  
         #
         # We have cleaned up the Shape File by deleting any areas not in the name table.
      
         WorkSf03_a  <- WorkSf03
      }
      #
      #####
      
      ##### 376x
      #
      #   7.6 - Compare Name Table to the ShapeFile Link Values
      #
      
      #  Compare --> 
      xm       <- match(NTableLMatch,WorkSf03LMatch)  # test Name table link against ShapeFile.
      
      #cat("***3760 Comparing the link values to tie the name table\n",
      #    "        to the shape file.\n")
      #print(NTableLMatch)
      #print(WorkSf03LMatch)
      
      MS_NT_m  <- is.na(xm)    # NA says Name Table has entry that is not in ShapeFile.
      #
      
      if (any(MS_NT_m)) {
         # Found entries in Name Table that have no polygons to map.
         #   Have to stop... Name Table must have polygons.
         
         MS_NT_List <- NTableLink[MS_NT_m]    # get list of unmatched name table entries.
         NTableAbbr <- NTable[MS_NT_m,"Abbr"]
         StopFlag   <- stopCntMsg(paste0("***3762 The following Name Table areas do not have boundaries in the\n",
                                         "        ShapeFile :\n ",
                                         "        List of Abbr:",paste0(MS_NT_List,collapse=', ',sep=""),".\n",
                                         "        Correct and retry."))
         # In this case stop, can't draw all of the areas of the map.
         
      } else {
         
         # At this point the number of areas in the Shape File (SPDF) and the Name Table
         # match and their values (links) match.  Both structures should have no duplicates.
         # the unionSpatialPolygons should have removed the SPDF duplicates and the 
         # Name Table was scanned and duplicates reported as errors.
         
         xmna <- is.na(xm)   # place holder
      }
      
      xm      <- match(WorkSf03LMatch,NTableLMatch)    # Get from polygons to the name table.
      xmNA    <- is.na(xm)
      if (any(xmNA))   {    # if any, then one of the polygons does not belong to a name table row.
         xmsg <- paste0("***3764 Some of the polygons in the Shape file still do not belong to \n",
                        "        areas in the name table. Polygon(s) are ignored. \n")
         ErrorFlag <- errCntMsg(xmsg)
         
      }
      WorkSf03$Key    <- NTable$Key[xm]
      WorkSf03$X__Key <- NTable$Key[xm]
      WorkSf03$Abbr   <- NTable$Abbr[xm]
    
      #
      #####
      #######
      #########
      
      #########   First application of sf::st_shift_longitude()
      #######
      #####
      #
     
      if (LLShift)  WorkSf03   <- sf::st_shift_longitude(WorkSf03)  # adjust for 180 long.
      
      #
      #####
      #######
      #########
      
        
      #########
      #######
      ##### 378x
      #
      #  *** Check point print of map after read in and before smoothing.
      #
      #   Part 4.0 - East/West Hemisphere Line for long/lat projection
      #     for RAW displays.  (First)
      #
      #   Check for long/lat and the East/West Hemisphere crossing -
      #   If found - correct to help with plots while building.
      #
      WorkSf03_b <- WorkSf03   #  (RAW - first image)
      #cat("printing map raw - WorkSf03\n")
      #plot(WorkSf03)
      
      vDebug <- debug 
      ##  if debug bits 256, 512, or 1024 are set - plot map for inspection.
      if (bitwAnd(debug,1024) != 0)  vDebug <- bitwOr(vDebug,512)    # requested to print first image.
      
      if (bitwAnd(vDebug,256+512) !=0) {
         # check point image - RAW  (all un-unioned polygons)
         WorkTemp     <- WorkSf03
         #cat("Generating scaled example of map - initial raw data.\n") 
         #cat("SamplePrts_sf - WorkSf03\n")
         PPsf         <- WorkTemp
         PPTitle      <- "RAW"
         PPMfrow      <- c(3,3)
         SamplePrts_sf(PPsf,PPTitle,PPMfrow,vDebug,NTable$Key,MapAvgH)   # first sample maps.
      }
      #
      
      #
      #####
      #######
      #########
           
      #########
      #######
      #####
      #
      #####  377x
      #   
      #   Part 7.8 - BuildBorderGroup - simplify, generalize sf
      #
      #   WorkSf03 carries the boundary information for all areas.
      #
      #cat("***3770 Simplifying the shape file boundary data with rmapshaper.\n")
      
      #   Part 7.8.1 - Simplify
      #          Run the shapefile through rmapshaper to reduce the 
      #          number of points and vertices in the map, yet 
      #          maintain the shared boundary between the sub-areas.
      #          No sub-area should be deleted during the 
      #          simplification.  Each sub-area should still have 
      #          a resemblenc to the original sub-area. This 
      #          reduces the size of the data and speeds up the
      #          drawing time.  Since the micromap image will be 
      #          about 1" by 1.5" using a characturized version is 
      #          best.  But none are available overly simplifying 
      #          the boundaries should be used.  
      #
      #          Make sure MapShaper will not allow any area to be 
      #          eliminated. Do not use the coordinate resolution 
      #          feature. It turns the boundaries into step 
      #          functions instead of smooth lines.
      #
      #          If an area becomes to small or are not seeable in 
      #          the micromap, you may have to manually adjust the 
      #          boundaries to enlarge the area to be visible.   
      #          More on this later.
      #
      #  Snap shoot of data before smoothing and simplification
      #
      #  sf_use_s2 must be disabled to repair polygons.
      #  
      
      ##### 377x
      #
            
      #cat("rmapshaper - ReducePC:",ReducePC, "\n")
      
      #
      MS_Keep      <- ReducePC/100   # convert from percentage (0 to 100) to decimal (0 to 1)
      MS_Weighting <- 0.9
      
      cat("***3772 rmapshaper parameters before simplification: Keep=",MS_Keep,"  Weight=",MS_Weighting,".\n")
      
      #SizeSf03 <- utils::object.size(WorkSf03)    # Size of structure before
      #cat("Geometry size before ms_simplify:",SizeSf03," (03)\n")
      WorkSf03_c <- WorkSf03   
      #cat("WorkSf03:\n")
      #print(WorkSf03,n=20)
        
      if (bitwAnd(debug,64) != 0) 
      {
         cat("sf structure data section:\n")
         print(sf::st_drop_geometry(WorkSf03))
      }
      #  rmapshaper call for ms_simplify   (affine - simplification)
      suppressWarnings(
          suppressMessages(
          WorkSf04 <- rmapshaper::ms_simplify( WorkSf03,
                             keep        = MS_Keep,         # def = 1.25 %  or 0.0125 units.
                             method      = "vis",           # NULL=Visvalingam Simplification (default)
                             weighting   = MS_Weighting,    # def = 0.925  
                             keep_shapes = TRUE,   # OK to lose if not the last one in a group # not default (FALSE) 
                             no_repair   = FALSE,  # do repairs       # default
                             snap        = TRUE,   # correct vertixes # default
                             explode     = TRUE,   # Not Default (FALSE)
                             drop_null_geometries = TRUE, # default
                             snap_interval = NULL, # default
                      )  # structure changed to ...
                          # the following are set to defaults within package and 
                          # the force_FC is throwing error - not valid parameter.
                          # Therefore, these two parameters are commented out
                          #   force_FC    = TRUE,   # default - passed to apply_mapshaper_commands
                          #   sys         = FALSE)  # default - passed to apply_mapshaper_commands
          )    )
      #SizeSf04  <- utils::object.size(WorkSf04)
      #cat("sf size after ms_simplify:",SizeSf04," (04)\n")
      
      # The rmapshaper in its simplification, places two polygons very close 
      # to each other - almost touching. The projection is LL for Alaska and 
      # the point is near Jueanu. When this point is projected to AEA or 
      # other projections, these two polygons intersect and "overlap" causing 
      # an invalid map. This impacts all of the rest of the spatial operations 
      # once the map is actually transformed after the modifications and before 
      # the VisBorder conversions.
      #
      #cat("***3774 rmapshaper processing completed.\n")

      WorkSf04     <- sf::st_make_valid(WorkSf04)     # fix up polygons.
      if (LLShift) WorkSf04 <- sf::st_shift_longitude(WorkSf04)  # reapply shift.

      #
      #   After simplification by rmapshaper
      #
     
      if (bitwAnd(debug,8) != 0) {
         WorkSfc04  <- sf::st_geometry(WorkSf04)
         OutPutP    <- paste0(BGPathName, "BBG-Shape_file_after_rmapshaper_simpl.pdf")
         grDevices::pdf(OutPutP,width=10, height=7)
         Sys.setFileTime(OutPutP,Sys.time())
         
         plot(WorkSfc04,main='',lwd=0.2,asp=1,key.pos=NULL)
         graphics::title("Shape File after rmapshaper simplification.")
         x          <- grDevices::dev.off()
      }
      
      #
      #
      #  Part 7.7.2 - COMBINE polygons under single area entries.  
      #        (unionSpatialPolygns)  (WorkSf04-> WorkSf05)
      #
      #cat("WorkSf04 before aggregation. Code 6523 \n")
      #print(WorkSf04, n=40)
       
      MList           <- WorkSf04$X_Link   # List of polygon Link names
                   
      WorkSfxx        <- sf::st_make_valid(WorkSf04)
           # even though nothing reports as invalid.  We found we have to do 
           # a st_make_valid call with sf_use_s2(FALSE) to get the geometry cleaned
           # for the aggregate after the rmapshaper.
      
      WorkSf05        <- aggregate(WorkSfxx,by=list(MList),FUN=aggFun )
                         # the coordinates modified sf::st_shift survive the aggregations.
                         
      #cat("Aggregation of Polygons completed.\n")
      #cat("projecton of WorkSf05 after aggregation:\n")
      #print(sf::st_crs(WorkSf05))
      
      WorkSf05       <- sf::st_make_valid(WorkSf05)
      
      #cat("***3778 Map area aggregation completed.\n")
      #
      # All of the header rebuild is already done by aggregate.
      
      if (bitwAnd(debug,64) != 0) {
         cat("lengths of - 02:",length(sf::st_geometry(WorkSf02)),
             "     03:",length(sf::st_geometry(WorkSf03)),
             "     04:",length(sf::st_geometry(WorkSf04)),
             "     05:",length(sf::st_geometry(WorkSf05)),"\n")
         cat("Sizes of   - 02:",utils::object.size(WorkSf02),
             "     03:",utils::object.size(WorkSf03),
             "     04:",utils::object.size(WorkSf04),
             "     05:",utils::object.size(WorkSf05),"\n")
         cat("ShapeFile combination completed. ",length(sf::st_geometry(WorkSf04))," now ",
                length(sf::st_geometry(WorkSf05))," areas. Code: 6543 \n")
      }
      #
      #####
             
      #####
      #
      #  Update the spatial data with the abbr and Link information that match the name table.
      WorkSf05_a <- WorkSf05
      
      #  cat("Matching Shape File to Name Table and setting Abbr, X__Key, row.names\n")
      #  Match the Name table to be able to move to the shape file the abbr and keys.  ??? LINKs
      
      #
      row.names(WorkSf05) <- WorkSf05$X__Key
      
      #cat("WorkSf05:\n")
      #print(WorkSf05)
      
      #
      #  Shape file now in WorkSf06
      WorkSf06   <- WorkSf05
      #
      #####
      #cat("Set Abbr and Key into WorkSf06.\n")
      #print(WorkSf06)
            
      StopFlag   <- FALSE
      ErrorFlag  <- FALSE
      #
      #####
      #
      if (bitwAnd(debug,8) !=0) {
         WorkSfc06 <- sf::st_geometry(WorkSf06)
         OutPutP   <- paste0(BGPathName,"BBG-Shape_file_after_multipolygon_agg.pdf")
         grDevices::pdf(OutPutP,width=10, height=7)
         Sys.setFileTime(OutPutP,Sys.time())
         
         plot(WorkSfc06,main='',lwd=0.2,asp=1,key.pos=NULL)
         graphics::title("ShapeFile after multipolygon agg ")
         x         <- grDevices::dev.off()
      }   
      if (bitwAnd(debug,64) !=0) {
         cat("New ShapeFile Data information - WorkSf06 data: Code 6599 \n")
         print(sf::st_drop_geometry(WorkSf06))
      }
      #
      #####
    
      WorkSf06a <- WorkSf06
      WorkSf06  <- sf::st_make_valid(WorkSf06a)
 
      #####
      #
      #  Create an AEA projection to use for area estimation on map areas
      #
      EstAEAProj <- NULL
      #cat("Code 6700 - ShpProjLL: ", ShpProjLL,"\n")
      
      if (ShpProjLL) {
           EstAEAProj <- AEAProjection(WorkSf06)
           #cat("Creating projection for area estimation using Alber Equal Area.\n")
      }
      
      #  may need for later, but must create now.
      #
      #####
    
      
      ##### 378x
      #
      #  Part 7.8.4 - check point plot of map after smoothing  (rmapshaper).
      #
      #  Call Parameters: sf, Legend Pos, Colors(T/F), Lattice DIM, Colors
      #
      
      WorkSf06_b   <- WorkSf06
      
      if (bitwAnd(debug,512) != 0) {
         # we have a long/lat projections  (only plot one map)
         # check for problems with E/W hemisphere crossing.
         #cat("Generating scaled example of map - after rmapshaper.\n")
             
         PPsf         <- WorkSf06
         PPTitle      <- "After rmapshaper"
         PPMfrow      <- c(3,3)
         SamplePrts_sf(PPsf,PPTitle, PPMfrow, debug, NTable$Key, MapAvgH)

      }

      #
      #####
              
      #####
      #
      #  ShapeFile simplification and process is done.  
      #
      #  End Function part 7 - Steps 1 to 9.   
      #
      #####
      #######
      
       
      ##### 380x
      #
      #  Part 8.0 - Add Neighbor relationships - shape file has been simplified,
      #   the polygons gathered under area names, data section should remain 
      #   the same for the rest of the run.
      #
      #   This information required to do the modification phase.
      #
      #cat("***3801 Identifying neighbors for each area.\n")
      
      #cat("spdep::poly2nb.\n")  
      suppressWarnings(
         suppressMessages(
          Sf06.nb <- spdep::poly2nb(sf::st_geometry(WorkSf06,queen=TRUE))
         )
      )
      #
      #
      #cat("Completed spdep::poly2nb\n")
      
      RNList        <- row.names(WorkSf06)              # Get list to help backfile in the nb list.
      NBList        <- sapply(Sf06.nb, function(x) RNList[x])
     
      xmm           <- match(NTable$Key, WorkSf06$X__Key)
      NTable[,"NB"] <- NA
      NTable[,"NB"] <- list(NBList[xmm])   # get list of neighbors for each area by "Key"
      # the adjacency list for each area is converted to the keys, not physical entry in sf.
      
      #cat("***3804 The list of neighbors for each area are in the name \n",
      #    "        table 'NB' column.\n")
      #
      #  Information needed to do modifications and replacment 
      #
      ##
      #####
      
         
      ##### 381x
      #
      #  Step  8.1 - Name Table Adjustments To ShapeFile.
      #
      #  Previously - the neighborship relations are calculated and 
      #    saved into the Name Table for this processing. (Sp06.nb)
      #
      #  Basic logic:
      #    No transformations have been done, since the adjustment metrics are in
      #      the original metrics (km, m, long/lat) of the original Shape file.
      #
      #    If the shapefile does not contain a projection, it was assumed to be
      #      long/lat and forced to long/lat for later transformations.
      #
      #    The scaling (percentage) and rotation (degrees) are not in the 
      #      shape files original metric, the X and Y offsets (shifts) are. 
      #      So, their processing must occur before any transformation.
      #      In the long/lat projection, the problem of the E-W crossing 
      #      can complicate the X and Y adjustments since the 
      #      X,Y values flip from -179 (like with Alaska) to 179.  Even though 
      #      there are no islands or sub-areas that exists across the 
      #      E-W crossing, Alaska does. 
      #      DONE..
      #
      #    The final calculated projection to AEA from Long/Lat requires
      #      is calculated to use the centroid of the centroid of the MAP. 
      #      If the map is already projected, it may need to be re-projected 
      #      to update the units to +units=m for uniform rounding and processing.
      #      If the caller has specified the final proj4 string, then 
      #      it is also inspected and adjusted to insure the +units will be "m".
      #
      #    All transformations are done after the map modification section
      #      is completed.  
      #
      #    The modification process is done in the following steps:
      #      1) If the projection is long/lat and involves the E-W crossing,
      #         the LL must be adjusted to permit the math to work.
      #         The x,y coordinates in polygons crossing or on the other side
      #         are adjusted to a range of 360 to 0 or -360 to 0.  This 
      #         adjustment is not liked by many spatial functions in R
      #         so it must be done at the very end function when no other
      #         R Spatial function will be used before conversion to the 
      #         VisBorder (micromapST) data.frame format.
      #
      #      2) Each modification is performed against one area at a time.
      #         If the area has multiple polygons within it. They are all
      #         modified in the same way.
      #
      #      3) The centroid of the area and sub-polygons is calculated and saved.
      #
      #      4) All points in the polygons in the area are normalized to the 
      #         centroid to ensure all point shift, scale or rotate together.
      #         To make the process simplier, the normalization is done in all
      #         cases even though it is not needed for the shift.
      #
      #      5) The X and Y shifts are applied to all points.  The X and Y 
      #         are handled separately and do not require both to be 
      #         present.
      #
      #      6) The scaling is applied to all points. This is done by 
      #         decreasing or increasing the distance of the X,Y points from 
      #         the centroid point, affectively a multiplier, since the 
      #         centroid is effectively the 0,0 point in the set of polygons.
      #
      #      7) The rotation is applied to all points about the centroid.
      #         Each X,Y point is modified by:
      # 
      #         (x',y') <- (x cos<a> + y sin<a> , -x sin<a> + y cos<a>)
      #
      #      8) Once shifting, scaling and rotation is completed, the polygons 
      #         are adjusted by the centroid back to their normal projection.
      #
      #      9) Since the areas polygons may not fit back into the map in the 
      #         same way. If the area overlays neighbors, must Spatial functions,
      #         will complain and may not function.  Attempts to simply repair
      #         the map have not worked.  So, the code uses the gDiff function
      #         of the modified areas polygons and its neighbors to cut 
      #         out a space in the map to put the modified area back into 
      #         the map.  It is possible to adversely impact the layout of 
      #         the other neighbors, so all modification must be visually
      #         inspected to see if the desired result occured.
      #
      #    The result should be a workable map that the user can manage 
      #    the shift, scale and rotate coordinates easily.
      #
      #    Investigations:
      #       cartograms - weighting is size of area.   This gives you the same map.
      #             Increase the size of the area and it is enlarged and the surrounding areas
      #             are adjusted.
      #
      #####
      #
      # Already have adjacency in the Name Table.. as lists of keys.
      #
      # Apply modifications and gDiff to neighors.
      # Section 8.1 - Apply to area
      # Section 8.2 - gDiff neighbors
      # Section 8.3 - re-insert all changed areas???
      #
      #####
      
      WorkSf06_d <- WorkSf06
      
      ##### 381x
      #
      #  Step 8.1 - Do any required Modifications
      #
      #       Sub-Step - resolve international date line and 180 degree longitude issue.
      #
      #       Resulting sf will be in the WorkSf07.
       
      WorkSf07    <- WorkSf06
     
      options(warn=1)
      
      # cat("Setting Xoffset and Yoffset to 0 if value NA.\n")
      
      # Set Default NA's to zero for the code.  
      # move the following to the Name Table validation code.
      
      x <- is.na(NTable$Xoffset)
      NTable$Xoffset[x] <- 0
      x <- is.na(NTable$Yoffset)
      NTable$Yoffset[x] <- 0
      
      #NTable[,c(Xoffset,Yoffset)]

      CurProj         <- sf::st_crs(WorkSf06)
      Curproj4        <- CurProj$proj4string
      #print(CurProj)
            
      if (any(NTable$DoAdj)) { 
     
         ##### 381x
         #
         #  The Name Table holds the modification columns:  Xoffset Yoffset, Scale, 
         #  Rotate, and ModOrder.
         #
         #  Loop through the sf and process the adjustements (offsets and scaling.)
         #
         #   Find areas in the Name Table that need adjustments (DoAdj=TRUE)
         #   Copy the polygons and multipolygons for the area needing adjustments from sf.
         #   Apply adjustment (Centroid Normalizing, Shift, Scale, and Rotate and 
         #     unNormalizing for the centroid)
         #   Get list of neighboring areas.
         #   Apply gDiff to the neighbors.
         #   Remove all changed areas and re-insert the new areas.
         #
         #  If the area needs to be shifted, scaled or rotated, the area is copied 
         #  out of the sf. The area is adjusted to be centered at 0,0 for the centroid
         #  of the space and the shift, scale and rotate operations are preformed,
         #  Once done, the area is de-normalized back to it initial centroid (x,y).
         #
         #  The Name Table DoAdj column provides a quick way to determine if an 
         #  area needs adjustments/scaling/rotation.  Only areas with DoAdj=TRUE 
         #  will be processed along with its neighbors. If the areas' polygons have
         #  changed, then the area is deleted from the sf and re-added.
         #  
               
         #
         #   it's not safe to assume within a region, all of the areas will not be outside
         #   of every other area's space.  We have several situations where an area lies within
         #   anothers boundaries.
         #   Check my Georgia example...  See what it is doing.  Still thinking sf is much
         #   smarter when ploting polygons and holes and back fills holes with a more intelligent 
         #   image.
         #
         
         # we have shifting, scaling and rotating to do, but which areas?
         # Think it is best to do them in their plot order in the sf.
         # The map is in WorkSf07.
         #
         #   Pick up next Key in the plot order, step through them.
         #
         Worksfc   <- sf::st_geometry(WorkSf07)
         Lensfc    <- length(Worksfc)  # number of areas.
         KeyList   <- WorkSf07$X__Key  # one per geometric entry.
         
         NTNames   <- names(NTable)    # list of name table columns
         #cat("NTNames:",NTNames,"\n")
         modNames  <- c("Xoffset","Yoffset","Scale","Rotate") # list of possible mod columns
         xm        <- match(NTNames,modNames)    # which do we have.
         # which columns for mod. do we have.
         nxmNA      <- !is.na(xm)                # get list of the one we have.
         modList    <- NTNames[nxmNA]
         
         names(Worksfc) <- KeyList    # add names to each row.
         
         #cat("KeyList:",KeyList,"\n")
         
         for (xKey in KeyList) {      #  See which areas need modification in order of geometry?
         
            ##  geometry order -> go to NTable and get flags.
         
            #cat("Checking modifications for ",xKey,"  \n")
            
            # Only need to work on rows with "DoAdj" flag set in Name Table.
            if (NTable[xKey,"DoAdj"]) {      # of NTable row says DoAdj, we have work to do on this area.
               
               cat("***3811 Area:",xKey," will be adjusted using the ",modList," values.\n")

               # pick up Xoffset, Yoffset, Scale and pass to the function to process sf row.
               # translate degrees to radian
               
               xAdjParms <- (NTable[xKey,modList])   # get modification values
               
               if (is.null(xAdjParms$Rotate) || is.na(xAdjParms$Rotate)) { # no rotate
                  xAdjParms$Rotate = 0
                  NTable[xKey,"Rotate"] = 0
                  xAdjParms$RotateR = 0
               } else {
                  xAdjParms$RotateR <- xAdjParms$Rotate * pi / 180  # get radian angle
               }
               if (is.null(xAdjParms$Scale) || is.na(xAdjParms$Scale))  { # no scale
                  xAdjParms$Scale = 1
                  NTable[xKey,"Scale"] = 1
               }
               #cat("***3812 Area: ",xKey," Xoffset:",xAdjParms$Xoffset," Yoffset:",xAdjParms$Yoffset,"\n",
               #    "        Scale:",xAdjParms$Scale, " Rotate :",xAdjParms$Rotate, " in radians:",xAdjParms$RotateR,"\n")
                  
                 # get sub-SF for area.
                 # pull off each areas sf structure by key.
               
               areaSFC    <- Worksfc[xKey]   # get sfc for the area to adjust.   
               
               #cat("get centroid of area. code 7011 \n")
               suppressWarnings(
                  areaCtr    <- sf::st_centroid(areaSFC)  # old coordinates  of sfc
               ) 
               if (LLShift)   {
                  areaCtr <- sf::st_shift_longitude(areaCtr)  # convert if needed.
                  #cat("LLShift set - need to adjust.\n")
               }
               #print(areaCtr)                        # OK - use average of bbox.
               
               #
               #  Alternate to the below code for Affine transformations
               # 
               #  area's geometry as = sfc  - I'm existed this equation works.
               #  It takes any geometry and 1) re-centers, rotates, scales, offsets then restores the centering.
               #  This is done with vectors and matrixs.
               #     rotation:  uses a 2x2 matrix of the cos/sin
               #     scaling:   uses a single scalar value
               #     shifting:  uses a 2x1 matrix (vector)
               #     recentering: uses a 2x1 matrix line shifting.
               #
               
               #cat("Do adjustment for ",xKey,"\n")
               areaSFC2         <- (areaSFC - areaCtr) * rot(xAdjParms$RotateR) * xAdjParms$Scale + c(xAdjParms$Xoffset,xAdjParms$Yoffset) + areaCtr
 	       # The above formula strips the crs from the spatial definition.
 	       # Add the crs back in the area sfc.
 	       sf::st_crs(areaSFC2)   <- CurProj   
 	       #if (LLShift) areaSFC2 <- sf::st_shift_longitude(areaSFC2)   # don't need to check on st_crs.

	       Worksfc[xKey]    <- areaSFC2  # re-insert the area into the sf
               cat("***3813 Re-inserting area polygons for ",xKey,"\n")
   	    
   	       # Working at the sfc level mays it easy to manipulate, plot,
               # and replace entry in the SF or SFC.
               # In some cases, the st_shift_longitude shift and st_crs 
               # projection are lost and must be reapplied.  
               # It appears MULTIPOLYGON and POLYGON
               # sf objects are automatically handled by matrix algebra.
               # You can manipulate the list form of the areaSFG for the modifications.  
    
               # Get list of Neighbors to be Evaluate for overlay
               NBList          <- NTable[xKey,"NB"][[1]]  # neighbor list for this area (xKey).
               
               #
               # Find a way to modify to check if the area overlays 
               # some areas near by, but not neighbors.   *** FUTURE
               #
               
               WorkAreaSFC <- Worksfc[c(NBList,xKey)]   # get the geometry for the area  
                                            # being modified and all of its neighbors. 
               NB_bbox     <- as.numeric(sf::st_bbox(WorkAreaSFC))  # Size the working area to get limits for plots.
               xLim        <- NB_bbox[c(1,3)]
               yLim        <- NB_bbox[c(2,4)]
               #  Got plotting limits for test and debug plots.  Add space if neighbors.
               
               # overlayed by below...
               
               #cat("***3814 plot of original area before modifications in blue :\n") # xKey and Violet
               # plot of area before modification
               #plot(areaSFC,border="blue",xlim=xLim,ylim=yLim)   # basic plot
	    	                
               #cat("***3815 plot of main area after modifications in green :\n") # Modified xKey and Green    
	       # plot of area after modification 
	       #par(new=TRUE)
	       #plot(areaSFC2,border="green",xlim=xLim,ylim=yLim,add=TRUE)
	    
               #
               # the area can be modified but not have neighbors.
               # don't handle case where you move an area over areas elsewhere.
               # This code only ties to adjust if the modified area overlays a neighbor.
               #
               
               if (length(NBList) > 0 ) {   # Do neigbhors exist?
                  # Yes neighbors exist. - get spatial data for neighbors
                  #  may have to check the type of results and neighbor to get the overlay done.
		                           
                  for (iNB in NBList) {   # process for each neighbor of this modified area.
                  
                     #  get neighbor area
                     #cat("***3818 Original neighbor boundaries in magenta for neighbor : ",iNB,"\n")
                     wsfSFC    <- Worksfc[iNB]    #  get sfc (each neighbor)
                     
                     #par(new=TRUE)
                     #plot(wsfSFC,border="magenta",xlim=xLim,ylim=yLim,add=TRUE)  # neighbor over print
                    
                     #cat("difference: ",xKey," and ", iNB,"\n")
                     
                     suppressMessages(
                       wsfTrimmedSFC      <- sf::st_difference(wsfSFC, areaSFC2)
                     )
                     if (LLShift) wsfTrimmedSFC <- sf::st_shift_longitude(wsfTrimmedSFC) # long values modified - reapply st_shift_longitute
                     
                     #cat("***3819 Trimmed neighbor boundaries in seagreen for ",iNB,"\n")
                     #  Now have the new boundaries for the neighbor  - sfc
                     #par(new=TRUE)
                     #plot(wsfTrimmedSFC,border="seagreen",xlim=xLim,ylim=yLim,add=TRUE)
                     
                     Worksfc[iNB] <- wsfTrimmedSFC   # save the change back into map.
                    
                     #print(Worksfc[iNB])
                  } # next neighbor
               }
               if (bitwAnd(debug,8) != 0) {            
	          WorkSfc04  <- sf::st_geometry(Worksfc)
	          OutPutP    <- paste0(BGPathName, "BBG-AT start of NT Mod loop-",xKey,".pdf")
	          grDevices::pdf(OutPutP,width=10, height=7)
	          Sys.setFileTime(OutPutP,Sys.time())
	          
	          plot(WorkSfc04,main='',lwd=0.2,asp=1,key.pos=NULL)
	          graphics::title("Shape File at head of NT Mod loop.")
	          x          <- grDevices::dev.off()
	       }

            }  # End of process of modifying values for one area.     
         }  # Loop through areas to see which need adjustments
         
         sf::st_geometry(WorkSf07) <- Worksfc   # put all of the areas back in to sf.
         # Do we have any work to adjust.
      } else {
         x <- 1
         #cat("***3810 Info:No modifications are required to map.\n")
      }
      #cat("end of name table modifications\n")
      #plot(st_geometry(WorkSf07))      
      # Results in WorkSf07 with modifications and East-West Crossing issue 
      # corrected (sf::st_shift_longitude).
      # Areas are modified and neighbors clipped to have the space is needed.
      #
      #cat("Delete NTable$NB.\n")
      #NTable$NB <- NULL   # clean up finished with them.
      #
      #####
      #######
      #########
      
      #########
      #######
      ##### 382x
      #
      #  Step 8.2 - Sample maps after name table modifications and st_difference to neighbors.
      #
      if (bitwAnd(debug,512) != 0) {
         # caller wants sample maps after the name table modifications - one map
         #cat("Generating scaled example of map - after Name Table modifications.\n")
         vDebug       <- bitwAnd(debug,bitwNot(256+1024))   # remove 1024 and 256
         PPsf         <- WorkSf07
         PPTitle      <- "After Name Table modifications"
         PPMfrow      <- c(3,3)
         SamplePrts_sf(PPsf,PPTitle,PPMfrow,vDebug,NTable$Key, MapAvgH)
      }
      #
      cat("***3822 Name Table modificiations to Shape file are complete.\n")
      
      #####
      #######
      #########
      
      #####
      #
      #
      #  Notes on area modification processing:
      #  1) Using st_buffer apparently - messes up the polygons of each 
      #     involved.  The edges get raged and areas no longer match up.
      #  2) When rmapshaper is reduced to low, the areas shapes are 
      #     mal-formed and sections become hard to recognize.  Example 
      #     is the state of Maryland the the western counties, looks like
      #     a pan handle.
      #  3) It is nice to be able to do the shifting, scaling, and rotation
      #     with one basic matrix equation using scalars, vectors, and 2x1 matrix
      #     values.  However, since you are working at the matrix level
      #     you need to rebuild the sfc and sf and restore the crs to the spatial
      #     structure.
      #  4) If you use the st_shift_longitude function to present areas like
      #     the US, Alaska, etc.  For some reason, when you process the 
      #     the sfg, sfc, or sf, the coordinates revert to the original coordinates
      #     from 180 to -180 instead of staying 0 to 360.  When this happens,
      #     you can recover by re-processing the results of the function 
      #     with st_shift_longitude function again. I decided to determine if 
      #     the current task required the shift and if so, set a flag that 
      #     I have used the shift function.  After that, when the program 
      #     does a function that causes the coordinates to be reverted, I 
      #     check the flag and re-execute the shift against the results.
      #  5) It is best to work with the spatial structure at the sfc level in 
      #     sf.  This separates the data from the geometry and allows you to 
      #     easily reference each area/multipolygon/polygon individually without
      #     having to deal with the overhead structure or extra list () level.
      #     At this level, the coordinates are basically matrix.  To do 
      #     a align transformation all you have to do is:
      #     results <-  (original - ctr point) + c(Xoff, Yoff) * scale (scalar) * rotFun(rad) + ctr point
      #     The rotFun is rotFun <- function(a) {
      #          matrix(c(cos(a),sin(a),-sin(a),cos(a)),2,2)   }
      #
      #  6) The modified area row (sfc) can be re-inserted back into the SF as follows:
      #     a) get the sf-columns from the sf by Wsfc <- st_geometry(Wsf)
      #     b) get the working row from the sfc via index number or assigned "names" to each
      #        row.   I like the names.   WsfcArea <- Wsfc[key]
      #     c) manipulate the geometry at the sfc level. Almost all of the functions
      #        will work with the sfc data and handle different classes (POLYGON, MULTIPOLYGON).
      #     d) When done, using the same key or index you can re-insert the row back 
      #        into the sfc structure.    Wsfc[key] <- WsfcArea
      #     e) The complete sfc can them be reinserted into the original sf by:
      #           st_geometry(Wsf) <- Wsfc    Warning, this will replace the entire geometry.
      #  7) Using any of the st_area, st_distance and more functions, does not change or add
      #     to the sf header data.  You must save the vector and cbind it / are $xxx<- it to 
      #     the data header of the sf structure.
      #
      #####    
                
      
      ##### 384x
      #
      #  Step 8.4 - Get area sq. ft (ll or m) to predict too small areas.
      #  Use a Temporary Projection to be able to get est. real area 
      #  values for each element. You can't do this with LL projections.
      #
      #  The calculation is done after the name table modifications have been 
      #  completed. So, the exact graphic image is represented in the results.
      #
      #  US small areas: DC @ 0.0019 %; RI @ 0.027 %; 
      #                  PR (eliminated - no name table entry); 
      #
      #    break point appears to be between 0.057 % and lower.
      #    0.331322 of 1104.407 units.
      
      if (ShpProjLL)  {
      
         WorkTemp        <- sf::st_transform(WorkSf07,EstAEAProj)  # transform to AEA to be able 
                                # do the calculation against the modified map
         WorkSf07$AreaM2 <- as.numeric(sf::st_area(WorkTemp))                               
      } else {
         # if shape file is not a LL projection - it's already set to check the areas.
         WorkSf07$AreaM2 <- as.numeric(sf::st_area(WorkSf07))
      }
      AllTempArea     <- sum(WorkSf07$AreaM2)        # get the total area for the region/map
      WorkSf07$PCA    <- WorkSf07$AreaM2/AllTempArea    # based on map modified areas.
                                 # Some may have been moved or scaled.
      
      if (bitwAnd(debug,64) != 0) {
         # show details table on areas
         cat("Detailed Report of possible areas that shaping may not be visible:\n\n")
         cat("For a total map area (m^2):",AllTempArea,"\n")
         cat("    Area Key","    % of Total","\n")
         for (inx in c(1:length(WorkSf07$PCA))) {
            print(paste0(WorkSf07$X__Key[inx],"      ",
                         formatC(WorkSf07$PCA[inx]*100,format="f",digits=4,width=8)
                       ))
         }
      }
      # set debug to 64 to get a full area/percentage report.
      #cat("AllTempArea:",AllTempArea,"\n")
      xm     <- WorkSf07$PCA < 0.00057
      if (any(xm)) {
         cat("***3843 These area coverage estimates were done after the name table modifications.\n",
             "        The area's coverage may have be increased or decreased from the original Shape file.\n",
             "        This coverage review is done against the current area's coverage after all modifications.\n",
             "        If an area's coverage is less than 0.057% of the total map coverage of \n",
             "        ", (AllTempArea * 0.00057) , " m^2 may not be large enough to be visible in a \n",
             "        in a linked micromap graphic.  The area(s) that should be reviewed is(are):\n")
         cat("***3844  ",paste0(WorkSf07$X__Key[xm],collapse=", ",sep=""),"\n" )
      
      }  else  {
         cat("***3845 All of the area appear to be big enough to show the shading\n",
             "        in a linked micromap.\n")
      }
      
      #
      #
      # Alternate method to create border group.
      #
      #  An alternate method is make a copy of the geographic area of the border group,
      #  identify any areas or polygons that will be to small in the micromap,
      #  without eliminating any shared boundaries or neighbor relationships - enlarge
      #  the small areas (this may involve using circular, ellipical, or other shapes), 
      #  if necessary, shared boundaries may be moved reducing the size of the larger 
      #  neighboring sub-areas, once the highlighted boundaries are completed on the 
      #  copied map, cover the map with tracing paper and copy the characterized
      #  boundaries to the trace paper, scan the trace paper into a image file, 
      #  make the boundaries to be characterized with a medium weight pen or marker.
      #

      
      #
      # End of area inspection
      rm(WorkTemp)
      #
      #####
      
      #########
      #######
      ##### 386x
      #
      #  Step 8.6 - transform projection
      #
      #  Apply transformations to the polygons.
      #
      #  The final transform is one of the following:
      #     a) user provide proj4
      #     b) ModProj4 to get +units=m
      #     c) A created AEA based on the centroid and long/lat of the original.
      #
      #  No NTable projection of individual area will be attempted.
      #
      #  Map is already unioned by area, so areas reported should 
      #  be for all polygons in area.
      
  
          
      #######  386x
      #
      #  Step 8.6 - Build Border Group - 6 - transform sf, if needed
      #
      #  Transform sf after all of the validity checks
      #
      #  Possible Flow:
      #    a) have proj4 specified in call - overrides ModProj4 and must be non-longlat.
      #        Execute Transform proj4   (DoUserProj4)
      #    b) No proj4 in call, not LL, shapefile +units=m - nothing to do.
      #    c) No proj4 in call, not LL, shapefile not +units=m 
      #        Execute ModProj4   (DoModProj4)
      #    d) No proj4 in call, LL, create AEA around Centroid 
      #        Execute Transform  AEAProj  (DoBldAEAProj)
      #
      #  Last step if transform did occur, is to check for Map Labels.
      #  If present, then transform the label points.
      #
      #cat("***3860 Transforming projection of Shape file and label points.\n")
     
      ##### 386x
      #
      #    Step 8.6 - Do projection 
      #
      WorkSf08   <- WorkSf07   # Map Bounderies Setup incase no transform.
      CurProj    <- sf::st_crs(WorkSf07)
      Tproj      <- NA
      
      # Do Map and MapX/Y point collection if present.
      #    MapL/X/Y present flag = MapLData = TRUE
      
      #
      #   Build POINT collection for MapL's MapX and MapY if present
      #
      xmMap  <- FALSE
      MapSF  <- NULL
      MapSF2 <- NULL
      if (MapLData) {    # Since the sf is transformed, the mapL variable points must
         # also be done - now.  However, if the shape file is saved and modified, then 
         # a checkpoint restart occurs, the routine will have to check the shape file
         # and see if the map points are still correct.
         # Problem - if only one label is used, operations don't work the same. (row.names)
          
         #### Build Spatial Point from MapL, MapX, MapY. 
         xmMap    <- !( is.na(NTable$MapL) | NTable$MapL == "" )   # T/F map
         if (any(xmMap)) {
            MapDF    <- NTable[xmMap,c("Key","MapL","MapX","MapY")]   # small DF of label data.
      	    MapDF$Rn <- row.names(MapDF)    # no problems this is a DF.
      	    #cat("Building MapXY table for transform.\n")
        	 
      	    # We have MapL entries
            xMRn   <- row.names(MapDF)    # row names
       	    # Convert point columns in to sfc for transform
       	    MapSF  <- sf::st_as_sf(MapDF,coords=c("MapX","MapY"))
       	    sf::st_crs(MapSF) <- CurProj
       	    xmMap         <- TRUE
         } else {
            xmMap         <- FALSE
         }
      }
      #  xmMap remains indicator the other end - TRUE - have points, FALSE - no points
      WorkSf08x <- WorkSf08
      
      #  Make sure the map and the points are 'valid'
      WorkSf08  <- sf::st_make_valid(WorkSf08)
      if (xmMap) MapSF <- sf::st_make_valid(MapSF)
      #cat("make valid - WorkSf08 and MapSF.\n")
      
      # Do transformations
      TransDone <- FALSE
        
      if (DoUserProj4) {   # execute the projection in the call parameters (it has been adjusted to meters)
         ##### Option 1 - User provided PROJ4 on call.
         Tproj        <- sf::st_crs(proj4)   # should be the same as CPproj4
         #cat("***3861 Using user provided projection :\n",
         #    "        ",proj4,"\n")
         WorkSf08     <- sf::st_transform(WorkSf07,Tproj)
         if (xmMap) MapSF2 <- sf::st_transform(MapSF,Tproj)
         
         # If a proj4 general transformation is being requested by the caller, 
         # then any transformation to none longlat will not be useful.  
         # Best to ignor any NTable$proj entries, and any transformation to get units set to meters.
         #
         TransDone <- TRUE
         # 
         # The area transforms must be done before this.  (actually only the adjustments.)
         #
      } else {
         #  No calling parameter "proj4".
         ##### Option 2 - ShapeFile had non-longlat proj4
         if (DoModProj4) {
            #cat("***3862 Re-transforming shape file using original projection, \n",
            #    "        with +unit= changed to meters.\n")
        	  #  BUT the +units are not Meters.  ModProj4 is the 
         	  #  proj4string character string with +units=m added.
         	  Tproj    <- sf::st_crs(ModProj4)    # should be the same as SFproj4
         	  WorkSf08 <- sf::st_transform(WorkSf07,Tproj)
         	  if (xmMap) MapSF2 <- sf::st_transform(MapSF,Tproj)
             #  This will preserve the users projection, gives 
             #  me the units in meters, but MapLabels SPDF must be converted.
             TransDone <- TRUE
         } else {
            ##### No Proj4 on call, ShapeFile has a longlat projection
            #  Need to build a AEA about the centroid of the map.
            if (DoBldAEAProj) {
               #cat("***3863 Projecting shape file using calculated AEA projection.\n")   ###
               AEAProj4     <- AEAProjection(WorkSf07)    # build based on modified map. 
               cat("Created AEA projection :\n",
                   "        ",AEAProj4,"\n")
               Tproj        <- sf::st_crs(AEAProj4)
               #  Do gross transformation to the map.  (NewProj4, if needed???)
               WorkSf08     <- sf::st_transform(WorkSf07,Tproj)
  	       if (xmMap)  MapSF2 <- sf::st_transform(MapSF,Tproj) 
  	       
  	       TransDone <- TRUE
  	    }
         }
      }
      WorkSf08 <- sf::st_make_valid(WorkSf08)
      
      #cat("***3865 Transformations completed.\n")
      
      if (xmMap) {
         # points are transformed. put them back into the Name Table
         #cat("restore MapXY to name table.\n")
         xm        <- match(MapSF2$R,NTable$Key)   # find name table entires that match
         # should not have na's ...   data came from Name Table
        
         WMat       <- matrix(sf::st_coordinates(sf::st_geometry(MapSF2))[,c("X","Y")],ncol=2)
         colnames(WMat)  <- c("X","Y")
         row.names(WMat) <- MapSF2$R
        
         #print(WMat)
         #cat("WMat length:",length(WMat),"  dim:",dim(WMat),"  class:",class(WMat),"\n")
       
         rn <- MapSF2$R
         #print(rn)
         NTable[rn,'MapX']  <- WMat[rn,"X"]
         NTable[rn,'MapY']  <- WMat[rn,"Y"]
         #rm(WMat,MapSF,MapSF2)
         class(NTable)
         #str(NTable)
      }
      
      #
      #  I need a way to validate the areas in the map and make sure they area
      #  not overlapping or causing gaps.
      #
      
      #  I need a way to build a point set, transform it, then replace them in to 
      #  the NTable.. in a more efficent manner.
      
      #####  387x 
      #
      #   Section 8.7 - Add Area Color Index for map coloring.
      #
      #    Get the neighbor relationship from nacol and the non-shared color index. 
      #    Also place in Name Table for later use after checkpoint.
      #    Take the position in the SPDF and translate it to the Name Table for 
      #    that area.
      #
      
      #cat("Calling getColoring Code 7372 \n")
      xNTCC           <- getColoring(sf::st_geometry(WorkSf08))   # color indexes assigned based on area neighbors.
                         # values are in a matrix.
      xNTCC           <- as.data.frame(xNTCC,stringsAsFactors=FALSE) # Convert matrix to DF.
      row.names(xNTCC)<- row.names(WorkSf08)                # save xNTCC table for later use. ? check point.
                                                            # can't recreate after conversion to VisBorders.
      NTable$CCode    <- xNTCC[row.names(NTable),1]         # save the color index for the area.
                             # CCode ranges from 1 to "n" - that is the number of colors required.
      
      #cat("Getting Color Codes - Completed.\n")
      #print(xNTCC)
      #
      #   Final sf Test Plot of Map before convertion, full color. 
      #
      if (bitwAnd(debug,8) != 0) {    #  Change dropped 512 
         # Do final test map  
         #cat("Generating scaled example of map.\n",
         #    "        - last check of sf structure before conversion.\n")
         
         BCol      <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999", "green")
         WorkSf08Data <- sf::st_drop_geometry(WorkSf08)
         WorkSf08sfc  <- sf::st_geometry(WorkSf08)
          
         if (length(WorkSf08sfc) != length(NTable$CCode)) {
            cat("***3872 The length of Name Table and number of areas in the \n",
                "        shape file are different.\n")
            stop()
         }
         
         PPTitle    <- "_TC_Ready for VisBorders"
         FTitle     <- gsub(" ","_",PPTitle)
        
         OutFileN   <- paste0(BGPathName,FTitle,OType)
         #cat("Printing map at:",OutFileN,"\n")     # got here...  No errors.
         
         if (OType == ".png") {
            grDevices::png(OutFileN,width=10,height=7,units="in",res=300)
         } else { 
            grDevices::pdf(OutFileN,width=10,height=7)
         }
         Title     <- "Test Chart - After Mods before VisBorder."
         NTCol     <- NTable[WorkSf08$X__Key,"CCode"]
         colList   <- BCol[NTCol]
               
         plot(WorkSf08sfc,main='',lwd=0.2,asp=1,key.pos=NULL,col=colList)
         graphics::text(NTable$MapX, NTable$MapY, NTable$MapL,cex=LabelCex)
         x         <- grDevices::dev.off()
         Sys.setFileTime(OutFileN,Sys.time())      
      }
      #
      #####
                   
      #####  390x
      #
      #  Section 9.0 - Get set up for building the VisBorder files
      #    and doing the data checkpoints
      #
      #  Start building the areaParms data.frame for the checkpoint.
      #
      #  Set up for build VisBorder data.frames.
      #  but need some of the information earlier
      #  for the areaParms data.frame.
      
      SaveProj4    <- sf::st_crs(WorkSf08)        # CRS of proj4string.   # keep
      SProj4I      <- as.character(SaveProj4$input)
      px0          <- stringr::str_locate(SProj4I,"\\+proj=")
      px1          <- stringr::str_sub(SProj4I,px0[1,2]+1,-1)
      px2          <- stringr::str_locate(px1," ")
      ProjUsed     <- stringr::str_sub(px1,1,px2[1,1]-1)    # keep?
      #print(SaveProj4)
      #cat("ProjUsed:",ProjUsed,"\nSProj4I:",SProj4I," Used:",ProjUsed,"\n")
         
      xBBoxBG      <- sf::st_bbox(WorkSf08)
      mBBoxBG      <- as.numeric(xBBoxBG)  # convert from 'bbox' class to 'numbers'
      xLim         <- mBBoxBG[c(1,3)]
      yLim         <- mBBoxBG[c(2,4)]
      bbdx         <- diff(xLim)
      bbdy         <- diff(yLim)
      VisAsp       <- bbdy/bbdx            # must keep.
      
      #cat("xLim:",xLim,"  yLim:",yLim,"\n")
      #cat("class:",class(xLim),"  ",class(yLim),"\n")
      #cat("Step 9.0 Code 7455 - VisAsp:",VisAsp,"  bbdx:",bbdx,"  bbdy:",bbdy,"\n")   #  get here
     
      WorkSfMaster <- WorkSf08
      #plot(sf::st_geometry(WorkSfMaster),xlim=xLim,ylim=yLim,xaxt="n",yaxt="n",key.pos=NULL)
      
      #
      #  Start areaParms table - Fill out areaParms Table
      #
      if (bitwAnd(debug,64) !=0) 
           cat("MapMinH:",MapMinH,"  MapMaxH:",MapMaxH,"  mean:",mean(c(MapMinH,MapMaxH)),"\n")
           
      suppressMessages(     
         sf_use_s2(TRUE)   # exiting for a while
      )
     
      # Get original names or types of the shape file.
      if (methods::is(ShapeFile,"character"))  {
         SFName <- ShapeFile
         SFDir  <- ShapeFileDir
      } else {
         SFName <- "BinaryImage"
         SFDir  <- ""
      }
      cat("The original Shapefile name is ",SFName," in ",SFDir,"\n")
      
      # check name table to see if L2 or Reg are all the same or completely unique.
           
      #
      #  The areaParms data.frame must contain everything needed to pickup
      #  and continue the border group build after a checkpoint restart.
      #
      areaParmsVer <- c("1")
      #cat("Building areaParms.\n")
      areaParms    <- NULL
      areaParms    <- data.frame(OrigProj4=as.character(sf::st_crs(WorkSf01)$input), 
                                 NewProj4 =as.character(sf::st_crs(WorkSf08)$input),
                                 OrigWkt  =sf::st_crs(WorkSf01)$wkt,   
                                 NewWkt   =sf::st_crs(WorkSf08)$wkt,
                                 stringsAsFactors=FALSE) 
      areaParms$Ver           <- areaParmsVer         # data.frame format version.
      areaParms$SReadDriver   <- SReadDriver          # Driver used to read shapefile.
      areaParms$Driver        <- ShapeDriver          # Shape File Driver used to write CheckPoint Shape File image
      areaParms$ReadFlag      <- ReadFlag             # Type of Shapefile Read.
      areaParms$ShapeFName    <- SFName               # Shape File Name or "BinaryImage" if binary
      areaParms$ShapeFDir     <- SFDir                # Shape File Dir or ""
      #   
      areaParms$BGDir         <- BGDir                # border group dir at build time.
      areaParms$BGBase        <- BGBase               # border group name (minus BG)
      areaParms$bordGrp       <- paste0(BGBase,"BG")  # Name of the border Group with BG on the end. 
      areaParms$areaUSData    <- FALSE                # Indicates the border group is of the U.S. geography. (Old MapLabel)
      areaParms$enableAlias   <- FALSE                # Disables the use of the Alias field for wild card area name matches.
      areaParms$Map.MinH      <- MapMinH              # The minimum height in inches a micromap drawing is allowed to be.
      areaParms$Map.MaxH      <- MapMaxH              # The maximum height in inches a micromap drawing is allowed to be.
      areaParms$MapLData      <- MapLData             # T/F indicating there is map label information in Name Table.
      
      areaParms$LabelCex      <- LabelCex             # the cex multiplier for the Map Labels 
      areaParms$Map.Aspect    <- VisAsp               # The micromaps aspect ratio :  width/height
      
      areaParms$Map.L2Borders <- L2Feature            # Are L2 boundards to be drawn where appropriate.
      
      if (is.na(MapHdr[1]))  MapHdr[1] = BGBase
      areaParms$Map.Hdr1      <- MapHdr[1]            # The first line of the Map Glyphic header
      if (is.na(MapHdr[2]))  MapHdr[2] = "Areas"
      areaParms$Map.Hdr2      <- MapHdr[2]            # The second line of the Map Glyphic heade
                                                      # if only one header is present, it is placed in the lower line.
      if (is.na(IDHdr[1]))   IDHdr[1] = BGBase
      areaParms$Id.Hdr1       <- IDHdr[1]             # The first line of the ID Glyphic header
      if (is.na(IDHdr[2]))   IDHdr[2] = "Areas"
      areaParms$Id.Hdr2       <- IDHdr[2]             # The second line of the ID Glyphic header
                                                      # if only one header is present, it is placed in the lower line.
      areaParms$aP_Regions    <- RegFeature           # set based on presents of regID and multiple values.
      areaParms$Map.RegBorders<- RegFeature           # regional boundary data is present.
      
      areaParms$aP_Units      <- "meters"             # Coordinates units - should always be meters.
      areaParms$aP_Proj       <- ProjUsed             # The final projection used, should be equal-area or user defined.
      areaParms$xLimL         <- xLim[1]              #  X coordinates Min,Max
      areaParms$xLimH         <- xLim[2]              #  X coordinates Min,Max
      areaParms$yLimL         <- yLim[1]              #  Y coordinates Min,Max
      areaParms$yLimH         <- yLim[2]              #  Y coordinates Min,Max
      
      areaParms   <- as.data.frame(areaParms, stringsAsFactors=FALSE)   # Make Sure it is a good DF
      
      #cat("areaParms table - Ckpt version\n")
      #str(areaParms)
      #
      #####
     
      ##### 391x
      #
      #  step 9.1.0 - Starting the Check pointing saves.  Build the unique 
      #  directory to save the 
      #  three files.
      #
      #  Checkpoint directory based on BorderGroupDir  - BGDir ends 
      #  with /
      
      CkptPath <- paste0(BGDir,"CheckPoint")
      #cat("***3910 The checkpoint files will be store in the Checkpoint directory :\n",
      #    "      ",CkptPath,"\n")
      if (!dir.exists(CkptPath)) {
         # create if it does not exist.
         # build checkpoint folder if first time (rebuild if needed.)
         dir.create(CkptPath,showWarnings=TRUE)  
      }
        
      #
      #  Build file names and path for the checkpoint datasets
      #
      #  Need to check for existing files.  If there, erase???
      #
      #  The numerical value of the data points are meters and have been adjusted.
      #  They may be too big to be saved.  Area m^2 can be a problem.
      #  Consider KM, or another solution.  Blogs recommend saving values as character types.
      #
      ########
      #
      #   9.1.1 Name Table
      #
      #   Clean up Name Table Variables - delete extra columns
      #
      
      NTable$NB       <- NULL
      NTable$MapLabel <- NULL
      NTable$area     <- NULL
      NTable$Link     <- NULL     # $Key exists and fill be the link from now on.
      
      #
      #  Name Table check points
      #
      NTCkpt     <- paste0("/",BGBase,"_NT_Ckpt.RDA")
      NTCkptcsv  <- paste0("/",BGBase,"_NT_Ckpt.CSV")
      NTPCkpt    <- paste0(CkptPath,NTCkpt)
      NTPCkptcsv <- paste0(CkptPath,NTCkptcsv)
      cat("***3912 Checkpoint - Name Table RDA Filename : ",NTCkpt,"\n",
          "        Checkpoint - Name Table CSV Filename : ",NTCkptcsv,"\n")
      
      areaNamesAbbrsIDs      <- as.data.frame(NTable)
                
      areaParms$CP_NTPath    <- NTPCkpt
      # Save name table as RDA file.
      save(areaNamesAbbrsIDs, file=NTPCkpt, compress="xz")  
      # Save name table as CSV file.
      if (file.exists(NTPCkptcsv)) {
         file.remove ( file=NTPCkptcsv )
      }
      utils::write.csv(areaNamesAbbrsIDs, file=NTPCkptcsv, row.names=FALSE)        
 
      #
      #  Name Table now written as .CSV and .RDA files.
      #
 
      #
      #   9.1.2 Shape File
      #
      #   Shape File Image
      #
       
      # Clean Up sf
      
      WSFNames <- names(WorkSfMaster)
      if (any('X__Link'==WSFNames)) WorkSfMaster$X__Link <- NULL   # will depend on the $X__Key field from now on.
      if (any('AreaM2' ==WSFNames)) WorkSfMaster$AreaM2  <- NULL   # remove AreaM2 field - too large and not needed.
      if (any('Group.1'==WSFNames)) WorkSfMaster$Group.1 <- NULL
      
      # Before writing shape files, since the numeric field sizes for the data
      # are limited and can trigger an error that will prevent the shape file from
      # being written, check all data fields and convert the fields value to 
      # character type if the maximum value is > 4,250,000,000.
            
      WSFng    <- sf::st_drop_geometry(WorkSfMaster)
      WSFNames <- names(WSFng)   # get list of data field names.
      for (ft in WSFNames) {    # check each field
         if (methods::is(WSFng[,ft],"numeric")) {   # if numeric
            if (max(WSFng[,ft]) > 4250000000) {    # if max > 4,250,000,000
               WorkSfMaster[,ft] <- as.character(WSFng[,ft])  # convert column to character.
            }
         }
      }
      
      # writeOGR (sf::st_write) layer - no extension and no "/" - put on by writeOGR
      SFCkpt       <- paste0(BGBase,"_SF_Ckpt")  
      SFCkptRDA    <- paste0(SFCkpt,".RDA")     # shape file RDA filename
      # writeOGR (sf::st_write) DSN
      SFPCkpt      <- paste0(CkptPath)  # no extension and no "/" - put on by writeOGR
      #
      SFPCkptRDA   <- paste0(CkptPath,"/",SFCkptRDA)    # full pathname for RDA shape file
      
      areaParms$CP_ShpDSN     <- SFPCkpt   # save location of shape file information.
      areaParms$CP_ShpLayer   <- SFCkpt
      
      cat("***3913 Checkpoint - The shape file spatial data is written to:",SFCkpt,"\n",
          "        as a set of ESRI Shapefiles.  The RDA image spatial data is in:",SFCkptRDA,"\n")
      save(WorkSfMaster, file=SFPCkptRDA, compress="xz")
      
      #print(head(WorkSfMaster,n=20))
      wFiles  <- dir(SFPCkpt,pattern=paste0(SFCkpt,'.*'))  # get list of existing file we will
      RMFiles <- paste0(SFPCkpt,'/',wFiles)
      file.remove(RMFiles)                                # overwrite files - erase old copies.
     
      #
      # Now I know how I read the shapefile in and the driver that was used.
      # I can now write it back the same way using the SReadDriver and the ReadFlag information.
      #
      
      sf::st_write(WorkSfMaster, dsn=SFPCkpt, layer=SFCkpt, quiet=TRUE,
                      driver=ShapeDriver, append=FALSE)    # ESRI Shapefile
                      
      #
      #  Shape Files are now written (ESRI Shapefile format) and .rda of data.frame.
      #
      
      #
      #   9.1.3 areaParms
      #
      #   areaParms image  - contains all of the variables to check point and restart.
      #
      APCkpt       <- paste0("/",BGBase,"_AP_Ckpt.RDA")
      APPCkpt      <- paste0(CkptPath,APCkpt)
      cat("***3915 Checkpoint - areaParms data.frame in : \n",
          "        ",APPCkpt,"\n        as ",APCkpt,"\n")
          
      save(areaParms,file=APPCkpt,compress="xz")
      #
      #  areaParms data.frame file is now written as .RDA file.
      #
      
      #
      #  9.1.4  Report where all the files for the shapefile and 
      #         the intermediate work data.frames are located and named. 
      #
      # After doing the check point, we continue to build 
      # the border group dataset.
      #
      cat("***3917 BuildBorderGroup has completed writing of the checkpoint\n",
          "        files to disk for possible editing and restart.\n",
          "        They are located in the following directory : \n")
      cat("        ",CkptPath,"\n")
      #cat("***3918 The check point Shape File for the border group is saved to:\n",
      #    "        ",SFCkpt,"\n")
      cat("***3919 After editing, the resulting files must be saved back to\n",
          "        the same directory and filename.\n")
      
      WorkSfMst <- WorkSfMaster
      
      # 
      # Files saved for the Check Point are:  ShapeFile, Name Table, and areaParms
      #
      
      #
      #####
      #######
      #########
      
      #  End of regular run and the creation of the checkpoint files.
      #
      #########

   } else {
      
      #########
      #
      #  Start of the re-start of the run using the checkpoint files.
      
      #########
      #######
      ##### 392x 
      #
      #   Part 9.2 - Pull Data in for checkPointReStart
      cat("***3920 Check Point Restart Process Initiated.\n")
      #
      #  A checkPointReStart process is being done.  
      #  Build the directory and path strings and reload 
      #  the:  ShapeFile, NameTable, areaParm table.
      #  Key calling parameters are:  NameTableDir (contains the check point folder),
      #  the BorderGroupName (part of the check point file names and the final border
      #  group name).
      #
      #  While the BorderGroupDir is the base directory for the checkpoint 
      #  files.  If missing try using the NameTableDir which could 
      #  have been the original source. In the BorderGroupDir was 
      #  created the "checkpoint" sub-directory to contain the files.
      #
      #  Need to load areaParms file. IT has all of the rest of the data.
      #  
      RecoveryBase <- NULL
      
      # Make sure the BorderGroupDir is provided as the base to find the 
      # checkpoint files.
      
      if (missing(BorderGroupDir) || is.null(BorderGroupDir) ) {
         # no border group dir - use name table dir as backup.
         if (missing(NameTableDir) || is.null(NameTableDir) ) {
            # no Name Table Dir - ERROR
            xmsg <- paste0("***3921 No Border Group or Name Table directory provides. Cannot\n",
                           "        find restart files. Process stopped.\n")
            stopCntMsg(xmsg)
         } else {
            # have Name Table Dir
            cat("***3922 Using the NameTable directory :\n",
                "        ",NameTableDir,"\n        to locate the checkpoint files.\n")
            RecoveryDir <- NameTableDir
         }
      } else {
         # have border group dir - use it.
          cat("***3923 Using the BorderGroup directory :\n",
              "        ",BorderGroupDir,"\n        to locate the checkpoint files.\n")
          RecoveryDir <- BorderGroupDir
      }
      
      # build full path to the check point files.
      RecoveryBase <- paste0(RecoveryDir,"/CheckPoint/",BGBase)
      
      cat("Checkpoint File Path used is :\n",
         "   ",RecoveryBase,"\n")
         
      
      #  Load #1 the areaParms dataset.
      
      #cat("***3925 Reading areaParms dataset : \n",
      #    "        ",paste0(BGBase,"_AP_Ckpt.RDA"),"\n")
      
      load(file=paste0(RecoveryBase,"_AP_Ckpt.RDA"))        # areaParms
      
      #  Load #2 the shape file (ERSI Shapefile format or whatever format it is.)
      SFCkpt        <- areaParms$CP_ShpLayer
      SFPCkpt       <- areaParms$CP_ShpDSN
      #cat("***3926 Reading shapefile : \n",
      #    "        dsn  =",SFPCkpt,"\n",
      #    "        layer=",SFCkpt,"\n")
      
      WorkSfMaster  <- sf::st_read(dsn=SFPCkpt,layer=SFCkpt)   # read in shape file.
      
      # The row.names of the shape file spatial structures are 
      # preserved by st_write and st_read. If the GIS system
      # does any modifications to the data base section of the shape
      # file, issues will develop.
      #
      # If the X__Key variable is maintained and the X__Key does not
      # match the row.names of the sf structure, the row.names will be
      # reset.
     
      SFNames <- names(WorkSfMaster)   # get names of fields.
     
      # Enforce the X__Key variable as the row.names on the sf.
      if (any("X__Key" == SFNames)) {
         # the variable exists.
         row.names(WorkSfMaster) <- WorkSfMaster$X__Key
      } else {
         xmsg <- paste0("***3928 The Shapefile has been modified and the 'X__Key' variable \n",
                        "        has been removed.  Rerun the BuildBorderGroup function to\n",
                        "        restore the variable.  Then re-edit the shapefile and \n",
                        "        do not remove the variable when editing the shape file.\n")
         StopFlag <- stopCntMsg(xmsg)
      }
      
      WorkSfMst     <- WorkSfMaster
      WorkSfMst     <- sf::st_make_valid(WorkSfMst)
      
      WorkSfMstData <- sf::st_drop_geometry(WorkSfMst)
      #  Shape file is loaded.
      
      #  Load the name table from the RDA
      NTCkpt        <- areaParms$CP_NTPath
      #cat("***3927 Reading NameTable: ",NTCkpt,"\n")
      load(file=NTCkpt)
      areaNamesAbbrsIDs <- as.data.frame(areaNamesAbbrsIDs,stringsAsFactors=FALSE)    
      NTable        <- areaNamesAbbrsIDs
      #   Name Table is loaded.
      
      #  Shape File restored to WorkSfMst
      #  Name Table restored to NTable
      #  areaParms table restored to areaParms
      
      #
      #  Reload some information and variables stored 
      #     in the areaParms data.frame
      #
       
      OrigProjFull <- areaParms$OrigProjFull
      CurProjFull  <- areaParms$CurProjFull
      
      BGDir        <- areaParms$BGDir
      BGBase       <- areaParms$BGBase
      bordGrp      <- areaParms$bordGrp
      areaUSData   <- areaParms$areaUSData
      enableAlias  <- areaParms$enableAlias
      MapMinH      <- areaParms$Map.MinH
      MapMaxH      <- areaParms$Map.MaxH
      MapAvgH      <- mean(MapMinH,MapMaxH)
      MapLData     <- areaParms$MapLData
      LabelCex     <- areaParms$LabelCex
      VisAsp       <- areaParms$Map.Aspect
      L2Feature    <- areaParms$Map.L2Borders
      MapHdr       <- NULL
      MapHdr[1]    <- areaParms$Map.Hdr1
      MapHdr[2]    <- areaParms$Map.Hdr2
      IDHdr        <- NULL
      IDHdr[1]     <- areaParms$ID.Hdr1
      IDHdr[2]     <- areaParms$ID.Hdr2
      RegFeature   <- areaParms$aP_Regions
      aP_Regions   <- RegFeature
   
      aP_Units     <- areaParms$aP_Units
      aP_Proj      <- areaParms$aP_Proj
      ProjUsed     <- aP_Proj
      
      # Reloading of the check point files is done.
   }  
      
   suppressMessages(      # Turn it off so we can process the shapefile.
      sf_use_s2(FALSE)
   )	
 
   #####  3A0x
   #
   # Since Shapefile could have been modified, make sure 
   # shape file is OK.
   #
   WorkSfMst     <- sf::st_make_valid(WorkSfMst)
   
   WorkSfMst     <- st_cast(WorkSfMst,"MULTIPOLYGON")   # Make sure all geometry elements are multipolygons
   WorkSfMstData <- sf::st_drop_geometry(WorkSfMst)
   WorkSfNames   <- names(WorkSfMst)   # get names of variables

   #
   #  Make sure the added variables are still there.
   #     X__Link should be gone..
   #     only X__Key should remain.
   if (!any("X__Key" == WorkSfNames)) {
      # X__Key variable are missing.   I can't restore row.names if needed to
      #  or compare them to see if they are right.
      xmsg <- paste0("***3A02 The shape file variables have been editted. The 'X__Key'\n",
                     "        variables are missing.  Redo the edits and do not \n",
                     "        delete the 'X__Key' variable.\n")
      stopCntMsg(xmsg)
   }
   
   KeyRowNames <- row.names(WorkSfMst)   # Check to see if the row.names match
                                         # the X__Key values.
   if (any(KeyRowNames != WorkSfMst$X__Key)) {
      # The row.names do not match the X__Key variables.
      xmsg <- paste0("***3A04 The shape file row.names in the spatial structure do not match \n",
                     "        the 'X__Key' variable values. Investigate and correct cause.\n",
                     "        row.names are reset to the 'X__Key' values.\n")
      row.names(WorkSfMst) <- WorkSfMst$X__Key
      stopCntMsg(xmsg)
    }
   
   #
   #####
   
   ##### 3A1x
   #
   #  Since projections have been done, no more dateline concerns.
   #
   #  Step 10.x - Build VisBorder data.frames from sf and UNION as needed.
   #
   #  a) Save sf Images for area, Regions, L2 and L3.
   #
   #  b) Preform UNIONS aS NEEDED ON sf for Regions, L2, and L3.
   #   
   #  c) Convert images into VisBorders format.
   #     Repeat for each layers sf (area, L2, L3, Regions)
   #
   #  d) Test images and Name Table together,
   #
   #  e) Write border group dataset.
   #
   #  f) Print out documentation on Name Table.
   #      labels to be used in data (Name, Abbr, Alt_Abbr, ID, and Alias.)
   #
   #  g) Draw the lattice maps and single map from the VisBorder 
   #      boundary dataset information.
   #
   
   #####  3A1x
   #
   #  Step 10.1 = Convert sf to VisBorder Data.frames and round vectex
   #
   #  User now has a usable SPDF for micromapST conversion and the start
   #  of the NameTable structure to enhance.
   #
   #  Rounding smoothing.  For lat/long rounding of 2 is approprivate (xxx.xx)
   #     However, this must be changed for other units of measure:
   #       Lat/Long  = round(x,4)     x.xxxx (0.00008983)   = 1/11131.94 of degree (0.008983% of degree)
   #          round(,6) = .xxxxxx,  5) = .xxxxx,   4) = .xxxx  ...  target =0.0001 (4)
   #       meters    = round(x,-1)    xxx0     = 10 meters or 36.9 feet.
   #       kilometer = round(x,-4)    x.xx0  = 10 meters 
   #
   #       1 mile = 1609.34 meters    (720 degrees around the world)
   #
   #       circumference of earth = 24901 miles  or 40,075,000 meters.
   #  
   #   The width and height of the map should also be taken into account.
   #   If the map covers a small area, rounding of the vectex x,y values
   #   may have to be changed to preserve the areas.
   #   At this time, the +units will always be "m".   
   #   Right now the rounding is at about the 36.4 to 36.9 feet increments.
   #
   #   Scaling and Rounding:
   #     a) Long/Lat = round(y, 4)   down to xxx.xxxx
   #     b) meters =   round(y,-1)   down to xxxx0 
   #     c) kilometers = round(y,-4) down to xx.xx0
   #
   #RndValue = 4, -1, -4   ???? fix
   #
   #   Current thought is must have at least 5000 increments.  
   #   For meters, it must come to 10s.  US map is 2660000 m high, 4509000 wide.
   #   To keep rounding to 10 meters, that gives up 266000 unite high, and 450900 wide.
   #   For Delaware: height=154,000m, width = 48,000m using 450,000 wide
   #   and 250,000 high. 
   #   US at 10m, so number of buckets is 250,000 high and 500,000 wide. So, 500,000 buckets 
   #   wins.   Applied to Delaware at 154,000m high and 48,000m wide = 154/500 high
   #   and 48/500 wide = 1/10 and 1/3... of a meter - smaller wins.
   #   Rhode Island = 77,000 high, 60,000 wide.
   #   Future improvement could be to calculate a better rounding value then 10 
   #   based on the size of the area.
   #
   
   # Select correct rounding factor for the size of map and distance on the map.

   ##### 3A3x
   #
   #  Step 10.1  - get sf for each boundary set - area, L2, L3, and Regions
   #
    
   cat("***3A30 Creating the 4 micromapST boundary data.frames (area, L2,\n",
       "        L3, and Regions).\n")
    
   BCol    <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999")
   
   vDebug  <- debug
   
   if (bitwAnd(vDebug,256) != 0) {              # change - dropped 512 and 1024
      # caller wants sample maps
       
      PPsf         <- WorkSfMst
      PPTitle      <- "Before conversion to VisBorder"
      PPMfrow      <- c(4,4)
      SamplePrts_sf(PPsf,PPTitle,PPMfrow,vDebug,NTable$Key,MapAvgH)    # gotcha
   }
   
   Wkbbx    <- sf::st_bbox(WorkSfMst)
   wDist    <- abs(Wkbbx[1]-Wkbbx[3])/7500

   SfMstrows <- row.names(WorkSfMst)  # the list of rows in the WorkSf08 sf
   #  No more unions on the area layer
   
   if (L2Feature) {    
      #
      #  L2 Groups - Create boundaries
      #
      #cat("Layer 2 Feature groups.\n")
      # do union based on L2_IDs in the Name Table
      # Get list from WorkSf row.names order from Name Table.
      WorkL2             <- WorkSfMst   # get copy of area master.
      L2Grps             <- NTable[SfMstrows,"L2_ID"]   # assuming one sf row (area) per name table row
      WorkL2$agg         <- L2Grps
      WorkL2$agg_name    <- NTable[SfMstrows,"L2_ID_Name"]
   
      WorkL2             <- aggregate(WorkL2,by=list(L2Grps),FUN=aggFun)
      row.names(WorkL2)  <- WorkL2$agg  # restore row.names
      #cat("calling getColoring for L2 Code 8014 \n")
      
      L2_NTCC            <- as.data.frame(getColoring(sf::st_geometry(WorkL2)),stringsAsFactors=FALSE)  # Have colors for super-areas.
      row.names(L2_NTCC) <- row.names(WorkL2)             # save xNTCC table for later use. ? check point.
                                                          # can't recreate after conversion to VisBorders.
      NTable$L2CCode     <- L2_NTCC[row.names(NTable),1]  # save the color index for the area.
   
      #### Only if L2 flag set. 
   
      if (bitwAnd(debug,8) != 0 ) {
         #cat("Calling getColoring - Code 8024 \n")
         WorkL2Neib   <- getColoring(sf::st_geometry(WorkL2))
         #cat("Looking at Neighbors:", WorkL2Neib, " (colors)\n")
         
         WorkL2ColK   <- BCol[WorkL2Neib] # find best color pattern
         
         WorkL2sfc    <- sf::st_geometry(WorkL2)
         OutPutP      <- paste0(BGPathName, "BBG-Level_2_map_image.pdf")
         grDevices::pdf(OutPutP,width=10, height=7)
         Sys.setFileTime(OutPutP,Sys.time())
                   
         plot(WorkL2sfc,main='',lwd=0.2,asp=1,key.pos=NULL, col=WorkL2ColK)
         graphics::title("Level 2 SP Shape file data")
         x <- grDevices::dev.off()
      }
   } 
 
   if (RegFeature) {
      #
      #  Regional Groups create boundaries
      #
      #cat("Regional Feature Groups Code 8045 .\n")
      WorkReg          <- WorkSfMst
      RegGrps          <- NTable[SfMstrows,"regID"]   # assuming one sf row per name table row.
      WorkReg$agg      <- RegGrps
      WorkReg$agg_name <- NTable[SfMstrows,"regName"]
      WorkReg          <- aggregate(WorkReg,by=list(RegGrps),FUN=aggFun)
      row.names(WorkReg) <- WorkReg$agg
   
      Reg_NTCC            <- as.data.frame(getColoring(sf::st_geometry(WorkReg)),stringsAsFactors=FALSE)
      row.names(Reg_NTCC) <- row.names(WorkReg)             # save xNTCC table for later use. ? check point.
                                                            # can't recreate after conversion to VisBorder
                                                            
      NTable$R2CCode   <- Reg_NTCC[row.names(NTable),1]       # save the color index for the area.
                                    # CCode ranges from 1 to "n" - that is the number of colors required.
  
      #print(sf::st_geometry(WorkReg))
 
      # Only if region flag set.
   
      if (bitwAnd(debug,8) != 0) {
         #WorkRegColK <- BCol[nacol(WorkReg)]
         #cat("Region outline - get Coloring\n")
         
         WorkRegColK  <- BCol[getColoring(sf::st_geometry(WorkReg))]
         WorkRegsfc   <- sf::st_geometry(WorkReg)
         OutPutP      <- paste0(BGPathName,"BBG-Regional_level_map_image.pdf")
         grDevices::pdf(OutPutP,width=10, height=7)
         Sys.setFileTime(OutPutP,Sys.time())
         
         plot(WorkRegsfc,main='',lwd=0.2,asp=1,key.pos=NULL,col=WorkRegColK)
         graphics::title("Region Shape file data")
         x            <- grDevices::dev.off()
      } 
   
      #
   }
   
   #  L3 (All) outline
   #
   #cat("Layer 3 - outline of all.\n")
   WorkL3          <- WorkSfMst

   WorkL3agg       <- aggregate(WorkSfMst,by=list(rep("L3",length(SfMstrows))),FUN=aggFun)
   #  Aggregate reverted the spatial objects back to polygons.  Have to re cast it again.
   #plot(sf::st_geometry(WorkL3agg))
   
   ##WorkL3aggb      <- sf::st_boundary(WorkL3agg)
   ##   In some cases st_boundary creates a linestring geometry.  This can't be
   ##   converted back to Multipolygon in one step.  Needs MULTIPOLYGON to 
   ##   ensure the conversion to VisBorders works.
   ##object.size(WorkL3aggb)
   
   #cat("Layer 3 - outline of all.\n")
   
   WorkL3          <- sf::st_cast(WorkL3agg,"MULTIPOLYGON")   # Make sure all geometry elements are multipolygons
   #            This is required for the BuildVisBorders to work right.
   WorkL3$agg      <- BorderGroupName
   WorkL3$agg_name <- BorderGroupName
  
   #
   #  Steps to find concave hull  - not used.
   #  a) st_union (not useful, skip)
   #  b) get bbox for sizing
   #  c) find distance for st_buffer  =  width of map / 7500 (meters)  = US is 4882 km = 4,882,000
   #     st_buffer increases the size of the file enormously..  find another method of 
   #     building a concave hull boundary for entire map.  
   #    I suppect, the modificates create a small river like gap between RI and MA on the right 
   #    and also a gap on the top border between MA and RI and CT.   st_boundary can't 
   #    catch it since it looks like a river.
   #
   #  -- Did not use st_buffer since it inflated the size of the objects by 2x and did not really help.
   #
   #  Alternate Steps:
   #  1) get a clean and valid copy of the boundaries.
   #  2) aggregate all boundaries to one key.
   #  3) Use st_boundary function to clean up edges.
   #  4) Set the results to ALL MULTIPOLYGON elements.
   #  5) Set the aggregate names in the data section.
   #
   #suppressMessages(    # removed.
   #   suppressWarnings(
   #      WorkL3          <- sf::st_as_sf(sf::st_union(WorkL3))  # all of the areas.
   #   )
   #)
   #WorkL3   <- sf::st_buffer(WorkL3,wDist)

   #plot(sf::st_geometry(WorkL3))
  
   if (bitwAnd(debug,8) != 0) {
      WorkL3sfc <- sf::st_geometry(WorkL3)
      OutPutP   <- paste0(BGPathName,"BBG-Map_outline_(L3)_map_image.pdf")
      grDevices::pdf(OutPutP,width=10, height=7)
      Sys.setFileTime(OutPutP,Sys.time())
         
      plot(WorkL3sfc,main='',lwd=0.2,asp=1,key.pos=NULL,col=BCol[2])
      graphics::title("L3 SP Shape file data")
      x         <- grDevices::dev.off()
   } 
   #
   ######
   
   ######   3A2x
   #
   #  Step 10.2  - convert each boundary Sf to the VisBorders format.
   #
   #    Warning 3A22 = Invalid Polygons.
     
   cat("Building VisBorders data.frames for the areas.\n")
   areaVisBorders      <- BuildVisBorder(WorkSfMst, "area")
   #head(areaVisBorders,20)
   GrpList <- c("areaVisBorders")
   
   uAVBKey <- unique(areaVisBorders$Key)
   #cat("Keys - areaVisBorders - Unique\n")
   #print(uAVBKey)

   if (L2Feature) {
      cat("Building VisBorders for the level 2 boundaries.\n")
      L2VisBorders        <- BuildVisBorder(WorkL2, "Level 2")
      #head(L2VisBorders,20)
      GrpList <- c(GrpList,"L2VisBorders")
      
      uL2VBKey <- unique(L2VisBorders$Key)
      #cat("Keys - L2VisBorders - Unique\n")
      #print(uL2VBKey)
   
   }
   if (RegFeature) {
      cat("Building VisBorders for the Regional boundaries.\n")
      RegVisBorders       <- BuildVisBorder(WorkReg, "Regional")
      #head(RegVisBorders,20)
      GrpList <- c(GrpList,"RegVisBorders")
   
      uRegVBKey <- unique(RegVisBorders$Key)
      #cat("uRegVBKey - RegVisBorders - Unique\n")
      #print(uRegVBKey)
   }
   cat("Building VisBorders for the Level 3 map outline.\n")
   L3VisBorders        <- BuildVisBorder(WorkL3, "Level 3 Map Outline")
   #head(L3VisBorders,20)
   GrpList <- c(GrpList,"L3VisBorders")
   #
   #cat("***3A28 Completed conversion to VisBorders format.\n")
   #
   ####
   
   ####
   #
   #  Clean up VisBorders DF
   #
   areaVisBorders$L1 <- NULL
   areaVisBorders$L2 <- NULL
   areaVisBorders$L3 <- NULL
  
   L3VisBorders$L1   <- NULL
   L3VisBorders$L2   <- NULL
   L3VisBorders$L3   <- NULL
  
   if (exists('L2VisBorders')) {
      L2VisBorders$L1   <- NULL
      L2VisBorders$L2   <- NULL
      L2VisBorders$L3   <- NULL
   }

   if (exists('RegVisBorders')) {
      RegVisBorders$L1   <- NULL
      RegVisBorders$L2   <- NULL
      RegVisBorders$L3   <- NULL
   }

   #####  3A3x
   #
   #  Step 10.3 - Display on the screen the final results.
   #
   
   if (bitwAnd(debug,2048) !=0) {
      ###### 
      #
      #  Test Plotting to windows of each VisBorders  (debug= 2048)
      #
      #  areaVisBorders
      #
      grDevices::dev.new()
      # The range of the CCode should only be between 4 and 6 at max.
      #cat("Drawing test images of the border group layers to the screen'\n")
      #cat("  one per windows.  Each must be manually closed.\n")\
      
     
      maxCol           <- max(NTable$CCode)
      if (maxCol < 5) maxCol = 5
      WANCol           <- RColorBrewer::brewer.pal(maxCol, "RdYlBu")
      WANAreaCCode     <- NTable[areaVisBorders[is.na(areaVisBorders$x),"Key"],c("CCode")] 
      WANAC            <- WANCol[WANAreaCCode]  # ordered by neigbhor
      
      #   areaVisBorders 
      PlotVis(areaVisBorders,WANAC)
      graphics::title("VisBorder of areas")
      grDevices::dev.new()
        
      if (L2Feature) {
         #   L2VisBorders
         #   Clean up unused columns
      
         L2VisB           <- NULL
         L2VisB$Key       <- L2VisBorders[is.na(L2VisBorders$x),"Key"]
         uniL2Keys        <- unique(L2VisB$Key)
         
         maxCol           <- length(uniL2Keys)
         #cat("L2 Max Colors:",maxCol,"\n")
         if (maxCol > 10) {
            WANCol1       <- RColorBrewer::brewer.pal(11, "RdYlBu")
            WANCol1       <- rep(WANCol1, maxCol / 11 + 1)
            L2Col         <- WANCol1[1:maxCol]
         } else {
            L2Col         <- RColorBrewer::brewer.pal(maxCol,"RdYlBu")
         } 
         #cat("Colors:", paste0(L2Col,collapse=", ",sep=""),"\n")
         xm               <- match(L2VisB$Key, uniL2Keys)
         L2VisB$Col       <- L2Col[xm]
        
         PlotVis(L2VisBorders,L2VisB$Col)
         graphics::title("VisBorders of L2")
         grDevices::dev.new()
      }
      
      if (RegFeature) {
         # RegVisBorders
         # Clean up unused columns
         
         RegVisB          <- NULL
         RegVisB$Key      <- RegVisBorders[is.na(RegVisBorders$x),"Key"]
         uniRegKeys       <- unique(RegVisB$Key)
         
         maxCol           <- length(uniRegKeys)
         #cat("Reg Max Colors:",maxCol,"\n")
         if (maxCol > 10) {
            WANCol1       <- RColorBrewer::brewer.pal(11, "RdYlBu")
            WANCol1       <- rep(WANCol1, maxCol / 11 + 1)
            RegCol        <- WANCol1[1:maxCol]
         } else {
            RegCol        <- RColorBrewer::brewer.pal(maxCol,"RdYlBu")
         } 
         xm               <- match(RegVisB$Key, uniRegKeys)
         RegVisB$Col      <- RegCol[xm]
         #cat("Colors:", paste0(RegCol,collapse=", ",sep=""),"\n")
         #
         #  Must be done when area is in spatial structure (sf).
         #  Then encode the color code and save in the levels name table for that level.
         #  Later use the name table to help translate the VisBorder entries back to 
         #  colors per polygon.
         #  Name Table.
         #
       
         PlotVis(RegVisBorders,RegVisB$Col)
         graphics::title("VisBorder of Regions")
         grDevices::dev.new()
      }  
       
      
      PlotVis(L3VisBorders,"green")
      graphics::title("VisBorder of L3")
      
   }  # end of the test plots of each VisBorder data.frame as a set of windows.
   #
   ###
      
   #####  3A5x
   #
   #   Plot final areaVisBorder in file.
   #
   #   Use Name Table for Color codes.
   #
   if (bitwAnd(debug,512+1024) != 0) {
      #  plot of final areaVisBorders map to PDF or PNG file.
      #cat("Generating scaled example of map - after conversion to micromapST format.\n")
      maxCol       <- max(NTable$CCode)
      WANCol       <- RColorBrewer::brewer.pal(maxCol, "RdYlBu")
      WANAreaCCode <- NTable[areaVisBorders[is.na(areaVisBorders$x),"Key"],c("CCode")] 
      WANAC        <- WANCol[WANAreaCCode]  # ordered by neigbhor
   
      PPTitle      <- "Final_areaVisBorders"
      PngH         <- 4 + .4
      xAsp         <- areaParms$Map.Aspect
      PngW         <- PngH / xAsp   #  Y / (Y/X)  <-  Y * X/Y
      #cat("xAsp:",xAsp,"  PngW:",PngW,"  PngH:",PngH,"\n")
      
      FTitle       <- gsub(" ","_",PPTitle)
      
      OutTestSm    <- paste0(BGPathName,"_FP_",FTitle,OType)
      if (OType == ".png") {
         grDevices::png(OutTestSm, res=300, width=PngW, height=PngH, units="in")
      } else {
         grDevices::pdf(OutTestSm, width=PngW, height=PngH)
      }
      
      Sys.setFileTime(OutTestSm,Sys.time())
         
      #cat("par('din')",par('din'),"  par('fin'):",par('fin'),"  par('pin'):",par('pin'),"\n")
           
      #par(mfrow=c(1,1))
      par(omi=c(0,0,0,0))
      par(oma=c(0,0,0,0))
      par(mai=c(0,0,0,0))
      par(mar=c(0,0,2,0))
              
      PlotVis(areaVisBorders,WANAC)     # micromapST defaults lwd to 0.5  
      #graphics::title(main=PPTitle,cex.main=0.1)
      
      # draw the extra characters (wrong - correct)
      MapT <- data.frame(l=NTable$MapL,x=NTable$MapX,y=NTable$MapY)
      row.names(MapT) <- NTable$Key
      #print(MapT)
      
      xm <- !is.na(MapT$l)
      MapT <- MapT[xm,]
      #print(MapT)
      
      if (dim(MapT)[1] > 0 ) {
         #cat("MapT matrix for labels:")
         #str(MapT)
         graphics::text(MapT$x, MapT$y, MapT$l, cex=LabelCex)  # micromapST LabelCex multiplier = def=0.25
      } 
      x <- grDevices::dev.off()
   
   }  # end of PDF or PNG final image plot of areaVisBorder borders.
   
   #
   #  Finish Name Table build out.
   #
   #####
   
   #####
   #
   #   Clean up entries in areaParms for final border Group - no checkpoint information.
   #
   areaParms$CP_NTPath   <- NULL
   areaParms$CP_ShpDSN   <- NULL
   areaParms$CP_ShpLayer <- NULL
   #
   #####
   
   
   ##### 3A5x
   #
   #
   #  Fields: 
   #   bordGrp  -  name of border group
   #
    
   #  Save the individual data.frames   (no need to dump so much.)
   #
   #cat("***3A53 Writing an images of each Border Group data.frame for ",BGBase,"\n")
   
   #save(areaNamesAbbrsIDs, file=paste0(BGPathName,"_areaNamesAbbrsIDs.rda"), compress="xz")
   #save(areaVisBorders,    file=paste0(BGPathName,"_areaVisBorders.rda"),    compress="xz")
   #if (L2Feature)  save(L2VisBorders, file=paste0(BGPathName,"_L2VisBorders.rda" ), compress="xz")
   #if (RegFeature) save(RegVisBorders,file=paste0(BGPathName,"_RegVisBorders.rda"), compress="xz")
   #save(L3VisBorders,      file=paste0(BGPathName,"_L3VisBorders.rda"),      compress="xz")
   #save(areaParms,         file=paste0(BGPathName,"_areaParms.rda"),         compress="xz")
   
   #
   #  Save the border group data set of all data.frames
   #
   saveL <- c("areaParms",  "areaNamesAbbrsIDs", GrpList)
   #saveL
   
   SavePath <- BorderGroupPath # paste0(BGPathName,"BG.rda")
   save(list=saveL,file=SavePath, compress="xz")   # save border group datasets for check point.
   
   cat("***3A55 Border Group Created - Successfully.\n")
   #
   #####
   
   #####
   #
   #   Create scale images of maps one per linked micromap image.
   #
   if (bitwAnd(debug,8192) != 0) {
         
      #cat("Generate scaled example of test maps - after rounding and convert to VisBorders.\n")
         
      VisB         <- areaVisBorders
      KeyList      <- unique(VisB$Key)
      
      KeyCol       <- data.frame(Key=KeyList,Col=NA)
      #KeyCol$Name <- NTable[KeyCol$Key,"Name"]
      
      KeyNum       <- dim(KeyCol)[1]  # Get number of rows.
      NumPanels    <- as.integer((KeyNum-1)/5) + 1  # calculate number of panels.
      #NumPanels   <- as.integer(NumPanels)
      
      BaseColors   <- c(mcolors[1],mcolors[2],mcolors[3],mcolors[4],mcolors[5])
      Base6Colors  <- c(BaseColors,mcolors[6])
      
      BlankColors  <- c(NA,NA,NA,NA,NA)
      
      # find the multiple polygons per area.  Must have colors per polygon.
      KeyNA        <- as.data.frame(VisB[is.na(VisB$x),"Key"])  # list of Keys and NA (x coordinates)
      names(KeyNA) <- c("Key")
      KeyNA$Col    <- NA
      KeyNA$Inx    <- match(KeyNA$Key,KeyCol$Key)
      
      VColors      <- c(BaseColors, rep(NA,KeyNum-5))
      
      # One image per group/row.
      PDFTest      <- paste0(BGPathName,"_TestChart_based_on_VisBorder.pdf")
      grDevices::pdf(PDFTest,width=10.5,height=7.75)
      Sys.setFileTime(PDFTest,Sys.time())
         
      
      par(mai=c(0.125,0.125,0.125,0.125))  #  1/8" around
      par(mar=c(1,1,2,1))
      par(oma=c(.5,.5,.5,.5))
      
      par(mfrow=c(4,4))   # setup to provide about the same space as a micromap
      
      for (inx in c(seq(1,NumPanels))) {
      
         KeyCol$Col   <- VColors
         # now match the polygon list KeyCol list and pick up the color
         KeyNA$Col    <- KeyCol[KeyNA$Inx,"Col"]
         PlotCol      <- KeyNA$Col
            
         PlotVis(areaVisBorders,PlotCol,xLwd=.5)
         par(new=TRUE)
        
         xm           <- !is.na(KeyCol$Col)
         KeyLeg       <- KeyCol$Key[xm]
         KeyLegCol    <- KeyCol$Col[xm]
      
         #legend("right", KeyLeg, text.col = "black", cex=0.5, bty="n",
         #       pch=NA, xpd=TRUE, vfont=c("san serif"), inset=-0.05)
         legend("right", KeyLeg, text.col = KeyLegCol, cex=0.5, bty="n",
                pch=NA, xpd=TRUE, inset=-0.05)
         
         VColors      <- c(BlankColors,VColors)[1:KeyNum]
      }
      
      x <- grDevices::dev.off()
      
   }  # end of multiple small image print out 
   #
   ###
   
   ### 3A6x
   #
   #   Final summary report of the names, abbrs, IDs, Alias's, and Alt_Abr used 
   #   in the name table as documentation of the border group.
   #
   #  Convert to write to Ascii file.
   #
   RepOutFile <- paste0(BorderGroupDir,"/",BGBase,"BG_rpt.txt")
   
   cat("***3A60 Summary build report of names, abbr, id and other data \n",
       "        is written to ",RepOutFile,"\n")
   
   sink(RepOutFile)
        
   cat("\n\n\nPUBLICATION INFORMATION FOR NAME TABLE IN BORDER GROUP : \n",
       "   ",paste0(BGBase,"BG")," ",format(Sys.time(),format="%Y-%m-%d %H:%M:%S"),"\n")
   cat("\n\n")
   
   NTNames  <- names(NTable)
   #print(NTNames)
   
   TCol     <- c("Name","Abbr","ID", "Alt_Abbr", "Alias")
   CCol     <- c("full","ab","id", "alt_ab", "alias")
   L2Col    <- c("L2_ID","L2_ID_Name")
   RegCol   <- c("regID", "regName")
   AltL2RegCol <- c(L2Col,RegCol)
   ModCol   <- c("Xoffset","Yoffset","Scale","Rotate")
   MapLabel <- c("MapL","MapX","MapY")
   AltModMapLCol <- c(ModCol,MapLabel)
   
   xm       <- match(NTNames,TCol)  # what columns are in Name Table
   #print(xm)
   
   # Have a number if there is a match otherwise a NA.
   xmNA     <- !is.na(xm)  # now a TRUE for the matches.
   #print(xmNA)
   NTUser   <- NTable[,xmNA]
   NTNames  <- names(NTUser)
   xmm      <- match(NTNames,TCol)
   xmmNA    <- !is.na(xmm)
   NTNamesC <- CCol[xmm]
   names(NTUser) <- NTNamesC
   
   print(NTUser)	    #  print a copy of the users name table. 
   cat("\n\n")
   
   ###  Duplicate - already done..
   
   NTrn    <- row.names(NTable)
   NTnr    <- dim(NTable)[1]              # number of rows in NTable
   
   if (L2Feature) {
      L2Uni   <- length(unique(NTable$L2_ID))  # number of unique entries in L2 list
      L2List  <- ( L2Uni != NTnr )
      L2Yes   <- L2List & (L2Uni > 1)
   }
   if (RegFeature) {
      RegUni  <- length(unique(NTable$regID)) 
      RegList <- RegUni != NTnr
      RegYes  <- RegList & ( RegUni > 1)
   }
   if (RegFeature && L2Feature) {
      # Both sets of columns are valid.
      cat("\n\nName Table Layer 2 and Regional Values\n")
      print(NTable[,AltL2RegCol])
      
   } else {
      # none, one or the other are needed.
      if (L2Feature) {
         cat("\n\nName Table Layer 2 Values\n")
         print(NTable[,L2Col])
      }   
      if (RegFeature) {
         cat("\n\nName Table Regional Values\n")
         print(NTable[,RegCol])
      }
   }
   
   MapLYes   <- sum(!is.na(NTable$MapL)) > 0
   DoAdjYes  <- sum(NTable$DoAdj) > 0
   
   if (MapLYes & DoAdjYes) {
      cat("\n\nName Table Modifications and Map Label Values\n") 
      print(NTable[,AltModMapLCol])
   
   } else {
      if (DoAdjYes) {
         cat("\n\nName Table Map Modifications Values\n")
         print(NTable[,ModCol])
      }
      if (MapLYes) {
         cat("\n\nName Table Map Label Values\n")
         print(NTable[,MapLabel])
      }
   
  } 
  cat("\n\nAny entries in the name table with location ids with a value \n",
      "of 'NA', '', or ' ' are considered empty and will not be ignored.\n\n")
  sink()   # end of report to file. 
  # END OF report for documentation.
  
  suppressMessages(
     sf_use_s2(TRUE)
  )

  cat("***3A69 Border Group:",paste0(BGBase,"BG")," is done.\n")
  
  invisible(SavePath)   # return the file path to the written border group .rda file.
}   
   
