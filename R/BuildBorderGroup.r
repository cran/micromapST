######
#
#  date: June 27, 2022
#
#  packages used by BuildBorderGroup function
#
#library<base>                     # bitwise function; 
#
#
#library(tools)			   #  tools: CRAN_check_xxx; file_ext; file_path_sans_ext; 
#
#
#library(stringr)	           #  stringr: str_trim; str_split; str_to_upper; str_pad; str_locate; str_replace;
#                                  #           str_sub;  str_to_upper; str_to_lower
#
#library(graphics)
#                                  # plot, plot.new, 
#
#library(graphicR)
#
#
#library(utils)			   #  utils:
#
#
#library(RColorBrewer)             #  RColorBrewer: brewer_pal
#
#
#library(readxl)                   #  readxl: read_xls; read_xlsx
#
#
#library(writexl)		   #  writexl: write_xlsx
#
#
#library(cleangeo)                 #  cleangeo:  <entire collection>
#
#
#
#   Retired
#library(rgdal)                    #  rgdal: *readOGR, *showWKT, *projInfo, *writeOGR, spTransform,
#                                  #         CRS-class, CRS; *CRSargs; 
#                                  #         rgdal_extSoftVersion;
#                                  #         set_rgdal_show_exportToProj4_warnings; set_thin_PROJ6_warnings;
#
#                       Replacements:
#       projInfo()			sf::st_proj_info()
#       readOGR()			sf::st_read  or  sf::read_sf
#       writeOGR()   			sf::st_write  or  sf::write_sf
#       showWKT()			sf::st_as_text(crs)
#       
#
#               
#
#                                  #         
#library(maptools)                 #  maptools: getinfo.shape, lineLabel, pointLabel, readShapePoly, sp2tmap, spCbind-methods,
#                                  #            spRbind, unionSpatialPolygons, checkPolygonsHoles, spCbind
#	unionSpatialPolygons		sf::st_union
#       
#
#
#
#library(sp)		           #  sp: *spTransform; sp::plot(?)
#                                  #      *slot(...,"proj4string"); *coordinates; *?aggregate; 
#                                  #      *bbox; *CRS; *wkt(?)
#                                  #      SpatialPolygons; SpatialPolygonsDataFrame; Polygons; Polygon
#
#            sp::spTransform(x, CRSobj) sf::st_transform(x, crs)
#            sp::coordinates()		sf::st_coordinates()
#            sp::coordinates()= ~x+y    sf::st_as_sf(x, coords=("x","y"))
#            sp::bbox(x)		sf::st_bbox(x) or matrix(sf::st_box(x),2)
#            sp::x@data                 sf::st_geometry(x, NULL)  or  sf::st_drop_geometry(x)
#            sp::proj4string(x)   	sf::st_crs(x)   or slot()
#            sp::proj4string(x)<-	sf::st_crs(x)<-
#	     sp::CRS(projargs)          sf::st_crs(proj4string)
#            sp::aggregate(x,by,mean,areaWeighted=TRUE)  sf::st_interpolate_aw(x,by,mean)  More research
#            sp::Polygon()		sf::st_polygon, sf::st_multipolygon
#            sp::polygons		sf::st_geometry
#            sp::polygons<-		sf::st_set_geometry
#            sp::Polygons() 		???
#            sp::SpatialPoints		sf::st_sfc...
#            sp::SpatialPointsDataFrame sf::st_sf
#            sp::SpatialPolygons        sf::st_sfc...
#            sp::SpatialPolygonsDataFrame  sf::st_sf
#            
#
#   Basics:	data.frame, cbind, rbind; row.names; as.data.frame;
#
#library(rgeos)                    #  rgeos: gIsValid, gCentroid, gUnion; gUnaryUnion?; gWithin
#
#          rgeos::gArea			sf::st_area
#          rgeos::gBoundary		sf::st_boundary
#          rgeos::gBuffer		sf::st_buffer
#          rgeos::gCentroid		sf::st_centroid
#          rgeos::gDifference		sf::st_difference
#          rgeos::gIsValid		sf::st_is_valid
#          rgeos::gPolygonize		sf::st_polygonize
#          rgeos::gSimplify		sf::st_simplify
#          rgeos::gTouches		sf::st_touches
#          rgeos::UnaryUnion		sf::aggregate
#          rgeos::gUnion		sf::st_union
#          rgeos::readWKT		sf::st_as_sf
#          rgeos::writeWKT		sf::st_as_text
#
#
#
#
#
##library(devtools)                #
##library(proj4)	           #  proj4:
##library(dplyr)		   #  dplyr:
#library(futile.logger)
#library(futile.options)
#library(MazamaCoreUtils)          #  ???


######
#
#   BuildBorderGroup  function to support micromapST.
#   Updated: 2021-1024,  2022-0316 (Testing and demo data)
#
#   This function takes a shapefile and name table (CSV or Excel format)
#   and creates a border group ".RD" of the geographic 
#   areas in the shape file for use as boundary data and location 
#   ids for micromapST. The micromapST package default border
#   group is a characterized US State map.  Several examples
#   of state county boundary, US Seer 18 Registry boundary, continent (Africa)
#   country (China, UK), and city (Seoul , S. Korea) are 
#   included in the micromapST package.
#
#   Several features of micromapST border groups are not supported
#   by this build script:
#
#      1) Level 2 regional boundaries.
#           If L2 boundaries differ from the area and regional 
#           boundaries, L2 is enabled and the VisBorders data.frame built.
#      2) Regional mapping based on areas containing data.
#           In this case, only regions in the border group data
#           containing data are mapped.
#           If the reg boundaries are different from the area boundaries
#           then regions is enabled and a regions VisBorders data.frame
#           is included in the BG.
#      3) The L2 boundaries must be a subset of the Regional boundaries. 
#           The hierarchy is always area, L2, regional, L3.  
#           All must be subsets of the higher level.
#      4) Characterization of the boundary data to ensure 
#           all areas are visible as clear or colored in the 
#           drawn micro map.  The areas must be manually adjusted
#           prior to calling this function.
#      
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
#               Name of @data column containing the link to the initial NameTable,
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
#                  f) Build full Name Table for use with the shape file processing and to validate 
#                     contents of columns, Backfill column information as needed.
#                  g) write out intermediate NameTable as check point, 
#                  h) Read Shapefile or accept SPDF from caller and validate.
#                  i) Check holes and validate completeness of the SPDFs geometry.
#                  j) Validate shape file (@data) and "ShapeLinkName", does shape file 
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
#               original shapefile - so that would be long/lat or a projection's +units.
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
#               projection it will be inspected and it's +units= parameter 
#               changed to meter. The actual map transformation is done at the end 
#               of the process before the micromapST border group is created. 
#               5) The proj4 calling parameter will override the shapefile's projection.
#               6) If after processing the transformations for the Shapefile and/or
#               the proj4 calling parameter, the SPDF is still a long/lat projection,
#               the build function will create a AEA projection based on the map's 
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
#               The user can modify the name table or the shapefile in any way.
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
#               in the SpatialPolygons structure. Once the @data is re-created,
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
#               the union, and @data rebuilding, but not doing any E/W Hemisphere 
#                adjustments. While the EWH adjustments are 
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
#  2022-0510  - EPSG code for Lat/Long in US  NAD83 = EPSG:4269
#                                    ESRI:102003 AEA us continental
#  2022-0513  - Place the suppendWarnings() function around any R statment
#               that generates a rgdal, sp, or proj6 warning in regard
#               the proj4string usage inside a used package that we don't
#               have access to the code.  It is hoped this cleans up
#               the messages generated to the user and removes the 
#               confusing warnings we and they cannot control.
#  2022-0514  - Updated - code and saved.
#  2022-0625  - Cleaned up error and output messages to be usable to users.
#             - Added call parameter to specify the size of the Labels when used.
#               This variable is also added to the areaParms data.frame
#             - Updated logic to enable Region feature in micromapST when regID column exists.
#  2022-0630  - Add Global address space - can't use micromapST's
#
#             
#             
#
# 
#
#  Operation Sequence:
#
#       0) (development) Load support libraries
#		maptools, stringer, foreign, sp, rgdal, cleangeo,
#               dev tools, rHub, cleangeo, 
# 	1) Verify all call parameters and referenced data structures.
#       2) Read shape file and convert into Spatial Polygon (sp) structure.
#       3) Validate and clean SP image of all boundaries
#       4) Read Name Table, verify columns, and verify content of columns.
#       5) Convert MapLabel columns into MapL, MapX, and MapY columns.
#       6) Process LINK between Name Table and Shape file and 
#          assign the "key" to use in micromapST.
#       7) Simplify SP data with rmapshaper. Reduce to 2 to 0.6
#       8) Union all polygons of areas under the area in the SPDF
#       9) Build areaParm data.frame
#       10) Clean up Name Table - remove work columns
#       11) Check Point Name Table and Shapefile to disk for possible 
#           processing.
#       12) Reload for check point restart (checkPointReStart = TRUE)
#       13) Plot to validate boundary data images.
#       14) Convert shape file data into VisBorder format for micromapST.
#       15) Create L2, Reg, and L3 boundary datasets
#       16) Bundle all border group dataset and write as one R dataset to
#           .rda compressed file.
#
#  Steps to build a new Border Group for micromapST from a shape file.
#
#  This routine accepts a shape file and creates a Boundary Group for 
#  use by the micromapST package.
#
#  Author:  James Pearson, StatNet Consulting
#  Updated: Auguar 13, 2022
#
#  Version:  1.00 - beta
#
#  library packages required by this code.  If this code is converted 
#  to a function and released as a package, the list of library packages
#  will be converted into a set of "required" packages.
#
#  Packages used in this function are:
#  sp:		spTransform
# 		Polygons
#		Polygon
#		slot(..., "proj4string")
#               bbox
#               cbind, rbind,
#               CRS
#               SpatialPolygons
#               SpatialPolygonsDataFrame
#               coordinates
#               aggregate
#               plot of sp
# 		wkt
#
#               CRSargs
#               readOGR
#  tools	CRAN_check_xxxx
#  		file_ext
#		file_path_sans_ext
#  stringr	str_trim
#               str_split
#               str_to_upper
#               str_sub
# 		str_pad
#		str_locate
# 		str_replace
#  <base>	bitwise functions
#               plot
#  readxl	read.xls
# 		read.slsx
#  writexl	write.xlsx
#  utils
#  RColorBrewer	brewer_pal
#  cleangeo     <entry collection of function>
#  logger	<due to cleangeo> 
#  graphicR	???
#  rmapshaper   ms_simplify
#  graphic
#  
##  proj4
##  devtools
##  dplyr
##  
# Retired packages used the need programming changes in the near future:
#  rgdal	readOGR    # done
#		writeOGR   # done
#		showWKT
#		CRS-class
#		CRS
#		CRSargs
#		projInfo
#               rgdal_extSoftVersion
#               set_rgdal_show_exportToProj4_warnings=none
#		set_thin_PROJ6_warnings
#		spTransform
#  maptools	getinfo.shape
#		lineLabel
#  		pointLabel
# 		spRbind
#		unionSpatialPolygons
#		spCbind and -method
#		checkPolygonsHoles
#               readShapePoly (?)
#		sp2tmap (?)
#  rgeos	gArea
# 		gUnion
#		gDifference
#		gIsValid
#		gCentroid
#		gWithin (?)
#		gContain (?)
#		gContain???  (?)
#		gUnaryUnion (?)
#
#		
#		
#   Retired
#library(rgdal)                    #                                
#library(maptools)                 #           
#library(rgeos)                    # 
#
#library(sp)		           #  General: as.data.frame;
#                                  #      data.frame; row.names; 
#library(cleangeo)                 #
#library(logger)                   # 
#
#library(sf)
#library(terra)
#library(stars)
#
##library(proj4)		   # 
##library(dplyr)		   #  
##library(devtools)                #

#


#####
#  Initialize the definition of Global variable outside of the BuildBorderGroup function name space.
   #
   utils::globalVariables(c(
    
	"warnCnt",         "stopCnt",        "errCnt",          "callVL", 
	"BorderGroupName", "BorderGroupDir", "BorderGroupPath", "callVarNames",
	"ShapeFile",       "ShapeFileDir",   "ShapeFilePath",   "ShapeLinkName",
	"NameTableFile",   "NameTableDir",   "NameTablePath",   "NameExt",       "NameTableLink",
	"debug",           "proj4",          "ReducePC",        
	"MapHdr",          "MapMinH",        "MapMaxH",         "IDHdr",        
	"LabelCex",        "convertPROJ4",   "AdjPolygons",     "clgeo_Clean2" 
      ),add=TRUE)
   #

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
#  Main Code
#

BuildBorderGroup <- function(ShapeFile       = NULL,		# Base filename of Shape file without extension for SPDF name
                             ShapeFileDir    = NULL,		# Directory containing Shape file 
                             ShapeLinkName   = NULL,		# Variable in @data data .frame containing link to NameTable
                             NameTableDir    = NULL,		# Directory containing the NameTable (.CS, .axils, .axils, or .RDA)
                             NameTableFile   = NULL,		# The filename with extension of the NameTable
                             NameTableLink   = NULL,            # The column in the NameTable to use to link the SPDF to the Name Table.
			     BorderGroupName = NULL, 		# Name of the Border Group (BordGrp)
			     BorderGroupDir  = NULL,		# Directory to write the Border Group data set into.
			     MapHdr          = NULL,		# One or two header lines for the Map Glyph column (Max 16 characters each)
			     MapMinH         = NULL,            # Minimum Height for micromap drawing (inches)
			     MapMaxH         = NULL,            # Maximum Height for micromap drawing (inches)
			     IDHdr           = NULL,		# One of two header lines for the ID Glyph column (Max 12 characters each)
			     LabelCex        = NULL,            # cex value for the Map Labels (Optional)
			     ReducePC        = 1.25, 		# The percentage of vertex to be kept by rmapshaper
			     proj4           = NULL,		# Callers requested micromap image final projection (string, not CRS)
			                # may be changed to not do AEA transform if projection is specified in shapefile.  
			     checkPointReStart = NULL,          # default = FALSE, True for restart.
			     debug           = 0		# Debug flag = 0 to 65535.
			     ) 
   {
   
   # 
   # Call Parameters Definitions:
   #
   #   ShapeFile	- a character string (1 element in vector) or a SPDF structure.  If ShapeFile
   #                      is a name of a shapefile file, it is used with ShapeFileDir to locate 
   #                      the file to be read.  The shapefile name should not have any extensions
   #                      included.  If ".shp", ".shx", ".dpf", and ".prj" extensions are present 
   #                      they will be removed.  Generally, No file extension should be included.  
   #                      The ShapeFileDir parameter is combined with the ShapeFile parameter to
   #                      test if the shape file exists, the ".shp" extension is used. 
   #                      Only the first element is used. It cannot be NULL or NA_character_.
   #
   #                      If the value is a SPDF (SpatialPolygonsDataFrame) of
   #                      the boundary data, then it is process as if it was just read in by the function,
   #                      
   #                      This parameter is required and has no default values.
   #
   #   ShapeFileDir     - a character string (1 element in vector) - is the directory name containing
   #                      the shape file .  It should not end in a slash, but if one if present 
   #                      it will be removed.  The directory is used as the adds= value in the readOGR
   #                      call to read the shape file.  Only the first element is used.
   #                      The default value for this parameter is the current working directory.
   #                      The existence of the directory is checked.
   # 
   #                      This parameter is "NA", then the current working directory is used.
   #                      If this parameter is omitted (NULL), then the value provided in the 
   #                      Name Table Directory will be used.
   #                      If the shape file is actually a SpatialPolygonsDataFrame of the shape file,
   #                      then this parameter is ignored.
   #
   #   ShapeLinkName    = a character string (1 element in vector) - is the name of the column in the 
   #                      shape files SPDF @data data .frame slot that contains an identifier for the 
   #                      area the polygon is associated with.  The number of unique values in the SPDF
   #                      must match the number of entries in the NameTable.  The values in the NameTable
   #                      "Link" column must match the unique values in the shape files SPDF @data slots 
   #                      column by the name provided in the ShapeLinkName= call parameter.  Only the first
   #                      element is used.  The @data field reserved for the Link values for the 
   #                      Shaoefile is "X__Link".
   #                      The default ShapeLinkName value is "NAME".
   #
   #   NameTableDir     - a character string (1 element in vector) - is the directory name containing
   #                      the callers provided NameTable .CS, .axils, .axils, or .RDA file.  This file
   #                      contains the NameTable to be used with the shape file.  If it is not completely 
   #                      filled out, the function will complete the table and write an intermediate .RDA
   #                      copy for later re-use or for review.  The existence of the directory is checked.
   #
   #   NameTableFile    - a character string (1 element in vector) - is the name of the initial or working
   #                      image of the NameTable provided by the caller to provide different types of 
   #                      labels for each area in the micro map boundary data. The values of NULL and NA_character_ 
   #                      can not be used.  The NameTableName is combined
   #                      with the NameTableDir to create the complete path to the file.  The path is 
   #                      tested to verify it exists before attempting to open the file.
   #
   #                      The data provide by the micromapST caller can use a "Name", "Abbr", "ID", 
   #                      "Alt_Abbr" or "Alias" identifier to match the data with the name table 
   #                      row of a specific area. (See the Name Table Requirements for more information.)
   #
   #   NameTableLink    - a character string (1 element in a vector) - This string specifies which 
   #                      column in the name table will be used to link the SPDF information with
   #                      the NameTable.  The default is "Link", but it could be the Name, Abbr, or ID fields.
   #
   #   BorderGroupName  - a character string (1 element in a vector) - This string provides the name of the 
   #                      border group.  It is used to label the border group inside its data set and as 
   #                      the name of the final border group data set file.  It is recommended the name be
   #                      kept short and end with "BG" to help provide quick identification of the border
   #                      group file.  Only the first value is used.  This value has no defaults, NULL and 
   #                      NA_character_ cannot be used.
   #
   #   BorderGroupDir   - a character string (1 element in a vector) - this 
   #                      string identifies the directory where the final 
   #                      version of the border group will be written.  
   #                      Any intermediate data sets will also be written 
   #                      to this directory.  Only the first value is used.
   #                      The default value is the current working directory 
   #                      if value is "NA". If value is missing, the Name 
   #                      Table Directory value is used.
   #
   #   MapHdr           - is a character vector with 1 or two elements - "c(a,b)" - the character vector provides the 
   #                      header labels for the Map glyph column containing the micro map drawings for the 
   #                      areas.   It is recommended the headers be no longer than 16 characters or the 
   #                      the usual width of the micro map. If the strings are too long, they will
   #                      be truncated.  The border group builder can specify 1 or two 
   #                      header labels for the map glyph.  For Example:  MapHdr=c("Header1","Header2")
   #                      If no values are provided for this parameter the BorderGroupName will be used.
   #
   #   MapMinH          - is a numeric value used to specify the minimum amount of space to allocate 
   #                      in the output graphic for the micromap drawing in the row group.  The default is 1".
   #
   #   MapMaxH          - is a numeric value used to specify the maximum amount of space to allocate
   #                      in the output graphic for the micromap drawing in the row group.  The default is 1.5".
   #                      Note: The actual value used for the height of the micromap will vary between 
   #                      the specified minimum and maximum depending on the number of areas being
   #                      drawn on the graphic.
   #
   #   IDHdr            - is a character vector with 1 or two elements - 
   #                      the character vector provides the header labels 
   #                      for the ID glyph column containing the area names.  The names can be
   #                      the full name ("Name") or the abbreviation 
   #                      ("Abbr") for the area.  It is recommended the 
   #                      headers be no longer than 12 characters or the 
   #                      the maximum width of the area names.  If the 
   #                      strings are too long, they will be truncated.  
   #                      The border group builder can specify 1 or 2 
   #                      header labels for the ID glyph.  
   #                      For Example:  IDHdr=c("U.S.","States")
   #                      If no values are provided for this parameter 
   #                      the BorderGroupName will be used.
   #
   #   LabelCex         - a numeric value to be used as the cex value for the 
   #                      Map Label text when Labels are used.  The default is .25.
   #
   #   ReducePC         - is a numeric value (1 element in a vectex) - the 
   #                      numeric is the goal reduction by rmapshaper to 
   #                      the shape file. The range of the value is from 
   #                      0.5 to 95 percent. A value of 1.5 indicates 
   #                      rmapshaper will keep 1.5% of the original vectex
   #                      in the map areas. The default value is 1.5%.
   #
   #   proj4           -  (optional) a character string (1 element in a 
   #                      vector) - this string is in proj4 syntax and 
   #                      specifies the callers desired projection to be 
   #                      used for the micro map Border Group image.  
   #                      The default projection is the Calibers 
   #                      equal-area projection based on the center of 
   #                      the area and using Lat1 and Lat2 values 1/4 
   #                      of the height below and 1/4 the height above 
   #                      the center the shape file with un its =meters.  If the caller desires a different 
   #                      micro map projection, they can specify the projection using the "proj4" 
   #                      call parameter.  The function will scan the provided string and insure
   #                      the +units= parameter is set to meters.
   #                      It is suggested, the caller use the border group 
   #                      without this call parameters and seeing if it meets 
   #                      their needs before specifying it. Recommended usage 
   #                      of "+finites:<number>".
   #                      Any proj4 projection string provided must be in the 
   #                      proj4 format. It will be converted into the current
   #                      proj6 format with comment when added to the 
   #                      SpatialPolygonsDataFrame and Shape file.
   #
   #   checkPointReStart - a logical value - this value is normally FALSE to 
   #                      indicate the function has a new border group to 
   #                      build.  In the process, checkpoint images of key 
   #                      data are written to a "checkpoint" directory in
   #                      the bordergroup target directory  for troubleshooting 
   #                      and to allow the user one last chance to do any 
   #                      manual modifications to the shape file of the 
   #                      boundaries.  It is required to cases where the 
   #                      areas need to be enlarge so any colored shading 
   #                      of the area will be visible to the border group 
   #                      users.  When the checkPoint call parameter is 
   #                      present and set to TRUE, the function read back 
   #                      in the checkpointed files and continues the process
   #                      to create the dataset for micromapST boundaries.
   #
   #   debug            - is a numeric value (1 element in a vector) - this is 
   #                      a debug value and is used to instruct the function 
   #                      to printout debug information based on this value when 
   #                      it is TRUE or > 0.  The valid range is 0 to 255.  
   #                      The default value is FALSE or 0.  The Values of 
   #                      1 = running stand-a-long not as a call, 2 = trace 
   #                      program flow, 4 = display projection processing and variable,
   #                      8 = Plot intermediate shape files and SPDF, 16
   #                      16 = SPDF processing and Variables, 32 = Name Table 
   #                      processing and variable, 64 = Display internal variuables 
   #                      related to SPDF and Shape File, 128 = 0:PDF, 1:PNG,
   #                      256 = write to a file a set of images of the map at each stage 
   #                      of processing, 512 = write to a file a single PNG or PDF 
   #                      image of the map at each stage of processing, 
   #                      1024 = write a PNG as requested in code, 
   #                      2048 = display the final VisBorders images in windows at 
   #                      the end of processing, 4096 = write final image of 
   #                      areaVisBorders File to PNG or PDF file, 8196 = write
   #                      a PDF file containing multiple samples of the map scales 
   #                      to the size the may appear in a linked micromap.
   #
   #
   #  Name Table Requirements:
   #
   #  The name table can be constructed using a test editor or spreadsheet program.  
   #  The output should be saved as a .csv or .xlsx formatted file.  The first row 
   #  must contain the names of each column. At least two columns must be present:  
   #  "Link" and one of the following "Name", "Abbr", or "ID". If at all possible 
   #  all three should be provided to make the border group more usable.
   #  The values in the "Link" column must match the values in "ShapeLinkName" 
   #  attribute in the SPDF.  The number of areas (rows in the name table) should 
   #  match the number of areas in the shape file or SPDF provided.  The values 
   #  for the "Name", "Abbr" and "ID" columns should be the most commonly
   #  accepted values for the full name of the area, the commmon abbreviation 
   #  for the area and a numerical ID for the area.
   #
   #  The name table can also contain special identifiers for each area:  
   #  "Alt-Abbr" and "Alias".  The "Alt-Abbr" column is use to provide 
   #  an alternate abbreviation for the areas when there are more then 
   #  one commonly accepted abbreviation.  For example an international 
   #  abbreviation and a local abbreviation.  The "Alias" identifier is 
   #  used with a wildcard match against the identifier provided in the 
   #  data tables.  The "Alias" character strings must match some part 
   #  of the identifer provide and must also be unique in the name table.  
   #  The "Alias" feature help use data produced by other programs and 
   #  websites that do not use standard identifiers.
   #
   #      The values in each column must be unique.
   #      The use definition of each of these rows are: 
   #         - Name (full name length name of an area), 
   #         - Abbr (an commonly accepted abbreviation for the area), or 
   #         - a numeric ID for the area.
   #      It is highly recommended that all three columns be included in the NameTable.
   #
   #      Additional columns may be specified:  
   #         - Alt_Abbr - an alternate abbreviation that can be when a 
   #              second abbreviation is needed (also commonly accepted);
   #         - Alias - a character string used to match data to the NameTable 
   #              using a regular expression match of "*<value>*" (this is useful 
   #              when the source of the data does not provide a clear area name 
   #              or abbreviation, but a unique string can be used to match the 
   #              name in the data); 
   #
   #      The initial NameTable must contain a "Link" column containing the values 
   #      used to link the NameTable row to an area in the SPDF.
   #
   #      Other information and values contained in the NameTable to support other features are:
   #         - L2_ID - a value to identify which Level 2 space the area belongs 
   #              (a matching L2_ID_Name should also be provided), and  
   #         - Reg_ID - a value to identify which region in the geological space the area 
   #              belongs (a matching Reg_Name should also be provided), this supports the 
   #              micromapST feature of only matching regions within a BordGrp in the micro map 
   #              drawing.  
   #
   #      More information on the NameTable s provided later.
   #
   #  Checkpoint restart:  At the checkpoint after the SPDF modifications and Name Table
   #   has been completed, copies of the Name Table (as .rda) and the SPDF as a 
   #   shape file are written out.  The Shape File can then be used to do custom
   #   modifications to the boundaries.
   #   To restart the processing, the Name Table .RDA and the Shape file are 
   #   retreived from the CheckPoint space in the Name Table directory.  The fact that 
   #   they are both read from the "CheckPoint" directory and the Name Table is an .rda file
   #   indicates this is a checkpoint restart of the process.  One the minimal information 
   #   is verified, the data is read in and processing is continued.  A copy of the 
   #   areaParm data.frame is also written to the checkpoint to save handling the areaParm
   #   data again.  The Name Table directory points to the area containing the checkpoint
   #   subdirectory.  The Checkpoint call parameter tell the function to add the 
   #   "checkpoint" directory to the path, read the three files, and then pickup at the end 
   #   part of the process.
   
   #########
   #######
   #####
   #
   #   create error and stop counters functions - must be in .GlobalEnv 
   #     so the panelXXXX functions can use them.
   #
   var   <- "errCnt"
   wstr  <- paste0("assign(var,NewCounter(),envir=.GlobalEnv)")
   eval(parse(text=wstr))
   
   var   <- "stopCnt"
   wstr  <- paste0("assign(var,NewCounter(),envir=.GlobalEnv)")
   eval(parse(text=wstr))
   #
   set_rgdal_show_exportToProj4_warnings(FALSE)
   rgdal::set_thin_PROJ6_warnings(TRUE)
   rgdal::set_P6_datum_hard_fail(FALSE)

   r2=rgdal::rgdal_extSoftVersion()
   r3=rgdal::new_proj_and_gdal()
   r4=options("rgdal_show_exportToProj4_warnings"="none")

   xColors      <- GetMColors()
   mcolors      <- xColors$mcolors
   colorsRgb    <- xColors$colorsRgb
   colorsRef    <- xColors$colorsRef
   colorsRefRgb <- xColors$colorsRefRgb
   
   #######
   #
   #  Common Functions
   #
   
   #####
   #
   #  function to convert PROJ4 string into CRS format, catch any errors and warnings, report them
   #  and return CRS to caller.
   #
   
   convertPROJ4 <- function (x) {
    
      #  function is designed to convert a proj4 string into CRS format
      #  and catch any errors or warnings.
      #
      #   x - user provided PROJ4 string
      #
      #   value = bres where [1] is the proj4 and [2] is the wkt values (character)
      #         = FALSE if errors or warnings.
      #
      #cat("convertPROJ4 routine \n")
      
      ErrFnd <- FALSE
      
      if (is.na(x))  return(NA_character_)
      if (x == "NA") return(NA_character_)
      if (stringr::str_trim(x) == "")   return(NA_character_)
      if (x == 0)    return(NA_character_)
      if (!is.character(x)) {
         # not a character vector - error
         ErrFnd      <- errCntMsg(paste0("***3718 The proj4 value character vector.  Must be a valid proj4 argument character string to be converted."))
         return(FALSE)     
      } else {
         # character vector - OK try the convert
         res <- tryCatch( 
         
         {  y <- sp::CRS(x) }, 
                    warning = function (war) { 
                              #print(paste0("My Warning: ",war))
                              return(paste0("WARNING:",war))
                             }, 
                    error   = function (err) {
                               #print(paste0("My Error:  ",err))
                               return(paste0("ERROR:",err))
                             }, 
                    finally = { }
                ) 
         
         if (is(res,"CRS")) {
            # its a CRS class - convert to string to print status of call.
            # The conversion was successful.
            bres         <- NULL
            bres[1]      <- rgdal::CRSargs(res)   # convert results into proj4.
            bres[2]      <- sp::wkt(res)
            return(bres) # return CRS class version.
         } else {
            # its not a CRS..  It's and error is FALSE...Most likely an error. Should be character.
            return(FALSE)         
   
          
         }   # end of valid conversion to CRS. 
      }   # end of check for characters
   }  # end of convertPROJ4 function
   #
   #####

   #####
   #
   #  AEAProjection - build AEA proj4 string for the ShapeFile image passed to it.
   #    The AEA projection will be based on the centroid of the map in the ShapeFile,
   #    with lon_0 and lat_0 set to the centroid, and lat_1 and lat_2 set to 1/4 the 
   #    distance below and above the lat_0.  The +units is set to meters.
   #
   #   Input: SPDF structure
   #
   #   Output: proj4string for the created AEA projection
   #
  AEAProjection <- function(wShp) { 
        #
        #  Calculate AEA projection
        #
        #  The basic AEA for the continental US is:
        #
        #  Set the center based on the shape file's centroid.  
        #  If shape file is long/lat, then centroid should be in long/lat.
        #  If shape file is long/lat, then proj4string should be present or NULL and 
        #  I can use the lab point for the entire shape file to get the center.
        #
        #   United States - continental center
        #   Projection = Alber equal area  => simpleconic
        #   Lat Parallel 1   = 33
        #   Lat Parallel 2   = 45
        #   Origin of Lat    = 39              # for US, center line is 39, move up and down 6 degrees. 
        #   central Meridian = -96   (96W)
        #
        #
        #cat("AEAProjection: Shape file now must be projected onto the right AEA \n",
        #    "based on the centroid of the space. This is done when the user has \n",
        #    "not specified a projection or the shape file has no projects.\n")
        #
        #  Build parameters for AEA projections based on centroid of map.
        #  Get the center and estimate a reasonable set of parallels 
        #  for the Albers Equal Area projects.
        #  Based on research in to how the lat_1 and lat_2 are chosen, it 
        #  appears they range based on the size of the area covered.
        #  Generally the factor is beteen 4 and 6.  So an approximation 
        #  using K=4 is appropriate.  That divides the height of the 
        #  map into 1/4th. The lat_0 is the center line 1/2 from the bottom
        #  and 1/2 from the top.  The lat_1 and lat_2 are positioned 1/4 from 
        #  the bottom and 1/4th from the top.  The area is equally divided in 1/4th.
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
        xCtr    <- c(0,0)    # initialize variable.
        # use the labpt as the centroid of each area.
        xLabpt  <- sapply(slot(wShp,"polygons"), function(x) x@labpt)    # collect the labpts for each area.
        xCtr[1] <- mean(xLabpt[1,])
        xCtr[2] <- mean(xLabpt[2,])
        #cat("AEAProjection: Center of area based on LABPT long/lat:",xCtr[1]," ",xCtr[2],"\n")
        
        WorkCtr  <- rgeos::gCentroid(wShp)@coords
        #cat("AEAProjection: gCentroid of SPDF:",WorkCtr,"\n")
        
        #
        # Build new AEA projections.
        #
        # Find center Long of area
        wLong0 = round(xCtr[1],3)    # centroid's X coordinate in LL, rounded to 3
        if (wLong0 < 0) {
           wLong0 <- as.character(paste0(abs(wLong0),"w"))
        }
        wLat0  = round(xCtr[2],3)   # get center latitude - round to three decimal places.
       
        #
        BBoxShp   <-  sp::bbox(wShp)            # in LL
        DLat4th   <-  diff(range(BBoxShp[2,]))/4   # 1/4 the lat of the space.
        #wLat0   is the middle (center)
        wLat1     <-  round(wLat0 - DLat4th,3)  # is the lower lat half way between middle and bottom edge      
        wLat2     <-  round(wLat0 + DLat4th,3)  # is the upper lat half way between middle and top edge
       
        #  calculated AEA projection for transform.
        AEAProj4  <- paste0("+proj=aea +units=m +datum=NAD83 +lat_1=",wLat1," +lat_2=",wLat2," +lat_0=",wLat0," +lon_0=",wLong0," +no_defs")
        #cat("AEAProjection: Calculated Proj4string for transform:",AEAProj4,"\n")
        return(AEAProj4)     
    }     
          
  
  
   ######
   #
   #  BuildVisBorder - converts a SPDF of all of the areas into a VisBorder
   #       format of all of the areas.
   #
   #
   #   Calls:   MakeVisPolygon, 
   
   BuildVisBorder <- function(wSPDF, PlOrd) {
      #
      #    wSPDF - spatialPolygonsDataFrame or spatialPolygons of polygons in a map.
      #    PlOrd   - list of the names or IDs of the areas in the wSPDF for the map.
      #
      # This function takes a SPDF and converts it into a VisBorder structure and returns
      # the VisBorder image.
      
      VisBorders   <- NULL    # Vis Border for micromapST
      VisBorders   <- data.frame(x=numeric(0),y=numeric(0), hole=logical(0),Key=character(0))
      VisRange     <- sp::bbox(wSPDF)
      #cat("xKey:",paste0(PlOrd$Key,collapse=", ",sep=""),"\n")
      
      for (xKey in PlOrd$Key)  {       #  Pull Out each areas polygons  In their plot order
        
         #  Convert to Vis in SPDF order.
         #cat("BuildVisBorder for Area Key:",xKey,"\n")
         
         #  This code pulls off the polygons@Polygons level associated with an area.
         
         areaSPDF    <- wSPDF[xKey,]          # pull off each areas SPDF structure by name.   
         areaRN      <- row.names(areaSPDF)      # should get only one.  Should be the same as xKey
         #cat("Areas Row.name:",areaRN,"\n")
         
         # debug plot of area for graphic check.
         #sp::plot(areaSPDF)   # check the area.
         
         #
         # The areaSPDF variable is the SPDF informaton for a single area.
         #
         # This routine scans the SPDF for each "polygons" it contains and then 
         # for each "Polygons" with a single "polygons", then for each "Polygon"
         # within a "Polygons". The MakeVisPolygon function converts each 
         # "Polygon" into a matrix of the "Polygon" coordinates and return that matrix.
         # Since sapply is used the result is a list of matrice.  The MakeVisPolygon
         # funcition also makes sure the first and last data point in the matrix for
         # a polygon is the same and that the matrix ends with a NA,NA coordintate
         # for the polygon drawing function. 
         #
         
         #  Step through areaSPDF (spatialPolygonsDataFrame structure for area) and pull out
         #    each polygon in the list.  Then process with MakeVisPolygon.
         #  This section services are the 
      
         VisPolygons <- sapply(slot(areaSPDF,"polygons"), 
                                function(x) {  # get the list of Polygons
                                  sapply(slot(x,"Polygons"), function(y) MakeVisPolygon(y), simplify=FALSE) # pull off a single Polygon
                                }
                           )
                        
         # ViaPolygons is a list of Polygon processed by MakeVisPolygon.
         #cat("Return VisPolygons.\n")
         #str(VisPolygons)
         
         VisArea        <- NULL
         VisArea        <- do.call(rbind,VisPolygons)   # rbind to empty VisArea.
        
         VisArea        <- as.data.frame(VisArea)       # convert data structure to data frame (matrix to DF)
         names(VisArea) <- c("x","y","hole")            # change the column headers
         VisArea$Key    <- areaRN                       # Add the Key column
         
         #
         #   VisArea is a matrix of the x,y,hole,key 
         #     the VisBorder format for the area. Later add L2_ID and regID columns.
         #
         #  VisPolygons list of matrice contains multiple areas (increments of 4 wide)
         #   1 = x
         #   2 = y
         #   3 = hole
         #   4 = Key
         #
         #  The VisPolygons are combined into a single matrix for the area and additional
         #  fields added to link the polygons to their area ->  "Key" column.
         #
         
         #  Areas matrix is combined with the rest of the areas boundary point matrices.
         VisBorders      <- rbind(VisBorders,VisArea)
         
      } # end of loop through all areas in plot order.
         
      #
      #  Remove duplicate x,y points in VisBorders data.frame
      #
      VisBorders  <- RemoveDups(VisBorders)
      
      return(VisBorders)
   }
   #
   ######
   
  
   ######
   #
   #   MakeVisPolygon - 
   #
   #  Input: Polygon structure of one polygon - coordinates
   #       Does not handle EWH.
   #
   #  Output: a data frame of modified points (x, y, hole, ??)
   
   MakeVisPolygon <- function (wPolygon)
      { 
         # wPolygon - Polygon class structure - @coords, @hole
         #cat("Entering MakeVisPolygon.\n")
         #str(wPolygon)
         
         xcoords     <- wPolygon@coords
         xhole       <- wPolygon@hole
      
         #print(xcoords)
         #print(xhole)
         
         #
         # Simplify coordinates by rounding and then removing duplicates
         # Rounding is a problem.  If the data is Lat/Long, you cant 
         #  really round more then 4 decimal places without losing a lot
         #  or coordinate information.  (Long/Lat, 1 degree = 69.16944 miles, 
         #  or 365214.7 feet or 111319.5 meters. -180 to 180 or 0 to 360 degrees.
         #  One Lat degree is 340,000 ft, 69 miles, 111,319 meters.
         #  0.0001 Lat degree is 340 ft, 0.069 miles, 111.319 meters.
         #  Rounding should 360 degrees -> 0.0001 and in meters -> units of 100 (-2)
         #
         #  Rounding based on number range:  if within 0 to 360 - assume degrees and 
         #    the rounding should .0001 degrees = 111 meters = 64 feet.
         #
         #  IMPORTANT:Assume all coordinates are in meters...  (after transforms)
         #
         RScale    <- -2                             # for meters (100 m or 330 ft.)
         xR        <- range(xcoords[,1],xcoords[,2])
         if (abs(xR[2]) <= 360) RScale = 3   # if x,y are long/lat (-360 to 360), the round to .0001 (3)
         
         # must be enhanced to allow for the rounding for large or small areas. ???
         
         NewCoords <- xcoords
         
         # simple rounding  and complete loop   (round to 100s of meters.)
         
         NewCoords <- round(NewCoords, RScale)      # round to 100 meters or 64 feet (.0001 degrees).
         
         NewCoords <- rbind(NewCoords, NewCoords[1,])  # end to start  - at start to the end for POLYGON function later
               
         NewCoords <- rbind(NewCoords, c(NA,NA))     # add NA, NA to end of the list to indicate end of polygon.
       
         #cat("NewCoords Results:\n")
         #print(str(NewCoords))
         
         # set up data.frame to return
         ODF       <- as.matrix(NewCoords)
         ODF       <- cbind(ODF,rep(xhole,dim(ODF)[1]))   # put hole indicator in all rows.
         
         #cat("MakeVisPolygon-Completed - resulting list of coordinates.\n")
         #cat("ODF return - class:",class(ODF),"  dim:",dim(ODF),"\n")
         #print(ODF)
         
         return(ODF)
      }
   #
   ######
   
   
   
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
   
   RemoveDups <- function(VisB) {
      #  VisB is a VisBorder matrix if x, y values for any set of spaces or areas.
      #  Remove duplicate x,y points in a matrix.
      
      #  Get the number of points in the matrix
      lenVis     <- dim(VisB)[1]   # Get number of rows in the matrix/data.frame
      #cat("RemoveDups -lenVis:",lenVis,"\n")
      
      #  Initial the first "previous" x,y points.
      oldX     <- VisB$x[1]
      oldY     <- VisB$y[1]
      #cat("Initial X,Y:",oldX, " ",oldY,"\n")
      #  Create a list of T/F matching each entry in the data.frame.  
      #  Any duplicates will be marked with a "FALSE" to indicate the row should be deleted.
      KeepList <- rep(TRUE,lenVis) 
      
      #  Scan the rest of the points to the end of the data.frame
      for (ind in c(2:lenVis))  {       # loop through all of the points from 2 to the end.
         
         if ( !is.na(VisB$x[ind]) && !is.na(VisB$y[ind]) ) {
            if ((oldX == VisB$x[ind]) && (oldY == VisB$y[ind])) {
               #  both equal  - we have a match of non-NA oxy points  Set keepInd = FALSE to delete it.
               KeepList[ind] <- FALSE        # indicate this point should be deleted.
               # move on to check next x,y point set.   
            }
            oldX <- VisB$x[ind]
            oldY <- VisB$y[ind]
         } else {
            # Found NA at ind.
            oldX <- VisB$x[ind+1]  # save the next one after the NA.
            oldY <- VisB$y[ind+1]
            ind  <- ind + 1        # set ind to the next after the NA.
         }
         # Move Forward.
      }
      # Done with scan.  KeepList tell me what entries to keep.
      
      VisB2      <- VisB[KeepList,]   # keep the good entries 
      #  Report on the accomplishment
      lenVis2    <- dim(VisB2)[1]     
      #cat("RemoveDups Completed-Original Length: ",lenVis," Final Length: ",lenVis2,"\n")
      return(VisB2)
   }
   #
   ######
   
   
   
   ######
   #
   #  AdjPolygons - Primary function in sapply to setting up the shift and scaling of a Polygons structure.
   #                The function sapplys the next level and passes Polygon structures
   #                to the AdjPolygon function to do the actual shift and scaling of the polygon
   #                within the area.  (example: Alaska)
   #
   #  wPolygons ->  one Polygons structure from the "polygons" part of an SPDF.
   #                Example: slot(WorkSp["ID",],"polygons")[]
   #                
   #  wParm     ->  vector of the shift/scale parameters: 
   #                   Xoffset - amount to move the X coordinates
   #                   Yoffset - amount to move the Y coordinates
   #                   Scale   - multiplier to scale the X,Y coordinates around the centroid of the area.
   #                   Rotate  - degrees of rotation about the centroid.
   #                   SignX   - in east or west hemisphere
   #                   LL      - coordinates are Long/Lat
   #               The xy offset values should be in meters. All projections
   #               produce spatial data in meters to make these 
   #               modifications easier to manage. If a user requests or 
   #               provides a shape file where the units are not Meters, 
   #               it is re-projected into units=m before adjustments 
   #               are made.
   #               
   #               For X coordinates, neg(-) value is a shift to the left, 
   #                   pos(+) is to the right.
   #               For Y coordinates, neg(-) value is a shift down and 
   #                   pos(+) is up.
   #
   #  		   Scale values are in the range from 0.01 to 3 as 
   #               multiplier. A value of 1 is no change or 100% of the 
   #               original size.
   #
   #               This routine should not be called unless the polygons 
   #               need to be shifted or scaled.
   #
   #         It is best if the shift/scale operation is done before any 
   #         transformation.  This lets the operation use the units of 
   #         measure of the originating ShapeFile. 
   #         
   # 
   # FUNCTION TO Setup to do shifts and scaling of polygons
   
   AdjPolygons <- function(wPolygons, xCtr, xParm)    
                   # around the areas (Polygons) centroid
                   # xParms contains Xoffset, Yoffset, Scale, SignX.
                   # xCtr is the centroid of the area.
      { 
         StopFlag <- FALSE
         #str(wPolygons)
         
         #  No checking is done on the xParms values.
         #  wPolygons must be the correct class.
    
         if (!is(wPolygons,"Polygons")) {
            # polygon not the right format - "Polygons" structure.
            xmsg <- paste0("***4010 AdjPolygons - wPolygons value is not",
                           " a 'Polygons' structure.")
            StopFlag = stopCntMsg(xmsg)
         }
         
         #
         #  wPolygons - is a list of Polygon structure for the area.
         #      local slots used:
         #        list of Polygon belonging to Polygons 
         #
         #  Info supplied by call:
         #      centroid of area
         #      SignX for date line adjustments:  -1=West, 0=none, 1=East
         #      Shift and Scale parameters
         #
         #  Function collects:
         #      Polygons ID
         #
         xPolys1     <- wPolygons
         # save to pass back when building the adjusted Polygons structure
         xID         <- wPolygons@ID  
         #
         
         #cat("Got Polygons for ",xID," xCtr:",xCtr$x," ",xCtr$y, "\n")
         
         #  Pull each "Polygon" off list and process.
         cmpPoly  <- sp::Polygons( 
                         sapply(slot(wPolygons,"Polygons"), 
                              function(y) AdjPolygon(y, xCtr, xParm)), xID)
         
         #  At end of processing, cmpPoly will have a Polygons 
         #  list with the correct area ID.
         return(cmpPoly)
      }
   #
   #####
      
   #####
   #
   #  AdjPolygon - handle the adjustment process for each Polygon in the
   #   Polygons list of a "polygons" entry.  This function is called from 
   #   AdyPolygons was each "Polygons" list is pulled from the 
   #   "polygons" list.
   #
   #   xParm contains the following variables: $Xoffset, $Yoffset, 
   #   $Scale, $SignX (+1 or -1), $Rotate, $LL.  The adjustments 
   #   are based on the xParms and the xCtr coordinates 
   #
   #   East/West Hemisphere processing for long/lat projections 
   #   is no longer done in this function.
   #
   
   AdjPolygon <- function(xPolygon,xCtr,xParm) 
      {
         # This function makes the coordinates adjustments to one 
         # Polygon and returns a Polygon when done.
         #cat("Entering AdjPolygon\n")
         
         # The calling parameters are:
         #     xCtr - the centroid of the area (Polygons)
         #     xParm - the modification parameters
         #        xParm$XOffset    for shifting on X axis
         #        xParm$YOffset    for shifting on Y axis
         #        xParm$Scale      for Scaling about the centroid
         #        xParm$Rotate     for Rotation about the centroid
         #        xParm$SignX      indicator for East or West Hemisphere
         #             - = for Western Hemishere  (X <= 360 or => -360
         #             + = for Eattern Hemishere]
         #             0 = No Date Line processing.
         #        xParm$LL         indicates coordinates are Long/Lat 
         #                         (Date line applys)
         #
         
         # received a single Polygon object - pull the coords and the 
         # hole information.
         xcoords     <- xPolygon@coords  # get coordinates for the polygon
         xhole       <- xPolygon@hole    # get key attribute - is it 
                                         # a hole. (is it a hole).
         
         ######
         #
         #   Step 1 - Adjust Coordinates for Date Line to allow shifts, 
         #            scaling and rotation.
         #
         #  Centroid sets up the hemishere the area is in (Long/Lat)
         #
         
         xcoords2 <- xcoords
         
         #  if xSign is zero (0), NA, or NULL, it did not cross date 
         #  line - nothing to do.
         
         #
         # Step 2 -  If Scale or Rotate to be done, 
         #cat("Step 2 - SCALE and ROTATE : Normalize Polygons and scale around xCtr:",xCtr$x, " ",xCtr$y,"\n")
         #
         #  Normalize to center (xCtr)
         #  Logically, we think about shifting the area first, then 
         #  scaling it at that new location.  We are using the 
         #  centroid of the area, so its better to scale first and 
         #  then shift the polygons points based on its centroid 
         #  (which hasnt changed.)  The polygon is normalized 
         #  around the areas center to (0,0) (-delta X and -delta Y), 
         #  then scaled, and returned to the normal x, y values 
         #  (+delta X and +delta Y).  The relationship between 
         #  polygons in the area should also be scaled in the same 
         #  process.
       
         Nxcoords    <- xcoords2 # save copy of original coordinates 
         modCoord    <- FALSE
         
         #  Normalize all coords to the centroid values so centroid 
         #  is 0,0 for scaling, shifting, and rotation
         
         saved_xCtr  <- xCtr
         xAdjCtr     <- -xCtr
           
         # normalize polygon points around 0,0 area center
	 #    coordinates save to reset the centroid later
	 
	 Nxcoords1     <- xcoords2
	 Nxcoords1[,1] <- xcoords2[,1] + xAdjCtr$x
	 Nxcoords1[,2] <- xcoords2[,2] + xAdjCtr$y
	            
         if (!is.na(xParm$Scale) || xParm$Scale != 1) {
            # if Scale present (not NA or value of 1) we have request 
            # to scale. All points in the polygon are relative to 
            # the 0,0 centroid.
            
            # apply the scaling percentage (.5 = smaller, 1.5 = larger)
            xScale        <- as.numeric(xParm$Scale)
            Nxcoords2     <- Nxcoords1
            Nxcoords3     <- Nxcoords1
            Nxcoords3[,1] <- Nxcoords1[,1] * xScale
            Nxcoords3[,2] <- Nxcoords1[,2] * xScale
            Nxcoords1     <- Nxcoords3   # put results in Nxcoords1.
            modCoord <- TRUE
         }
         if (!is.na(xParm$Rotate) && xParm$Rotate != 0) {
            Nxcoords4     <- Nxcoords1
            Nxcoords5     <- Nxcoords1
            # rotation about the centroid at the rotate degrees.
            # (x',y') <- (x cos<a> + y sin<a> , -x sin<a> + y cos<a>)
            Rad <- xParm$rotate * pi / 180
            xCos <- cos(Rad)
            xSin <- sin(Rad)
            Nxcoords5[,1] <-  Nxcoords1[,1]*xCos + Nxcoords1[,2]*xSin
            Nxcoords5[,2] <- -Nxcoords1[,1]*xSin + Nxcoords1[,2]*xCos
            Nxcoords1     <-  Nxcoords5   # put results in Nxcoords1
            modCoord <- TRUE
         }
         if (!is.na(xParm$Xoffset) && xParm$Xoffset != 0) {
            # ahift X coordinate
            Nxcoords6      <- Nxcoords1
            Nxcoords7      <- Nxcoords1
            Nxcoords7[,1]  <- Nxcoords1[,1] + xParm$Xoffset
            Nxcoords1      <- Nxcoords7    # put results in Nxcoods1
            modCoord <- TRUE
         }
         if (!is.na(xParm$Yoffset) && xParm$Xoffset != 0) {
            # shift Y coordinate
            Nxcoords8      <- Nxcoords1
            Nxcoords9      <- Nxcoords1
            Nxcoords9[,2]  <- Nxcoords1[,2] + xParm$Yoffset
            Nxcoords1      <- Nxcoords9    # put results in Nxcoords1
            modCoord <- TRUE
         }
         # results are in Nxcoords1
         
         # Back to center coordinates.
         Nxcoords     <- Nxcoords1
         Nxcoords[,1] <- Nxcoords1[,1] - xAdjCtr$x
         Nxcoords[,2] <- Nxcoords1[,2] - xAdjCtr$y
            
         #  Return coordinates as a Polygon structure
         
         OPoly <- sp::Polygon(Nxcoords,hole=xhole)
         return(OPoly)
      }
   #
   #####
   
   #####
   #
   #  Adjust for areas spanning the East/West hemispheres 
   #  - Now done in mainline code.  If it tests out, kill this section.
   #
   #    wSPDF - spatialPolygonsDataFrame of polygons in a map.
   #   0) Get centroid - sets the hemisphere of the map.
   #        sign = - for west,  sign = + east.  Mimics the 
   #        long (X) value.
   #   0.1) Any X with the opposite sign and > abs(90) degrees 
   #        needs adjustment.
   # 
   #   1) check bbox to see if this space has passed over the EWH.
   #   2) get an area
   #      2.1) check bbox to see if this area has passed over the EWH.
   #      2.2) peel down in to the area.
   #      2.3) check each polygon
   #      2.4) has polygon gone over the EWH.
   #   3) adjust Polygon passed over EWH.
   #
   #   if a areas passes over the EWH, then the distance between 
   #   min and max X bbox data is > abs(180 degrees)
   #
   # This function takes a SPDF and and adjusts the Long/Lat values 
   # to compensate for the East/West Hemisphere issue Long/Lat 
   # coordinates issue (wrap around).
   #
             
   FixIDLMain <- function(spdf,debug) {
      ##### 
      #
      #  This is the code that does the EWH processing.
      #  Strip out the other stuff (modifactions, etc.) to leave the 
      #  LL East/West Hemisphere adjustment code.
      #
      MapCtr        <- as.data.frame(rgeos::gCentroid(spdf))
      AreaList      <- row.names(spdf)
      spdfData      <- spdf@data
      areaSign      <- sign(MapCtr[[1]])
      
      if (bitwAnd(debug,64) != 0) {
         cat("FixIDLMain:\n")
         cat("Long/Lat projection being used and that appears to cross between the East and West Hemisphere at the 180 degree mark.\n")
         print(MapCtr)
         print(AreaList)
         print(areaSign)
      }
      
      #   Pick up next Key in the plot order, step through them.
      for (xKey in AreaList) {  
         #  Pull Out each polygon in SPDF in natural order.
         #  Must handle SPDFs before the polygons are unioned under 
         #  their areas identifier. So, use the native row.names
         #  of the SPDF and handle one polygon at a time.
         #  The @data space must be preserved.
         
         # Get polygons sub-SPDF from full map.
         areaSPDF <- spdf[xKey,]  
         # pull off each polygons of the SPDF structure by id.
         AName    <- paste0(areaSPDF@data$NAME,"-",xKey)
         areaBBox <- sp::bbox(areaSPDF)      # get bbox of area.
         xR       <- areaBBox[1,]            # get X range
         x1 <- (areaSign == sign(xR[1]))
         x2 <- (areaSign == sign(xR[2]))
         x3 <- abs(diff(xR))
         
         if (bitwAnd(debug,64) != 0) {
            cat("Area/Poly ID:",xKey,"\n")
            cat("xR:",xR[1], " ",xR[2],"\n")  
            cat("x1:",x1," x2:",x2," x3:",x3,"\n")
         }
         
         if (!x1 || !x2 || !x3) {

            if (bitwAnd(debug,64) != 0) 
                 cat("Area:",AName," may need to be adjusted.\n")
            # if x high and low are not the same sign 
            # or differnce of high and low x are > 180 degrees.
            # we may have cross the East/West Hemispheres.
            
            # Should get only one.  Should be the same as the other.
            areaRN      <- row.names(areaSPDF)
            # save the @data section.
            areaData    <- areaSPDF@data           
            # get proj4 string. (Gets a CRS structure.)
            areaProj    <- slot(areaSPDF,"proj4string")   
            
            # get a Polygons structure
            cmpLLPolygons <- sapply( 
                    slot(areaSPDF,"polygons"),  
                         function(s0) FixIDLPolygons(s0, areaSign) )
      
            #str(cmpLLPolygons)
            # Build new area SPDF from the sapply results
            areaSP2     <- sp::SpatialPolygons(cmpLLPolygons,
                                 proj4string=areaProj)
            areaSPDF2   <- sp::SpatialPolygonsDataFrame(areaSP2,areaData)
                                   
            # cmpLLPolygons is a list of "Polygon"s from AdjPolyons.
            
            if (bitwAnd(debug,64) != 0) 
               cat("area deleted and added:",xKey,"\n")   
            #   Delete the original area Polygons LIST from the SPDF
           
            xm          <- (xKey == row.names(spdf))  
            spdf        <- spdf[!xm,]    # list of all of them to keep.
                   
            #  Add the Adjusted area back to the SPDF
            suppressWarnings(spdf <- maptools::spRbind(spdf,areaSPDF2))   
            # It will be out of position.
         
         }  # end of check for IDL for LL for Polygons.
         #  The ones that were changed were replaced in SPDF
         #
         # Results in spdf  - end of main loop through the polygons.
         #
      }  # end of for	Results in function return
      #end of main loop through the polygons.
      return(spdf)
   }  # function call. 
   #
   #
   ##### 
     
   #####
   #
   #  FixIDLPolygons - Primary function in sapply to find and fix LL East/West
   #                 Hemisphere cross over polyons.
   #
   #  wPolygons ->  one Polygons structure from the "polygons" part of an SPDF.
   #                Example: slot(WorkSp["ID",],"polygons")[]
   #                
   #  wSign     ->  hemisphere locator - "-" West, "+" East
   #
   
   FixIDLPolygons <- function(wPolygons, wSign)   # FUNCTION TO Setup to do shifts and scaling of polygons 
                                               # around the areas (Polygons) centroid
       { 
         #
         #  wPolygons - is a list of Polygon structure for the area.
         #      wSign for date line adjustments:  -1 West, 0 = none, 1 = East
         #
         
         xPolys      <- wPolygons
         xPolysID    <- wPolygons@ID                 # save to pass back when building the adjusted Polygons structure
         
         #cat("xPolys@ID:",xPolysID,"\n")
         #
         #  Pull each "Polygon" off list and process.
         
         cmpLLpoly   <- sp::Polygons( sapply(slot(wPolygons,"Polygons"), 
                                      function(y) FixIDLPolygon(y, wSign)), xPolysID)
                                      
         # return as a Polygons for the list of Polygon..
         
         return(cmpLLpoly)
      }
   #
   ######
     
     
   ######
   #
   #  FixIDLPolygon - handle the adjustment process for each Polygon in the Polygons list
   #
   
   FixIDLPolygon <- function(xPolygon,xSign) 
      {
         # This function makes the coordinates adjustments to one Polygon and returns a Polygon when done.
         #cat("Entering FixIDLPolygon\n")
         
         xcoords     <- xPolygon@coords     # get coordinates for the polygon
         xhole       <- xPolygon@hole       # get key attribute - is it a hole. (is it a hole).
         modCoord    <- FALSE 
        
         #  SignX set for Date Line adjustments - NOW
         if (xSign>0) {
            # if Sign positive then must use + 360 to handle the Lat/Long
            #cat("xSign=plus.  Adjust using +360\n")
            xd             <- (xcoords[,1] < 0)         # find x coordinates that crossed.
            if (any(xd)) {
               xcoords[xd,1]  <- xcoords[xd,1] + 360
               modCoord       <- TRUE
            }
         }
         if (xSign<0) {
            # if Sign negative then must use - 360 to handle the Lat/Long
            #cat("SignX=Neg.  Adjust using -360\n")
            xd             <- (xcoords[,1] > 0)         # find x coordinates that crossed.
            if (any(xd)) {
               xcoords[xd,1]  <- xcoords[xd,1] - 360
               modCoord       <- TRUE
            }
         }
         #  if SignX is zero (0), NA, or NULL, it did not cross date line - nothing to do.
         #  Return coordinates as a Polygon structure
         
         OPoly <- sp::Polygon(xcoords,hole=xhole)
         return(OPoly)
      }
   #
   #####
   
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
       stopCnt()
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
      warning(msg, call.=FALSE)
      return(TRUE)
   }
   #
   ######
   
   ######
   #
   # Fixed Version of clgeo_Clean  -> clgeo_Clean2
   #    Fixed proj4string references.
   #    Remove logging.info references - could not find function
   #
   # Credit - original cleangeo package.  Modified code to handle new PROJ6
   
   clgeo_Clean2 <- function(sp, errors.only = NULL,
                            strategy = "POLYGONATION",     #  or "BUFFER"
                            verbose = FALSE)  {
      #
      #   input parameters:
      #    sp -> Spatialxxxx structure
      #    errors.only = MULL ( ??? )
      #    strategy  = "POLYGONATION", "BUFFER", ...
      #    verbose = FALSE (or TRUE), output details
      #
      #   uses:
      #     cleangeo::clgeo_CollectionReport(sp)
      #     cleangeo::clgeo_SuspiciousFeatures(report, errors.only)
      #        report is structure produced by clgeo_CollectionReport
      #     cleangeo::clgeo_IsValid() ->
      #     cleangeo::clgeo_CleanByPolygonation.SpatialPolygons
      #
      #     sp::SpatialPolygons()
      #     sp::SpatialPolygonsDataFrame
      #     rgeos::gBuffer
      #     pbapply::pblapply  or lapply
      #
      #   Value = cleaned Spatialxxx structure.
      #
      
      if(!(strategy %in% c("POLYGONATION", "BUFFER")))
         stop("cleanGeo: Unknown advanced cleaning method. Accepted values: 'POLYGONATION', 'BUFFER'")
      
      report <- cleangeo::clgeo_CollectionReport(sp)
      nv     <- cleangeo::clgeo_SuspiciousFeatures(report, errors.only)
      
      # set apply mode. 
      applyHandler <- if (requireNamespace("pbapply", quietly = TRUE))  
         pbapply::pblapply else lapply   
        
      fixed.sp.list <- applyHandler(1:length(sp), function(x){   # lapply on each item in sp.
         polygon    <- slot(sp, "polygons")[[x]]                       # get polygons slot(x)
         ID         <- slot(polygon, "ID")                                  # get ID of polygon group
         if (!all(is.na(nv))){                                      # not all of the layers are NA in report
            if (x %in% nv){                                           # check each layer of polygon
               polygons      <- slot(polygon, "Polygons")                  # get Polygons at that layer -> List of Polygon
               poly.nb       <- length(polygons)                           # get number of Polygon on list (Polygons)
               removedHoles  <- vector()                              # empty vector for next step
            
               if (poly.nb > 0){                                      # if number of Polygons > 0
                  newpolygons <- list()                                  # empty list for next step
                  for (i in 1:poly.nb) {                                    # Step through all the Polygons
                     #if we found an orphaned hole, we remove it
                     if (slot(polygons[[i]], "hole")) {                         # if Polygon is holes
                        if (dim(unique(slot(polygons[[i]], "coords")))[1] < 3) {   # if number of coords in Polygon are < 3, not a Polygon
                           if (length(removedHoles) == 0 & verbose) {
                              #logger.info(sprintf("Cleaning orphaned holes at index %s", x))
                              cat(sprintf("Cleaning orphaned holes at index %s", x))
                           }
                           removedHoles <- c(removedHoles, i)  # accum list of holes in Polygons
                       } else {
                           newpolygon                <- polygons[[i]]
                           slot(newpolygon, "hole")  <- TRUE
                           newpolygons               <- c(newpolygons, newpolygon)
                       }
                    } else {
                       # Not a hole
                       newpolygon               <- polygons[[i]]
                       slot(newpolygon, "hole") <- FALSE
                       newpolygons              <- c(newpolygons, newpolygon)
                    }
                
                 }  # end of for (finished steping through list of Polygon  (Polygons)
                 slot(polygon, "Polygons") <- newpolygons
              }
              polygon <- sp::SpatialPolygons(Srl = list(polygon))
            
              #testing validity after removing holes
            
              isValid <- report[x,]$valid
              if (length(removedHoles) > 0){
                 if (verbose){
                    #logger.info(sprintf("Checking geometry validity at index %s", x))
                    cat(sprintf("Checking geometry validity at index %s",x))
                 }
              
                 tryCatch({
                    slot(polygon, "polygons") <<- lapply(slot(polygon, "polygons"), maptools::checkPolygonsHoles)
                 }, warning = function(msg){
                       if(verbose) {
                         #logger.info(sprintf("Catched MAPTOOLS warning '%s'",msg))
                         cat(sprintf("Catched MAPTOOLS warning '&s'",msg))
                       }
                    }, error = function(err){
                          if(verbose) {
                             #logger.info(sprintf("Catched MAPTOOLS error '%s'",err))
                             cat(sprintf("Catched MAPTOOLS error '%s'", err))
                          }
                       }
                 )
              
                 isValid    <<- cleangeo::clgeo_IsValid(polygon, verbose)
              }
            
              #test clean geometry validity
              if (is.null(errors.only) & !isValid) {
                 if (verbose){
                    report.msg <- NULL
                    if (!is.na(report[x,"error_msg"])){
                       report.msg <- report[x,"error_msg"]
                    } else if (!is.na(report[x,"warning_msg"])){
                              report.msg <- report[x,"warning_msg"]
                           }
                    #logger.info(sprintf("Cleaning geometry at index %s (%s)", x, report.msg))
                    cat(sprintf("Cleaning geometry at index %s (%s)", x, report.msg))
                 }
                 if (strategy == "POLYGONATION"){
                    #run polygonation algorithm
                     polygon <- cleangeo::clgeo_CleanByPolygonation.SpatialPolygons(polygon, 
		                          verbose)
                    } else if (strategy == "BUFFER"){
                              #try applying buffer attempts
                              attempt <- 1
                
                              polygon <- rgeos::gBuffer(polygon, id = ID, width = 0)
                              while (attempt < 3) {
                                 if (!cleangeo::clgeo_IsValid(polygon, verbose)) {
                                    attempt  <- attempt + 1
                                    polygon  <- rgeos::gBuffer(polygon, id = ID, 
                                         width = 0)
                                 } else { 
                                    break;
                                 }
                           }
                 }
              }
              if (!is.null(polygon)) {
                 polygon             <- polygon@polygons[[1]]
                 slot(polygon, "ID") <- ID
              } else {
                 if (verbose) {
                    #cat("logger Code: 1883 verbose:",verbose,"\n")
                    #logger.info(sprintf("Removing false polygon at index %s", x))
                    cat(sprintf("Removing false polygon at index %s", x))
                 }
              }
           }
        }
        
        return(polygon)
      } )   # end of function in ?apply  (results is a list of polygon (Polygons) 
      
      
      if (!is.list(fixed.sp.list)) fixed.sp.list <- as.list(fixed.sp.list)
      
      fixed.sp.list <- fixed.sp.list[!sapply(fixed.sp.list, is.null)]
      
      fixed.sp     <- NULL
      if (length(fixed.sp.list) > 0) {
         fixed.sp   <- sp::SpatialPolygons(
            Srl = fixed.sp.list,
            #proj4string = sp::CRS(slot(sp,"proj4string")@projargs)     # corrected for PROJ6
            proj4string  = slot(sp,"proj4string")   # transfer the proj4string from the original sp (CRS)
           )
        
         if (is(sp, "SpatialPolygonsDataFrame")) {
            sp.df    <- as.data.frame(sp)    # fix instead of as(sp,"data.frame")
            ids      <- sapply(slot(fixed.sp,"polygons"), slot, "ID")   
            if (nrow(sp.df) != length(ids)) {
               sp.df  <- sp.df[ids,]
            }
            row.names(sp.df) <- ids
            fixed.sp <- sp::SpatialPolygonsDataFrame(Sr = fixed.sp, data = sp.df)
         }
      }
      
      return(fixed.sp)
   }
   
   #logger.info <- function(x) warning(x,call.=FALSE)
   #
   #####
   
   
   #####
   #   
   #  Area color selection - to avoid neighbors having the same colour.
   #
   #  Credit: Stack Overflow - R
   #  Uses   spdep::poly2nb, 
   #
   nacol <- function(spdf) {
       resample   <- function(x, ...) x[sample.int(length(x), ...)]
       nunique    <- function(x){ unique(x[!is.na(x)]) }
       #    This only works if you have an SPDF passed to you
       #np         =  nrow(spdf)           # number of polygons in SPDF
       #    The following will work on SP and SPDF structures
       np         =  length(spdf@polygons)
       if (np > 1) {
          adjl       =  spdep::poly2nb(spdf) # get the adjacency list for each area.
          cols       =  rep(NA, np)          # vector of colors
          cols[1]    =  1
          nextColour =  2
   
          for (k in 2:np) {
             adjcolours = nunique(cols[adjl[[k]]])
             if (length(adjcolours) == 0) {
                cols[k] = resample(cols[!is.na(cols)],1)
             } else {
                avail   = setdiff(nunique(cols), nunique(adjcolours))
                if (length(avail)==0) {
                   cols[k]    = nextColour
                   nextColour = nextColour+1
                } else {
                   cols[k]    = resample(avail,size=1)
                }
             }
          }
       } else {
          cols = 1
       }
       return(cols)
   } 
   # Assigned in numbers - can't be done if polygons/areas are moved around.
   #
   #####
  
  
   #####
   #
   #  Draw text with black outline - shadow
   #
   
   shadowtext <- function (x, y=NULL, labels, col="white", bg="black", 
           theta=(seq(pi/4, 2*pi, length.out=8)), r=0.1, ... ) {
           
           # Input:  x, y    coordinates for the labels.
           #         Labels  labels to be written 
           #         col     Color of the text
           #         bg      background - outline around the text
           #         theta   list of different locations for each redraw of the black 
           #                 version of the character to form the outline in radians.
           #                 The default is a vector with 8 values every 45 degrees.
           #         r       is the realtive size of the outline. The large the number to bigger the outline.
           #         ...     parameters passed to the text plot function.
           #                 
           xy = xy.coords(x,y)
           xo = r*strwidth('A')
           yo = r*strheight('A')
           
           for (i in theta) {
              graphics::text(xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ...)
           }
           graphics::text(xy, labels, col=col, bg=NULL, ...)
        }   
           
   #    
   #####
   
   #####
   #
   #  SamplePrts - one lattic style - multiple small maps with 5 areas
   #  colored each time and one scaled small image map with 5 areas colored.
   #
   #  Used to graphically show how the map areas will looked shaped.
   #  Dependes on debug set to 256 and/or 512..
   #  1024 is used to signal to the caller, the first and last map should
   #  be displayed via the 512 value.
   #
   #  This print sample routine must handle two situations:
   #   a) the SPDF contains polygons not grouped by area. Thus
   #      the number of polygons is much larger than the number of areas.
   #      However, the polygons should be grouped by area and colored.
   #   b) The SPDF has been unioned and all of the polygons are held 
   #      under the entry for each area.
   #  Color are assigned by area identifiers.
   #
   
   SamplePrts <- function(PPSp, PPTitle, PPMfrow, debug, NTLink, NTKey, MAvgH=NULL, NCol = 5) {
      # Color list setup.
      #cat("length of PPSp:", length(PPSp),"\n")
      # set up for 5 colors.
      BaseColors   <- c(mcolors[1],mcolors[2],mcolors[3],mcolors[4],mcolors[5])
      BlankColors  <- c(NA,NA,NA,NA,NA)
      
      NNN          <- 5
      if (NCol != 5) {
         # if number of colors is not 5, then use 6
         BaseColors  <- c(BaseColors,mcolors[6])
         BlankColors <- c(NA,NA,NA,NA,NA,NA)
         NNN         <- 6
      }
  
      if (missing(MAvgH) || is.null(MAvgH)) {
         MAvgH = 1.3
      }
     
      PPSpData         <- PPSp@data
      PLinkList        <- PPSpData$X__Link  # use link to pull it all together
      PLinkListSize    <- length(PLinkList)
      
      Poly2Area        <- match(PLinkList,NTLink)  # match polygon to NTable.
      
      #cat("length of Shape file List:", length(PLinkList),"\n"," Shapefile List:\n")
       
      UniPLinkList     <- unique(PLinkList)   # unique number of areas & names.
      
      UniPLinkListSize <- length(UniPLinkList)
      
      xNNN             <- UniPLinkListSize - NNN
      NTLinkCol        <- data.frame(NTLink=NTLink,Key=NTKey,Col=NA)  
     
      if (bitwAnd(debug,256) != 0) {
         #cat("Generate scaled example of test maps - ",PPTitle,".\n")
         # only writes PDF.
         
         # display multiple maps with 5 colored/shaded areas per map 
         # and draw on one sheet at the approximate scale
         
         #cat("UniPLinkListSize:",UniPLinkListSize,"  NNN:",NNN,"  xNNN:",xNNN,"\n")
         # Table - area -> color
         #cat("length of NTLink:",length(NTLink),"\n")
          
         NumPanels        <- (length(NTLink)-1)/NNN + 1       # calculate number of panels.
         NumPanels        <- as.integer(NumPanels)
         
         # This printout is only done to PDF.
         #   Build PDF filename and title.
         PDFTest          <- paste0(BGDir,BGBase,"_TestChart_",PPTitle,".pdf")
         Title            <- paste0("Test Chart - ",PPTitle)
         
         # open PDF file for output 
         grDevices::pdf(PDFTest,width=10.5,height=7.75)
         # set up for multiple images.
         #  outside and inside margins.
         par(mai=c(0.125,0.125,0.125,0.125))  #  1/8" around
         par(mar=c(1,1,2,1))
         par(oma=c(.5,.5,.5,.5))
         
         #cat("par('din')",par('din'),"  par('fin'):",par('fin'),"  par('pin'):",par('pin'),"\n")
         
         par(mfrow=PPMfrow)   # setup to provide about the same space as a micromap
         
         # initialize the color pattern (one per area)
         VColors       <- c(BaseColors, rep(NA,xNNN))    # moving vector(areas) as we draw.
    
         #cat("xNNN:",xNNN,"  NumPanels:",NumPanels,"  length of VColors:",length(VColors),"\n")
           
         for (inx in c(seq(1,NumPanels))) {
            # find the multiple polygons per area.
       
            NTLinkCol$Col <- VColors
            #cat("VColors:",paste0(VColors,collapse=", ",sep=""),"\n") 
            ColList       <- NTLinkCol[Poly2Area,"Col"]  # get color per polygon
             
            # now match the polygon list KeyCol list and pick up the color
            xm            <- !is.na(NTLinkCol$Col)
            
            NTLinkLeg     <- NTLinkCol$Key[xm]
            NTLinkLegCol  <- NTLinkCol$Col[xm]
            #cat("NTLinkCol with colors.\n")
            #print(NTLinkCol)
         
            #  Plot one map - 5 colors/areas
            sp::plot(PPSp, col=ColList,lwd=0.2)     # micromapST defaults lwd=0.5
            par(new=TRUE)
            graphics::title(main=Title)
            
            #legend("right", KeyLeg, text.col = "black", cex=0.5, bty="n",
            #       pch=NA, xpd=TRUE, vfont=c("san serif"), inset=-0.05)
            graphics::legend("right", NTLinkLeg, text.col = NTLinkLegCol, 
                   cex=0.5, bty="n", pch=NA, xpd=TRUE, inset=-0.05)
         
            #  slide colors over for the next set.
            VColors      <- c(BlankColors,VColors)[1:UniPLinkListSize]
         }
         
         # end of page, close file.
         grDevices::dev.off()
      }  #  done with panels of multiple scaled maps with shapings.
      
      #  Generate single image of map pdf or png based on debug = 128
      if (bitwAnd(debug,512) != 0) {
      	 # Starting point for mcolors.
      	 # This printout is done to png or pdf
      	 # get aspect and scale right.
         xBBox        <- sp::bbox(PPSp)
      	 xAsp         <- diff(xBBox[2,])/diff(xBBox[1,])  # y/x
      	 PngH         <- MAvgH                       # y = 1.25
      	 PngW         <- MAvgH / xAsp                # x = 2.2? 
      	 #cat(" xAsp:",xAsp,"  PngW:",PngW,"  PngH:",PngH,"\n")
      	
      	 OutTestSm    <- paste0(BGDir,BGBase,"_CP_",PPTitle,OType)
      	
         # initialize the color pattern (one per area)
         VColors       <- c(BaseColors, rep(NA,xNNN))    # moving vector(areas) as we draw.
   
         NTLinkCol$Col <- VColors
         PCol          <- NTLinkCol[Poly2Area,"Col"]         
         # disable all users of space around the map.
         par(mai=c(0,0,0,0))
         par(mar=c(0,0,0,0))
         par(omi=c(0,0,0,0))
         par(oma=c(0,0,0,0))
         #cat("par pin:",par("pin"),"  fin:", par("fin"),"\n")
         
         #  we have two debug flag numbers.  512 prints these images at each processing step of the SPDF,  
         #      1024 only prints the RAW and the FINISHED images.  If 1024 is set, when the first and 
         #      final maps are being processed, the 512 setting will be asserted.
      
         if ( OType == ".png" ) {
            grDevices::png(OutTestSm,res=300,width=PngW, height=PngH, units="in")
         } else {
            grDevices::pdf(OutTestSm, width=PngW, height=PngH)
         }
           
         sp::plot(PPSp,col=PCol,lwd=0.05)
         graphics::title(main=PPTitle,cex.main=0.1)
         
         grDevices::dev.off()
       
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
   ##### 010x
   
   ErrorFlag <- FALSE
   StopFlag  <- FALSE
   
   #####
   #
   #  Quick check out of the debug parameter.
   #
   def_debug <- 0
   if (is.null(debug) || missing(debug)) {
       debug <- def_debug
   } else {
       if (!is.numeric(debug)) {
          xmsg <- paste0("***3100 debug call parameter is not a numeric value. The default value of ",def_debug," will be used.")
          errCntMsg(xmsg)
          debug <- def_debug
       }
   }
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
                   ShapeLinkName=NULL, NameTableFile=NULL, 
                   NameTableDir=NULL, NameTableLink = NULL,
                   BorderGroupName=NULL, BorderGroupDir=NULL,
                   MapHdr=NULL, MapMinH=NULL, MapMaxH=NULL, 
                   IDHdr=NULL, ReducePC=NULL, proj4=NULL, 
                   checkPointReStart=NULL, debug=debug)
      #
      # set call variable list (callVL) to the defined list and defaults.
      callVL       <- frml    
      
      #cat("callVL typeof:",typeof(callVL)," class:",class(callVL),"\n")
      #cat("   Contents:\n")
      #print(callVL)
      
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
      #cat("   updated contents:\n")
      #print(callVL)
          
      callVarNames    <- names(callVL)    # get list of all names.
      #killlist       <- c("proj4","ReducePC")
      #callVarNames   <- callVarNames[is.na(match(callVarNames,killlist))]   # emulating only values in call.
   }  
   for (ivar in names(callVL)) {
      #  Extract the variables from list and then from parameters
      # build cm to assign value to Global Variable.
      wstr <- paste0("assign(ivar,callVL$",ivar,",envir=.GlobalEnv)")  
      #print(wstr)
      eval(parse(text=wstr))   
   }
   
   #print(callVL)    # list of call parameters and function defined parameters.
   parmNames <- callVarNames
   
   ##### 010x
   #
   # Initialize Variables  0.1
   #
  
   r1=rgdal::set_thin_PROJ6_warnings(TRUE)
   r2=rgdal::rgdal_extSoftVersion()
   r3=rgdal::new_proj_and_gdal()
   r4=options("rgdal_show_exportToProj4_warnings"="none")
   
   ReqCParms    = c("ShapeFile", 
                    "NameTableFile", "BorderGroupName", 
                    "MapHdr", "IDHdr") 
   ReqCkptParms = c("NameTableDir", "BorderGroupName")
   
   # List of required call parameters
   # Everything else we can make up.  
   # If no directories are specified, we use the current working directory.
   
   #
   #  Set up project 4 strings  -  Original if none is present in the ShapeFile.
   #
   OrigLongLat    <- "+proj=longlat +datum=NAD83 +no_defs"
   OrigCRSLongLat <- sp::CRS(OrigLongLat)
   #
   DoUserProj4   <- FALSE  # user provided proj4 for final projection
   DoModproj4    <- FALSE  # have modified shapefile proj4 with meters 
                           #(only if shapefile projection was modified to Meters.)
   DoBldAEAProj  <- FALSE  # ShapeFile is LL, no proj4, need to build AEA projection
   ShpProjLL     <- FALSE  # Indicator of LL projection in ShapeFile, or none and set to LL.
   
   OType    <- ".pdf"
   if (bitwAnd(debug,128) !=0)  OType  <- ".png"
   
   sf::sf_use_s2(FALSE)   # disable use of S2 functions.
       
   
   if (bitwAnd(debug,2) != 0) 
         cat(" Code: 2301 \n")
   if (bitwAnd(debug,4) != 0) 
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
             "  DoUserProj4:",DoUserProj4,
             "  DoModproj4:",DoModproj4,
             "  DoBldAEAProj:",DoBldAEAProj,"\n")
   
   Modproj4      <- NA     # modified ShapeFile Proj4 modified to meters
   
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
   
   #####
   #
   #
   #
   #####
   
   
   #####  011x
   #
   #  Part 1.1 - checkPointReStart call parameter and logic wrapper.
   #
   #  If checkPointReStart is TRUE. then the critical call parameters to check first
   #  are the checkPointReStart, NameTableDir (foundation for checkpoint folder), and
   #  BorderGroupName (part of the checkpoint file name in folder).
   #  Then we can continue or bypass the other checks.
   #
   #  Required for normal run:  
   #       ShapeFile, NameTableFile, BorderGroupName, 
   #       MapHdr, IDHdr
   #    Optional:
   #       ShapeFileDir, NameTableDir, NameTableLink, BorderGroupDir, ReducePC, debug, proj4, 
   #       MapMinH, MapMaxH, ShapeLinkName
   #
   #  Required for checkPointReStart run:
   #       NameTableDir*, BorderGroupName*, checkPointReStart
   #    Optional:
   #       BorderGroupDir, debug
   #	Not Used:
   #       ShapeFile, ShapeFileDir, ShapeLinkName, NameTableFile, NameTableLink, 
   #       MapHdr, IDHdr, MapMinH, MapMaxH, proj4, ReducePC
   #    * must be the same value as used in original run.
   #
   
   ##### 012x
   #
   #  Part 1.2 - validate checkPointReStart
   #
   #  Check for checkPointReStart call parameter
   
   if (missing(checkPointReStart) || is.null(checkPointReStart)) {
      # checkPointReStart call parameter is missing, set to default
      checkPointReStart <- FALSE
   } else {
      checkPointReStart <- checkPointReStart[[1]][1]  # get first value
      if (!is.logical(checkPointReStart)) {
         # not a logical variable
         xmsg     <- "***3120 The checkPointReStart call parameter is not a logical value."
         StopFlag <- stopCntMsg(xmsg)
      } else {
         if (checkPointReStart) {
            if (bitwAnd(debug,2048) != 0) {
               cat("***3122 Check Point Restart has been requested.  Check point files will be read\n")
               cat("***3122 from folder:",NameTableDir,"/Checkpoint","  directory.")
            }
         }
       }
   }
   # P.S. The directories could be "" representing the current working directory.
   #
   #####
   
   #####  013x
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
      StopFlag <- stopCntMsg(paste0("***3130 - Required call parameters are missing : ",paste0(missingRegList,collapse=", ")," - execution stopped."))
   }
   #cat("Checking for all parameters and built callVarNames and callVL\n")
   #print(paste0(callVarNames,collapse=", ", sep=""))
   
   if (StopFlag) {
   	xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
   	stop(xmsg)
   }
   #
   #####
      
   #####  020x
   #
   #  Part 1.4 - NameTable dir (#1) 
   #                (required may be the backup dir for BorderGroupDir.)
   #
   NTDir  <- NULL
    
   if (is.null(NameTableDir) || is.na(NameTableDir) ) {
   
       # make sure its NULL
       NameTableDir <- NULL    # no directory provided.
   
   } else {
       # validate the directory exists and is reference.
       NameTableDir <- stringr::str_trim(NameTableDir)                   # trim spaces.
       x  <- stringr::str_sub(NameTableDir,-1,-1)
       if (x=="/" || x=="\\") {
          NameTableDir  <- stringr::str_sub(NameTableDir,1,-2)
       }
    
       if (!dir.exists(NameTableDir))  {
          # NameTableDir path does not exist.
          StopFlag   <- stopCntMsg(paste0("***3202 NameTable directory specified in the NameTableDir call parameter does not exist. Value=",NameTableDir))
       }  else {
          # have a good NameTable directory
          NTDir  <- paste0(NameTableDir,"/")
       }
       #  We should have a good NameTable Directory or a StopFlag.
   }
   
   callVL$NameTableDir <- NameTableDir
   callVL$NTDir        <- NTDir		    # with /
   #
   #####
   
   #####   015x
   #
   #  Part 1.5 - BorderGroup Name and Directory & Restart Directory.
   #
   
   if (is.null(BorderGroupDir) ) {
      BorderGroupDir <- NameTableDir
      BGDir          <- paste0(NameTableDir,"/")     # with /
   } else {
      if ( is.na(BorderGroupDir) ) {
         # BorderGroupDir set to NA - use working directory
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
         }
      
         if (!dir.exists(BorderGroupDir))  {
            # BorderGroupDir path does not exist.
            StopFlag <- stopCntMsg(paste0("***3150 BorderGroup directory specified in the call parameter does not exist. Value=",BorderGroupDir))
         } else {
            # have a good directory 
            BGDir <- paste0(BorderGroupDir,"/")	# with /
         }
      }
   }      
   callVL$BorderGroupDir <- BorderGroupDir
   callVL$BGDir          <- BGDir		  # with /
   
   if (is.null(BorderGroupName)  || is.na(BorderGroupName) ) {
      StopFlag <- stopCntMsg(paste0("***3152 The required BorderGroupName call parameter is missing."))
   } else {
      if (!is.character(BorderGroupName)) {
         StopFlag <- stopCntMsg(paste0("***3154 BorderGroupName is not a character string. Value=",BorderGroupName))
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
   
   callVL$BGBase       <- BGBase      # Name without BG as the ending.
      
   BGFile              <- paste0(BGBase,"BG.rda")
   callVL$BGFile       <- BGFile
   
   BorderGroupPath     <- paste0(BGDir,BGFile)
   
   callVL$BorderGroupName <- BorderGroupName
   callVL$BorderGroupPath <- BorderGroupPath
   
   cat("Border Group will be written to:",BorderGroupPath,"\n")
   
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
   
   if (!checkPointReStart) {   # normal validation of call parameters.
   
      #######
      #####
      #
      #  NORMAL PATH
      #
      #  Start validating and processing the calling parameters.
      #
      if (bitwAnd(debug,2) != 0) cat("Code: 2535 \n")     #    pick up testing....
       
      ##### 020x
      #
      #  Part 2.0 - Name Table filename.
      #
      def_NameTableType  <- 1          #   default .csv
      def_NameExt        <- ".csv"     #   default extension
      NameTableFile      <- NameTableFile[[1]][1]
      
      if (is.null(NameTableFile) || is.na(NameTableFile) ) {
         StopFlag   <- stopCntMsg(paste0("***3204 NameTableFile parameter has not been provided. Value=",NameTableFile))
      } else {
         # validate the directory exists and is reference.
         NameTableFile <- stringr::str_trim(NameTableFile)        # trim spaces.
         
         # if not check to see if the .RDA file exists.
         fnSplit       <- stringr::str_split(NameTableFile,"[.]")[[1]]   # split up user provided name.
         NameTableFileBase <- fnSplit[1]    # base filename only
         
         if (is.null(fnSplit[2]) || is.na(fnSplit[2])) {
            # if no extension (missing) - then add .csv
            ErrorFlag <- errCntMsg(paste0("***3205 File extension on name table filename is missing - .csv added."))
            NameExt        <- def_NameExt         #  "csv"
            NameTableFile  <- paste0(NameTableFileBase,".",NameExt)    # make it a CSV file
            NameTableType  <- def_NameTableType   # 1   (CSV)
            
         } else {
            # if extension is present - must be .csv, .xls, .xlsx, or .RDA
            #cat("Found file ext of ",fnSplit[2],".\n")
            NameExt    <-  stringr::str_to_upper(fnSplit[2])  # make uppercase so only have to match uppercase versions.
            
            NTExtList  <- c("CSV","XLS","XLSX","RDA")
            xm         <- match(NameExt,NTExtList)
            
            if (all(is.na(xm))) {   # nor VALID TYPE OF FILE - ERROR AND STOP
               # error - extension must be .csv, .xls, .xlsx, .RDA or .RData.
               StopFlag <- stopCntMsg(paste0("***3206 The NameTable file is not a .csv, Excel, or R .RDA format. "))
            
            } else {
               # have valid match, XM is type (position in table above)
               NameTableType = xm
               #  extent in NameExt for later use.
               #  NameExt has validated file extension
               #  NameTableFileBase has the filename base
               #  NameTableType is the type of file (1=CSV, 2&3=Speadsheet, 4=RDA)
               NameTablePath <- paste0(NameTableDir,"/",NameTableFile)
           }
         }
      }
      #
      #####
      
      ##### 020x
      #
      #   NameTableLink call parameter
      def_NameTableLink <- "Link"
      
      if (missing(NameTableLink) || is.null(NameTableLink)) {
         NameTableLink <- def_NameTableLink   # if not there assign the default
      } else {
         if (!is.character(NameTableLink)) {
            xmsg <- paste0("***3208 The NameTableLink call parameter is not a character string. Fix and rerun.\n")
            stopCntMsg(xmsg)
         #} else {
            # Using "Link" or user provided.
            # Can't check if it exist until later.
         }
      }
      #
      #####

      ######  021x messages
      #
      #  Part 2.1 - ShapeFile - simple character string, filename with no extensions or SPDF structure for the shapefile.
      #	       Directory should be provided in ShapeFileDir parameter if filename.
      #        Used with readOGR shapefile read, as layer= parameter.
      #
      WorkSp01      <- NULL
      SPDFPassed    <- FALSE
      ShapeFilePath <- NULL
      ShapeFileExt  <- NULL
      
      if (is.null(ShapeFile) || is.na(ShapeFile) ) {
      
         StopFlag <- stopCntMsg(paste0("***3212 Shape File parameter has not been provided. "))
           
      } else {
         # see if character string or SPDF.
         if (is(ShapeFile,"SpatialPolygonsDataFrame")) {
            # value passed to function is a real SPDF.
            SPDFPassed <- TRUE
            WorkSp01   <- ShapeFile       # save the passed Shape File into the SPDF

         } else {
            if (!is.character(ShapeFile)) {
               StopFlag <- stopCntMsg(paste0("***3213 The Shape File call parameter is not a SPDF or character variable."))
            } else {
               # validate the directory exists and is reference.
               ShapeFile    <- stringr::str_trim(ShapeFile[[1]][1])           # trim spaces.
               ShapeFileExt <- ""
      
               # handle the problem callers may include extension.
               x <- tools::file_ext(ShapeFile)     # ext last 4 characters of shape file name.
               if (x != "") {
                  if (x == "shp" || x == "shx" || x == "dbf" || x == "prj") {
                     # extension matches one of the shape file extensions.
                     #  try to find shape file with and without ".ship" extension.
                     ShapeFile     <- file_path_sans_ext(ShapeFile)
                     ShapeFileExt  <- x
                     # strip the extension, but will need one for later file check.
                  }
               } else {
                  #  Have no extension - save a "shp" for later
                  ShapeFileExt <- "shp"
               }
               #
               # If ShapeFile is pointing to a file, then must process the ShapeFileDir 
               # call parameter.  Otherwise, it is ignored.
               #
               #  ShapeFileDir - Where the shape file is located.
               #
               if (is.null(ShapeFileDir)) {
                  # no directory specified - use NameTableDir
                  ShapeFileDir  <- NameTableDir                 # no ShapeFileDir provided, use Name Table Dir.
                  ShapeFilePath <- paste0(NameTableDir,"/",ShapeFile)  # path is dir,"/", ShapeFile without ext.
               } else {
                  if ( is.na(ShapeFileDir) ) {
                     # the Shape Directory is NA force to use working directory
                     ShapeFileDir  <- NULL           # no directory provided.
                     ShapeFilePath <- ShapeFile
                  } else {
                     # validate the directory exists and is reference.
                     ShapeFileDir <- stringr::str_trim(ShapeFileDir)                   # trim spaces.
                    
                     if (!dir.exists(ShapeFileDir))  {
                        # ShapeFileDir path does not exist.
                        StopFlag <- stopCntMsg(paste0("***3214 Shape file directory specified in the ShapeFileDir call parameter does not exist. Value=",ShapeFileDir))
                     } else {
                        #  Have a valid existing ShapeFile directory
                        x  <- stringr::str_sub(ShapeFileDir,-1,-1) # strip possible trailing / or \\
                        if (x=="/" || x=="\\") {
                           ShapeFileDir  <- stringr::str_sub(ShapeFileDir,1,-2)
                        } # end of cleaning up directory
                        
                        #  Have valid name and dir -> build ShapeFile path   Dir without / or \\
              
                        if (is.null(ShapeFileDir)) {
                           # no directory specified. or supplied, use working directory.
                           ShapeFilePath <- ShapeFile
                        } else {    
                           # build full pathnames
                           ShapeFilePath <- paste0(ShapeFileDir,"/",ShapeFile)  # path with no ext.
                        }  # end of path build
                          
                        # now that the extensions have been stripped, add it back for the exist test.
                     } # end of directory and path build 
                  } # end of directory check - parameter exist and the rest.
               } 
               xS <- paste0(ShapeFilePath,".",ShapeFileExt)  # put full filename back together.
              
               if (!file.exists(xS))  {
                  # Shapeliest (dir and name) does not exist.
                  StopFlag   <- stopCntMsg(paste0("***3216 Shape file (dir & name) does not exist. Value=",xS))
               }  # end of existance test for ful path.
               SPDFPassed <- FALSE
            }  # end of ShapeFile check as filename (with and without directory)
         } # end of ShapeFile check for type
      }  # end of ShapeFile check
      
      callVL$ShapeFileDir   <- ShapeFileDir
      callVL$ShapeFile      <- ShapeFile
      callVL$ShapeFilePath  <- ShapeFilePath
      callVL$ShapeFileExt   <- ShapeFileExt
      callVL$SPDFPassed     <- SPDFPassed
      
      #
      #  end Shape file name/directory or SPDF
      #
      ######
      
      ######   022x
      #
      #  Part 2.2 - ShapeLinkName
      #
      ShapeLinkName <- ShapeLinkName[[1]][1]
      if (missing(ShapeLinkName) || is.null(ShapeLinkName)  || is.na(ShapeLinkName) ) {
         ShapeLinkName = "NAME"   # the default.
         ErrorFlag  <- errCntMsg(paste0("***3220 The ShapeLinkName call parameter is missing. The default value of 'NAME' will be used."))
      } else {
         # ShapeLinkName is present
         if (!is.character(ShapeLinkName)) {
            StopFlag  <- stopCntMsg(paste0("***3221 ShapeLinkName is not a character string. Value=",ShapeLinkName))
         }
      }
       
      callVL$ShapeLinkName <- ShapeLinkName
      cat("Shape file @data data.frame Link column name is ",ShapeLinkName,"\n")
      
      #  End of Shape file
      #
      #######
      
      if (StopFlag) {
        xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
      	stop(xmsg)
      }
      if (bitwAnd(debug,2) != 0) cat("Code: 2874 \n")
       
      #
      #  Done Checking the require parameters 
      #
      ######
      
     
      ###### 023x
      #
      #  Part 2.3 - MapHdr
      #
      def_MapHdr <- c("",BGBase)
      #
     
      if (is.null(MapHdr) || (length(MapHdr)==1 && is.na(MapHdr)) ) {
         # empty parameter - use BGBase (BorderGroupName minus the BG) as header # 1
         MapHdr <- def_MapHdr
      } else {
         if (!is.character(MapHdr)) {
            # MapHdr strings are not characters
            ErrorFlag <- errCntMsg(paste0("***3230 The MapHdr parameter does not contain character ",
                                 "strings for use as the column headers. MapHdr is ignored."))
            MapHdr    <- def_MapHdr    # Use default of a blank line and border group name
         } else {
            # MapHdr is characters, check length
            if (!is.vector(MapHdr)) {
               errCntMsg(paste0("***3232 The MapHdr parameter must be a simple vector type.  ",
                                     "MapHdr is ignored."))
               ErrorFlag <- FALSE
               MapHdr    <- def_MapHdr
            } else {
               if (length(MapHdr) > 2 || length(MapHdr) == 0 ) {
                  ErrorFlag <- errCntMsg(paste0("***3234 The MapHdr parameter has zero or more than 2 elements. Only the first 2 will be used."))
                  warning(xmsg,call.=FALSE)
               } else {
                  if (max(nchar(MapHdr)) > 16) {
                     warning(paste0("***3236 It is suggested the max length of the MapHdr strings be ",
                                    "16 characters."),call.=FALSE)
                  }
               }
            }
         }
      }
      callVL$MapHdr <- MapHdr
      cat("MapHdr Header Labels used : ",paste0(MapHdr,collapse=", ",sep=""),"\n") 
      #
      ######
      
      ###### 024x
      #
      #  Part 2.4.1 - MapMinH - minimum height for the micromap
      #
      def_MapMinH <- 0.5
      #
      MapMinH <- MapMinH[[1]][1]
      if (is.null(MapMinH) || is.na(MapMinH)) {
         # empty parameter - use default 
         MapMinH         <- def_MapMinH
      } else {
         if (!is.numeric(MapMinH)) {
            # MapMinH strings are not characters
            ErrorFlag    <- errCntMsg(paste0("***3242 The MapMinH parameter does not contain numeric value. Default Value is used."))
            MapMinH      <- def_MapMinH    # Use default of a def_MapMinH
         } else {
            # Numeric -> Pick only the first element of MapMinH
            MapMinH      <- MapMinH[[1]][1]
            # Check to make sure its within range.
            if (MapMinH < def_MapMinH || MapMinH > 2.5 ) {
               ErrorFlag <- errCntMsg(paste0("***3244 The MapMinH minimum height value is out of range (0.4 to 2 inch). The default will be used."))
               MapMinH   <- def_MapMinH
            }
         }     
      }
      callVL$MapMinH <- MapMinH
      #
      ######
      
      ######  024x
      #
      #  Part 2.4.2 - MapMaxH - Maximum height for the micromap
      #
      def_MapMaxH <- 1.5
      #
      MapMaxH <- MapMaxH[[1]][1]
      
      if (is.null(MapMaxH) || is.na(MapMaxH)) {
         # empty parameter - use default 
         MapMaxH         <- def_MapMaxH
      } else {
         if (!is.numeric(MapMaxH)) {
            # MapMaxH strings are not characters
            ErrorFlag    <- errCntMsg(paste0("***3246 The MapMaxH parameter does not contain numeric value. Default Value is used."))
            MapMaxH      <- def_MapMaxH    # Use default of a def_MapMaxH
         } else {
            # Pick up only the first element of the variable
            MapMaxH      <- MapMaxH[[1]][1]
            # Make sure within range.
            if (MapMaxH > 2.5 || MapMaxH < def_MapMinH ) {
               ErrorFlag <- errCntMsg(paste0("***3248 The MapMaxH maximum height value is out of range (0.4 to 2 inches). The default will be used."))
               MapMaxH   <- def_MapMaxH
            }
         }
      }
      callVL$MapMaxH <- MapMaxH
      
      ######  Check values, then make sure Min < Max
      # 
      #  Part 2.4.3 - check min and max values
      #
      if (MapMinH > MapMaxH) {
        ErrorFlag <- errCntMsg(paste0("***3249 The MapMinH value must be less than the MapMaxH value. Will swap values."))
        x <- MapMinH
        MapMaxH <- MapMinH
        MapMinH <- x
      }
      MapAvgH <- mean(c(MapMinH,MapMaxH))
      #
      ######
      
      cat("The Map's minimum and maximum height will be:",MapMinH,
          " & ",MapMaxH,"\n")
      
      if (StopFlag) {
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stop(xmsg)
      }
      if (bitwAnd(debug,2) != 0) cat("Code: 2869  \n")
         
      ######  025x
      #
      #  Part 2.5 - IDHdr
      #
      def_IDHdr <- MapHdr   # use the MapHdr as the default
      #
      if (is.null(IDHdr) || (length(IDHdr)==1 && is.na(IDHdr)) ) {
         # empty parameter - use MapHdr values.)
         IDHdr     <- def_IDHdr
      } else {
         if (!is.character(IDHdr)) {
            # IDHdr strings are not characters
            ErrorFlag <- errCntMsg(paste0("***3250 The IDHdr parameter does not contain character strings for use ",
                               "as the column headers. The MapHdr values will be used."))
            IDHdr     <- def_IDHdr
         } else {
            # IDHdr is characters, check length
            if (!is.vector(IDHdr)) {
               ErrorFlag <- errCntMsg(paste0("***3252 The IDHdr parameter must be a simple vector type. The MapHdr values will be used."))
               IDHdr       <- def_IDHdr
               
            } else {
               if (length(IDHdr) > 2) {
                  IDHdr <- IDHdr[1:2]   # keep only the first two elements
                  ErrorFlag <- errCntMsg(paste0("***3254 The IDHdr parameter has more than 2 elements. Only the first 2 will be used."))
               } else {
                  if (max(nchar(IDHdr)) > 12) {
                    warning(paste0("***3256 It is suggested the max length of the IDHdr strings be 12 characters."), call.=FALSE)
                  }
               }
            }
         }
      }
      callVL$IDHdr <- IDHdr
      
      cat("IDHdr header labels:",paste0(IDHdr,collapse=", ",sep=""),"\n")
      #
      ######
      
      if (bitwAnd(debug,2) != 0) cat("Code: 2910 \n")
      
      ######   026x
      #
      #  Part 2.6 - Reduce PC    (Range:  .1 to 100 percent)
      #
      def_ReducePC   <- 2.0     # Value is remaining vectors precentage of original.
      #
      ReducePC <- ReducePC[[1]][1]
      
      if (is.null(ReducePC) || is.na(ReducePC)) {
         # empty parameter - use default of 2 % keep value
         ReducePC   <- def_ReducePC
      } else {
         ReducePC <- as.numeric(ReducePC)   # if not numeric, will become NA
         if (is.na(ReducePC)) {   # after conversion, if = NA, its was not numeric to start.
            # ReducePC value must be a numeric
            ErrorFlag <- errCntMsg(paste0("***3266 The ReducePC parameter must be a numeric value. The default of 10% will be used."))
            ReducePC  <- def_ReducePC
         } else {
            # ReducePC is a numeric, check length
            if (!is.vector(ReducePC) ) {
               ErrorFlag   <- errCntMsg(paste0("***3267 The ReducePC parameter must be a simple vector. The default value of 10% will be used."))
               ReducePC    <- def_ReducePC 
            } else {
               if (length(ReducePC) > 1) {
                  ReducePC    <- ReducePC[1]
                  ErrorFlag   <- errCntMsg(paste0("***3268 The ReducePC parameter has more than one value. Only the first value will be used."))
               } else {
                  if (ReducePC < .1 || ReducePC > 100) {  # out of range  (percentage to keep)
                       ErrorFlag   <- errCntMsg(paste0("***3269 The value of ReducePC is out of range (0.75 to 95 %). The default value of 10% will be used."))
                       ReducePC  <- def_ReducePC
                  }
               }
            }
         }
      }
      callVL$ReducePC <- ReducePC
      cat("Map simplification keep value:",ReducePC," (% or decimal).\n")
      #
      ######
      
      if (StopFlag) {
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stop(xmsg)
      }
      if (bitwAnd(debug,2) != 0) cat("Code: 2956 \n")
       
      ######   029x
      #
      #  Part 2.9 - LabelCex (Range:  .01 to 10 )
      #
      def_LabelCex   <- .25     # Value is remaining vectors precentage of original.
      #
      LabelCex <- LabelCex[[1]][1]
      
      if (is.null(LabelCex) || is.na(LabelCex)) {
         # empty parameter - use default 
         LabelCex   <- def_LabelCex
      } else {
         LabelCex <- as.numeric(LabelCex)   # if not numeric, will become NA
         if (is.na(LabelCex)) {   # after conversion, if = NA, its was not numeric to start.
            # LabelCex value must be a numeric
            ErrorFlag <- errCntMsg(paste0("***3296 The LabelCex parameter must be a numeric value. The default of 0.25 will be used."))
            LabelCex  <- def_LabelCex
         } else {
            # LabelCex is a numeric, check length
            if (!is.vector(LabelCex) ) {
               ErrorFlag   <- errCntMsg(paste0("***3297 The LabelCex parameter must be a simple vector. The default value of 0.25 will be used."))
               LabelCex    <- def_LabelCex
            } else {
               if (length(LabelCex) > 1) {
                  LabelCex    <- LabelCex[1]
                  ErrorFlag   <- errCntMsg(paste0("***3298 The LabelCex parameter has more than one value. Only the first value will be used."))
               } else {
                  if (LabelCex < .05 || LabelCex > 10) {  # out of range  (percentage to keep)
                       ErrorFlag   <- errCntMsg(paste0("***3299 The value of LabelCex is out of range (0.05 to 10). The default value of 0.25 will be used."))
                       LabelCex    <- def_LabelCex
                  }
               }
            }
         }
      }
      callVL$LabelCex <- LabelCex
      cat("Map Labels Cex value:", LabelCex," multiplier.\n")
      #
      ######
      
      if (StopFlag) {
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stop(xmsg)
      }
      if (bitwAnd(debug,2) != 0) cat("Code: 3002 \n")
      
      ######  027x
      #
      #  Part 2.7 - proj4 parameter - micromap projection.  For all of the map.
      #
      #    Done in step (setup 2.2, executed in 6.0
      
      #
      #  **** How to deal with the new PROJ6 and the WKT structures?????
      #  **** New policy: The caller should transform the shapefile to the projection 
      #       they want to use in the micromap maps, then save the shapefile for 
      #       reloading by this function (could make changes to allow the shapefile/SP
      #       data.frame to be passed directly to the function instead of reading it in.
      #       Proj4 is only used when caller provides a long/lat projected shapefile or
      #       one with no proj4string (is.na) and want to override the default
      #       transform based on AEA and the calculated Albers parallels and central median
      #       and center latitude.  
      #

      #  There is no default value for the proj4 parameter.  If it is NULL or NA, it stays
      #  that way until its time to test the projection of the shapefile.  If none Long/Lat,
      #  the proj4 parameter is checked.  If none if provided then the default AEA
      #  projection with calculated parameters is created and used.
      #
      def_proj4     <- NULL
      def_CRSproj4  <- NULL
      
      #  general projection flags:
      DoUserProj4   <- FALSE  # user provided proj4 for final projection
      DoModproj4    <- FALSE  # have modified shapefile proj4 with meters (only if shapefile projection was modified to Meters.)
      DoBldAEAProj  <- FALSE  # ShapeFile is LL, no proj4, need to build AEA projection
      ShpProjLL     <- FALSE  # Indicator of LL projection in ShapeFile, or none and set to LL.
      
      Modproj4      <- NA    # modified ShapeFile Proj4 modified to meters
      #
      #  Projection Status:
      #
      #  Override the default map projection with the users projection..
      #  The projection string is provided in proj4 format and must 
      #  be convert able by CRS to a usable projection.  It must also be 
      #  reversible back to the proj4 string as a validation.
      #
      #  The transformation is done right before printing the maps.
      #  The projection of the maps is returned to the SeerMapper caller.
      #
      #  If present, proj4 is inspected for LL. If so, not permited and ignored - set to NULL.
      #  If proj4 is non-LL, string inspected and +units set to meters.
      #  If shapefile projection exist and not LL, inspect for +units = meters. 
      #     Build projection to change, after modifications.
      #  If proj4 and ModProj4 both exist, don't do ModProj4.
      #  If proj4 is missing and shapefile was LL, then indicage AEAproj needed.
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
      
      proj4 <- proj4[[1]][1]
      
      if (is.null(proj4) || is.na(proj4)) {
         # no proj4 string specified.  Set default of NULL
         #cat("No proj4 parameter found in provided shape file, value was NULL or NA.\n")
         proj4    <- def_proj4
         CRSproj4 <- def_CRSproj4
      } else {
         # have proj4 string parameter, check for proj4 string 
         proj4    <- proj4[[1]][1]    # pick up callers provided proj4
         if (is.na(proj4)) {
            # nothing specified or NA, clear out the parameter - set defaults NULL
            proj4       <- def_proj4
            CRSproj4    <- def_CRSproj4
         } else {
            # a string was provided
            proj4       <- stringr::str_trim(proj4)   # clean up string and CRS it.
            CRSproj4    <- sp::CRS(proj4)
            DoUserProj4 <- TRUE
         }
      }
      # two stage check
      if (!is.null(proj4)) {   # projection string provided.
         # validate and check proj4 to CRS conversion.
         #cat("Off to convertPROJ4 with ",proj4,"\n")
         
         save_proj4 <- proj4
         
         res <- convertPROJ4(proj4)     # attempt to convert string.
         
         #cat("Back from convertPROJ4\n")
         
         if (res[1] != FALSE) {  
            # then res = bres[2] containing PROJ4 and wkt
            # got a conversion to CRS - looks good.
            CRSproj4    <- sp::CRS(res[1])
            proj4       <- rgdal::CRSargs(CRSproj4)
            DoUserProj4 <- TRUE
         } else {
            # Error found and reported by attempt conversion to CRS.
            ErrorFlag   <- TRUE   # error - set defaults.
            proj4       <- def_proj4
            CRSproj4    <- def_CRSproj4
            
            xmsg        <- paste0("***3272 Error processing proj4 parameter. The provided proj4 call parameter will be ignored.")
            ErrorFlag   <- errCntMsg(xmsg)
            
            DoUserProj4 <- FALSE
         }
      }
      Save_proj4 <- proj4
      if (!is.null(proj4)) {
        # If proj4 calling parameter will not NULL, check to make sure
        # check the +units= keyword to make sure the results will be in meters.
        
        if (is.na(stringr::str_locate(proj4,"\\+proj=longlat"))[1]) {     ### problems.
            # It is not longlat, Is the +units=m or not?
            if (is.na(stringr::str_locate(proj4,"\\+units=m |\\+units=m$"))[1]) {  
               # Not longlat and +units not set to meters.
                     
               xmsg <- "***3274 The proj4 call parameter does not have +units=m, changing string to meters."
               warning(xmsg,call.=FALSE)
               
               matchstr    <- "\\+units=[[:alpha:]]+"     # RegExp string to find +units=???.
                     
               #  find and replace any "+units=<>" string with "+units=m " 
               proj4       <- stringr::str_replace(proj4,matchstr,"\\+units=m ")
               #cat("New proj4 call parameter is: ",proj4,"\n")
               CRSproj4    <- sp::CRS(CRSproj4)
               DoUserProj4 <- TRUE
            }
         } else {
           xmsg        <- paste0("***3276 The proj4 call parameter specifies a long/lat projection.\n",
                                 "        proj4: ",proj4,"\n",
                                 "        The final projection can't be a longlat projection.\n",
                                 "        The proj4 parameter is ignored and a AEA projection will be created.")
           ErrorFlag   <- errCntMsg(xmsg)
           proj4       <- def_proj4
           CRSproj4    <- def_CRSproj4
           DoUserProj4 <- FALSE
         } 
      }
            
      #  Note if +proj=LONGLAT, there is no units... Only if not LONGLAT
      #  In wk world its GEOGCRS, DATUM, ELLIPSOID, LENGTHUNIT["met re",1]
      #     LENGTHUNIT[\"met re\",1] go for this anywhere.  
      #
       
      callVL$proj4      <- proj4
      callVL$CRSproj4   <- CRSproj4
      callVL$DoUserProj4<- DoUserProj4
       
      #if (!is.null(proj4)) cat("Caller requested projection at end of processing SPDF is:\n",proj4,"\n")  
      
      #
      #  At some the ShapeFile proj4string and the proj4 parameter string will be 
      #    adjusted to make sure +unit= is set to meters.
      #
      #####
      #######
      #########
      
       
      #########
      #######
      #####
      #
      #   Early read of the ShapeFile.  Need some parameters to handle questions
      #   processing the Name Table.
      #
      
      #######   03xx - 030x   (3154)
      #
      #  Part 3.0 - Setup to process ShapeFile and find shapefile 
      #     for areas to use.
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
      DoModproj4    <- FALSE

      if (!SPDFPassed) {
         # Filename passed in the function call
         #   ShapeFile common extensions:
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
         #  Read shapfile from:
         cat("Read shape file from\n",
             "   dir: ",SFDir,"\n",
             "   file:",SFName,"\n")
        
         #
         #  Read shapefile into SPDF <>  Look at readShapePoly instead.
         #
         WorkSp01     <- rgdal::readOGR(SFDir, SFName, 
                          verbose=TRUE, 
                          stringsAsFactors=FALSE,
                          dropNULLGeometries=TRUE)
      } else {
        cat("***3300 The Shape file SPDF strucure was passed to the function in the call.\n")
        WorkSp01 <- ShapeFile
      }
        
      StopFlag     <- FALSE
      
      WorkSp01Data <- WorkSp01@data
      #str(WorkSp01)
      SaveSp01     <- WorkSp01
      SaveSp01Data <- WorkSp01@data
      #
      #####
            
      #####  031x
      #
      #  Part 3.1 - Inspect and set projection on shapefile.
      #
      
      WorkSp01BBox  <- sp::bbox(WorkSp01)
      MapBox        <- WorkSp01BBox
      WorkSp01Proj4 <- stringr::str_trim(slot(WorkSp01,"proj4string")@projargs)
      
      if (bitwAnd(debug,64) != 0) 
          cat("WorkSp01Proj4 projargs:",WorkSp01Proj4,"\n")
            
      if (is.null(WorkSp01Proj4) || is.na(WorkSp01Proj4) || WorkSp01Proj4 == "") {
         # no projection supplied in ShapeFile.  
         # Set to generic longlat projection.
         cat("***3310 The proj4string field in the Shape File is empty, set to \n",
             "***3310    ",OrigLongLat,"\n")
      
         slot(WorkSp01,"proj4string") <- sp::CRS(OrigLongLat)   # generic Long/Lat
         WorkSp01Proj4                <- OrigLongLat
         ShpProjLL                    <- TRUE
         DoBldAEAProj                 <- TRUE
      } 
      # may have a proj4 set in the shapefile, it could be long/lat?
      # was it found? Check the start value!
      if (is.na(stringr::str_locate(WorkSp01Proj4,"\\+proj=longlat"))[1]) {  
         # if not found, the start and end positions will be NA.
         # the proj4string@projargs are not a longlat projection
         ShpProjLL                    <- FALSE
         DoBldAEAProj                 <- FALSE
         Modproj4                     <- NA
      } else {
         # the projection in the shapefile is long/lat
         ShpProjLL                    <- TRUE
         DoBldAEAProj                 <- TRUE
      }
      cat("proj4string check: ",slot(WorkSp01,'proj4string')@projargs,"\n")
      if (bitwAnd(debug,4) != 0) cat("Proj Flags -ShpProjLL:",ShpProjLL,
                                     "  DoUserProj4:",DoUserProj4,
                                     "  DoModproj4:",DoModproj4,
                                     "  DoBldAEAProj:",DoBldAEAProj,"\n")

      ##### 032x
      #
      #  Part 3.2 - Handle Link column in Shape File ; put it away.
      #
      cat("Handle Shape Link Name Column:",ShapeLinkName,"\n")
      
      WorkSp01Names <- names(WorkSp01Data)  # get names of shape file data columns
      if (bitwAnd(debug,64) != 0) 
         cat("Shape File @data variable names:",paste0(WorkSp01Names,collapse=", ",sep=""),"\n")
      
      #   Make sure "LinkName" exists in the @data data.frame
      if (!any(ShapeLinkName == WorkSp01Names)) {     # is ShapeLinkName is valid?
         # no = not valid name
         StopFlag <- stopCntMsg(paste0("***3324 The ShapeLinkName provided: ",ShapeLinkName," does not exist in the shapefile data."))
      } else {
         # column present
         cat("Have good column name - trim, make character and store in column and X__Link.\n")
         shpLinkData <- stringr::str_trim(as.character(WorkSp01@data[,ShapeLinkName]))
         WorkSp01@data[,ShapeLinkName] <- shpLinkData   # put it back into the source column, cleaned up.
         WorkSp01@data[,"X__Link"]     <- shpLinkData
      }
      if(StopFlag) stop("***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry.")
          
      if (bitwAnd(debug,8) != 0){
         grDevices::pdf("BBG-Raw Shape File Image",width=10,height=7)
         sp::plot(WorkSp01)
         graphics::title("Shape File - Original data")
         dev.off()
      }  # now done via Sample Prts
      
      #  fill empty proj4string in SPDF
      
      #  The SPDF is in WorkSp01  is RAW no modifications.
      #
      #
      cat("End of section 3.2 - 3321\n")
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
      
      cat("Name Table Link is:",NameTableLink,"\n")
      
      ##### 05xx and 06xx
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
      #    column name in the Shape @data data.frame.  This link is 
      #    also used to group the multiple polygons of an area
      #    under one "polygons" structure in the SPDF (unionSpatialPolygons).
      #
      #    - a character string used to link the Name Table rows
      #    to the "polygon" entries for the area in the 
      #    SpatialPolygonsDataFrame (SPDF) created from the shape file 
      #    and simplified and generalized.  The "ShapeLinkName" call 
      #    parameter provides the name of a column in the @data 
      #    data.frame created when the shape file was read into 
      #    the SpatialPolygonsDataFrame.  The "Link" values in the 
      #    Name Table in each row (representing an area) must match 
      #    the values in the SPDF @data$<ShapeLinkName> field.  
      #    Since there may be multiple polygons in the SPDF for 
      #    each area, the important attribute is there are the same 
      #    number of rows in the Name Table as there are unique strings 
      #    in the shape file SPDF identified in the 
      #    @data$<ShapeLinkName> column.
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
      #  collection of polygons in the SPDF and it's row.name.  
      #  If a "Key" value is provided by the caller in the .csv
      #  Name Table, it is used as the "Key" value for the Name Table
      #  and the Shape file and Visborder data (later).
      #  If no is provided, the "Abbr" value is used, followed by 
      #  the "ID" value, "Alt_Abbr", and finally the "Name" column. 
      #  The row.names ("Key") of the Name Table will be used to set
      #  the "Key" values in all table and the border data for lookups.
      #
      #  It is recommended the NameTable have values for each type 
      #  of reference to an area (Name, Abbrev, ID, Link.) If any column 
      #  is missing, the function will attempt to backfill (use) 
      #  information from other columns to create a unique value for 
      #  the field. The most used references columns are the "ID" 
      #  and the "Abbr" columns.
      #
      #  Process of backfilling column values when not present:
      #     The process of backfilling missing location identifier 
      #     columns with the "Abbr" column. If it is not present, 
      #     the function looks to see if any of there columns are 
      #     present and uses their value as the "Name", "Abbr", 
      #     "Alt_Abbr", "Alias", "ID", 
      #
      #  If the Shape file contains more areas then listed 
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
      #  The following fields are required in the name table and 
      #  do provide additional functions.  But for now, set them 
      #  as follows:
      #
      #  L2_ID - numerical ID of logical (Layer 2) groupings of areas within the 
      #    L2 boundary group.  This grouping layer resides between the areas
      #    and the total geographic space of the L2 boundary group.
      #    It is recommended this field be set to an ID that represents the 
      #    L2 boundary group.  In the cases of US states, its should be the 
      #    US States FIPS code value.  The boundaries of member areas are 
      #    combined to create a single boundary for each L2 group.  It is 
      #    used to provide a denser boundary line around the areas in an L2 
      #    space. If not provided, this field will be set to the areas ID value.
      #
      #  L2_ID_Name - character string representing the L2_ID numerical ID.
      #    For US States, this can be a states name or abbreviation.
      #    If not provided, this field will be set to the areas Name value.
      #
      #  regID - numeric ID of the geographic region the area belongs to.
      #    In the US, states can belong to one of 4 census regions.  Each area
      #    can belong to only one region, but must belong to a region, identified 
      #    by regID and regName. The micromapST package will only draw areas 
      #    in regions containing data. This allows a border group to be built and 
      #    used for large area like the UK and Ireland. For US states, if data is 
      #    only presented for states in the north-eastern region, then only the 
      #    north-eastern region will be drawn and the south, Midwest and western 
      #    regions will omitted.  This lets micromapST draw a large map with less
      #    areas. If not provided, this value will be set to the AreaID value.
      # 
      #  regName - character string of the name of the region identified by 
      #    regID. If the regID is not provided, this field will be set to the 
      #    area Name value.
      #
      #  MapLabel - for each area, MapLabel provides a vector of three values: 
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
      #    MapLabel column is being retired.
      #
      #  The following optional columns are supported to help manipulate 
      #  the boundary data for an area. The offset moves are applied after 
      #  the transformation of the original SPDF, so its units should 
      #  be in meters.
      #
      #     Xoffset - (-) moves X values to the left and 
      #               (+) to the right 
      #     Yoffset - (-) moves Y values to down and (+) up
      #     Scale   - scaling in percentage (10 to 200%) done around 
      #               the centroid of the area. For example, scaling Alaska 
      #               down by 50% and moving the state from the NW of 
      #               the continental US to just below California to make 
      #               the overall map more compact.
      #
      #  Once the name table is constructed, a list of its contents is 
      #  printed for the callers reference and documentation on the 
      #  Name Table included in the border group. The name table is 
      #  saved in the border group directory as the areaNamesAbbrsIDs 
      #  data.frame.  The caller can use this table to restart 
      #  border group construction or open it and make modifications.
      #  When the final border group data set is pulled together 
      #  (all 5 data.frames), the Name Table is included as one of 
      #  the data.frames.
      #
      #  When inspected, the following columns were added to complete 
      #  the NameTable:
      #      Key, Link, MapLabel.
      #  The "Key" is always set to the content of the Abbrev field.  
      #  If the "Abbr" field is empty, the "Name" field will be used. 
      #  If the "Name" field is empty, the "ID" field will be used.  
      #
      #  The "Link" field is set to values that allow the function 
      #  to associate each Name Table row with an area in the Shape file.
      #
      #  The "MapL", "MapX", and "MapY" ("MapLabel) is a custom reserved 
      #  field. It is used in rare cases, when a label is required for 
      #  a moved area. It is only drawn on the first micromap in the graphic.
      #
      #     The MapLabel c(L,X,Y) triplet is broken up into three columns in 
      #     the name table:  MapL, MapX, and MapY to make it easier to handle later.
      #
      #  A good tools for editing and building the NameTable prior to running 
      #  this function is a spreadsheet. The finished worksheet representing
      #  the NameTable must be the first worksheet in the spreadsheet, the 
      #  first row must be the accepted column names, and no rows should be
      #  blank (no data and does not represent an area.) The
      #  spreadsheet can be saved as an .CS, .xls, or xlsx file.
      #
      #  The Name Table also includes a few special columns used in the construction of the 
      #  Border Group but are not used in micromapST.  There fields provide information to 
      #  shift the location of an area and scale the area.  In the U. S. these parameters 
      #  were needed to handle Alaska, Hawaii, District of Columbia and Puerto Rico.  
      #  These columns will not be added to the Name Table, but if they are present, 
      #  all four will be created and validated for use to modify the polygons 
      #  of the associated area.
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
      #       The default is NA.  The valid range is from 5 to 300.
      #
      #   Rotate - The number of degrees to rotate an area, from -360 to 360.  The default is 0.
      #       The rotation is done around the centroid of the area.  After rotating, the area may be 
      #       scaled or moved.
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
      #  Required Columns in csv / xlsw file  (NameTable)
      #
      
      ##### 050x
      #
      #  Variable Setup
      #
      #cat("Name Table Variable Setup - 3642 \n")
      UserLinkCol <- c(NameTableLink)           # columns that must be present
      LinkCol   <- c("Link")
      OneCol    <- c("Abbr","Name","ID")        # columns that at least one must be present
      OptCol1   <- c("Alt_Abbr")                # extension to the OneCol list.
      OptCol3   <- c("Alias")
      OptCol2   <- c("L2_ID","L2_ID_Name","regID","regName")  # optional columns to be added.  L2 and Regional Info.
      ManCol    <- c("Xoffset","Yoffset","Scale","Rotate")
      #ProjCol   <- c("Proj","Proj4")                    # optional columns used for adjustments.
      ProjCol    <- NULL
      LabCol    <- c("MapLabel")
      Lab2Col   <- c("MapL","MapX","MapY")
      TotCol    <- unique(c("Key", OneCol, OptCol1, OptCol2, OptCol3, ManCol, ProjCol, LabCol, LinkCol, UserLinkCol, Lab2Col, "DoAdj"))  # combined list of all columns kept in the NameTable.
      
      MustCol   <- unique(c(LinkCol,OneCol,OptCol1))     # Link, Name, Abbr, ID columns  Plus
      KeyCol    <- unique(c(LinkCol,"Abbr","Alt_Abbr","ID","Name","Alias"))   # possible fields for the Key and in order of selection.
      AltCol    <- c("Link","Name","Abbr","ID","Alt_Abbr","Alias","L2_ID_Name", "regID","regName",
                     "Xoffset","Yoffset","Scale","Rotate","Proj","MapLabel","MapL","MapX","MapY",
                     "Key")
      AltColCaps <- stringr::str_to_upper(AltCol)  # get list of column names in upper case.
      
      StopFlag  <- FALSE
      ErrorFlag <- FALSE
      
      #cat("List of all columns keep in name table:\n")
      #print(TotCol)
      #cat("List of all columns that may be used for the KEY link to the shapefile:\n")
      #print(KeyCol)
        
      ######  051x
      #
      # Part 5.1 - read nametable .csv and validate   (or excel spreadsheet)
      #     The type of the NameTable file is determined in the call parameter check.
      #
       
      # select correct code to read name table
      cat("The Name Table was read from:\n",
          "   ",NameTableType," ",NameTablePath,"\n")
        
      NTable <- NULL
      NTable <- switch(NameTableType,
                      utils::read.csv(NameTablePath),    # .csv
                      readxl::read_xls(NameTablePath),    # .xls    
                      readxl::read_xlsx(NameTablePath),   # .xlsm 
                      load(NameTablePath)         # .rda
                     )
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
               if (!is.data.frame(NTable)) {
                  StopFlag <- stopCntMsg("***3512 The NameTable in the .rda file is not a data.frame. Please correct and retry.")
               }
               # We have good data.frame to use as the NameTable
            } else {
               StopFlag <- stopCntMsg(paste0("***3514 There are more than one data.frame in the .rda file provided for the NameTable. Provide only one data.frame table in the .rda and retry."))
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
      
      if (StopFlag) {
          stop("***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry.", call.=FALSE)
         
      }
      StopFlag = FALSE
      #cat("End of Name Table Read - 3721 \n")
     
      ##### 052x
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
         cat("Updated Name Table column names:  Code: 3747 \n")
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
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stop(xmsg)
      }
      
      #
      #  Must haves:   "Link"  (or one provided by user) and 
      #      at least one "Location Identifier" - "Name", "Abbr", "ID"
      #      Optional ties to data are "Alt-Abr" should only be used 
      #      if "Abbr" is filled and an alternate is needed.
      #      "Alias" is a special identifier to be used as needed.
      #
      
      ##### 053x
      #
      #  Part 5.3 is the Link column still in the table?  (required columns)
      #
      # check "link" column name (provided by user or default)
      #cat("Part 5.3 - 3781 \n")
      
      NTable03    <- NTable
      
      NTableLink  <- rep(NA,dim(NTable)[1]) 
      # build empty Link table column.
      #cat("Empty NTlink Table = length:",length(NTableLink),"\n")
      
      xm          <- any(NameTableLink == NTNames)     # is the name in the list.  def="Link"
      if (!xm) {  # error link not present.
         xmsg        <- paste0("***3532 The column specified in the NameTableLink calling parameter does not exist in the loaded Name Table.\n")
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
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag," \n")
      ###### 054x 
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
        
         xmsg <- paste0("***3542 At least one of the following columns must be present in the NameTable file:", paste0(OneCol,collapse=", "),"\n")
         warning(xmsg, call.=FALSE)
         xmsg2 <- "  Please correct the spreadsheet and try again."
         stop(xmsg2,call.=FALSE)
         StopFlag <- TRUE
      }
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
      
      ###### 055x
      #
      #  Part 5.5 - Remove any columns not in our acceptable list  (can delete the user named link - NOW!
      #
      #cat("Part 5.5 - 3836 \n")
      
      NTable04 <- NTable
      
      xmM          <- TotCol %in% NTNames          # identify good columns
      KeepList     <- TotCol[xmM]
      DelList      <- NTNames[!(NTNames %in% TotCol)]
      if (length(DelList) > 0 ) {
         cat("***3550 The following columns are not needed and will be deleted from the Name Table:\n")
         print(DelList)
         NTable    <- NTable[,KeepList]           # Clean up what we cant handle
      } 
      
      NTNames      <- names(NTable)
       
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
     
      ###### 056x
      #
      #         Columns that should have values and no duplicates.
      #
      #  Part 5.6  - Clean up the data in each column.  Included Link column
      #            Check for NA or "" Values
      #            Check for duplicate Values
      #
      #cat("Part 5.6 - 3861 \n")
      
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
            xmsg       <- paste0("***3562 The ",inx," column contains duplicate entries. Correct and retry.\n")
            StopFlag <- stopCntMsg(xmsg)
         }
         
         if (any(is.na(Wrk)) | any("" == Wrk)) {
            xmsg       <- paste0("***3564 The ",inx," column contains NA or blank values that are not allowed.\n")
            StopFlag <- stopCntMsg(xmsg)
         }
      }
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
      
      if (StopFlag) {
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stop(xmsg)
      }
      #
      ######
      
      ######
      #
      #  Now work on cleaning up the contents of the columns and back fill 
      #  important columns 
      # 
      #
      NTable05   <- NTable
      
      ###### 057x
      #
      #  Part 5.7 - Backfill to create "Abbr" -> "Name" ->  "ID"  (Location IDs)
      #
      
      #cat("Part 5.7 - Name Table Backfill:  If Abbr is missing, can use Name, ID, Alt-Abbr or Alias VALUES.\n")
      
      ######  055x & 057x
      #
      #  Part 5.7.1 - Abbr - field (# 1)
      #
      xm1     <-  any("Abbr" == NTNames)
      if (!xm1) {
         warning("***3571 The Addr column in the Name Table is not included. Will attempt to backfill it from other information.",call.=FALSE)
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
         	             xmsg <- paste0("***3558 None of the columns needed are present.  \n",
         	                     "      The Link and one of the Name, Abbr, and ID column should have been there.\n",
         	                     "      This should never happen withthe previous checks.\n")
         	             stop(xmsg,call.=FALSE)
         	          }
         	       }
         	    }
            }
         }
      } else {
        # Abbr field is present - no backfill needed.
        if (bitwAnd(debug,32) != 0) 
           cat("***3573 The Name Table Abbr field is persent - no backfill required.\n")
      } 
      #   Add check for length of abbreviation
      
      xAbbrMax <- max(nchar(NTable$Abbr,keepNA=TRUE))   # get maximum length of Abbr values
      if (xAbbrMax > 6) {
         xmsg <- paste0("***3574 Some of the Name Table Abbr values are longer than 6 characters.\n", 
                        "        It is recommended the Abbr values be keep short.\n")
         warning(xmsg, call.=FALSE)
      }
      
      #  Abbr field is now setup no matter what.
      
      ######  057x
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
         	                     "The Link and one of the Name, Abbr, and ID column should have been there.\n",
         	                     "This should never happen withthe previous checks.\n")
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
        
      ###### 057x
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
      
      ###### 057x
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
      
      ###### 058x
      #
      #  Part 5.8 ID column
      #
      cat("***3582 Checking ID column in the name table to make sure the values\n",
          "***3582 are numeric with leading zeros.\n")
      
      xm1    <- any("ID" == NTNames) 
      if (!xm1) {
         # ID field is not present.  Create the ID column using 
         #  a numerical sequence 1 to n.
         #  Since it must be a numeric field, must fill it in with a 
         #  sequence of numbers.
         xmsg  <- paste0("***3584 The ID column is not present.",
                  " A numerical sequence number has been used to",
                  " fill the column.")
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
            # get row.names for the ID's with problems.
            BadList   <- NTable$Name[xmna]     
            ErrorFlag <- 
               errCntMsg(paste0("***3586 The ID data column is not all",
                   " numeric values. Values will be assigned."))
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
      
      ##### 059x
      #
      #  Part 5.9 - validate and fill in L2_ID and L2_ID_Name 
      #
      #  L2_ID  variable (level 2 boundaries)
      # 
      #cat("Clean up L2_ID and L2_ID_Name columns if needed.\n")
      #cat("Part 5.9 - L2_ID and Name.\n")
      
      xm2          <- any("L2_ID" == NTNames)
      if (!xm2) {
         # no L2_ID present   
         
         xm3       <- any("L2_ID_Name" == NTNames)
         if (!xm3) {
            #  L2_ID not present and L2_ID_Name is not present 
            #  Create entries one area per L2 area.
            NTable$L2_ID      <- seq(from=1,to=NumNTable)
            NTable$L2_ID_Name <- paste0("L2_",NTable$L2_ID)
            #
         } else {
            # L2_ID is not present, but L2_ID_Name is.
            # get unique list.
            uL2_ID_Name       <- 
                unique(sort(stringr::str_trim(NTable$L2_ID_Name)))
            # get an ID value
            tempL2_ID         <- match(NTable$L2_ID_Name,uL2_ID_Name)
            # create L2_ID
            NTable$L2_ID      <- tempL2_ID                              
            # use L2_ID_Name for any pattern and repeats. Then copy 
            # that numerically into L2_ID
         }
      } else {
         # L2_ID is present, but no L2_ID_Name column
         NTable$L2_ID  <- stringr::str_trim(NTable$L2_ID)
         xm3   <- any("L2_ID_Name" == NTNames)
         if (!xm3) {
            # no L2_ID_name   
            NTable$L2_ID_Name <- paste0("L2_",NTable$L2_ID)
         } else {
            # Both exist - leave them along.
            NTable$L2_ID_Name <- stringr::str_trim(NTable$L2_ID_Name)
         } 
      }
       
      #  Reset NameTable name lists - could have added one.
      NTNames <- names(NTable)  # refresh the name list

      ###### 059x
      #
      #   Part 5.9 - Validate RegID and RegName
      #
      #cat("Clean up regID and regName columns.\n")
      
      xm7 <- any("regID" == NTNames)
      if (!xm7) {
         # no regID present
         xm8 <- any("regName" == NTNames)
         if (!xm8) {
            # no regName present  - one region per area.
            NTable$regID   <- seq(from=1, to=NumNTable)
            NTable$regName <- paste0("RegN",NTable$regID)
         } else {
            # no regID present but the regName column is.
            uRegName          <- 
                      unique(sort(stringr::str_trim(NTable$regName)))
            tempRegID         <- match(NTable$regName, uRegName)
            NTable$regID      <- tempRegID
            # use regName to set the pattern for the regions
         }
      } else {
         # regID are present.
         NTable$regID <- stringr::str_trim(NTable$regID)
         xm7 <- any("regName" == NTNames)
         if (!xm7) {
            # but no regName column
            NTable$regName <- paste0("RegN",NTable$regID)
         } else {
            # both columns are present - nothing to do..
            NTable$regName <- stringr::str_trim(NTable$regName)
         }
      }   
      
      #  Reset NameTable name lists - could have added one.
      NTNames <- names(NTable)  # refresh the name list
      #print(NTable[,c("L2_ID","L2_ID_Name","regID","regName")])
      #
      #####
      
      #####
      #
      #  Do we activate the regional feature?
      #
      RegionFeature = FALSE
      #
      if (any("regID" == NTNames)) {
         # we have a regID column.  Are they all the same?
         x <- unique(NTable$regID)
         if (length(x) > 1)  RegionFeature=TRUE
      }
      #
      #####
      
      #####
      #
      #  Assorted columns
      #
      NTable06 <- NTable
            
      ##### 061x   #####
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
         cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag," \n")
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
              "  DoUserProj4:",DoUserProj4,
              "  DoModproj4:",DoModproj4,
              "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
      #
      #####
      
         
      ##### 062x
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
      NTable07 <- NTable
      
      ManCol    <- c("Xoffset","Yoffset","Scale","Rotate")
      
      WorkSp01BBox  <- sp::bbox(WorkSp01)
      if (diff(WorkSp01BBox[1,]) <= 360 ) {
         #  best guess is it's longlat.
         ModCols <- data.frame(n=ManCol, 
              low = c(-360, -360,0.01,-360), 
              high=c(360,360,5,360), 
              def=c(0,0,1,0))
      } else {
         # Otherwise it's meters, km, etc.
         ModCols <- data.frame(n=ManCol, 
              low = c(-24000000, -24000000,0.01,-360), 
              high=c(24000000,24000000,5,360), 
              def=c(0,0,1,0))
      } 
      row.names(ModCols) <- ModCols$n
      
      NTable     <- as.data.frame(NTable,stringsAsFactors=FALSE)
      NTNames    <- names(NTable)
      
      # As a coding safety measure, if a modification column does 
      # not exist, one will be created and filled with NA.
      
      xm         <- !(ManCol %in% NTNames)
      
      NTable[,ManCol[xm]] <- NA
      xm         <- !xm
      
      # list of columns to check-The others were just added.
      WorkColList <- ManCol[xm]     
      
      # Now every modification column is present in the Name Table.
      #
      # The modification columns, if present - can be NA, 0 
      # (except scale=1), or "" to indicate no modification. 
      # They all must be numeric if they contain a value.
      # .
      # Not all of the rows are checked. 
      #cat("Name Table - Modifier Columns to validate:",
      #   paste0(ModCols$n,collapse=", ",sep=""),"\n")
      
      Def_ModVal     <- rep(FALSE,dim(NTable)[1])
      # One per row.  Each valid operation forces a TRUE.
      NTable$DoAdj   <- FALSE    
      
      NTableRN       <- row.names(NTable)   # list of rows to check
      #1 get name of row to work on.  Cycle through rows.
      for (inxRN in NTableRN) { 
         AllDoMod  <- FALSE    # initially - no work to do for this row.
         # check to see if NTable row is set to the defaults 
         # - no need to check further.
     
         for (inx in WorkColList) { 
            #   Check each value for row for this modifier.
            DoMod       <- FALSE            
            ModDef      <- ModCols[inx,"def"]
            
            WrkVal <- NTable[inxRN,inx]  # a value to inspect.
            if (is.na(WrkVal)) next      # NA - next value
            if (WrkVal == 0)   next      # 0  - next value
            if (WrkVal == "")  next      # "" - next value
            if (WrkVal == ModDef)  next  # default - next value
        
            #cat("Checking Name Table Manipulation Row-Column:",inxRN,
            #    " - ",inx,"\n")
            # cycle through each value for each row
            
            # set up for the specific test for this column
            LLim        <- ModCols[inx,"low"]    
            HLim        <- ModCols[inx,"high"]
                  
            # A row may have NA, 0, <value>, junk included per 
            # parameter. We can do something with the <value>, 
            # but the rest dont work.
               
            #cat("Mod Table:",inx," value test-Low:",LLim,", High:",HLim,
            #    ", def=",ModDef,".\n")
            # it is not one of the no action values, check it for real.
            
            WrkVal      <- stringr::str_trim(WrkVal)  # Clean up value
            WrkVal2     <- as.numeric(WrkVal)
            #cat("inxRN:",inxRN,"  inx:",inx,"  WrkVal:",WrkVal2,"\n")
 
            if (is.na(WrkVal2)) { #3 it's not a numeric value.
               xmsg <- paste0("***3592 The Name Table in the ",inxRN,
                            " area row and in the ",inx,"column\n",
                         "***3622 is not numeric and has a bad value of: ",WrkVal,
                            " Value set to zero. Fix and retry.")
               ErrorFlag <- errCntMsg(xmsg)
               WrkVal2 <- 0
            } else { #3
               # good numeric - now check range.
               # Range Check.
               if (WrkVal2 > HLim || WrkVal2 < LLim) { #4
                  DoMod <- FALSE
                  xmsg     <- 
                     paste0("***3594 Data in row:",inxRN,
                            " for ",inx," parameter ",WrkVal2,
                            " is out of range. (",LLim," to ",HLim,")")
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
      NTNames  <- names(NTable) ###  Line Code: 4396 - Point a
      
      #
      #####
      
      if (bitwAnd(debug,4) != 0) {
         cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
             "  DoUserProj4:",DoUserProj4,
             "  DoModproj4:",DoModproj4,
             "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
      if (StopFlag) {
         xmsg <- "***3999 Errors have been found and noted.  Execution must be stop. Fix problems and retry."
         stopCntMsg(xmsg)
         stop()
      }
      #
      #####
       
      NTable08 <- NTable
      cat("Save NTable into NTable08 \n")
      
      ##### 363x
      #
      #  Part 6.3 - MapLabel validation - two formats
      #      MapLabel=c(AK,1,2)   or MapL="AK", MapX=1, MapY=2
      #
      #  Check for the MapL,MapX,and MapY format and columns first.
      #
      
      ###### 363x
      #
      #  Part 6.3.1 - MapL, MapX, and MapY Name Table columns
      #
      #  If MapL is present with valid MapX and MapY, ignore 
      #  MapLabel column - retired.
      #
      #  Setup up for MapL, MapX, MapY, conversion of MapLabel 
      #  and then validation
      #
      MapLData <- FALSE
      xMapL    <- NA
      xMapX    <- NA
      xMapY    <- NA
      
      #
      #  Do we have MapL columns?
      #
      im8      <- any("MapL" == NTNames)        # is MapL column present
      
      if (im8) {
         # MapL present, validate MapL, MapX, and MapY.  
         # If good, ignore MapLabel.
         # remove head and tail white space.
         MapL    <- stringr::str_trim(MapL)       
         if (all(is.na(MapL)) | MapL=="") {
            # MapL column is empty  - don't have to check MapX and MapY
            # - go check for MapLabel.
         } else {
            # Validate the MapL
            xm    <- !is.character(MapL)
            xmMap <- !xm   # the list of rows with MapL labels.
            if (xm) {
               # error-MapL is not a character string.
               xmsg <- 
                  paste0("***3630 A value in MapL is not a character",
                               " variable in the following rows:\n",
                    "***3630    ",paste0(NTable$Name[xm],collapse=", ",sep=""),
                         "\n",
                    "***3630    Please correct and rerun.\n")
               ErrorFlag <- errCntMsg(xmsg)
            } else {
               im9 <- (any("MapX" == NTNames) && any("MapY" == NTNames) ) 
               if (!im9) {
                  # one of the companion columns is missing.
                  xmsg <- paste0("***3631 If the MapL column is present with",
                           " a label, then MapX and MapY must be present.",
                           " Correct and rerun,\n")
                  ErrorFlag <- errCntMsg(xmsg)
                  NTable$MapX  <- NA
                  NTable$MapY  <- NA
               } else {
                  # have both needed x,y coordinates columns
                  NTable$MapX <- as.numeric(NTable$MapX)
                  NTable$MapY <- as.numeric(NTable$MapY)
                  if (all(is.na(NTable$MapX))) {
                     # the MapX values are all missing
                     xmsg <- paste0("***3632 MapL is present and there are no",
                          " MapX coordinates. Correct and rerun.")
                     ErrorFlag <- errCntMsg(xmsg)
                  } else {
                     # we have MapX
                     if (all(is.na(NTable$MapY))) {
                        # The MapY values are all missing
                        xmsg <- paste0("***3633 MapL is present and",
                               " there are no MapY coordinates. Correct and rerun.")
                        ErrorFlag <- errCntMsg(xmsg)
                     } else {
                        # step through the values for final validation
                        # xmMap has TRUE set for each row with MapL
                        MapList <- seq(1,length(NTable$MapL))[xmMap]
                        #MapList
                        
                        for (inx in MapList) { 
                           # Get the row index and check it out
                           # label for error messages
                           NTN <- NTable[inx,"Name"]   
                           if (nchar(NTable[inx,"MapL"]) > 3) {
                              # too long a label
                              xmsg <- 
                                 paste0("***3634 The MapL label for ",
                                   NTN," should be 3 or less characters.",
                                   "  Please correct and rerun.")
                              ErrorFlag <- errCntMsg(xmsg)
                           }
                           # build table of just the MapL labels.
                           MapLData <- TRUE
                        }
                        # it is all stored in the Name Table - validate 
                        # later.
                     }
                  }
               }
            }
         }
      }
      #
      #  End of MapL processing
      #  Have information in NTable data.frame
      #
      #####
    
      ##### 063x
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
         # No MapL, MapX, MapY columns present.  Thus no MapLData 
         # processed.  Can have one or the other, but not both.
           
         # is the MapLabel column present?  
         xm8 <-  any("MapLabel" == NTNames)  
     
         NTable$MapL     <- NA
         NTable$MapX     <- NA
         NTable$MapY     <- NA
                   
         if (!xm8) {
            # no MapLabel field (column)  create the column and 
            # fill with NA
            
            #cat("no MapLabel, fill with NA\n")
            # use name field or backfill
            NTable$MapLabel <- NA   
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
                 
                  if (is.vector(WrkValV) && length(WrkValV)==3 ) {   
	             # Yes, value must be a vector of three items
	             # Check each out the three values, none can be 
	             # blank or NA
	             WrkValV <- stringr::str_trim(WrkValV)
	             if (any(is.na(WrkValV)) || any(WrkValV == "")) {
	                # Empty entry one of the values is blank or NA
	                xmsg <- paste0("***3636 Some of the items in",
	                      " the MapLabel entry for ",inxRN,
	                      " are NA or blanks. Will be ignored.")
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
	                 "***3637 The MapLabel value for ",inxRN,
	                     " is not valid.\n",
	                 "***3637 Must be a character string with",
	                     " three values separated by commas.\n",
	                 "***3637 The value is ignored.\n")
	             ErrorFlag <- errCntMsg(xmsg)               
	          }
	       }  # end of for loop
	    }
         } 
         NTNames <- names(NTable)  # refresh the name list
      }   
      # Finished with MapLabel and MapL, MapX, and MapY initial 
      #   processing and validation.
      #
      #####
      
      ##### 063x
      #
      #  Part 6.3.3 - data converted into MapL/MapX/MapY format.
      #    Now validate the data format and ranges.
      #
      #  If we found any information under MapLabel or MapL, 
      #  then we need to validate it NOW
      #
            
      if (MapLData) {   
         # we have MapLabel data, now is the time to validate 
         # the MapLabel or MapL/MapX/MapY data.
         
         # find all rows with MapL information
         xmMap    <- !is.na(NTable$MapL)          

         # get the access names (abbr) for entries
         MapRN    <- row.names(NTable)
         MapRN    <- MapRN[xmMap]        
         #  multiplier
         MapMul   <- matrix(c(.66,.66,1.5,1.5),ncol=2)
         # increase the values.
         MapBox2  <- MapBox * MapMul   
                 
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
	       xmsg      <- paste0("***3638 The label value in the ",
	                      "MapLabel entry for ",inxRN," is > 3 char.",
	                      "  Only first 2 characters used.")
	       ErrorFlag <- errCntMsg(xmsg)
	       xlen      <- nchar(xMapL)
	       if (xlen > 3) xlen = 3
	       xMapL     <- stringr::str_sub(xMapL,1,xlen)
	    }
	    if ( is.na(xMapX) || is.na(xMapY) || (xMapX=="") || (xMapY=="") ) {
         	    xmsg <- paste0("***3639 One of the coordinates ",
         	              "in the MapLabel entry for ",inxRN,
         	              " is not a number. ",xMapX," or ",xMapY)
         	    ErrorFlag <- errCntMsg(xmsg)
         	    next
            }
            # Check Range of X,Y coordinates.
            
            if (xMapX < MapBox2[1,1] || xMapX > MapBox2[1,2] || 
                    xMapY < MapBox2[2,1] || xMapY > MapBox2[2,2] ) {               
	       # out or range - any of the TRUE - out of range              
	       xmsg <- paste0("***3640 One of the MapLabel coordinates for ",
	                      inxRN," are out of range. Entry ignored.")               
	       ErrorFlag <- errCntMsg(xmsg)               
	    } else {               
	       # Every thing is fine with this entry.
	       NTable[inxRN,"MapL"] <- xMapL    # put back in Name Table               
	       NTable[inxRN,"MapX"] <- xMapX               
	       NTable[inxRN,"MapY"] <- xMapY               
	       MapLData <- TRUE               
	    } 
         }
      }
      NTNames <- names(NTable)  # refresh the name list
         
      if (MapLData) {
         
         #### Build Spatial Point from MapL, MapX, MapY. 
	 xMapL  <- NTable$MapL
	 xMapX  <- NTable$MapX
	 xMapY  <- NTable$MapY
	 xMapRn <- row.names(NTable)
	 xmMap  <- !( is.na(xMapL) | xMapL == "" )
	 
	 if (any(xmMap)) {
	    # We have MapL entries
	    xMRn   <- xMapRn[xmMap]
 	    MapDF <- data.frame(R=xMRn,L=xMapL[xmMap],
 	                        X=xMapX[xmMap], Y=xMapY[xmMap], 
 	                        row.names=xMRn)
	 }
	 MapSP   <- sp::SpatialPoints(coords=MapDF[,c("X","Y")],
	                       proj4string=slot(WorkSp01,"proj4string"))
         MapSPDF <- sp::SpatialPointsDataFrame(MapSP,MapDF[,c("R","L")])
         #str(MapSP)
         #str(MapSPDF)
         
      }
      
      #
      #####
      #######
      #########
      #cat("End of Name Table Processing - 4767 \n")
      
      #  End of Name Table Processing and setup.
 
      if (bitwAnd(debug,4) != 0) 
                  cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
            
      #########
      #######
      #####
      #
      #  BuildBorderGroup_Part 7 <- Read and validate shapefile into SPDF
      #
      
      if (bitwAnd(debug,64) != 0) cat("ShapeFile Current Proj4 String:",WorkSp01Proj4,"\n")    #     String
           
      #####  
      #
      #  Later this code will be turned into a function
      #
      #  Part 7 - Read, process, simplify shapefile and get initial
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
      
      #     Note 1: Read ShapeFile - was moved to just behind
      #     the completion of the call parameter validation.
      #     This was done to have the ShapeFile @data available
      #     for the Name Table content validate and building the 
      #     "links" and "keys". 
      #  
      #     If the proj4string was empty, longlat was inserted.
      #     If one is present from the shapefile, it is checked to see if 
      #     it is a long/lat projection.
      #     If the projection is long/lat the ShpProjLL and DoBldAEAProj flags
      #       are set.  Otherwise it's not a long/lat projection and the flags
      #       are set to FALSE.
      #     WorkSp01Proj4 carries the slot(xxx, "proj4string")@projarg image.
      #
      
      ##### 371x  
      #
      #   Part 7.1 - Handle processing of Shape File Proj4, see if we will be 
      #              re-projecting or not.
      #
      #  If empty, set to generic long/lat projection. Check the proj4 to see if
      #  user set it to a long/lat or other projection and set flags.
      #  If user set proj4 to non-long/lat, we check for +units=m.  If not,
      #  a new projection is built to change it.
      #
      #  Processing based on proj4 in shape file.
      #
      
      ModProj4       <- NA          # none created at this time.
      DoModProjM     <- FALSE
      
      # WorkSp01Proj4 has already been retreived from the initial load 
      # of the shapefile and if empty back filled with a basic long/lat proj4 string. 
      # ShpProjLL - was set up when shapefile opened.  No proj4 - forced LL and set flag.
      #             projection present with long/lat - set flag
      #             projection present but not long/lat - reset flag.
      # If user proj4 parameter is Long/lat, it is ignore with error message.]
      #
      # Is proj4string in shapefile a longlat projection? 
      #cat("Process shape file's projectsion - 4844 \n")
      
      if (!ShpProjLL) {
         #cat("Check non-long/lat projection in shapefile to see if it's +units are meters.\n")
         
         # Is not longlat, need to check for +units=m or not?
         if (is.na(stringr::str_locate(WorkSp01Proj4,"\\+units=m |\\+units=m$"))[1]) {  
            # +units not set to meters.
            cat("***3711 The projection provided in the Shape File does not have +units=m, modify and setup for ",
                "re-projection to change to meters.\n")
            CurProj     <- WorkSp01Proj4    # get copy of projection string
            #cat("   Before CRS: ",CurProj,"\n")
         
            matchstr    <- "\\+units=[[:alpha:]]+"     # RegExp string to find +units=???.
            
            #  find and replace any "+units=<>" string with "+units=m " 
            ModProj4    <- stringr::str_replace(CurProj,matchstr,"\\+units=m ")   # find start and end and do replacement
            suppressWarnings(CRSModproj4 <- sp::CRS(ModProj4))            # rebuild CRS
            #cat("New Projection for ShapeFile -later:",ModProj4,"\n")
            
            DoModProj4 <- TRUE   
            
            # Should not do transform until after modifications....
            # Save the CRS and Proj4
            
         } else {
            #cat("***3712 Found +units=m in proj4string of non-longlat projection in the shape file.\n")
            # If ShapeFile projection is not Long/Lat and has +units=m, no projection later
            x <- 1
            
         } # end of adjustment for +units=
      }  # end of non-longlat checking.
      
      if (bitwAnd(debug,4) != 0) 
          cat("Proj Flags - ShpProjLL:",ShpProjLL,
                      "  DoUserProj4:",DoUserProj4,
                      "  DoModproj4:",DoModproj4,
                      "  DoBldAEAProj:",DoBldAEAProj,"\n")
    
      #
      #  When the proj4 call parameter is inspected and found to be good,
      #    DoUserProj4 is set to TRUE.  If the proj4 string does not contain
      #    +units=m, it is edited and changed.
      #
      #  If call parameter proj4 is a longlat protection, a warning is thrown
      #    and the parameter is ignored.  Having the final projection LL does not
      #    make any sense.
      #
      #  If the shapefile's projections is non-long/lat and +units=m, then ModProj4 will be
      #    set to NA and DoModProj4 to FALSE to correct the +units.
      #
      #  Since all adjustments are in native units, no transformation can be done until
      #    after the adjustments are made.
      #
      #  If shapefile default or original proj4 is longlat, then both ShpProjLL and 
      #    DoBldAEAProj are set to TRUE. 
      #
      #  If DoUserProj4 is set, then DoModProj4 and ModProj4 value will be ignored.
      #    They would be overriden by the user's proj4 request.
      #
               
      WorkSp02     <- WorkSp01        # advance the SPDF for the next phase of this process.
      WorkSp02Data <- WorkSp02@data   # save for later if need to restore.
      NTable09     <- NTable
      cat("Save NTable to NTable09 - 4908 \n")
      
      #
      #####
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag,
                                     " ErrorFlag:",ErrorFlag,"\n")
     
      #####  372x
      #
      #  Part 7.2 - BuildBorderGroup - inspect and clean the SPDF 
      #            polygon structure
      #
      #      Execute Cleangeo on the shapefile.
      #
      cat("CleanGeo Section - 4923 \n")
      
      if (bitwAnd(debug,16) != 0) {
             cat("Shape file - Cleaning up polygons: cleangeo, gIsValid,",
                 " checkPolygonsHoles\n")
           }
      CGreport  <- cleangeo::clgeo_CollectionReport(WorkSp02)
      CGsummary <- cleangeo::clgeo_SummaryReport(CGreport)
      
      if (bitwAnd(debug,16) != 0) print(CGsummary)
      
      ##
      #  Step 7.2b Clean up the SPDF
      #
      #cat("***3720 Cleaning up geometry in shape file using cleangeo inspect and correction.\n")
      # custom is clgeo_Clean2
      WorkSp03  <- clgeo_Clean2(WorkSp02,verbose=FALSE)    
      
      #cat("done --> on to clgeo_CollectionReport.\n")
      # Recheck to make sure problem solved.
      CGreport_clean <- cleangeo::clgeo_CollectionReport(WorkSp03)
      CGsummary2     <- cleangeo::clgeo_SummaryReport(CGreport_clean)
             
      if (bitwAnd(debug,8) != 0) {
         grDevices::pdf("BBG-Cleaned Shape File Image.",width=10, height=7)
         sp::plot(WorkSp03)
         graphics::title("Shape File after cleangeo")
         dev.off()
      }
      
      #
      #  WorkSp03 now has the SPDF
      #
      #####
      #######
      #########
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag,
                                       " ErrorFlag:",ErrorFlag,"\n")
      
      #########
      #######
      #####
      #
      #  We have the basic NTable and Shape file @data information
      #  Time to link the two tables. It will be a one to many link
      #  at this time.
      #
      ##### 
      #
      #  Step 7.4 - Get DATA and KEY from WorkSp03.
      #
      if (bitwAnd(debug,16) != 0) 
           cat("ShapeFile LINK:",ShapeLinkName,"  NameTable Link: Link \n")   
                
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
      #cat("Part 7.4 - 4987 - shape file LINK \n")
      ##### 374x
      #
      #  7.4 - Set Up Shape Link and Name Table Link for match:
      #
      #  Whatever the links are - numberic or character.  It's best to 
      #  pad all of the links to the same length with leading "0"s.  
      #  So trim, get max length and pad with "0". It works best 
      #  with numbers, and should not really impact character strings.
      #  Just don't modify any column but true "Link" columns.
      #  If a user specifies a "name" or "Abbr" column, copy it to 
      #  link first.  Same goes for the ShapeFile@data
      #
      # Build the matching strings from EACH link column.   
      #    (always paded to the left with "0" in case it's FIPS codes.)
      #
      #  The SPDF link name is valid, get the data.   (We are assuming 
      #    the link is a numeric and need leading 0 padding.)
      #  Pull data from user provided name (or default), place in 
      #    the X__Link column, and prep for the match.
      
      #####
      #
      #  Part 7.4 - Sort & Setup NameTable by "Name" field and link to Shape file.
      #
         
      NTable10     <- NTable
      # Name Table is re-ordered bsaed on "name" field.
      xst         <- order(NTable$Name)
      NTable      <- NTable[xst,]
      row.names(NTable) <- NTable$Key
                      
      NTable       <- as.data.frame(NTable,sringsAsFactors=FALSE)
      NTNames      <- names(NTable) # reset Name Table column names.
    
      #print("Name Table - Code: 5007 ")
      #print(NTable)
      #
      # WorkSp @ data DF
      WorkSp03Data <- WorkSp03@data
      #print("Shape file - WorkSp03")
      #print(WorkSp03@data)
      
      WorkSp03Link         <- stringr::str_trim(WorkSp03Data$X__Link)
      maxSp03              <- max(nchar(WorkSp03Link))
      #cat("WorkSp03 - Link, maxChar:",maxSp03,"\n")
      #print(WorkSp03Link)
 
      # Name Table link
      #  Move Name Table "link" coiumn info to NTable$Link column.
      #  The name table link is now in the correct spot.
      NTableLink           <- stringr::str_trim(NTable$Link)
      maxNT                <- max(nchar(NTableLink))
      #cat("NTable - Link, maxChar:",maxNT,"\n")
      #print(NTableLink)

      maxBoth              <- max(maxSp03,maxNT)
    
      #
      # Trim trailing and leading blanks, pad to left with "0" to make all of the strings
      #  the same length, and make all of the strings uppercase to maximize the possibility
      #  of a reasonable match.
      #
      WorkSp03LMatch       <- WorkSp03Link
      WorkSp03LMatch       <- stringr::str_pad(WorkSp03LMatch,maxBoth,side="left",pad="0")
      WorkSp03LMatch       <- stringr::str_to_upper(WorkSp03LMatch)
      WorkSp03@data$X__Link <- WorkSp03LMatch
      #print("WorkSp L Match")
      #print((WorkSp03LMatch))
      
      NTableLMatch         <- NTableLink
      NTableLMatch         <- stringr::str_pad(NTableLMatch,maxBoth,side="left",pad="0")
      NTableLMatch         <- stringr::str_to_upper(NTableLMatch)
      NTable$Link          <- NTableLMatch
      #cat("NameTable L Match \n")
      #print((NTableLMatch))
   
      #
      #####
      
      ##### 375x
      #
      #  7.5 - Compare ShapeFile links to the Name Table links
      #
      #cat("Part 7.5 - 5071 - compare Name Table and Shape File links \n")
      xm          <- match(WorkSp03LMatch, NTableLMatch) # check is all of the areas in the SPDF have name table rows?
      #print("xm index for matching SP to NT.")
      #print(xm)
      
      MS_SP_m     <- is.na(xm)      #  NA values for any ShapeFile Link that does not have a Name Table entry.
      if (bitwAnd(debug,16) != 0) 
         cat("Compare ShapeFile Link to Name Table Link values.\n")
      
      if (any(MS_SP_m)) {
         # have entries in ShapeFile that are not in NTable.
         
         if (bitwAnd(debug,16) != 0) {
            print(NTable)
            print(WorkSp03@data)
         
            print(MS_SP_m)
            print(WorkSp03Link[MS_SP_m])
         }
         
         #
         #  Delete extra areas or polygons that don't have a Name Table entry.
         #
         MS_SP_List <- WorkSp03Link[MS_SP_m]   # get list of Shape File areas not in Name Table.
         ErrorFlag  <- errCntMsg(paste0("***3752 The following Shape File areas are not in Name Table:\n",
                          "***3752 ",paste0(MS_SP_List, collapse=", ", sep=""),"\n",
                          "***3752 The areas will be dropped."))
         #
         ShpDelList     <- (WorkSp03Link %in% MS_SP_List)  # find which polygons get deleted. (could be multiple polygons)
         #cat("***3754 Deleting the following polygons from the shape file:\n")
         #cat("***3754 ",paste0(WorkSp03Link[ShpDelList],collapse=", ",sep=""),"\n")
         #
         #  Do the deletes from the WorkSp03 SPDF
         WorkSp03x      <- WorkSp03[!ShpDelList,]    # keep the rest.
         
         #cat("Delete - ",length(MS_SP_List)," polygons.  Before:",length(WorkSp03),"  After:",length(WorkSp03x),"\n")
         
         WorkSp03       <- WorkSp03x
         
         # Clean up the match list for deleted polygon/area.
         WorkSp03LMatch <- WorkSp03LMatch[!ShpDelList]  
         #
      }
      #
      #####
      
      NTable11 <- NTable
      
      ##### 376x
      #
      #   7.6 - Compare Name Table to the ShapeFile Link Values
      #
      
      #  WARNING: This logic may delete a polygon or get messed up on a name table row 
      #    due to a misspelling or typo.  That's why there are warning alerts for both checks.
      
      #  Compare --> 
      xm       <- match(NTableLMatch,WorkSp03LMatch)  # test Name table link against ShapeFile.
      cat("***3760 Comparing the link values to tie the Name Table to the shape file.\n")
      #print(NTableLMatch)
      #print(WorkSp03LMatch)
      
      MS_NT_m  <- is.na(xm)    # NA says Name Table has entry that is not in ShapeFile.
      #
      
      if (any(MS_NT_m)) {
         # Found entries in Name Table that have no polygons to map.
         if (bitwAnd(debug,16) != 0) cat("Compare Name Table Link values to ShapeFile Link Values.\n")
         
         MS_NT_List <- NTableLink[MS_NT_m]    # get list of name table entries.
         StopFlag <- stopCntMsg(paste0("***3762 The following Name Table areas do not have boundaries in the ShapeFile:\n ",
                                       "***3762  List of Abbr:",paste0(MS_NT_List,collapse=', ',sep=""),".\n",
                                       "***3762 Correct and retry."))
         # In this case stop, can't draw all of the areas of the map.
         
      } else {
         
         # At this point the number of areas in the Shape File (SPDF) and the Name Table
         # match and their values (links) match.  Both structures should have no duplicates.
         # the unionSpatialPolygons should have removed the SPDF duplicates and the 
         # Name Table was scanned and duplicates reported as errors.
         
         xmna <- is.na(xm)   # place holder
      }
      
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
      WorkSp03x <- WorkSp03   #  (RAW)
      
      vDebug <- debug 
      if (bitwAnd(debug,1024) != 0)  vDebug <- vDebug+512
      
      if (bitwAnd(vDebug,256+512) !=0) {
         # check point image
         if (ShpProjLL) {
            # we have a long/lat projections
            # check is possible East-West crossing problem
            bboxPP    <- sp::bbox(WorkSp03)     
            xDif      <- diff(bboxPP[1,])   # range of x (lat) degrees
            if (abs(xDif) > 180) {
               cat("Called FixIDLMain for WorkSp03\n")
               #####
               #
               # Main loop to process areas in the "other" hemisphere and adjustment
               #
               #  Replace temp image if modified.
               WorkSp03x <- FixIDLMain(WorkSp03, debug) 
         
            }# end of matrix box check 180 degrees
         }  # is shape file LL and need inspection for East/West hemisphere crossing.
         #
         # EWH - Proceeds
         # the people with hose and workers with faucet.
         # Printer works - validate you - please keep quite.
         #
         # Don't keep WorkSp03x - only for check point images.
         #
         # lattice print of series of small images.
         cat("SamplePrts - WorkSp03\n")
      
         PPSp         <- WorkSp03x
         PPTitle      <- "RAW"
         PPMfrow      <- c(3,3)
         #cat("length of link table:",length(NTable$Link),"\n")
         cat("Call SamplePrts - WorkSp03 image.\n")
         SamplePrts(PPSp,PPTitle,PPMfrow,debug,NTable$Link,NTable$Key,MapAvgH)
      }
      #
      
      if (bitwAnd(debug,4) != 0) {
         cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
             "  DoUserProj4:",DoUserProj4,
             "  DoModproj4:",DoModproj4,
             "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
      #
      #####
      #######
      #########
           
      #########
      #######
      #####
      #
      #####  378x
      #   
      #   Part 7.8 - BuildBorderGroup - simplify, generalize SPDF
      #
      #   WorkSp03 carries the boundary information for all areas.
      #
      cat("Simplifying the shape file boundary data with rmapshaper.\n")
      
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
      
      ##### 341x
      #
      if (bitwAnd(debug,64) != 0) 
          cat("Shape file before simplification by rmapshaper.\n")
      #
      MS_Keep      <- ReducePC/100   # convert from dec to percentage
      MS_Weighting <- 0.9
      #cat("Rmapshaper parameters before simplification : Keep=",MS_Keep,
      #    "  Weight=",MS_Weighting,"\n")
      #
      
      #SizeSp03 <- utils::object.size(WorkSp03)    # Size of structure before
      #cat("SPDF size before:",SizeSp03," (03)\n")
         
        
      if (bitwAnd(debug,64) != 0) {
         cat("SPDF data section:\n")
         print(WorkSp03@data)
      }
      #  rmapshaper call for ms_simplify   (affine - simplification)
      suppressWarnings(
      WorkSp04 <- rmapshaper::ms_simplify( WorkSp03,
                            keep        = MS_Keep,         # def = 0.02
                            method      = "vis",  
                            weighting   = MS_Weighting,    # def = 0.925
                            keep_shapes = TRUE, # OK to lose if not the last one in a group 
                            no_repair   = FALSE,# do repairs
                            snap        = TRUE, # corrrect vertixes
                            explode     = TRUE,
                            force_FC    = TRUE,
                            drop_null_geometries = TRUE,
                            snap_interval = NULL,
                            sys         = FALSE)
      )
      #SizeSp04  <- utils::object.size(WorkSp04)
      #cat("SPDF size after:",SizeSp04," (04)\n")
      
      # The rmapshaper in it's simplification, places two polygons very 
      # close to each other - almost touching.
      # The projection is LL for Alaska and the point is near Jueanu.
      # When this point is projected to AEA or other projections,
      # these two polygons intersect and "overlap" causing an 
      # invalid map.  (gIsValid = FALSE)  This impact all of the 
      # rest of the spatial operations once the map is actually 
      # transformed after the modifications and before the VisBorder
      # conversions.
      #
      
      #
      #   After simplification by rmapshaper
      #
     
      if (bitwAnd(debug,64) != 0) { 
         cat("ShapeFile@data after simplification:\n")
         print(WorkSp04@data)
      }
       if (bitwAnd(debug,8) != 0) {
         grDevices::pdf("BBG-Shape File after rmapshaper simpl.pdf",width=10, height=7)
         sp::plot(WorkSp04)
         graphics::title("Shape File after rmapshaper simplification.")
         dev.off()
      }
      
      #
      ######
      
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag,
                                     " ErrorFlag:",ErrorFlag,"\n")
      
      ###### 378x
      #
      #  Part 7.8.2 - COMBINE polygons under single area entries.  
      #        (unionSpatialPolygns)  (WorkSp04-> WorkSp05)
      #
      WorkSp04len     <- length(WorkSp04)
      WorkSp04Data    <- WorkSp04@data   # data.frame from shapefile
      # do merge on "Link" data which ties to Name Table
      LinkArea         <- as.character(WorkSp04Data$X__Link)   
      #cat("LinkList for polygons merge into areas:\n")
      #print(LinkArea)
      # place key data back in data DF - Vector of character strings
      WorkSp04Link     <- LinkArea
      #cat("Unique Polygons via $Link:",length(unique(LinkArea)),"\n")
      #cat("Length of WorkSp04:",length(WorkSp04),"\n")
      
      # combine  (upgrade)
      WorkSp05        <- maptools::unionSpatialPolygons(WorkSp04,LinkArea)  
      #cat("Length of WorkSp05:",length(WorkSp05),"\n")
      
      if (bitwAnd(debug,64) != 0) {
         cat("lengths of SPDFs - 02:",length(WorkSp02),
             "                   03:",length(WorkSp03),
             "                   04:",length(WorkSp04),
             "                   05:",length(WorkSp05),"\n")
         cat("Sizes of SPDFs   - 02:",utils::object.size(WorkSp02),
             "                   03:",utils::object.size(WorkSp03),
             "                   04:",utils::object.size(WorkSp04),
             "                   05:",utils::object.size(WorkSp05),"\n")
         cat("ShapeFile combination completed. ",WorkSp04len," now ",
                length(WorkSp05)," areas. Code: 5344 \n")
      }
      #
      #####
      
      WorkSp05a <- WorkSp05  # save image
      suppressWarnings(WorkSp05  <- rgeos::gBuffer(WorkSp05,width=0,byid=TRUE))
      
      # Fix for invalid geometry seen at this point that will
      # not survive a transform. Solution appears to be do a 
      # gBuffer with width = 0 and byid=TRUE.
      #
      # only one extra message from s2 routines.
        
      #
      #  At this point, the SpatialPolygonsDataFrame can no longer
      #  be transformed.  reason= self-intersection at or near point
      #  -2643559.7558800224, 2761086.9448261256  
      #                   due to the simplification & transform
      #  -1943728.0079966546 -1159032.9801981568
      # 
      #
      #pdf("c:/projects/statnet/r code/BuildBG/badpoint.pdf",
      #      width=10.5, height=8)
      #xy <- c(  -2643559.7558800224, 2761086.9448261256 )
      #sp::plot(xsp05,lwd=0.05)
      #par(new=TRUE)
      ##graphics::points(xy[1],xy[2],pch=19,col="blue",type="p",lwd=0.1)
      #grDevices::dev.off()
        
      ###### 378x
      #
      #   Part 7.8.3 - Finish union on the SpatialPolygon by rebuilding 
      #        @data section. After the merge(union) in Step 7.3.4, 
      #        the row.names of each polygon set will be the "LINK" value.
      #
      #   Link now is the Key (ABBR) the same value we did the union with.
      #   
      #      Match up the link values with the information in the old 
      #      @data df
      # GET LINK NAME ASSOCIATED WITH EACH POLYGON SET (row.names)
      # We are now at the point where there is only one link for each 
      # area in the SPDF (may be with multiple polygons inside.)
      # The Name Table already has a Key assigned to each entry and 
      # could be transfered to the SPDF at this time.
      #
      WorkSp05Link    <- row.names(WorkSp05)   # should be the Links.  
      
      #print(WorkSp05Link)
      #print(WorkSp04Link)
      
      # step a - look up Sp05 links in Sp04 to find @data rows.
      xm              <- match(WorkSp05Link,WorkSp04Link)   # match to old SPDF and get index to one old entry.
      # one copy of a single entry - no summing of numbers.
      Sp05Data        <- WorkSp04Data[xm,]
      
      # step b - look up Sp05Link in Name table and get Key value
      xm              <- match(WorkSp05Link,NTable$Link)  # match NTable.
      Sp05Data$X__Key <- NTable$Key[xm]
      #
       
      row.names(Sp05Data) <- Sp05Data$X__Link  # Needed Link for SPDF build.
      #print(Sp05Data)
     
      #      Put the DF back on the SP to get a new merged SPDF
      if (bitwAnd(debug,64) != 0) {
         cat("Match New ShapeFile RN with old ShapeFile Key.\n")
         cat("ShapeFile - build WorkSp06 SDPF full structure.\n")
      }
      
      WorkSp05b     <- WorkSp05
      WorkSp06      <- SpatialPolygonsDataFrame(WorkSp05,Sp05Data)  # remount the DF on the SP.
      # Once SPDF build, change row.names to the Keys.
      row.names(WorkSp06) <- WorkSp06@data$X__Key
      
      #cat("WorkSp06 - length:",length(WorkSp06),"\n")
      #str(WorkSp06@data)
      
      #  Shape file now in WorkSp06
      #
      #####
      
      StopFlag   <- FALSE
      ErrorFlag  <- FALSE
      
      if (bitwAnd(debug,4) !=0) {
         cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"\n")
         cat("Proj Flags - ShpProjLL:",ShpProjLL,
              "  DoUserProj4:",DoUserProj4,
              "  DoModproj4:",DoModproj4,
              "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
      if (bitwAnd(debug,8) !=0) {
         grDevices::pdf("BBG-Shape File after unionSpatialPolygons.pdf",width=10, height=7)
         sp::plot(WorkSp06)
         graphics::title("ShapeFile after unionSpatialPloygons to get the areas organized")
         dev.off()
      }   
      if (bitwAnd(debug,64) !=0) {
         cat("New ShapeFile@Data information: Code: 5443 \n")
         cat("WorkSp06@data:\n")
         print(WorkSp06@data)
       }
      #
      #####
      
      ##### 378x
      #
      #  Part 7.8.4 - check point plot of map after smoothing.
      #
      #  Call Parameters: SPDF, Legend Pos, Colors(T/F), Lattice DIM, Colors
      #
      
      WorkSp06x    <- WorkSp06
      
      #cat("Generate scaled example of test maps - after rmapshaper.\n")
      
      if (bitwAnd(debug,512) != 0) {
            # we have a long/lat projections  (only plot one map)
            # check for problems with E/W hemisphere crossing.
            bboxPP    <- sp::bbox(WorkSp06)   # check is possible East-West Crossing problem
            xDif      <- diff(bboxPP[1,])   # range of x (lat) degrees
            if (abs(xDif) > 180) {
      
               #####
               #
               # Main loop to process E/W crossing adjustment
               #
               WorkSp06x <- FixIDLMain(WorkSp06, debug)  #  Replace temp image if modified.
         
            }# end of matrix box check 180 degrees
         PPSp         <- WorkSp06x
         PPTitle      <- "After rmapshaper"
         PPMfrow      <- c(3,3)
         SamplePrts(PPSp,PPTitle,PPMfrow,debug,NTable$Link,NTable$Key,MapAvgH)

      }

      #
      #####

         
      ###### 379x
      #
      #  Part 7.9  - Build PlotOrd DF and link to Name Table
      #
      #      Build table of the merged Polygons by plot order.
      #         Key=ShapeLinkName;  SPDF$plotOrder;     SPDF$RN (SPDF row.names)
      #
       
      PlotOrd       <- data.frame(Key=WorkSp06@data$X__Key,
                                  Ord=WorkSp06@plotOrder, 
                                  RN=row.names(WorkSp06), 
                                  stringsAsFactors=FALSE)
      
      #  Complete assembly of PlotOrd DF.
      
      NTable$Ord        <- PlotOrd$Ord[xm]   # put order in to Name Table
      PlotOrd$Key[xm]   <- NTable$Key        # put key into the PlotOrd table
      PlotOrd           <- PlotOrd[order(PlotOrd$Ord),]   # put plot order table in plot order - use order later.
      #  PlotOrd DF is not in order of 1 to 52(n)
           
      if (bitwAnd(debug,4) != 0) cat("StopFlag:",StopFlag,
                   " ErrorFlag:",ErrorFlag,"\n")
     
      ###
      #
      #  ShapeFile simplification and process is done.  
      #
      #  End Function part 7 - Steps 1 to 9.   
      #
      #######
      
      ##### 381x
      #
      #  Part 8.1 - Add Neighbor relationships - shape file has been simplified,
      #   the polygons gathered under area names, @data should remain 
      #   the same for the rest of the run.
      #   With the unionSpatialPolygons impact, the sf::sf_use_s2 must 
      #   be set to FALSE.
      #
      #   This information required to do the modification phase.
      #
      Sp06.nb        <- spdep::poly2nb(WorkSp06)
      RNList         <- row.names(WorkSp06)   # Get list to help backfile in the nb list.
      NBList         <- sapply(Sp06.nb, function(x) RNList[x])
     
      xmm            <- match(NTable$Key, WorkSp06@data$X__Key)
      NTable[,"NB"]  <- NA
      NTable[,"NB"]  <- list(NBList[xmm])
      
      #
      ##
      #####
      
      if (bitwAnd(debug,4) != 0) {
         cat("ShapeFile & Name Table Processing done.\n")
         cat("StopFlag:",StopFlag," ErrorFlag:",ErrorFlag,"  End of Simplification \n")
         cat("Proj Flags-ShpProjLL:",ShpProjLL,
             "  DoUserProj4:",DoUserProj4,
             "  DoModproj4:",DoModproj4,
             "  DoBldAEAProj:",DoBldAEAProj,"\n")
      }
         if (bitwAnd(debug,64) != 0) {
         cat("ShapeFile Data:\n")
         print(WorkSp06@data)
      }
      
      #
      #####
      
      NTable12 <- NTable
      
      ##### 382x
      #
      #  Step 8.2 - Get area sq. ft (ll or m) to predict too small areas.
      #  Temporary Projections  to be able to get real area values for each
      #  component.   You can't do this with LL projections.
      #  Temp drawing of the lattice and small sized map.
      #
      #  US small areas: DC @ 0.0019 %; RI @ 0.027 %; 
      #                  PR @ 0.0703 %; 
      #    break point appears to be between 0.03 % and lower.
      #    0.331322 of 1104.407 units.
      
      Sp06Area        <- sapply(slot(WorkSp06,"polygons"), function(x) x@area)
      Sp06Area        <- as.data.frame(Sp06Area)
      RNList          <- row.names(WorkSp06)
      row.names(Sp06Area) <- RNList
      Sp06Area$Key        <- RNList
      
      names(Sp06Area) <- c("area","Key")
      Sp06Tot         <- sum(Sp06Area$area)
      Sp06Area$pc     <- Sp06Area$area/Sp06Tot
      NTable[RNList,"area"] <- Sp06Area[,"area"]  # save area size in Name Table
      
      RepDF   <- data.frame(RN = RNList, Area=Sp06Area[RNList,"area"],stringsAsFactors=FALSE)
      #print(RepDF)
      
      #print("RNList - area sqft.")
      #print(RNList)
      #print(Sp06Area[RNList,"area"])
      #print(NTable[,c("Key","area")])
      
      # print table of areas and percentages
      if (bitwAnd(debug,64) != 0) {
         # show details table on coverage
         cat("Report of possible areas that shaping may not be visible:\n\n")
         cat("Total map area (sq.ft. or m):",Sp06Tot,"\n")
         cat("    Area Key","    Coverage","  Percent of Total","\n")
         for (inx in c(1:dim(Sp06Area)[1])) {
            print(paste0(Sp06Area[inx,"Key"],"      ",
                         formatC(Sp06Area[inx,"area"],format="f",digits=4,width=12),
                         "  ",formatC(Sp06Area[inx,"pc"]*100,format="f",digits=4,width=8)))
         }
      }

      cat("Coverage of 0.03 % of the total surface is:",(Sp06Tot * 0.0003),"\n")
      xm     <- Sp06Area$pc < 0.0003
      if (any(xm)) {
         cat("***3823 The following areas may be too small (<-0.03%):\n",
             "***3823   ",paste0(Sp06Area$Key[xm],collapse=", ",sep=""),"\n" )
      }
      #
      #
      #####
      
      ##### 383x
      #
      #   Step  8.3 
      #
      #   Getter yet - figure out how to use the graphic spsce
      #    and the size of the Map (coverage) to estimate
      #    the minimum % an area must be to be seen.  
      #    In the US states, it turned out to be 0.03% or about 
      #    1/40th sq. in. for a 1.5 x 2 inch plot.
      #
      #   How to use maps coverage and determine what the lowend 
      #   value should be based on % of map plotting area.
      #   Thought it would be 1/8 sq, but looks like it's more like 1/32 sq.
      #   If I assume map is only 50% of plot space, then .03% become 1/48 sq.
      #   If in the middle at 75% of plot spqce, then 0.03% becomes 1/38 sq,
      #
      #   So parameters,  Plot area  = "x" sq. in.
      #                   percentage coverage by map = "75%"
      #                   smallest visible space around = 0.0003 or 0.03 %
      #
      
      #MapGSpace        <- 1 * mean(c(MapMinH,MapMaxH))     # total area of destination map (sq. in.)
      #MapGSmall        <- (1/8)^2
      #MapGR            <- MapGSmall/MapGSpace
      #cat("Target Area Sizes: Plot size-MapGSpace:",MapGSpace,"  Smallest-MapGSmall:",MapGSmall,"  Ratio-MapGR:", MapGR,"\n")
      #
      #pcTarget         <- MapGR * TotSqFt      # pcTarget is the size limit to stay above
      #
      #####
      
      ##### 384x
      #
      #  Step  8.4 - Adjustments To ShapeFile.
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
      #      shape file's original metric, the X and Y offsets (shifts) are. 
      #      So, their processing must occur before any transformation.
      #      In the long/lat projection, the problem of the E-W crossing 
      #      can complicate the X and Y adjustments since the 
      #      X,Y values flip from -179 (like with Alaska) to 179.  Even though 
      #      there are no islands or sub-areas that exists across the 
      #      E-W crossing, Alaska does. 
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
      #      9) Since the area's polygons may not fit back into the map in the 
      #         same way. If the area overlays neighbors, must Spatial functions,
      #         will complain and may not function.  Attempts to simply repair
      #         the map have not worked.  So, the code uses the gDiff function
      #         of the modified area's polygons and it's neighbors to cut 
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
      # Apply modifications and gDiff to neighors.
      # Section 8.1 - Apply to area
      # Section 8.2 - gDiff neighbors
      # Section 8.3 - re-insert all changed areas???
      #
      ##### 384x
      #
      #  Step 8.4.1 - Do any required modifications
      #
      
      WorkSp07   <- WorkSp06
      NTable13   <- NTable
      
      #options(warn=1)
      if (any(NTable$DoAdj)) { 
         # if any wants to be modified then GO.
         if (bitwAnd(debug,16) != 0) cat("The adjustments to areas are done using the original metrics and units provided\n",
             "in the original Shape file.  Any transformation of the map is done after the \n",
             "areas are modified.\n")
              
         #
         #  Adjust long/lat for East-West crossing at 180 degrees before modifications.
         #
         if (ShpProjLL) {
	    # we have a long/lat projections
	    # check for problems with E/W crossing adjustments.
	    bboxPP    <- sp::bbox(WorkSp06)   # check is possible E-W Crossing problem
	    xDif      <- diff(bboxPP[1,])   # range of x (lat) degrees
	    if (abs(xDif) > 180) {
	 
	       #####
	       #
	       # Main loop to process East-West Crossing adjustment
	       #
	       WorkSp07 <- FixIDLMain(WorkSp06, debug)  # Replace temp image if modified.
	 
	    }# end of matrix box check 180 degrees
	 }
	 #  Hopefully, in most cases, the modifications will pull an area
         #  that crosses from the East-West Hemisphere to one side or another.
         #  Then the other Spatial functions will not complain.  However,
         #  if the area still the East-West crossings line after
         #  the modifications, we may see complaints by some of the Spatial
         #  functions.
         #
         
         #
         #  The Name Table holds the modification columns:  Xoffset Yoffset, Scale and Rotate.
         #
         #  Loop through the SPDF and process the adjustements (offsets and scaling.)
         #
         #   Find areas in the Name Table that need adjustments (DoAdj=TRUE)
         #   Copy the "Polygons" for the area needing adjustments from SPDF.
         #   Apply adjustment (Centroid Normalizing, Shift, Scale, and Rotate and 
         #     unNormalizing for the centroid)
         #   Get list of neighboring areas.
         #   Apply gDiff to the neighbors.
         #   Remove all changed areas and re-insert the new areas.
         #
         #  If the area needs to be shifted, scaled or rotated, the area is copied 
         #  out of the SPDF. The area is adjusted to be centered at 0,0 for the centroid
         #  of the space and the shift, scale and rotate operations are preformed,
         #  Once done, the area is de-normalized back to it initial centroid (x,y).
         #
         #  The Name Table DoAdj column provides a quick way to determine if an 
         #  area needs adjustments/scaling/rotation.  Only areas with DoAdj=TRUE 
         #  will be processed along with it's neighbors. If the areas' polygons have
         #  changed, then the area is deleted from the SPDF and re-added.
         #  
                 
         #cat("Name Table:\n")
         #print(NTable)
         rgdal::set_thin_PROJ6_warnings(TRUE)
         x <- rgdal::new_proj_and_gdal()
         
         #cat("Name Table Columns:",paste0(names(NTable),collapse=", ",sep=""),"\n")
         #cat("Modification setup Code: 5813 - PlotOrd:\n")
         #print(PlotOrd)
              
         # we have shifting, scaling and rotating to do, but which areas?
         # Think it's best to do them in their plot order in the SPDF.
         # The map is in WorkSp07.
         #
         #   Pick up next Key in the plot order, step through them.
         for (xKey in PlotOrd$Key) {	           #  Pull Out each area's polygons.
                                                   #  in their plot order
            #  Got the plot order -> go to NTable and get flags.
            
            #cat("Checking modifications for ",xKey,"  \n")
            if (NTable[xKey,"DoAdj"]) {    # of NTable row says DoAdj, we have work to do on this area.
               
               #cat("Doing modifications for ",xKey,"  \n")
               
               # get list of modifiers
               xAdjParms <- (NTable[xKey,c("Xoffset","Yoffset","Scale","Rotate")])
               
               WorkSpRn  <- row.names(WorkSp07)    # get list of row names for add and delete. 
                                           # Get new list = it changes every cycle.
               # pick up Xoffset, Yoffset, Scale and pass to the function to process "Polygons" 
               
               #cat("Area:",xKey," will be adjusted - Xoffset, Yoffset, Scaled, or Rotate\n")
               
               # get sub-SPDF for area.
                 # pull off each area's SPDF structure by name.
               areaSPDF <- WorkSp07[xKey,]   
                 # should get only one.  Should be the same as xKey
               areaRN   <- row.names(areaSPDF)  
                 # save the @data section.
               areaData <- areaSPDF@data  
                 # get proj4 string. (Gets a CRS structure.)
               areaProj <- slot(areaSPDF,"proj4string")  
                 # get centroid of area.
               areaCtr  <- as.data.frame(rgeos::gCentroid(areaSPDF)) 
               # get bbox of area.
               areaBBox <- sp::bbox(areaSPDF)      
               
               ## for long/lat projections, E/W crossing Lines has already been handled.
               
               ## setup the SignX value for the adjustment 
               ##   (is Date Line adjustment needed?)
              
               #
               # pull off each Polygons in the area and process.
               cmpPolygons <- sapply( slot(areaSPDF,"polygons"),  
                                       function(x) AdjPolygons(x, areaCtr, xAdjParms) )
                                       
                                       # AdjPolygons - handle "Polygons" (1 per area.)                      
               
               # cmpPolygons is the resulting list of Polygons for the area after modification.
               # Build new area SPDF
               areaSP2     <- sp::SpatialPolygons(cmpPolygons,proj4string=areaProj)
                 # put the @data section back on the SP for the area.
               areaSPDF2   <- sp::SpatialPolygonsDataFrame(areaSP2,areaData)
                           
               # cmpPolygons variable is a list of "Polygon"s from AdjPolyons.
               
               NewSPDF  <- areaSPDF2     # start with modified area.
               areaSP2  <- sp::geometry(areaSPDF2)
               NewSP2   <- areaSP2
               
               # Evaluate Neighbors
               #cat("merging polygons for ",xKey,"\n")
               NBList   <- NTable[xKey,"NB"][[1]]     # get list of keys for neighbors
               #cat("NBList for an area:\n")
               #print(NBList)
               
               if (length(NBList) > 0 ) {
                  # Yes neighbors exist.
                  for (iNB in NBList) {
                     #sp::plot(areaSP2,border="blue")
                     wSPDF   <- WorkSp07[iNB,]
                     wData   <- wSPDF@data
                     wSP     <- sp::geometry(wSPDF)
                     #par(new=TRUE)
                     #sp::plot(wSP,border="magenta",add=TRUE)
                     xSP     <- rgeos::gDifference(wSP,areaSP2,id=iNB)
                     #par(new=TRUE)
                     #sp::plot(xSP,border="red",add=TRUE)
                     xSPDF   <- sp::SpatialPolygonsDataFrame(xSP,wData)
                     NewSPDF <- rbind(NewSPDF,xSPDF)
                     #grDevices::dev.new()
                  }
               }
               
               ModList <- c(NBList,xKey)
                    
               #
               #   Delete the original area from the SPDF
               
               xm       <- match(row.names(WorkSp07),ModList)   # find the matching old polygons
               xmna     <- is.na(xm)    # areas to keep 
               WorkSp07 <- WorkSp07[xmna,]         # list of areas modified.
               
               #  Add the Adjusted area back to the SPDF
               suppressWarnings(WorkSp07 <- maptools::spRbind(WorkSp07,NewSPDF))
               #
               # One "polygons" per SPDF, multiple "Polygons-class" per "polygons" (one per area after merge),
               # multiple "Polygon-class" per "Polygons-class" for each polygon in area.
               #
               # The following scans the "polygons" list of "Polygons" (one per area), takes the "Polygons" list
               # and scans for the "Polygon" items, then processes each Polygon and returns the same.  Problem is to 
               # do scaling and shifts you must apply it to the entire collection of "Polygon"s.
               #
               # AdjPolygon takes each "Polygon" processes it and returns a "Polygon" object. 
         
            }  # End of process of modifying values for one area.     
         
         }  # Loop through areas to see which need adjustments
         
         # Do we have any work to adjust.
      } else {
        x <- 1
        #cat("***3840 Info:No modifications are required to map.\n")
      }
      # Results in WorkSp07 with modifications and East-West Crossing issue. 
      # corrected.
      # Areas are modified and neighbors clipped to have the space is needed.
      #
      #####
      #######
      #########
      
      #########
      #######
      ##### 384x
      #
      #  Step 8.4.b - Sample maps after modification and gDiff to neighbors.
      #
      if (bitwAnd(debug,512) != 0) {
         # caller wants sample maps after the modifications - one map
         
         # There should not be any areas spanning the East and West hempispheres
         #  between the US/South America and Asia at this point. 
         #  The LL coordinates were resolved to do the modifications.
         PPSp         <- WorkSp07
         PPTitle      <- "After modifications and LL East-West Hem."
         PPMfrow      <- c(3,3)
	 SamplePrts(PPSp,PPTitle,PPMfrow,debug,NTable$Link,NTable$Key, MapAvgH)
      }
      #
      #cat("Shape file - modification and adjustments-Done\n")
      
      #####
      #######
      #########
      
      #########
      #######
      #
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
      
      if (bitwAnd(debug,4) != 0) 
      	    cat("Proj Flags - ShpProjLL:",ShpProjLL,
      	        "  DoUserProj4:",DoUserProj4,
      	        "  DoModproj4:",DoModproj4,
      	        "  DoBldAEAProj:",DoBldAEAProj,"\n")
    
      #####
      #
      #  Get initial coordinates and calculate the Original width and 
      #  height values for ratios after transformation. Not sure this 
      #  is needed, but..  SpatialPoints will do the trick.
      #
      #####
      
      #######  386x
      #
      #  Step 8.6 - Build Border Group - 6 - transform SPDF if needed
      #
      #  Transform SPDF after all of the validity checks
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
      cat("Transforming projection of Shape file and label points.\n")
      
      ##### 386x
      #
      #    Step 8.6.1 - Do projection 
      #
      WorkSp07z  <- WorkSp07
      WorkSp08   <- WorkSp07   # Map Bounderies Setup incase no transform.
      NTable14   <- NTable
      Tproj      <- NA
      
      ##### Option 1 - User provided PROJ4 on call.
        
      if (DoUserProj4) {   # execute the projection in the call parameters (it has been adjusted to meters)
         Tproj      <- sp::CRS(proj4)
         cat("Using user provided projection:\n",proj4,"\n")
         WorkSp08   <- sp::spTransform(WorkSp07,Tproj)
         
         # If a proj4 general transformation is being requested by the caller, 
         # then any transformation to none longlat will not be useful.  
         # Best to ignor any NTable$proj entries, and any transformation to get units set to meters.
         # 
         # The area transforms must be done before this.  (actually only the adjustments.)
         #
      } else {
         #  No calling parameter PROJ4.
         ##### Option 2 - ShapeFile has non-longlat proj4
         if (DoModproj4) {
            cat("Re-transforming shape file using original projection, with +unit= changed to meters.\n")
        	  #  BUT the +units are not Meters.  Modproj4 is the 
         	  #  proj4string character string with +units=m added.
         	  Tproj    <- sp::CRS(ModProj4)
         	  WorkSp08 <- sp::spTransform(WorkSp07,Tproj)
             #  This will preserve the user's projection, gives 
             #  me the units in meters, but MapLabels SPDF must be converted.
         } else {
            ##### No Proj4 on call, ShapeFile has a longlat projection
            #  Need to build a AEA about the centroid of the map.
            if (DoBldAEAProj) {
               cat("Projecting shape file using created AEA projection.\n")
               AEAProj4   <- AEAProjection(WorkSp07)
               Tproj      <- sp::CRS(AEAProj4)
               #  Do gross transformation to the map.  (NewProj4, if needed???)
               WorkSp08     <- sp::spTransform(WorkSp07,Tproj)
  	    }
         }
      }
      #
      #   Transform MapLabels if present
      #
      NTable15 <- NTable
      
      if (is.na(Tproj)) {
         x <- 1
         #cat("***3866 Info:No transformation was done to the map.\n")
         
      } else {
         #cat("***3867 Info:Transform Map Label points.\n")
         if (any(!is.na(NTable$MapL))) {
            # We have at least one label to draw on map.l
            xm        <- !is.na(NTable$MapL)   # get list of labels and X,Y Coordinates
            MapRn     <- row.names(NTable)[xm]
            MapDF     <- data.frame(R=MapRn,
                                L=NTable[xm,"MapL"],
                                X=NTable[xm,"MapX"],
                                Y=NTable[xm,"MapY"], stringsAsFactors=FALSE, row.names=MapRn)
            #str(MapDF)
            suppressWarnings(MapSP     <- sp::SpatialPoints(MapDF[,c("X","Y")],proj4=slot(WorkSp07,"proj4string")))
            MapSPDF   <- sp::SpatialPointsDataFrame(MapSP,MapDF[,c("R","L")])
            suppressWarnings(MapSPDF08 <- sp::spTransform(MapSPDF,CRS=Tproj))
            MapDF08   <- as.data.frame(MapSPDF08)
            NTable[MapDF08$R,"MapL"] <- MapDF08$L
            NTable[MapDF08$R,"MapX"] <- MapDF08$X
            NTable[MapDF08$R,"MapY"] <- MapDF08$Y
            #cat("After MapL transformed.\n")
            #str(MapSPDF)
            #print(MapDF08)
            #cat("***3868 Info:Transformation of MapLabel completed.\n")
         }
      }
      #
      #  Add Area Color Index 
      #    get the neighbor relationship from nacol. Since that is based
      #    on the SPDF, translate the values to the positions in the 
      #    Name Table.
      #
      xNTCC           <- nacol(WorkSp08)
      xNTCC           <- as.data.frame(xNTCC,stringsAsFactors=FALSE)
      row.names(xNTCC)<- row.names(WorkSp08)
      
      NTable$CCode    <- xNTCC[row.names(NTable),1]
      
      #
      if (bitwAnd(debug,4) != 0) 
           cat("Proj Flags - ShpProjLL:",ShpProjLL,
               "  DoUserProj4:",DoUserProj4,
               "  DoModproj4:",DoModproj4,
               "  DoBldAEAProj:",DoBldAEAProj,"\n")
               
               
      if (bitwAnd(debug,512+8) != 0) {
         # Do final test map    
         BCol      <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999")
         BCol2     <- c(BCol,BCol,BCol,BCol,BCol)[1:52]
         BColList  <- BCol[NTable$CCode]
         if (length(WorkSp08) != length(NTable$CCode)) {
            cat("***3868 The length of Name Table and the number of areas in the shape file are different.\n")
            stop()
         }
         
         PDFTest   <- paste0(BGDir,BGBase,"_TC_Ready for VisBorder.pdf")
         Title     <- "Test Chart - After Mod's before VisBorder."
         grDevices::pdf(PDFTest,width=10,height=7)
          
         sp::plot(WorkSp08,col=BCol2[NTable$CCode[WorkSp08@data$Key]])
         graphics::text(NTable$MapX, NTable$MapY, NTable$MapL,cex=LabelCex)
         graphics::title("Shape File - final version before conversion.")
         #graphics::title("BBG-Test Area Map with Labels.")

         grDevices::dev.off()
      }
      
      #####  387x
      #
      #  Step 8.7 - correct MapLabel
      #
      #  Now that the shifts, scales and rotations are done.
      #  We need to gather the information to make the adjustments to the 
      #  MapLabel x,y coordinates.   They are the native coordinates of 
      #  the shapefile.  Now that is about to change (transform) to the 
      #  new projection and to "meters".
      #
      #  When the old MapLabel was valided at the start of the function,
      #  the three values were check and placed in the MapL, MapX, and MapY
      #  Name Table columns. 
      #
      #  We have the bbox from the original graphics.
     
      #
               
      if (bitwAnd(debug,4) != 0) cat("Proj Flags - ShpProjLL:",ShpProjLL,
                                     "  DoUserProj4:",DoUserProj4,
                                     "  DoModproj4:",DoModproj4,
                                     "  DoBldAEAProj:",DoBldAEAProj,"\n")
   
      ####### 390x
      #
      #  Step 9.0 = Convert SPDF to VisBorder Data.frames and round vectex
      #
      #  User now has a usable SPDF for micromapST conversion and the start
      #  of the NameTable structure to enhance.
      #
      #  Rounding smoothing.  For lat/long rounding of 2 is approprivate (xxx.xx)
      #     However, this must be changed for other units of measure:
      #       Lat/Long  = round(x,3)     x.xxx   = 1/1000 of degree or 0-364 feet
      #       meters    = round(x,-2)    x,x00   = 100 meters or 369 feet.
      #       kilometer = round(x,0)
      #
      #       1 mile = 1609.34 meters    (720 degrees around the world)
      #
      #       circumference of earth = 24901 miles  or 40,075,000 meters.
      #  
      #   The width and height of the map should also be taken into account.
      #   If the map covers a small area, rounding of the vectex x,y values
      #   may have to be changed to preserve the areas.
      #   At this time, the +units will always be "m".   
      #   Right now the rounding is at about the 364 to 369 feet increments.
      #
      # Modifications Needed - Alternate method to create border group.
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
      
      #####
      #
      #  Set up for build VisBorder data.frames.
      #  but need some of the information earlier.
      
      SaveProj4    <- slot(WorkSp08,"proj4string")        # CRS of proj4string.   # keep
      SaveProj4x   <- SaveProj4@projargs                  # just the proj4string  # keep
      px0          <- stringr::str_locate(SaveProj4x,"\\+proj=")
      px1          <- stringr::str_sub(SaveProj4x,px0[1,2]+1,-1)
      px2          <- stringr::str_locate(px1," ")
      ProjUsed     <- stringr::str_sub(px1,1,px2[1,1]-1)    # keep?
      #print(SaveProj4)
      #cat("ProjUsed:",ProjUsed,"\nSaveProj4x:",SaveProj4x,"\n")
         
      xBBoxBG      <- sp::bbox(WorkSp08)
      bbdx         <- diff(xBBoxBG[1,])
      bbdy         <- diff(xBBoxBG[2,])
      VisAsp       <- bbdy/bbdx            # must keep.
      
      #cat("Step 9.0 Code: 6212 - VisAsp:",VisAsp,"  bbdx:",bbdx,"  bbdy:",bbdy,"\n")   #  get here
     
      WorkSpMaster <- WorkSp08
      
      #
      #  step 9.0 - Start areaParms table - Fill out areaParms Table
      #
      if (bitwAnd(debug,64) !=0) 
           cat("MapMinH:",MapMinH,"  MapMaxH:",MapMaxH,"  mean:",mean(c(MapMinH,MapMaxH)),"\n")
            
      #
      #  The areaParms data.frame must contain everything needed to pickup
      #  and continue the border group build after a checkpoint restart.
      #
      areaParms    <- NULL
      areaParms    <- data.frame(OrigProj4=slot(WorkSp01,"proj4string")@projargs, NewProj4=slot(WorkSp08,"proj4string")@projargs, stringsAsFactors=FALSE)   
      areaParms$OrigProjFull <- slot(WorkSp01,"proj4string")
      areaParms$CurProjFull  <- slot(WorkSp08,"proj4string")
      
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
      
      areaParms$Map.L2Borders <- TRUE                 # Are L2 boundards to be drawn where appropriate.
      
      if (is.na(MapHdr[1]))  MapHdr[1] = ""
      areaParms$Map.Hdr1      <- MapHdr[1]            # The first line of the Map Glyphic header
      if (is.na(MapHdr[2]))  MapHdr[2] = ""
      areaParms$Map.Hdr2      <- MapHdr[2]            # The second line of the Map Glyphic heade
                                                      # if only one header is present, it is placed in the lower line.
      if (is.na(IDHdr[1]))   IDHdr[1] = ""
      areaParms$Id.Hdr1       <- IDHdr[1]             # The first line of the ID Glyphic header
      if (is.na(IDHdr[2]))   IDHdr[2] = ""
      areaParms$Id.Hdr2       <- IDHdr[2]             # The second line of the ID Glyphic header
                                                        # if only one header is present, it is placed in the lower line.
      areaParms$aP_Regions    <- RegionFeature        # set based on presents of regID and multiple values.
      areaParms$aP_Units      <- "meters"             # Coordinates units - should always be meters.
      areaParms$aP_Proj       <- ProjUsed             # The final projection used, should be equal-area or user defined.
      
      areaParms     <- as.data.frame(areaParms, stringsAsFactors=FALSE)   # Make Sure it's a good DF
      
      #cat("areaParms table - Ckpt version\n")
      #str(areaParms)
      #
      #####
     
      ##### 391x
      #
      #  step 9.1 - Starting the Check pointing saves.  Build the unique 
      #  directory to save the 
      #  three files.
      #
      #  Checkpoint directory based on BorderGroupDir  - BGDir ends 
      #  with /
      
      CkptPath <- paste0(BGDir,"CheckPoint")
      cat("The Checkpoint - Folder:\n",
          "   ",CkptPath,"\n")
      if (!dir.exists(CkptPath)) {
         # create if it does not exist.
         # build checkpoint folder if first time (rebuild if needed.)
         dir.create(CkptPath,showWarnings=TRUE)  
      }
        
      #
      #  Build file names and path for the checkpoint datasets
      #
      #  Name Table check points
      #
      NTCkpt     <- paste0("/",BGBase,"_NT_Ckpt.RDA")
      NTCkptcsv  <- paste0("/",BGBase,"_NT_Ckpt.CSV")
      NTPCkpt    <- paste0(CkptPath,NTCkpt)
      NTPCkptcsv <- paste0(CkptPath,NTCkptcsv)
      cat("Checkpoint - Name Table:",NTCkpt,"\n",
          "              NTCkptcsv:",NTCkptcsv,"\n")
      
      areaNamesAbbrsIDs    <- as.data.frame(NTable)
         
      areaParms$CP_NTPath  <- NTPCkpt
      # save everything here.
      save(areaNamesAbbrsIDs, file=NTPCkpt, compress="xz")  
      # reduced Name Table - no neighbor list
      
      ANAInames <- names(areaNamesAbbrsIDs)
      ANAIkeep  <- !("NB" == ANAInames)
      ANAI <- areaNamesAbbrsIDs[,ANAIkeep]
      # cannot write NB list
      utils::write.csv(ANAI, file=NTPCkptcsv, row.names=FALSE)        
      #
      #   Shape File Image
      #
      # writeOGR layer - no extension and no "/" - put on by writeOGR
      SFCkpt     <- paste0(BGBase,"_SF_Ckpt")  
      # writeOGR DSN
      SFPCkpt    <- paste0(CkptPath)           
      
      # no extension and no "/" - put on by writeOGR
      SFCkptRDA  <- paste0(BGBase,"_SF_Ckpt.rda")  
      SFPCkptRDA <- paste0(CkptPath,"/",SFCkptRDA)
      
      areaParms$CP_ShpDSN  <- SFPCkpt
      areaParms$CP_ShpLayer <- SFCkpt
      cat("Checkpoint - Shape File:",SFCkpt,"\n",
          "              SFCkptRDA:",SFCkptRDA,"\n")
      save(WorkSpMaster, file=SFPCkptRDA, compress="xz")
      rgdal::writeOGR(WorkSpMaster, dsn=SFPCkpt, layer=SFCkpt, 
          driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      #
      #   areaParm image
      #
      APCkpt    <- paste0("/",BGBase,"_AP_Ckpt.RDA")
      APPCkpt   <- paste0(CkptPath,APCkpt)
      cat("Checkpoint - areaParms: ",APCkpt,"\n")
      save(areaParms,file=APPCkpt,compress="xz")
      
      #
      # After doing the check point, we continue to build 
      # the border group dataset.
      #
      cat("***3914 BuildBorderGroup has completed write the check point files for possible editing\n",
          "***3914 and restart.  They are located in the following directory: \n")
      cat("***3914 ",CkptPath,"\n")
      cat("***3915 The check point Shape File for the border group is saved to:\n",
          "***3915 ",SFCkpt,"\n")
      cat("***3916 After editing, the results must be save back to the same directory and filename.\n")
      
      WorkSpMst <- WorkSpMaster
      #
      #####
      #######
      #########

   } else {
      
      #########
      #######
      ##### 392x 
      #
      #   Part 10.0 - Pull Data in for checkPointReStart
      #cat("Check Point Restart Process Initiated.\n")
      #
      # 
      #
      #  We are doing a checkPointReStart.  Build the directory and path
      #  strings and reload the:  ShapeFile, NameTable, areaParm table.
      #  Key calling parameters are:  NameTableDir (contains the check point folder),
      #  the bordergroupname (part of the check point file names and the final border
      #  group name).
      #
      #  Question use Border Group Dir or Name Table Dir
      #  Need to load areaParms file. IT has all of the rest of the data.
      #  
      RecoveryBase <- NULL
      if (missing(BorderGroupDir) || is.null(BorderGroupDir) ) {
         # no border group dir - use name table dir
         if (missing(NameTableDir) || is.null(NameTableDir) ) {
            # no Name Table Dir - ERROR
            xmsg <- paste0("***3920 No Border Group or Name Table directory provides. Cannot find restart files. STOP.\n")
            stopCntMsg(xmsg)
         } else {
            # have Name Table Dir
            cat("NameTable:",NameTableDir,"\n")
            RecoveryDir <- NameTableDir
         }
      } else {
         # have border group dir - use it.
         RecoveryDir <- BorderGroupDir
      }
      RecoveryBase <- paste0(RecoveryDir,"/CheckPoint/",BGBase)
      cat("RecoveryBase:\n",
          "   ",RecoveryBase,"\n")
      
      load(file=paste0(RecoveryBase,"_AP_Ckpt.RDA"))  # areaParmsUS
      
      SFChpt        <- areaParms$CP_ShpLayer
      SFPCkpt       <- areaParms$CP_ShpDSN
      cat("ShapeFile recovery:\n",
          "   ",SFCkpt,"\n")
      WorkSpMaster  <- rgdal::readOGR(dsn=SFPCkpt,layer=SFCkpt)
      # The write and read OGR to a shapefile, does not preserve
      # and return the key set as the row.names.  You have to 
      # reset it to procede.
      row.names(WorkSpMaster) <- WorkSpMaster@data$X__Key
      WorkSpMst     <- WorkSpMaster
      WorkSpMst     <- rgeos::gBuffer(WorkSpMaster,width=0,byid=TRUE)  # clean map
      WorkSpMstData <- WorkSpMst@data
      
      
      NTCkpt  <- areaParms$CP_NTPath
      #cat("NameTable Recovery:\n","   ",NTCkpt,"\n")
      load(file=NTCkpt)
          
      NTable <- areaNamesAbbrsIDs
      
      #  Shape File restored to WorkSpMst
      #  Name Table restored to NTable
      #  areaParms table restored to areaParms
   }  
   
   #####
   #
   # Make sure SPDF if OK.
   #
   WorkSpMst <- rgeos::gBuffer(WorkSpMst,width=0,byid=TRUE)
   
   #####
   #
   #  Hopefully we will not need anything else (variable) except what is in
   #  the recovery files.
   #
   #  Change variable reference to areaParms ...
   #
   #####
     
   ##### 0Axx
   #
   #  Since projections have been done, no more dateline concerns.
   #
   #  Step 10 - Build VisBorder data.frames from SPDF and UNION as needed.
   #
   #  a) Save SPDF Images for area, Regions, L2 and L3.
   #
   #  b) Preform UNIONS aS NEEDED ON SPDF for Regions, L2, and L3.
   #   
   #  c) Convert images into VisBorders format.
   #     Repeat for each layer's SPDF (area, L2, L3, Regions)
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
   ##### 101x
   #
   #  Step 10.1  - get SPDF for each boundary set - area, L2, L3, and Regions
   #
    
   cat("Converting ShapeFile into the 4 micromapST boundary layers (area, L2, L3, and Regions).\n")
   
   BCol        <- c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf","#999999")
   BCol        <- c(BCol,BCol,BCol,BCol)
   
   WorkL2s      <- WorkSpMst
   WorkRegs     <- WorkSpMst
   WorkL3s      <- WorkSpMst
   
   vDebug  <- debug
   #if (bitwAnd(debug,1024))  vDebug <- vDebug + 512
   
   if (bitwAnd(vDebug,256+512) != 0) {
      # caller wants sample maps
      #cat("***3970 Generate scaled example of test maps - Before Conversion.\n")
      
      PPSp         <- WorkSpMst
      PPTitle      <- "Before conversion to VisBorder"
      PPMfrow      <- c(3,3)
      SamplePrts(PPSp,PPTitle,PPMfrow,debug,NTable$Link,NTable$Key,MapAvgH)
   }
   
   SpMstrows <- row.names(WorkSpMst)  # the list of rows in the WorkSp08 SPDF
   #  No more unions on the area layer
   
   #cat("Length of WorkSpMst:",length(WorkSpMst),"\n")
   #cat("Lengths of Objects-NTable:",dim(NTable)[1],"  Size:",utils::object.size(NTable),"\n")
   #cat("Orig Areas-WorkSpMst Size:",utils::object.size(WorkSpMst),"  length:", length(WorkSpMst),"\n")
   #cat("row.names of WorkSpMst:\n")
   #print(row.names(WorkSpMst))
   
   
   #
   #  L2 Groups - Create
   #
   #cat("Layer 2 group.\n")
   # do union based on L2_IDs in the Name Table
   # Get list from WorkSp row.names orderr from Name Table.
   L2Grps     <- NTable[SpMstrows,"L2_ID"]  
   suppressWarnings(WorkL2     <- maptools::unionSpatialPolygons(WorkL2s,L2Grps))
   #str(WorkL2)
   #cat("row.names of WorkL2:\n")
   #print(row.names(WorkL2))
   
   if (bitwAnd(debug,8) != 0 ) {
      WorkL2Neib   <- nacol(WorkL2)
      #cat("Looking at Neighbors:", WorkL2Neib, " (colors)\n")
      WorkL2ColK   <- BCol[WorkL2Neib] # find best color pattern
      grDevices::pdf("BBG-Level 2 Map Image.pdf",width=10, height=7)
      sp::plot(WorkL2,col=WorkL2ColK)
      graphics::title("Level 2 SP Shape file data")
      dev.off()
   }
   #
   #  Regional Groups
   #
   #cat("Regional Groups.\n")
   RegGrps     <- NTable[SpMstrows,"regID"]
   suppressWarnings(WorkReg     <- maptools::unionSpatialPolygons(WorkRegs,RegGrps))
   #str(WorkReg)
   #cat("row.names for WorkRegs:\n")
   #print(row.names(WorkRegs))
  
   if (bitwAnd(debug,8) != 0) {
      WorkRegColK <- BCol[nacol(WorkReg)]
      grDevices::pdf("BBG-Regional Level Map Image.pdf",width=10, height=7)
      sp::plot(WorkReg,col=WorkRegColK)
      graphics::title("Region SP Shape file data")
      dev.off()
   } 
   
   #
   #  L3 (All) outline
   #
   #cat("Layer 3 - outline.\n")
   L3Grp   <- rep(stringr::str_sub(BorderGroupName,1,3),length(WorkL3s))
   suppressWarnings(WorkL3  <- maptools::unionSpatialPolygons(WorkL3s,L3Grp))
   #str(WorkL3)
   
   if (bitwAnd(debug,8) != 0) {
      WorkL3ColK <- BCol[nacol(WorkL3)]
      grDevices::pdf("BBG-Map Outline (L3) Map Image.pdf",width=10, height=7)
      sp::plot(WorkL3,col=WorkL3ColK)
      graphics::title("L3 SP Shape file data")
      dev.off()
   } 
   #
   ######
   
   ###### 068x
   #
   #  Step 7.2  - convert each boundary SPDF to the VisBorders format.
   #  (Make function)
       
   #  Now the areas - Adjustments like Scaling, Shifting - must be done area by area.  The 
   #  parameters come from the Name Table (extension) columns:  Xoffset Yoffset, Xscale, Yscale.
   #    If Yscale is missing, Xscale is used for the Y axis.
   #    No adjustment is done if all of the values are NULL or NA.
   #  The resulting SpatialPolygons are re-inserted into the SPDF.
   
   # check the Name Table to see if adjustment columns Xoffset, Yoffset, Xscale, and Yscale exist. 
   # If they don't exist, then skip the adjustment section of the processing.f
   
   #cat("The conversion of SPDF to VisBorder format startes---\n")
   #cat("Source SpatialPolygonsDataFrame:\n")
   #str(WorkSpMst)
   #print(NTable)
      
   PlOrd          <- NTable[,c("Key","Ord")]
   PlOrd          <- PlOrd[order(PlOrd$Ord),]
   #str(PlOrd)
   #cat("areaVisBorders.\n")
   #print(PlOrd)
   
   areaVisBorders <- BuildVisBorder(WorkSpMst, PlOrd)
   #head(areaVisBorders,20)
   
   L2PlOrd        <- data.frame(Key=row.names(WorkL2), Ord=WorkL2@plotOrder, stringsAsFactors=FALSE)
   row.names(L2PlOrd) <- L2PlOrd$Key
   L2PlOrd        <- L2PlOrd[order(L2PlOrd$Ord),]
   #cat("L2VisBorders.\n")
   #str(L2PlOrd)
   L2VisBorders   <- BuildVisBorder(WorkL2, L2PlOrd)
   #head(L2VisBorders,20)
   
   RegPlOrd       <- data.frame(Key=row.names(WorkReg), Ord=WorkReg@plotOrder, stringsAsFactors=FALSE)
   row.names(RegPlOrd) <- RegPlOrd$Key
   RegPlOrd       <- RegPlOrd[order(RegPlOrd$Ord),]
   #cat("RegVisBorders.\n")
   #str(RegPlOrd)
   RegVisBorders  <- BuildVisBorder(WorkReg, RegPlOrd)
   #head(RegVisBorders,20)
   
   L3PlOrd        <- data.frame(Key=row.names(WorkL3), Ord=WorkL3@plotOrder, stringsAsFactors=FALSE)
   row.names(L3PlOrd) <- L3PlOrd$Key
   L3PlOrd        <- L3PlOrd[order(L3PlOrd$Ord),]
   #cat("L3VisBorders.\n")
   #str(L3PlOrd)
   L3VisBorders   <- BuildVisBorder(WorkL3, L3PlOrd)
   #head(L3VisBorders,20)
   #
   cat("Completed conversion to VisBorder format.\n")
   #
   ######
   
   #####
   #
   #  Display the final results.
   #
   
   if (bitwAnd(debug,2048) !=0) {
      ###### 069x
      #
      #  Test Plotting to windows of each VisBorders  (debug= 2048)
      #
      grDevices::dev.new()
      #cat("Drawing test images of the border group layers to the screen'\n")
      #cat("  one per windows.  Each must be manually closed.\n")
      maxCol         <- max(NTable$CCode)
      WANCol         <- RColorBrewer::brewer.pal(maxCol, "RdYlBu")
      WANAreaCCode   <- NTable[areaVisBorders[is.na(areaVisBorders$x),"Key"],c("CCode")] 
      WANAC          <- WANCol[WANAreaCCode]  # ordered by neigbhor
      
      PlotVis(areaVisBorders,WANAC)
      graphics::title("VisBorder of areas")
      grDevices::dev.new()
      
      L2VisB         <- NULL
      L2VisB$Key     <- L2VisBorders[is.na(L2VisBorders$x),"Key"]
      uniL2Keys      <- unique(L2VisB$Key)
      maxCol         <- length(uniL2Keys)
      #cat("L2 Max Colors:",maxCol,"\n")
      if (maxCol > 10) {
         WANCol1     <- RColorBrewer::brewer.pal(11, "RdYlBu")
         WANCol1     <- rep(WANCol1, maxCol / 11 + 1)
         L2Col       <- WANCol1[1:maxCol]
      } else {
         L2Col       <- RColorBrewer::brewer.pal(maxCol,"RdYlBu")
      } 
      #cat("Colors:", paste0(L2Col,collapse=", ",sep=""),"\n")
      xm             <- match(L2VisB$Key, uniL2Keys)
      L2VisB$Col     <- L2Col[xm]
        
      PlotVis(L2VisBorders,L2VisB$Col)
      graphics::title("VisBorders of L2")
      grDevices::dev.new()
      
      RegVisB        <- NULL
      RegVisB$Key    <- RegVisBorders[is.na(RegVisBorders$x),"Key"]
      uniRegKeys     <- unique(RegVisB$Key)
      maxCol         <- length(uniRegKeys)
      #cat("Reg Max Colors:",maxCol,"\n")
      if (maxCol > 10) {
         WANCol1     <- RColorBrewer::brewer.pal(11, "RdYlBu")
         WANCol1     <- rep(WANCol1, maxCol / 11 + 1)
         RegCol      <- WANCol1[1:maxCol]
      } else {
         RegCol      <- RColorBrewer::brewer.pal(maxCol,"RdYlBu")
      } 
      xm             <- match(RegVisB$Key, uniRegKeys)
      RegVisB$Col    <- RegCol[xm]
      #cat("Colors:", paste0(RegCol,collapse=", ",sep=""),"\n")
      
      PlotVis(RegVisBorders,RegVisB$Col)
      graphics::title("VisBorder of Regions")
      grDevices::dev.new()
       
      PlotVis(L3VisBorders,"green")
      graphics::title("VisBorder of L3")
      
   }   # end of the test plots of each VisBorder data.frame as a set of windows.
   #
   ###
      
   #####
   #
   #   Record final areaVisBorder in file.
   #
   if (bitwAnd(debug,1024) != 0) {
      #  plot of final areaVisBorders map to PDF or PNG file.
      maxCol       <- max(NTable$CCode)
      WANCol       <- RColorBrewer::brewer.pal(maxCol, "RdYlBu")
      WANAreaCCode <- NTable[areaVisBorders[is.na(areaVisBorders$x),"Key"],c("CCode")] 
      WANAC        <- WANCol[WANAreaCCode]  # ordered by neigbhor
   
      PPTitle      <- "Final_areaVisBorders"
      PngH         <- MapAvgH
      xAsp         <- areaParms$Map.Aspect
      PngW         <- PngH / xAsp
      #cat("xAsp:",xAsp,"  PngW:",PngW,"  PngH:",PngH,"\n")
      
      OutTestSm    <- paste0(BGDir,BGBase,"_FP_",PPTitle,OType)
      if (OType == ".png") {
         grDevices::png(OutTestSm, res=300, width=PngW, height=PngH, units="in")
      } else {
         grDevices::pdf(OutTestSm, width=PngW, height=PngH)
      }
      
      #cat("par('din')",par('din'),"  par('fin'):",par('fin'),"  par('pin'):",par('pin'),"\n")
           
      #par(mfrow=c(1,1))
      par(omi=c(0,0,0,0))
      par(oma=c(0,0,0,0))
      par(mai=c(0,0,0,0))
      par(mar=c(0,0,0,0))
              
      PlotVis(areaVisBorders,WANAC)     # micromapST defaults lwd to 0.5
      graphics::title(main=PPTitle,cex.main=0.1)
      
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
      grDevices::dev.off()
   
   }  # end of PDF or PNG final image plot of areaVisBorder borders.
   
   #
   #  Finish Name Table build out.
   #
   #####
   
    
   ##### 071x
   #
   #
   #  Fields: 
   #   bordGrp  -  name of border group
   
   #
   #  Save the individual data.frames
   #
   cat("Writing Border Group Package for ",BGBase,"  \n")
   
   save(areaNamesAbbrsIDs, file=paste0(BGDir,BGBase,"_areaNamesAbbrsIDs.rda"), compress="xz")
   save(areaVisBorders,    file=paste0(BGDir,BGBase,"_areaVisBorders,rda"),    compress="xz")
   save(L2VisBorders,      file=paste0(BGDir,BGBase,"_L2VisBorders.rda"),      compress="xz")
   save(L3VisBorders,      file=paste0(BGDir,BGBase,"_L3VisBorders.rda"),      compress="xz")
   save(RegVisBorders,     file=paste0(BGDir,BGBase,"_RegVisBorders.rda"),     compress="xz")
   save(areaParms,         file=paste0(BGDir,BGBase,"_areaParms.rda"),         compress="xz")
   
   #
   #  Save the border group data set of all data.frames
   #
   saveL <- c("areaParms", "areaNamesAbbrsIDs", "areaVisBorders", "RegVisBorders", "L2VisBorders", "L3VisBorders")
   saveL
   
   SavePath <- BorderGroupPath # paste0(BGDir,BGBase,"BG.rda")
   save(list=saveL,file=SavePath, compress="xz")
   
   cat("Border Group Created - Successfully.\n")
   #print("")
   #print("")
   
   if (bitwAnd(debug,8192) != 0) {
         
      #cat("Generate scaled example of test maps - after rounding and convert to VisBorders.\n")
         
      VisB         <- areaVisBorders
      KeyList      <- unique(VisB$Key)
      
      KeyCol       <- data.frame(Key=KeyList,Col=NA)
      #KeyCol$Name  <- NTable[KeyCol$Key,"Name"]
      
      KeyNum       <- dim(KeyCol)[1]  # Get number of rows.
      NumPanels    <- as.integer((KeyNum-1)/5) + 1  # calculate number of panels.
      #NumPanels    <- as.integer(NumPanels)
      
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
      PDFTest      <- paste0(BGDir,BGBase,"_TestChart_based on VisBorder.pdf")
      grDevices::pdf(PDFTest,width=10.5,height=7.75)
      
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
      
      grDevices::dev.off()
      
   }  # end of multiple small image print out 
   #
   ###
   
   ###
   #
   #   Final summary report of the names, abbrs, and ID used in the name table 
   #   as documentation of the border group.
   #
      
   cat("\n\n\nPUBLICATION INFORMATION FOR NAME TABLE IN BORDER GROUP : \n",
       "   ",paste0(BGBase,"BG"),"\n")
   cat("\n\n")
   
   NTNames  <- names(NTable)
   #print(NTNames)
   
   TCol     <- c("Name","Abbr","ID", "Alt_Abbr", "Alias")
   CCol     <- c("full","ab","id", "alt_ab", "alias")
   L2Col    <- c("L2_ID","L2_ID_Name")
   RegCol   <- c("regID", "regName")
   ModCol   <- c("Xoffset","Yoffset","Scale","Rotate")
   MapLabel <- c("MapL","MapX","MapY")
   
   
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
   
   print(NTUser)	    #  print a copy of the user's name table. 
   cat("\n\n")
   
   NTrn    <- row.names(NTable)
   
   if (length(unique(NTable$L2_ID)) > 1) {
      cat("\n\nName Table Layer 2 Values\n")
      print(NTable[,L2Col])
   }
   if (length(unique(NTable$regID)) > 1) {
      cat("\n\nName Table Regional Values\n")
      print(NTable[,RegCol])
   }
   if (sum(!is.na(NTable$MapL)) > 0) {
      cat("\n\nName Table Map Label Values\n")
      print(NTable[,MapLabel])
   }
   if (sum(NTable$DoAdj) > 0) {
      cat("\n\nName Table Map Modifications Values\n")
      print(NTable[,ModCol])
   }
   cat("\n\n*** Any entry with and 'NA' is empty or has value is not available.\n\n")
   
   
   cat("Finished building Border Group:",paste0(BGBase,"BG"),"\n")
      
   return(SavePath)
}   
   
