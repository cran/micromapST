\name{BuildBorderGroup}
\alias{BuildBorderGroup}
\title{Building new border groups for Linked Micromap created by the micromapST package}
\description{
 The package's \var{micromapST} function created linked micromaps for 
 any geographic region. The information related to the selected 
 regions is provided to the \var{micromapST} function via a \var{bordergrp} 
 dataset.  This dataset contains all of the operational information, the 
 needed boundary dataset and a name table required for \var{micromapST} to
 draw the linked micromaps for the desired region of the world or elsewhere.
 The dataset must contain the general information data.frame (areaParms), the 
 area boundary data.frame (list of points for each area in the boundery) and 
 a name table that provide the the "Name", "Abbr", and "ID" location names for 
 each area in the boundary group.  The Name Table also assists in providing
 the L2 and Regional features. The border groups were originally created 
 manually, one by one. \cr
 The \var{BuildBorderGroup} function is a compilation 
 of the learning from building past border groups. The function 
 tries to provide a common foundation to address many of the unique 
 situations discovered building the border groups manually. However, there
 are still some situations that required the builder's intervention in 
 order to produce a usable map for linked micromaps. \cr
 The \var{BuildBorderGroup} function accepts a shape file (ESRI format)
 and a user built name table.  The name table provides the location ids 
 (name, abbreviation or id) to allow the micromapST user ways to specify 
 the identity of individual area using one of several forms of identifier.
 The forms used by the \var{BuildBorderGroup} and \var{micromapST} functions 
 are "Name", "Abbr" (abbreviation), and the numerical "ID" identifiers
 that have been assigned and accepted over time for the area as the 
 primary location identifiers. \cr
 In two special cases \var{micromapST} has been extended to support an 
 "Alt_Abbr" and an "Alias" form of the location identifiers.  The "Alt_abbr"
 form was added to handle geographic area that have two sets of commonly 
 used abbreviations.  The name table was expanded to contain both sets 
 of abbreviations and allows the user of the border group to select the 
 abbreviation that matches data frame location ids.  The "Alias" identifier 
 was implement to handle a special case where the source of the statistical 
 data did not use the accepted "Name" or "Abbr" location ids, but used 
 a more generallized string.  To keep the matching as flexiable 
 as possible, a wildcard matching alias was introduced.  As each 
 identifier in the data is examined, it is compared against the alias 
 string with leading and traiing "*" matching character.  As long as 
 the Alias strings are unique and do not appear in two data rows,
 the matching provide the link between the data and the geographical 
 area. Over the past 10 years, the source program has been changed many 
 times, but the location id label matches has continued to work.\cr
 The user created name table also provides additional information 
 regional identification and sub-setting of the map, and area labeling. 
 During the border group building process, the name table 
 The Name Table also provides parameters to the \var{BuildBorderGroup} 
 function to make specific modifications to areas. The modifications to 
 areas include shifting it location, scaling it size, and rotating 
 the area. These modifiers were used to scale Alaska to 35% of 
 it's normal size and move it to below California to reduce the size 
 of the total map.  Hawaii was also moved below the U. S. to reduce 
 the total size of the map. \cr  
 The name table also allows the builder to assocate areas with regions 
 in their map and Level 2 border outlining. Check the \var{micromapST} 
 documentation on how the Level 2 and Region boundaries are to be used.  
 The most important task the name table does is help provide the linkage 
 between the rows in the user's data frames to the an area's collection 
 of polygons from the shapefile.  The Name Table also provide multiple 
 location ids to allow the user to provide the full formal name ("Name"), 
 one of two abbreviates (if available), or the numerical ID 
 assigned to each area.\cr
 When the processing is completed, the user has a border group .RDA file 
 ready for use with \var{micromapST} with boundaries usable in the 
 small maps in linked micromaps and the name table documentation for 
 the border group.
 
 The \var{BuildBorderGroup} function validates all of the call parameters,
 inspects the information provided by the builder in the name table,
 inspects the shape file provided, inspects the projections used or 
 requested, ensures the +unit= parameter in the projection is set to 
 meters, The function performs the following steps to construct a border 
 group: may different spatial areas by using different border groups.
 
 In Version 3.0.0 of the \var{BuildBorderGroup} function, the function was 
 upgraded to retire and package functions from the rgdal, rgeos and 
 maptools packages as of October 16th, 2023.  The spatial functions are 
 not done by the "sf" package.
 
 The following describes the process of conconstructing a border group dataset:
  \itemize{
   \item{1. Validate all of the calling parameters provided on the 
    function call and provide targeted error and warning messages.
   }
   \item{2. Read the shape file to gather initial shape file variables.  The 
    shape file can be read prior to the call to the BuildBorderGroup function, 
    modified, and passed as a structure using the ShapeFile call parameter.
   }
   \item{3. Read the name table file and verifies all required columns 
    are present.  The Name Table data can also be pre-read and passed to 
    BuildBorderGroup function as a data.frame .
   }
   \item{4. Validate the data in the name table columns: characters vs. 
    numeric vs. NA; duplicate valids, and range of values.  
   }
   \item{5. Validate the geometry in the shape file data.
   }
   \item{6. Simplify the shape file geometry to provide a caricatured 
    map with minimal vectex, but maintaining the ability to recognize 
    the areas. This generally reduces the size of the boundary 
    information to between 1% and 0.05% of the orignal shape file complexity
    using the \var{rmapshaper} package.
   }
   \item{7. Preform a union the polygons in the shape file to organize  
    polygons by their associated area.  In SP, it would be collecting all 
    polygon data for an area as a list of polygons as one group.  In sf, it 
    is collecting all of the polygons for an area and forming a multipolygon of 
    the data. Since we have moved entirely to sf operation, this union is 
    preformed by the aggregate.sf features in sf.
   }
   \item{8. Match the name table rows (entries) with the area polygons 
    in the spatial structure.  Once the match is established, a key is assigned 
    for the area and set in both the name table and spatial structure.  
    The abbreviation id is normally used for the key, but if the abbreviation 
    is not provided, a short key is created.   Abbreviation for areas are not 
    always available.
   }
   \item{9. The projection of the map makes a difference in how usable the
    map in making linked micromap.  It was desided to no use longitute/latitude 
    in the final resulting map.  Therefore, the user has three choices: specify a 
    non-long/lat projection on the original shape file; specify final projection 
    parameters in the "proj4" call parameter, or let the function construct a
    Albers Equal Area projection about the centroid of the map with the north and 
    south latitudes half the distance from the middle to the north and south 
    limits of the map. If there are any map labels 
    requested, their location points are transformed.  Any needed transformation
    is done at this time.
   }
   \item{10. The areaParms data.frame table is constructed to permit
    restarting the build process and to save all of the other call 
    parameters and operation parameters needed by \var{micromapST}.  
    The \option{MapHdrs}, \option{IDHdrs}, \option{MapMinH}, 
    \option{MapMaxH}, and \option{LabelCex} variables are saved in 
    the areaParms data.frame.
   }
   \item{11. The name table, areaParms, and the 4 boundary data.frame 
    structures are check pointed to disk for possible manual editing.  
    If manual editing of the shape file or name table is done, 
    they result of the edits must be saved using the original file 
    name and the file placed in the original check point directory. 
    The check point files will be read when the BuildBorderGroup is 
    call with the checkPointRestart parameter set to \var{TRUE} the 
    name table directory provided is the same, and the check pointed 
    files are located. Manual editing should be done very carefully.  
   }
   \item{12. On a "checkPointReStart=TRUE" the \var{BuildBorderGroup}
    function reloads all of the check pointed data.frame to pick up 
    the Border Group building process from where it left off.
   }
   \item{13. On either a restart or a normal run, the function gathers 
    the boundary information for the area boundaries, layer 2 boundaries, 
    regional boundaries, and a map outline boundary (Layer 3).
   }
   \item{14. The name table becomes the areaNamesAbbrsIDs data.frame 
    and any working columns not needed in the final name table are deleted.
   }
   \item{15. The name table to aggregate the area boundaries to form the 
    regional boundary spatial structure, the Level 2 spatial structure, and the 
    Lever 3 spatial structure (outline of the entire set of areas.)  This is 
    done to make sure all of the layers of boundaries correctly overlay each other.
   }
   \item{16. Each set of spatial images are converted into the \var{micomapST} 
    boundary data.frame structures (not sp or sf) that are compatiable 
    with the R polygon drawing function.
   }
   \item{17. The final collection data.frames for the border group are: 
    areaParms, areaNamesAbbrsID, areaVisBorders, L2VisBorders, RegVisBorders, 
    and L3VisBorders data.frames are written out to a single .RDA 
    dataset file as the completed border group dataset.
   }
  }
 
 Each border group is a different dataset containing the unique boundaries 
 and operational information to allow \var{micromapST} to work in a 
 different spatial area. The structure of each border group dataset is 
 identical with the same variable names and types of structures.  
 A user can build their own border group dataset to meet their 
 specific spatial area needs.  Because the package contains several border 
 group datasets each one using  the same data.frame names and structure, 
 the use of lazydata or lazyloading had to be disabled.  This means the 
 R system cannot preload the datasets and have them waiting for use, 
 they would effectively overlay each other.
 
 The name of the border group is specified in the bordGrp call parameter.
 To permit a user to reference a border group dataset not contained 
 in the package, and reside in a user's folder, the bordDir must be 
 used to direct the package to the border group.  The border group must be 
 saved under R using the \var{save} function with the file extension of 
 ".rda". \cr
 For example: \code{bordGrp="private", bordDir="c:/SavedBorderGroups"}
  
 Each border group contain six (6) datasets by the same data.frame names.  
 This allows the micromapST package the ability to quickly load a 
 particular border group and create the requested micromaps.  
 The six data.frames are:  areaParms, areaNamesAbbrsIDs, areaVisBorders, 
 L2VisBorders, RegVisBorders, and L3VisBorders. Since the same data.frame 
 names are reused in each border group, the R lazyload feature is 
 disabled in the package.

 Several border group \option{bordGrp} examples are contained 
 in the package and include the 51 states and DC of the United States, 
 the counties of Kansas, Maryland, New York, Utah, the countries and provinces 
 of the U.K. and China, and the U. S. Seer Registries used by the 
 National Cancer Institute.  
 
 The example in this section shows how to build the Kentucky County 
 border group using a simple name table and the U. S. Census Bureau 
 Kentucky county 2000 boundary data.
 
}
 
 \usage{BuildBorderGroup(ShapeFile         = NULL,   # required	       
                         ShapeFileDir      = NULL,   # defaults to NameTableDir
                         # required if not the default value of "link"
                         ShapeLinkName     = NULL,   
                         NameTableFile     = NULL,   # required
                         NameTableDir      = NULL,   # required
                         # required if not the default value of "link"   
                         NameTableLink     = NULL,       
                         BorderGroupName   = NULL,   # required
                         # defaults to NameTableDir	
                         BorderGroupDir    = NULL,  	
                         MapHdr            = NULL,   # optional		
                         MapMinH           = NULL,   # optional           
                         MapMaxH           = NULL,   # optional      
                         # required, default is the BorderGroupName and "Areas"
                         IDHdr             = NULL,  		
                         LabelCex          = NULL,   # optional      
                         # optional, but highly recommended 
                         ReducePC          = 1.25,   # percent value	
                         proj4             = NULL,   # optional		
                         checkPointReStart = NULL,   # optional
                         debug             = 0	     # debug only	
                      ) 
 			     
 }   
\arguments{
  \item{ShapeFile}{a character string of the name of the ERSI formated 
    shape file.  Only the main part of the shape file name should be 
    provided.  The .shp, .shx, .dbf extensions should be omitted.
   }
  \item{ShapeFileDir}{a character string defining the path to the folder
    containing the ERSI shape file. If the ShapeFileDir parameter is not 
    provided or empty, the Name Table Directory will be used.
   }  
  \item{ShapeLinkName}{a character string defining the name of the 
    variable within the shape file @data slot to use to match the 
    polygons in the shape file with the area's row in the Name Table.  
    The default value is "__Link". 
   }
  \item{NameTableDir}{a character string defining the path to the folder 
    containing the Name Table File. There is no default value for this 
    parameter.
   }
  \item{NameTableFile}{a character string defining the name of the 
    excel spreadsheet file or a .csv file containing the user built 
    Name Table columns and information.
   }
  \item{NameTableLink}{a character string defining the Name Table 
    column name that should be used to match the \var{ShapeFileLink} 
    variable to link the Name Table to the area's collection of 
    polygons. The default value is "link".
   }
  \item{BorderGroupName}{a character string to use as the border group's 
    name and dataset name.  If the string ends with a "BG", it will be 
    striped and re-added when the border group is built. If the string 
    does not end with "BG", "BG" will be added to designate the file is
    a border group.
   }
  \item{BorderGroupDir}{a character string defining the path to the 
    folder where the border group dataset will be written at the end of 
    the processing.  If this parameter is not provided or "NA", the 
    \var{NameTableDir} parameter will be used as the 
    \var{BorderGroupDir} parameter.  If the \var{BorderGroupDir} path does not
    exist, the \var{BuildBorderGroup} function will create the directory.
    }
  \item{MapHdr}{is a two element character vector used to modify the 
    pre-defined map header labels in \var{micromapST} for map type glyphs. 
    The value is entered as \cr \code{MapHdr = c("1stHdr", "2ndHdr")}. 
    Check the \var{micromapST} documentation for more details.
    The first element (MapHdr[1]) is not implemented and is reserved for 
    a future release.  The MapHdr[2] element is used to generated the 
    map headers for all of the map glyph tyeps.  It should
    specify the type of area being mapped.  For example for the US States
    map, it was set to "States". The default value is 
    MapHdr=c(<border group name>,"Areas").
    This call parameter is not required.
   }
  \item{MapMinH}{is a numerical variable specifying the minimum height 
    the maps should be in the group/rows in a linked micromap graphic.  
    The default value is 0.5.
   }
  \item{MapMaxH}{is a numerical variable specifying the maximum height 
    the maps should be in the group/rows in a linked micromap graphic.  
    The default value is 1.75. 
   }
  \item{IDHdr}{is a two element character vector used to modify the id 
    glyph headers in \var{micromapST}. The two values are entered as 
    \code{IDHdr=c("1stHdr","2ndHdr")}. The defaults for this parameter 
    are \var{""} and \var{"States}.  See \var{micromapST} documentation 
    for more details.
   }
  \item{LabelCex}{a numerical value indicating the cex multiplier to 
    use when drawing the Map Labels (MapL) on the first map in a linked 
    micromap graphic.  The default value is 0.4 to match the micromapST maps.
   }
  \item{ReducePC}{a numerical value between .01 and 100 %.   100% is used to 
    tell the package to not reduce the number vertex in the shapefile. This is 
    change from earlier releases where 0 to 1 could be used to represent 0 to 100%.
    With the use of smaller keep values, the scale of the parameter can't be 
    determined. Reduced percentage below 1 are common.  Therefore, 0.65 is not
    65 % but a 0.65 % keep value.  It represents the presentage of vertex 
    that be remaining in the shape file after simplification by rmapshaper. 
    The default value is 1.25 %.  In many cases the minimizing can be reduced 
    to even 0.65% or more. Finding the best value may require multiple runs of 
    BuildBorderGroup to determine how this factor attects your boundary 
    data. 
   }
  \item{proj4}{is a character string representing a projection using the 
    Proj4 notation. The transformation to this projection is done as the 
    last step in the processing of the shape file before converting the 
    boundary data into the micromapST boundary data.frame. \option{proj4} 
    is provided, the projection in the original shape file is used.
    If the projection in the original shape file is missing or Long/Lat, 
    then the function will create an Albers Equal Area protection centered
    on the centroid of the map's area.
   }
  \item{checkPointReStart}{The \var{BuildBorderGroup} function allows for 
    the builder to manually adjust the shape file, just before building the 
    \var{micromapST} boundary data.frame. During the normal process, the 
    function writes check point images of the shape file, areaParms 
    data.frame, and the Name Table data.frame to the "CheckPoint" folder 
    in the \option{NameTableDir} folder. The builder can inspect and 
    modify any of the tables and the shape file, but must be very 
    careful with what and how they are modified.  When done, the builder 
    can re-issue the \var{BuildBorderGroup} function call with the
    \option{checkPointReStart} parameter set to \var{TRUE}.  
    The function will bypass all of the processing up to the check point,
    then read in the check point files and continue building the border 
    group. This will frequently be required when the map region contains
    many small area that will not be seen when shaped and simple scaling 
    or shifting does not produce the desired arrangement of the areas.
   }
  \item{debug}{is a numerical value from 0 to 65536.  It is used by the 
    developers to turn on specific actions or information displays to
    aid in the debugging and troubleshooting this function.  The 
    developers assigned a single actions to each bit in a 16 bit integer.
    In this way, multiple actions can be requested without any posibility
    of them interferring with each other.  For example: the value
    of 512 (b'00000010 00000000') is assigned to plotting the border
    group map at each of the 4 stages of processing the shapefile.  The 
    four stages are: 1) RAW, just read; 2) after being process by rmapshaper;
    3) after the polygons are manipulated as specified in the Name Table;
    and finally 4) the boundaries after they are converted into the point
    table format (VisBorders) for use by micromapST.  The output plots 
    are saved as PDF files. All that is needed is to set debug=512.  
    Another action the developers created is to save the plots created 
    by the 512 action in PNG type files, not PDF. This can be done by 
    setting debug = 512+128 = 640, in binary: b'00000010 00000000' OR 
    b'00000000 10000000' = b'00000010 10000000'.  The integer values are 
    much easier to work with then the long 0s and 1s binary represention 
    of the bits. Default is 0. 
    
    The full table of assigned values is: 
    \tabular{lll}{
      bit #\tab	value\tab definition\cr
      1\tab       1\tab  line by line debugging is being used and some code accomodates are required.\cr 
      2\tab	  2\tab  Outputs variable data to trace the function processes\cr
      3\tab       4\tab  Display Information related to projection processing\cr
      4\tab       8\tab  Plot intermediate Shape file, SF and SPDF (not the same plots as generated by 256 or 512)\cr
      5\tab       16\tab Display processing and variables related to the SF\cr
      6\tab       32\tab Display processing and variables related to the Name Table\cr
      7\tab       64\tab Display other internal variables during processing of the data\cr
      8\tab       128\tab = 0 sets output file type for the 512 option to PDF (default)\cr
       \tab          \tab = 1 sets output file type for the 512 option to PNG\cr
      9\tab       254\tab Generate multiple plot graphics of the map using a small format similar to the image\cr
       \tab          \tab   in the linked micromap with each plot with each plot having only 5 areas shaped.\cr
       \tab          \tab   Number of images = Areas/5 + 1.\cr
      10\tab      512\tab Generate a 4" x 4" plot of the area at key processing stage: RAW, After rmapshaping,\cr 
        \tab         \tab   After Name Table modifications, and after transformation and the conversion \cr
        \tab         \tab   to the micromapST VisBorder format (Final).\cr
      11\tab      1024\tab Same as 512, but only generates 4" x 4" plots for the RAW and Final images.\cr
      12\tab      2048\tab Display the final BorderGroup map for each layer on the screen: Areas, Level 2,\cr
        \tab          \tab   Regions, Map Outline (level 3). Each is display in a separate window and \cr
        \tab          \tab   must be manually closed.\cr
      13\tab      4096\tab Future Use - not assigned.\cr
      14\tab      8192\tab Write to disk a PDF file of multiple area boundary maps with 5 areas colored in each\cr
        \tab          \tab   map as done for the 256 option.\cr
      15\tab      16384\tab Future Use - not assigned.\cr
      16\tab      32768\tab Future Use - not assigned.\cr
     }
    Using any of these debug options will greatly increase the size of the output generated by BuildBorderGroup
    and should not be used unless requested by the package developer.
   }
 }
 \details{
 The output of this function is a single R dataset containing 6 data.frames 
 and a text report for inclusion in documentation for the border group.
 The details on each of the 6 data.frames and their contents can be found in the 
 bordGrp section.
 
 The default border group is \var{USStatesBG} to be compatible with older 
 R scripts using previous versions of the \var{micromapST} package.  
  
 Each of the border groups contained in this package have detail descriptions of
 the specific geographic area they represent. See the "xxxxx"BG section for each 
 border group:
 
    \var{USStatesBG}
    \var{USSeerBG}
    \var{KansasBG}
    \var{MarylandBG}
    \var{NewYorkBG}
    \var{UtahBG}
    \var{UKIrelendBG}
    \var{ChinaBG}
    \var{SeoulSKoreaBG}
    \var{AfricaBG}
    
 }

\value{Path to the saved Border Group file.}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\seealso{
  \link{micromapST},
  \link{micromapSEER}
}
\examples{

# Load libraries needed.
stt1 <- Sys.time()
library(stringr)
library(readxl)
library(sf)

# Generate a Kentucky County Border Group
#
# Read the county boundary files.  (Set up system directories. 
#   Replace with your directories to run.)
TempD<-"c:/projects/statnet/"  # my private test PDF directory exist, 
                               #don't use temp.
# get a temp directory for the output PDF files for the example.
if (!dir.exists(TempD)) {
     TempD <- paste0(tempdir(),"/") 
     DataD <- paste0(system.file("extdata",package="micromapST"),"/")
} else {
     DataD <- "c:/projects/statnet/r code/micromapST-3.0.2/inst/extdata/"
}

cat("Temporary Directory:",TempD,"\n")
# get working data directory
#cat("Working Data Directory:",DataD,"\n")

KYCoBG  <- "KYCountyBG"  # Border Group name
KYCoCen <- "KY_County"   # shape file name(s)

KYCoShp <- st_read(DataD,KYCoCen)
st_crs(KYCoShp)  <- st_crs("+proj=lonlat +datum=NAD83 +ellipse=WGS84 +no_defs")

# inspect name table
KYNTname <- paste0(DataD,"/",KYCoCen,"_NameTable.xlsx")
#cat("KYNTname:",KYNTname,"\n")

KYCoNT    <- as.data.frame(read_xlsx(KYNTname))
#head(KYCoNT)
spt1 <- Sys.time()
cat("Time to get data and boundaries for Counties:",spt1-stt1,"\n")
\dontrun{
#
#  building border group for all counties in Kentucky
#
stt2 <- Sys.time()
# Build Border Group
BuildBorderGroup(ShapeFile     = KYCoShp,
                 ShapeLinkName = "NAME",
                 NameTableLink = "Name",
                 NameTableDir  = DataD,
                 NameTableFile = paste0(KYCoCen,"_NameTable.xlsx"),
                 BorderGroupName = KYCoBG,
                 BorderGroupDir  = TempD,
                 MapHdr        = c("","KY Counties"),
                 IDHdr         = c("KY Co."),
                 ReducePC      = 0.9
                )
                
# Setup MicromapST graphic
spt2 <- Sys.time()
cat("Time to build KY Co BG:",spt2-stt2,"\n")
stt3 <- spt2
KYCoData  <- as.data.frame(read_xlsx(paste0(DataD,"/",
                           "KY_County_Population_1900-2020.xlsx")))
#head(KYCoData)

KY_Co_PD <- data.frame(stringsAsFactors=FALSE,
                 type=c("map","id","dot","dot"),
                 lab1=c(NA,NA,"2010 Pop","2020 Pop"),
                 col1=c(NA,NA,"2010","2020")
              )

KYCoTitle  <- c("Ez23ax-Kentucky County","Pop 2010 and 2020")
OutCoPDF   <- paste0(TempD,"Ez23ax-KY Co 2010-2020 Pop.pdf")
grDevices::pdf(OutCoPDF,width=10,height=13)   # on 11 x 14 paper.

micromapST(KYCoData,KY_Co_PD,sortVar=c("2020"), ascend=FALSE,
           rowNames="full", rowNamesCol = c("Name"),
           bordDir = TempD, bordGrp = KYCoBG, 
           title   = KYCoTitle
          )

x <- dev.off()
spt3 <- Sys.time()
cat("Time to micromapST KY Co graph:",spt3-stt3,"\n")
}   # end of dontrun.

stt4 <- Sys.time()

# Aggregate Kentucky Counties into ADD areas
#
# The regions in the Kentucky County Name Table (KYCoNT) are the ADD districts
# the county was assigned to.
# The KYCoShp has the county boundaries.
#
KYCoShp$NAME   <- str_to_upper(KYCoShp$NAME)
KYCoNT$NameCap <- str_to_upper(KYCoNT$Name)

aggInx <- match(KYCoShp$NAME,KYCoNT$NameCap)
#print(aggInx)

xm     <- is.na(aggInx)  # which polygons did not match the name table?
if (any(xm)) {
  cat("ERROR: One or more polygons/counties in the shape file did not match\n",
      "the entries in the KY County name table. They are:\n")
      LLMiss <- KYCoNT[xm,"Name"]
      print(LLMiss)
      stop()
}
#  

#####
#  aggFUN - a function to inspect the data.frame columns and determine
#    an appropriate aggregation method - copy or sum.
#
aggFUN <- function(z) { ifelse (is.character(z[1]), z[1], sum(as.numeric(z))) } 
#
#####

#
aggList  <- KYCoNT$regID[aggInx]
#print(aggList)

KYADDShp <- aggregate(KYCoShp, by=list(aggList), FUN = aggFUN)
names(KYADDShp)[1] <- "regID"  # change first column name to "regNames"
row.names(KYADDShp) <- KYADDShp$regID

KeepAttr <- c("regID","AREA","PERIMETER","STATE","geometry")
KYADDShp <- KYADDShp[,KeepAttr]
st_geometry(KYADDShp) <- st_cast(st_geometry(KYADDShp),"MULTIPOLYGON")

#plot(st_geometry(KYADDShp))
spt4 <- Sys.time()
cat("Time to aggregate KY ADDs from Cos:",spt4-stt4,"\n")
stt5 <- spt4
# Build Border Group

BuildBorderGroup(ShapeFile       = KYADDShp,  
        # sf structure of shapefile of combined counties into AD Districts
                 ShapeLinkName   = "regID",
                 NameTableFile   = "KY_ADD_NameTable.xlsx",
                 NameTableDir    = DataD,
                 NameTableLink   = "Index", 
                 BorderGroupName = "KYADDBG",
                 BorderGroupDir  = TempD,
                 MapHdr          = c("","KY ADDs"),
                 IDHdr           = c("KY ADDs"),
                 ReducePC        = 0.9
               )

spt5 <- Sys.time()
cat("Time to build ADD BG:",spt5-stt5,"\n")
stt6 <- spt5
# Test micromapST
KYADDData <- as.data.frame(readxl::read_xlsx(
                            paste0(DataD,"KY_ADD_Population-2020.xlsx")),
                           stringsAsFactors=FALSE)
#
KY_ADD_PD <- data.frame(stringsAsFactors=FALSE,
                 type=c("map","id","dot","dot"),
                 lab1=c(NA,NA,"Pop","Proj. Pop"),
                 lab2=c(NA,NA,"2020","2030"),
                 col1=c(NA,NA,"DecC2020","Proj2030")
              )
#
KyTitle <- c("Ez23cx-KY Area Development Dist.",
             "Pop 2020 and proj Pop 2023")
OutPDF2 <- paste0(TempD,"Ez23cx-KY ADD Pop.pdf")

grDevices::pdf(OutPDF2,width=10,height=7.5)

micromapST(KYADDData,KY_ADD_PD,sortVar="DecC2020",ascend=FALSE,
           rowNames= "full", rowNamesCol = "ADD_Name",
           bordDir = TempD,
           bordGrp = "KYADDBG",
           title   = KyTitle
          )
x <- grDevices::dev.off()
spt6 <- Sys.time()
cat("Time to do micromapST of KY ADDs:",spt6-stt6,"\n")
}

