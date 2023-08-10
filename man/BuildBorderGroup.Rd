\name{BuildBorderGroup}
\alias{BuildBorderGroup}
\title{Building new border groups for special areas for Linked Micromap created 
by the micromapST package}
\description{
 The package's \var{micromapST} function created linked micromaps for 
 any geographic region. The information related to the selected 
 regions is provided \var{micromapST} via a \var{bordergrp} dataset
 that contains all of the operational information and the boundary 
 data in a single dataset.  The dataset consists of several data.frames
 of the boundary data and a name table to map the builder's location IDs to 
 the area's boundary data. The border groups were originally created 
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
 form was added to handle geographic area that have two sets of commonly used 
 abbreviations.  The name table was expanded to contain both sets of abbreviations
 and allows the user of the border group to select the abbreviation that matches
 data frame location ids.  The "Alias" identifier was implement to handle a special case
 where the source of the statistical data did not use the accepted "Name" or 
 "Abbr" ids, but used a more generallized string.  To keep the matching as flexiable 
 as possible, a wildcard matching alias was introduced.  As each identifier in the data
 is examined, it is compared against the alias string with leading and traiing "*" matching
 character.  As long as the Alias strings are unique and do not appear in two data rows,
 the matching provide the link between the data and the geographical area. Over the past 
 10 years, the source program has been changed many times, but the location id 
 label matches has continued to work.\cr
 The user created name table also provides additional information 
 regional identification and sub-setting of the map, and area labeling. 
 During the border group building process, the name table 
 The Name Table also provides parameters to the \var{BuildBorderGroup} 
 function to make specific modifications to areas. The modifications to areas 
 include shifting it location, scaling it size, and rotating the area. 
 These modifiers were used to scale Alaska to 35% of it's normal size and move it 
 to below California to reduce the size of the total map.  Hawaii was also moved 
 below the U. S. to reduce the total size of the map. \cr  
 The name table also allows the builder to assocate areas with regions in their map and 
 Level 2 border outlining. Check the \var{micromapST} documentation on 
 how the Level 2 and Region boundaries are to be used.  The most important 
 task the name table does is help provide the linkage between the rows in the user's
 data frames to the an area's collection of polygons from the shapefile.
 The Name Table also provide multiple location ids to allow the user to provide the 
 full formal name ("Name"), one of two abbreviates (if available), or the numerical ID 
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
  \item{debug}{is a numerical value from 0 to 65536.  Each bit in the 
    value represents a group of dianostic displays to aid in debuging 
    the function. The values of 256 and 512 will instruct the function 
    to print the map at different stages of processing for review and 
    parameter adjustment.  Default is 0.
   }
 }
 \details{
 The output of this function is a single R dataset containing 6 data.frames 
 and a text report for inclusion in documentation for the border group.
 
 A definition of the 6 data.frames in the border group dataset are:
  \describe{
   \item{areaParms}{ - contains specific parameters and operational 
     information for the border group.
    }
   \item{areaNamesAbbrsIDs}{ - The Name Table containing the names (full text), 
     name abbreviations, numerical identifier, an alternate name abbreviation,
     wildcard string for name matching, regional and Level 2 collections of 
     areas in the border group area.  If the border group is for a state, 
     the areas would be the counties within the state.  If the border group 
     is for a continent, then the areas are the countries on the continent. 
     If the border group is for a country like the United States, then the 
     areas are the administrative areas (like states, provinces, 
     special administrative areas, or cities) within that country.
    }
   \item{areaVisBorders}{ - the boundary point lists for each area in 
     the border group.
    }
   \item{L2VisBorders}{ - when a border group needs to have an intermediate
     set of boundaries drawn for clarity, the border group can provide 
     the package a Level 2 set of boundaries via the L2VisBorders data.frame.
     The structure consists of the boundary information (point lists) of 
     these areas and associated keys. Each sub-area is linked to it's L2 
     boundary via the L2_ID variable (column) in the name table 
     (areaNamesAbbrsIDs data.frame). At this time only the U. S. Seer 
     Registry border group uses the L2 boundary overlays.  It uses 
     the L2VisBorders to draw the boundaries of U. S. States around 
     Seer Registries within a state. The areaParms$Map.L2Borders variable 
     in the border group must be set to TRUE for the package to draw the 
     Level 2 boundaries. If a border group does not use an intermediate 
     level outline, L2VisBorders should be set to L3VisBorders or NA 
     and the areaParms$Map.L2Borders variable set to FALSE. In this case, 
     no L2 boundaries are drawn.  The L2 boundary data is optional.
    }
   \item{RegVisBorders}{ - when the border group wants to work with the areas
     on a regional basis, the regID variable in the name table 
     areaNamesAbbrsIDs, the RegVisBorders boundary information data.frame, 
     and the areaParms$aP_Regions are used to enable the feature and 
     provide the information to map only regions of areas and overlay 
     areas with regional boundaries highlights.
     When the regions call parameter is set to TRUE and the selected 
     border group has the areaParms$aP_Regions set to TRUE, the 
     package will scan the data provided by the caller and determine 
     which regions are being referenced and which are not.  The package 
     uses the regID variable in the name table (areaNamesAbbrsIDs) 
     to associate a area with a region and as a key into the RegVisBorders
     data.frame to draw the region boundaries. 
     Any region not containing data and any L2 area and areas within 
     those regions will not be drawn. If a border group does not use 
     the regions feature the RegVisBorders data.frame in the border 
     group should be set to the L3VisBorders data.frame or NA and the 
     areaParms$aP_Regions variable set to FALSE.  This will disable the 
     regions feature for the border group.  Boundary data for regions 
     is optional.
    } 
   \item{L3VisBorders}{ - the boundary point list for an outline of the 
     entire geographica area referenced by the border group. For the U.S. 
     or a country border group, this is the outline of the country. 
     For a state border group like Kansas, this is an outline of the state. 
     For smaller areas like Seoul, it is an outline of the city. 
     The L3VisBorders data frame must be present in the border group. 
     The L3VisBorders are not drawn if the regional sub-mapping
     feature is active and not all of the map is drawn.
    }
  }
 
The default border group is \var{USStatesBG} to be compatible with older 
R scripts using previous versions of the \var{micromapST} package.  
  
This section provide the data frame structural details of each of 
data.frame and variables in the border group dataset.

  \describe{
    \item{areaParms data.frame}{ The \var{areaParms} data.frame contains 
      specific information about and in support of its border group.  
      It provides column headers for the map and id glyphics and controls 
      several features that related to handling a border group by 
      the \var{micromapST} package. There controls do not change 
      from \var{micromapST} call to call and are therefore not part of 
      the calling parameter set.  
 
      For example, there are several built in titles and labels for the 
      map and id glyphics. These will change for different border groups.  
      For the \var{USStatesBG} border group, the map title is always 
      "U.S. States", while in the \var{USSeerBG} border group the map 
      glyphic title is "U.S. Seer Areas".
      
      This dataset contains the specific values related to a specific 
      border group for the following variables.  

      The areaParms data.frame for each border group dataset contains 
      the following variables: 
      \describe{
        \item{areaUSData}{ - logical variable.  If \var{TRUE} then the 
          border group represents the entire US area and labels are placed 
          on the first map for Alaska, Hawaii, and DC. This is option is 
          only used with the \var{USStatesDF} and \var{USSeerBG} border 
          groups.  For the state/county border groups and foreign country 
          border groups, \var{areaUSData} must be set to \var{FALSE}.
         }
        \item{enableAlias}{ - If \var{TRUE}, enables the use of the 
          \option{rowNames}=\var{alias} call argument option. This permits 
          a partial ("contains") match of the data in the \option{rowNamesCol} 
          link column in the \var{statsDFrame} or the row.names of the 
          \var{statsDFrame} data.frame. This is only supported in the 
          \var{USSeerBG} bordGrp to allow direct use of data exported
          from the SEER*Stat website and match on the exported SEER*Stat 
          registry names. This feature can be expanded when needed to other 
          border groups.
         }
        \item{aP_Regions}{ - If \var{TRUE}, the package contains a valid 
          \var{RegVisBorders} data.frame and the name table 
          (\var{areaNamesAbbrsIDs}) contains a key for each area to 
          the region boundaries in the \var{regID} variable.  If the 
          caller set the \option{regions} parameter to \var{TRUE},
          the package will only draw regions and their boundaries 
          if the region contains areas with data.  For examples: 
          this allows you to provide data for the west coast U. S. 
          states and not have the midwest, south, or northeastern
          states drawn.  This feature is available to all border 
          groups, but is currently only used by the USStatesBG, 
          USSeerBG, UKIrelandBG and ChinaBG border groups.
          If set to \var{FALSE}, the regions feature is disabled 
          and all regional information ignored.  The \var{RegVisBorders} 
          should still exist, but should be set to equal the 
          \var{L3VisBorders} data.frame. The \option{regions} call 
          parameter will be ignored.
          As an example: In the USStatesBG and USSeerBG border groups, 
          the regions are set up using the 4 U. S. census regions of: 
          West, South, Midwest, and NorthEast. If only data 
          for states in the NorthEast are passed to micromapST, only 
          the NorthEast region and its states will be mapped when 
          \option{regions} is set to \var{TRUE}.  If \option{regions} 
          is set to \var{FALSE} then all of the boundaries for all 
          of the U. S. States and DC are drawn eventhough data was 
          only provided for the states in the northeast region.
          This feature also allows a border group with a large number 
          of area, like the UK and Ireland border group to be assembled 
          and used on a regional basis with fewer area were the 
          full border group is not really usable with linked 
          micromap presentations.
         }
        \item{ID.Hdr.1}{ - 1st title for the id type glyph column.
         }
        \item{ID.Hdr.2}{ - 2nd title for the id type glyph column.
         }
        \item{Map.Hdr.1}{ - 1st title for the map type glyph column.
         }
        \item{Map.Hdr.2}{ - 2nd title for the map type glyph column.
         }
        \item{LabelCex}{ - a number representing the cex multiplier used 
         on the \var{text} function when the map labels are drawn.
         }
        \item{Map.Aspect}{ - is the X/Y aspect ratio for the map borders 
          in this border group.  This is used to adjust the map glyphic 
          to keep the area's aspect looking correct.
         }
        \item{Map.MinH}{- the minimum height of a group/row in inches
         }
        \item{Map.MaxH}{- the maximum height of a group/row in inches.
         }
        \item{bordGrp}{ - a character vector of the name of the 
          border group.
         }
        \item{Map.L2VisBorders}{ - logical variable. If \var{TRUE}, 
          the L2VisBorder border overlay is drawn on the map glyphics.  
          If \var{FALSE}, the \var{L2VisBorders} boundaries are not 
          drawn. This option is currently only used by USSeerBG to 
          draw state boundaries for states containing multiple Seer 
          Registries.
         }
        \item{aP_Regions}{ - logical variable.  If \var{TRUE}, the 
          regions feature is enabled. When the feature is enabled, the 
          \var{RegVisBorders} data.frame should be included in the 
          border group, but it is not required.  The key to the regional 
          feature is the \var{regID} column in the name table 
          (\var{areaNamesAbbrsIDs}) that identifies the region 
          associated with the area and the regional boundaries in the 
          \var{RegVisBorders} data.frame.  If \var{FALSE}, the regions 
          feature is disabled. If the border group contains a name table 
          with regID and regName columns that contain duplicate values, 
          meaning the region information is usable, then the \var{aP_Regions} 
          variable is set to \var{TRUE}.
         }
        \item{aP_Proj}{ - a character vector containing the projection used to
          create the areaVisBorders, L2VisBorders, RegVisBorders, and L3VisBorders
          boundary point lists.
         }
        \item{aP_Units}{- a character vector containing the measurement units of the 
          coordinates in the VisBorders boundary point lists.  
         }
        \item{Map.L3VisBorders}{ - logical variable. If \var{TRUE}, the L2VisBorder 
          boundary data is available to drawn on the map glyphics.  If \var{FALSE}, 
          the \var{L2VisBorders} boundaries are not available and are not drawn. 
          This option is currently only used by the following border groups:  
          USSeerBG.
         }
        \item{Map.RegVisBorders}{ - logical variable. If \var{TRUE}, the 
          \var{RegVisBorder} boundary data is available in the border group 
          and can be used to drawn a regional boundary overlay on the 
          map glyphics.  If \var{FALSE}, the \var{RegVisBorders} boundary 
          data is not available and regional boundaries are not drawn. 
          This set of boundaries are only available in the following border 
          groups: USStatesBG, USSeerBG, ChinaBG, and UKIrelandBG.  The drawing 
          of the regional boundaries is controlled by the \option{regionB} 
          call parameter.
         }
       }
     }
    \item{areaNamesAbbrsIDs}{The \var{areaNamesAbbrsIDs} data.frame provides 
      the linkages between the fullname, abbreviation, and numerical identifier 
      to link the \var{statsDFrame} data to the boundaries for the county areas.  
      It is also used to validate the incoming data to ensure the linkage can 
      be established.  Within the boundary files, the area abbreviation is the 
      key linkage. areaNamesAbbrsIDs data.frame.
 
      areaNamesAbbrsIDs dataset provides a table to permit the translation 
      of a numerical ID (e.g., FIPS codes), area abbreviatation (should be 
      less than 4-6 characters), optional alternate abbreviation,
      and full area names in the \var{micromapST} function. 

     \describe{
       \item{Name}{- character string of each sub-area name.  Used to link 
         the areas to boundaries when \option{rowNames} = "full" is speccified.  
         Used as the represented name of the sub-area in "ID" glyphics columns 
         when \option{plotNames} = "full" is spedified (default).
        }
       \item{Abbr}{- the name abbreviation for each sub-area. Should 
         be 2 to 3 character, but no more than 6 characters. Used to link the 
         sub-areas to boundaries when \option{rowNames} = "ab" is 
         specified. Used as the represented name of the area in "ID" glyphics 
         columns when \option{plotNames} = "ab" is specified (default).
        }
       \item{Alt_Abr}{- an alternate name abbreviation for each sub-area.  
         Should be 2 to 3 characters, but no more than 6 characters.  Most of the 
         time this field is identical to the \var{Abbr} field. In some cases, 
         multiple sets of authenticated abbreviations were found for the sub-areas 
         in a continent or country.  When this happened, the most common 
         abbreviation should be placed in the \var{Abbr} field and the second 
         abbreviation placed in the \var{Alt_Abr} field.  This features
         allows the border group to be used by a wider audience.  
         To access the \var{Alt_Abr} abbreviates, set the \option{rowNames} call 
         argument/parameter to "alt_ab".  The labels in the \var{statsDFrame} 
         statistics data.frame will be matched against the alternate abbreviations.
        }
       \item{ID}{- numerical identifier for each sub-area.  Used to link the data 
         to boundary information when \option{rowNames} = \var{id} is specified.  
         The row.names in the user provided data.frame or specificed 
         \option{rowNamesCol} column must match the IDs in the name table.  
         If no match a warning is generated and the data row ignored.
        }
       \item{Alias}{- a character string for each area used to do a wildcard 
         match ("contains") with the \option{rowName} or \option{rowNamesCol} 
         specified in the \var{micromapST} call when the \var{USSeerBG} border 
         group is used. When a match is completed, the abbreviation is used 
         to link the user's data to the boundary data.  The alias match is done 
         when \option{rowNames} is set to "alias".  There should be only one match 
         in the data for each sub-area alias.  If more are found, an error is 
         raised.  This feature is only supported in the \var{USSeerBG} border group.
        }
       \item{L2_ID}{- is the level 2 identifier.  Used to link the sub-area
         to the L2VisBorders boundary data point data.frame.  In the case of the 
         USSeerBG border group, the L2 boundaries are state boundaries and the L2_ID
         value is the state 2 character abbreviation.
        }
       \item{L2_ID_Name}{- is the full name L2 area.  
        }
       \item{regID}{- is the region identifier.  Used to link the sub-area
         to the RegVisBorders boundary data point data.frame.  The USStatesBG 
         and USSeerBG border groups use this field to like the sub-areas to the
         four (4) U. S. Census regions (Northeast, South, MidWest, and West)
         This association is used with the \option{regions} call parameter
         to determine which regions and their sub-areas, etc. will be 
         drawn when the caller provide data is mapped.
        }
       \item{regName}{- is the full name of a region.  
        }
       \item{Key}{- is a character string used to link the name table to the 
         boundary data in the VisBorders data.frames.
        }
       \item{Link}{- when the initial border group is created, the \var{Key}, 
         \var{Name} or the \var{Abbr} variables may not be able to provide 
         a link to the original boundary data. When this happens, the border 
         group creater must use an alternate "link" to tie the
         name table to the boundary data.  The \var{link} field is used 
         to accomplish this in the package provided border group building 
         steps and functions.  Once the border group is fully constructed, 
         the \var{Link} field is not use.
        }
       \item{MapL, MapX, MapY}{- when an area is moved or enlarge on a map 
         for better visibility, it may not be clear to the user what 
         sub-area is being represented.  In the U. S. border groups, 
         the states of Alaska, Hawaii and the District of Columbia 
         were modified and relocated to a new position and/or size. 
         The \var{MapL} column value can be used to draw a label for the 
         area on the first map drawn in a linked micromap graphic.  The 
         \var{MapX} and \var{MapY} columns provide the coordinates 
         for the label on the map. The text label should be as small as possible, 
         at most 2 characters.  This should only be done if absolutely necessary. 
         If too many area's are labeled, the map glyphs becomes unreadable.
         If value is missing (NA) or any of the components are missing (NA), 
         then the label is not drawn.  The x/y coordinate represent 
         the lower left position of the location to print the string and must 
         be in the same coordinates system used by the original shape file, but may 
         be shifted to the areas new location.  The size of the \var{MapL} label 
         is controled by the \var{LabelCex} variable in the border group.
        }
      }
     }

    \item{areaVisBorders, L2VisBorders, RegVisBorders, and L3VisBorders}{There 
        are four boundary dataset:
        The boundaries of the areas being micromapped (counties). (areaVisBorders)
        The boundaries of an intermediate level for general orientation. 
        (L2VisBorders)
        The boundaries of regional areas for highlight overlays and 
        regional only mapping. (RegVisBorders)
        The boundaries of the an outline of the entire mapping area. 
        (L3VisBorders)

      These four data.frames contain the boundary point lists for the 
      area, regions and areas.  
      The L2VisBorders, RegVisBorders, and L3VisBorders data.frames 
      are used to outline groups of areas, regions of the area and the 
      entire area.  
      The areaVisBorders data.frame contains the point lists for 
      each areas and are keyed to the name table (\var{areaNamesAbbrsIDs}).
     
      The data structure for each of the following four boundary data.frames is:

          seq Key  x      y      hole
            
      \describe{
        \item{seq}{- a numerical sequence number of the boundary points in 
          the data.frame.  (optional)
         }
        \item{Key}{- the Key field had different uses in each of the 
          four VisBorders structured data.frames.
          In the areaVisBorders data.frame, the \var{Key} is the unique 
          key for the sub-area as defined in the name table 
          (\var{areaNamesAbbrsIDs}).  
          All of the points with the same \var{Key} form one or more polygons 
          and represent boundaries of a sub-area.  This allows the package 
          to locate the boundary data for a specific sub-area when needed.
          
          In the RegVisBorders boundary data.frame, the \var{Key} 
          is the region ID associated with the boundary points (polygons).
          One or more sub-areas can be linked to an region boundary via the 
          \var{regID} variable in the name table.  The USStatesBG, USSeerBG,
          UKIrelandBG,and ChinaBG border groups may use of the regions feature.
        
          In the L3VisBorders, all of the \var{Key} field values area set to
          a name that represents the entire border group area.  The \var{Key}
          field is not used in drawning the area's outline.
          The level 3 boundary outline data.frame is always drawn 
          when the entire geographic area is mapped. If not all of the regions 
          are being mapped, then the L3 boundary is not drawn. 
         }
        \item{x}{- a numerical value for the \var{x} coordinates of a polygon.  
          The end of the polygon is signaled by an \var{x} value of \var{NA}.
          The first point in the polygon does not have to be repeated. 
          The polygon draw function used by micromapST will close the figure. 
          An area may be composed of several polygons. 
          Holes in areas are also represented by polygons and are associated 
          with the sub-area.
         }
        \item{y}{- a numerical value for the \var{y} coordinates of a polygon.  
          The end of the polygon is signaled by an \var{y} value of \var{NA}.
         }
        \item{hole}{- a logical value. If \var{TRUE}, the associated polygon 
          is a hole within the sub-area identified by the \var{Key} field. 
          A hole polygon is always drawn using the maps background color. 
          For this reason, areas containing holes (lakes, rivers, 
          or other areas), must proceed any area in the data.frame 
          that it may overlay with the hole.  For example, if one area 
          "A" is contained within area "B", area "B" must have 
          a hole where area "A" is located and must preceed
          area "A" in the VisBorders data.frame.  In this way, 
          area "B" and its hole are drawn before area "A" preventing 
          area "B" hole from overlaying area "A".
         }  
       }
   
      Hole are processed by re-drawing the hole area with the current 
      background color. Therefore, any area with holes must be in the 
      data.frame prior to any areas that may occupy the hole's space 
      in the map.

      The order of the area's boundaries in these files is very important 
      to allow correct processing of the holes and any areas that overlay holes.  
      Holes are not unpainted polygons, but polygons repainted back to 
      the background color.
      The order should be areas with holes must preceed areas that overlay 
      hole space.  
      This is required to ensure an area is not over-painted by an area's hole 

      Each data.frame should be validated before using to make sure they are 
      clean and will not generate errors when used by \var{micromapST}.
    }

    Each border group contains the same six data.frames using the same six 
    names.  This allows the micromapST package the ability to quickly load 
    a particular border group and create the requested micromaps.
     
    See the write up on each included border group for details on the 
    specific content of their border group dataset and the list of sub-area 
    names, abbreviations, and id that should be used to link the data to the 
    boundary information.
   
    The \option{regions} feature and RegVisBorder overlays are only supported 
    in the following provided border groups:
   
    \var{USStatesBG}
    \var{USSeerBG}
    \var{UKIrelendBG}
    \var{ChinaBG}
    
    border groups.
    
    In this version of \var{micromapST} a function is included 
    \var{BuildBorderGroup} to assist the user in converting a 
    shape file into their own borderGrp dataset for \var{micromapST}.  
    Refer to the section on \var{BuildBorderGroup} for more details.
   }
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
     DataD <- "c:/projects/statnet/r code/micromapST-3.0.0/inst/extdata/"
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

BuildBorderGroup(ShapeFile       = KYADDShp,  # sf structure of shapefile of 
                                         # combined counties into AD Districts
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
KYADDData <- as.data.frame(readxl::read_xlsx(paste0(DataD,"KY_ADD_Population-2020.xlsx")),
             stringsAsFactors=FALSE)
#
KY_ADD_PD <- data.frame(stringsAsFactors=FALSE,
                 type=c("map","id","dot","dot"),
                 lab1=c(NA,NA,"Pop","Proj. Pop"),
                 lab2=c(NA,NA,"2020","2030"),
                 col1=c(NA,NA,"DecC2020","Proj2030")
              )
#
KyTitle <- c("Ez23cx-KY Area Development Dist.","Pop 2020 and proj Pop 2023")
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

