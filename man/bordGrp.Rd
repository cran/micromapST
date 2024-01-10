\name{bordGrp}
\docType{data}
\alias{bordGrp}
\alias{areaParms}
\alias{areaNamesAbbrsIDs}
\alias{areaVisBorders}
\alias{L2VisBorders}
\alias{RegVisBorders}
\alias{L3VisBorders}

\title{Defining different spatial areas for Linked Micromap using
   the micromapST package}

\description{
 The \var{micromapST} function can be used to create linked
 micromaps for may different spatial areas by using different
 border groups. Several border group (bordGrp) examples are
 contained in the package and include the 51 states and DC of the
 United States, the counties of Kansas, Maryland, New York, Utah,
 the countries and provinces of the U.K. and China, and the U. S.
 Seer Registries used by the National Cancer Institute.  Each
 border group is a different dataset containing the unique
 boundaries and operational information to allow micromapST to
 work in a different spatial area. The structure of each border
 group dataset is identical with the same variable names and
 types of structures. A user can build their own border group
 dataset to meet their specific spatial area needs.  Because the
 package contains several border group datasets, the use of
 lazydata or lazyloading has been disabled.

 The name of the border group is specified in the
 \option{bordGrp} call parameter. To permit a user to reference a
 border group dataset not contained in the package, and reside in
 a user's folder, the \option{bordDir} must be used to direct the
 package to the border group.  The border group must be saved
 under R using the \var{save} function with the file extension of
 ".rda". For example: bordGrp="private",
 bordDir="c:/SavedBorderGroups"

 Each border group contain six (6) datasets by the same
 data.frame names. This allows the micromapST package the 
 ability to quickly load a particular border group and 
 create the requested micromaps. The five data.frames are: 
 areaParms, areaNamesAbbrsIDs, areaVisBorders, L2VisBorders, 
 RegVisBorders, and L3VisBorders. Since the same data.frame 
 names are reused in each border group, the R lazyload 
 feature is disabled in the package.

 The following describes the purpose and structure of each
 data.frame in the border group dataset:
 \describe{
   \item{\option{areaParms}}{ - contains specific parameters and
     operational information for the border group
    }
   \item{\option{areaNamesAbbrsIDs}}{ - containing the names 
     (full text), name abbreviations, wildcard string for name 
     matching, alternate name abbrevations, regional (Level 2) 
     association of the sub-area, and a numerical identifier 
     for the areas in the border group.  If the border group 
     is for a state, the areas would be the counties within 
     the state.  If the border group is for a continent, then 
     the areas are the countries on the continent. When the 
     border group is for a country like the United States, 
     then the areas are the administrative areas (like states, 
     provinces, special administrative areas, or cities) 
     within that country.
    }
   \item{\option{areaVisBorders}}{ - the boundary point lists 
     for each area in the border group.
    }
   \item{\option{L2VisBorders}}{ - when a border group needs 
     to have an intermediate set of boundaries drawn for 
     clearity, the border group can provide the package a 
     layer 2 set of boundaries via the \var{L2VisBorders} 
     data.frame. The structure consists of the boundary 
     information (point lists) of these areas and associated 
     keys. Each area is linked to it's L2 boundary via the 
     \var{L2_ID} variable (column) in the name table 
     (\var{areaNamesAbbrsIDs} data.frame. At this time only 
     the U. S. States and U. S. Seer Registry border group 
     uses the L2 boundary overlays.  It uses L2VisBorders to 
     draw the boundaries of U. S. States around Seer Registries 
     within a state. The \var{areaParms$Map.L2Borders} variable 
     in the border group must be set to \var{TRUE} for the 
     package to draw the layer 2 boundaries. If a border group 
     does not use an intermediate level outline, L2VisBorders 
     should be set to L3VisBorders or NA and the 
     \var{areaParms$Map.L2Borders} variable set to FALSE. 
     In this case, no L2 boundaries are drawn.
    }
   \item{\option{RegVisBorders}}{ - when the border group 
     wants to work with the areas on a regional basis, the 
     \var{regID} variable in the name table (\option{areaNamesAbbrsIDs}), 
     the \option{RegVisBorders} boundary information data.frame, 
     and the \var{areaParms$aP_Regions} are used to enable the 
     feature and provide the information to map only regions 
     of areas and overlay areas with regional boundaries 
     highlights. When the \option{regions} call parameter is 
     set to \var{TRUE} and the selected border group has the 
     \var{areaParms$aP_Regions} set to \var{TRUE}, the package 
     will scan the data provided by the caller and determine 
     which regions are being referenced and which are not.  
     The package uses the \var{regID} variable in the name
     table(\option{areaNamesAbbrsIDs}) to associate a area with
     a region and as a key into the \var{RegVisBorders}
     data.frame to draw the region boundaries. Any region not
     containing data and any L2 area and areas within those
     regions will not be drawn. If a border group does not use
     the regions feature the \option{RegVisBorders} data.frame in
     the border group should be set to the L3VisBorders
     data.frame or NA and the \var{areaParms$aP_Regions}
     variablel set to \var{FALSE}. This will disable the regions
     feature for the border group.
    } 
   \item{\option{L3VisBorders}}{ - the boundary point list for an outline
     of the entire geographica area referenced by the border
     group.   For the U.S. or a country border group, this is the
     outline of the country. For a state border group like
     Kansas, this is an outline of the state. For smaller areas
     like Seoul, it is an outline of the city. The L3VisBorders
     data frame must be present in the border group. }
  }
 }
 \details{
  The default border group is \var{USStatesBG} to be compatible
  with older R scripts using previous versions of the
  \var{micromapST} package.

  This section provide the data frame structural details of each
  of the 6 data.frames and thier variables that make up a border group dataset. 
  
  \describe{
    \item{areaParms data.frame}{ The \option{areaParms} data.frame
      contains specific information about and in support of its
      border group. It provides column headers for the map and id
      glyphics and controls several features that related to handling a 
      border group by the \var{micromapST} package. These controls are 
      specific to a border group and do not changed from one \var{micromapST} 
      call to the next and don't have to be part of the calling parameters.

      For example, there are several built in titles and labels for the map 
      and id glyphics. These will change for different border groups. For 
      the \var{USStatesBG} border group, the map title is always 
      "U.S. States", while in the \var{USSeerBG} border group the map glyphic 
      title is "U.S. Seer Areas".

      This data.frame contains the following variables that are used by 
      micromapST when handling a border group.
      
      The areaParms data.frame variables are:
      \describe{
        \item{areaUSData}{ - logical variable.  If \var{TRUE} then the border 
          group represents the entire US area and labels will be placed on 
          the first map for Alaska, Hawaii, and DC. This is option is only 
          used with the \var{USStatesDF} and \var{USSeerBG} border groups.  
          For the state/county border groups and foreign country border 
          groups, \var{areaUSData} must be set to \var{FALSE}.  This 
          variable is being retired.
         }
        \item{enableAlias}{ - logical variable. If \var{TRUE}, enables the use of the "alias" 
          name matching feature and the \option{rowNames}=\var{alias} call 
          parameter. The "alias" name matching feature permits a partial 
          ("contains") match of the loc id data in the \option{rowNamesCol} 
          link column in the \var{statsDFrame} or the row.names of the
	  \var{statsDFrame} data.frame. At the present time, the "alias" 
	  feature is only used in the \var{USSeerBG} bordGrp.  It was 
	  implemented to be able to use directly use data generated by the
	  SEER*Stat website and match the loc ids to the internal SEER*Stat 
	  registry names in the border group. This feature can be expanded 
	  when needed to other border groups if the "Alias" column in the name
	  table is filled in with unique strings.
         }
        \item{aP_Regions}{ - logical variable. If \var{TRUE}, the package contains a valid 
          \var{RegVisBorders} data.frame and the name table 
          (\var{areaNamesAbbrsIDs}) contains the \var{regID} (or name) of a 
          region boundary associated with the area. If the caller set the 
          \option{regions} call parameter to \var{TRUE}, the package will 
          only draw areas in regions and the region boundaries if the region
          contains areas with data. For examples: this allows you to
	  provide data for the west coast U. S. states and not have the 
	  midwest, south, or northeastern states drawn.  This feature is 
	  available to all border groups, but is currently only used by the 
	  USStatesBG, USSeerBG, UKIrelandBG and ChinaBG border groups. If set 
	  to \var{FALSE}, the regions feature is disabled and all regional 
	  information ignored.\cr
	  The \var{RegVisBorders} should still exist, but should be set to 
	  equal the \var{L3VisBorders} data.frame. The \option{regions} 
	  call parameter will be ignored. As an example: In the USStatesBG 
	  and USSeerBG border groups, the regions are set up using the 
	  4 U. S. census regions of: West, South, Midwest, and NorthEast. 
	  If only data for states in the NorthEast are passed to micromapST,
	  only the NorthEast region and its states will be mapped when 
	  \option{regions} is set to \var{TRUE}.  If \option{regions} is set 
	  to \var{FALSE} then all of the boundaries for all of the U. S. 
	  States and DC are drawn eventhough data was only provided for the 
	  states in the northeast region. This feature also allows a border
	  group with a large number of sub-area, like the UK and Ireland 
	  border group to be assembled and used on a regional basis with 
	  fewer sub-area were the full border group is not really usable 
	  with linked micromap presentations.
         }
        \item{ID.Hdr.1}{ - character variable. 1st title for the id type 
          glyphics column. 
	 }
        \item{ID.Hdr.2}{ - character variable. 2nd title for the id type 
          glyphics column. 
	 }
        \item{Map.Hdr.1}{ - character variable. 1st title element for the 
          map type glyphics column. This variable is not implemented in this 
          release.
	 }
        \item{Map.Hdr.2}{ - character variable. 2nd title element for the map 
          type glyphics column. This variable provides the type of areas in 
          the map. This string should be kept to less than 12-16 characters.
	 }
        \item{Map.Aspect}{ - a numeric variable. The X/Y aspect ratio for the 
          map borders in this border group.  This is used to adjust the map 
          glyphic to keep the area's aspect looking correct. 
	 }
        \item{Map.MinH}{- a numeric variable. The minimum height of a 
          group/row in inches 
	 } 
	\item{Map.MaxH}{- the maximum height of a group/row in inches.
         }
        \item{LabelCex}{ - a number representing the cex multiplier used 
          on the \var{text} function when the map labels are drawn.
         }
        \item{bordGrp}{ - a character vector of the name of the border group. 
	 }
        \item{Map.L2VisBorders}{ - logical variable. If \var{TRUE}, the 
          L2VisBorder border overlay is drawn on the map glyphics.  If 
          \var{FALSE}, the \var{L2VisBorders} boundaries are not drawn. 
          This option is currently only used by USSeerBG to draw state
	  boundaries for states containing multiple Seer Registries. 
	 }
        \item{aP_Regions}{ - logical variable.  If \var{TRUE}, the regions 
          feature is enabled. When the feature is enabled, the 
          \var{RegVisBorders} data.frame should be included in the border 
          group, but it is not required.  The key to the regional feature 
          is the \var{regID} column in the name table 
          (\var{areaNamesAbbrsIDs}) that identifies the region associated 
          with the area and the regional boundaries in the \var{RegVisBorders}
	  data.frame. If \var{FALSE}, the regions feature is disabled.
         }
        \item{aP_Proj}{ - a character vector containing the projection used 
          to create the areaVisBorders, L2VisBorders, RegVisBorders, and 
          L3VisBorders boundary point lists.
         }
        \item{aP_Units}{- a character vector containing the measurement units
          of the coordinates in the VisBorders boundary point lists.
         }
        \item{Map.L3VisBorders}{ - logical variable. If \var{TRUE}, the 
          L2VisBorder boundary data is available to drawn on the map glyphics.
          If \var{FALSE}, the \var{L2VisBorders} boundaries are not available 
          and are not drawn. This option is currently only used by the
	  following border groups: USSeerBG.
         }
        \item{Map.RegVisBorders}{ - logical variable. If \var{TRUE}, the 
          \var{RegVisBorder} boundary data is available in the border group
          and can be used to drawn a regional boundaryy overlay on the map 
          glyphics.  If \var{FALSE}, the \var{RegVisBorders} boundary data 
          is not available and regional boundaries are not drawn.  This set 
          of boundaries are only only available in the following border 
          groups: USStatesBG, USSeerBG, ChinaBG, and UKIrelandBG.  The 
          drawing of the regional boundaries is controlled by the 
          \option{regionB} call parameter.
         }
       }
     }
    \item{areaNamesAbbrsIDs data.frame}{The \var{areaNamesAbbrsIDs}
      data.frame provides the linkages between the fullname, abbreviation, 
      and numerical identifier to link the \var{statsDFrame} data to the 
      boundaries for the county areas. It is also used to validate the 
      incoming data to ensure the linkage can be established.  Within the 
      boundary files, the area abbreviation is the key linkage. 
      
      The areaNamesAbbrsIDs dataset provides a table to permit the
      translation of a numerical ID (e.g., FIPS codes), area
      abbreviatation (should be less than 4-6 characters),
      optional alternate abbreviation, and full area names in the
      \var{micromapST} function.
      
     \describe{
       \item{Name}{- character string of each area name.
	 Used to link the areas to boundaries when
	 \option{rowNames} = "full" is specified. Used as the
	 represented name of the sub-area in "ID" glyphics
	 columns when \option{plotNames} = "full" is spedified
	 (default).
        }
       \item{Abbr}{- the name abbreviation for each area.
	 Should be 2 to 3 character, but no more than 6
	 characters. Used to link the sub-areas to boundaries
	 when \option{rowNames} = "ab" is specified. Used as the
	 represented name of the area in "ID" glyphics columns
	 when \option{plotNames} = "ab" is specified (default).
        }
       \item{Alt_Abr}{- an alternate name abbreviation for each
	 sub-area. Should be 2 to 3 characters, but no more than
	 6 characters.  Most of the time this field is identical
	 to the \var{Abbr} field. In some cases, multiple sets of
	 authenticated abbreviations were found for the sub-areas
	 in a continent or country.  When this happened, the most
	 common abbreviation should be placed in the \var{Abbr}
	 field and the second abbreviation placed in the
	 \var{Alt_Abr} field.  This features allows the border
	 group to be used by a wider audience. To access the
	 \var{Alt_Abr} abbreviates, set the \option{rowNames}
	 call argument/parameter to "alt_ab".  The labels in the
	 \var{statsDFrame} statistics data.frame will be matched
	 against the alternate abbreviations.
        }
       \item{ID}{- numerical identifier for each area.  Used
	 to link the data to boundary information when
	 \option{rowNames} = \var{id} is specified. The row.names
	 in the user provided data.frame or specificed
	 \option{rowNamesCol} column must match the IDs in the
	 name table. If no match a warning is generated and the
	 data row ignored.
        }
       \item{Alias}{- a character string for each area used to do
	 a wildcard match ("contains") with the \option{rowName}
	 or \option{rowNamesCol} specified in the
	 \var{micromapST} call when the \var{USSeerBG} border
	 group is used. When a match is completed, the
	 abbreviation is used to link the user's data to the
	 boundary data.  The alias match is done when
	 \option{rowNames} is set to "alias".  There should be
	 only one match in the data for each sub-area alias.  If
	 more are found, an error is raised.  This feature is
	 only supported in the \var{USSeerBG} border group.
        }
       \item{L2_ID}{- is the level 2 identifier.  Used to link the area to 
         the L2VisBorders boundary data point data.frame.  In the case of 
         the USSeerBG border group, the L2 boundaries are state boundaries 
         and the L2_ID value is the state 2 character abbreviation.
        }
       \item{L2_ID_Name}{- is the full name L2 area.  
        }
       \item{regID}{- is the region identifier.  Used to link the area to the
         RegVisBorders boundary data point data.frame.  The USStatesBG and 
         USSeerBG border groups use this field to like the sub-areas to the 
         four (4) U. S. Census regions (Northeast, South, MidWest, and West)
	 This association is used with the \option{regions} call parameter to
	 determine which regions and their areas, etc. will be drawn when the
	 caller provide data is mapped.
        }
       \item{regName}{- is the full name of a region.  
        }
       \item{Key}{- is a character string used to link the name
	 table to the boundary data in the VisBorders
	 data.frames. 
	}
       \item{Link}{- when the initial border group is created,
	 the \var{Key}, \var{Name} or the \var{Abbr} variables
	 may not be able to provide a link to the original
	 boundary data. When this happens, the border group
	 creater must use an alternate "link" to tie the name
	 table to the boundary data.  The \var{link} field is
	 used to accomplish this in the package provided border
	 group building steps and functions.  Once the border
	 group is fully constructed, the \var{Link} field is not
	 use.
        }
       \item{MapL, MapX, and MapY}{These three columns have replaced
         the MapLabel column outlined below.  The MapL column
         provides the label to be drawn at the coordinates provided 
         for the area.  Only a few areas should require labels.  
         To many labels will make the map unreadable in a linked 
         micromap.  The MapX and MapY columns in the name table 
         provide the x and y coordinates for the label on the map.
         The coordinates are in the same units as the boundary 
         coordinates used.
        } 
       }
     }
    \item{areaVisBorders, L2VisBorders, RegVisBorders, and 
        L3VisBorders}{There are four boundary dataset - The boundaries 
        of the areas being micromapped (counties) are in areaVisBorders. 
	The boundaries of an intermediate level (2) for general 
	orientation are in L2VisBorders. The boundaries of regional 
	areas for highlight overlays and regional only mapping and 
	are in RegVisBorders. The boundaries of the an outline of 
	the entire mapping area is in L3VisBorders. 

        These four data.frames contain the boundary point lists for
        the areas, regions and total map space. The L2VisBorders,
        RegVisBorders, and L3VisBorders data.frames are used to
        outline groups of areas, regions of the area and the
        entire space. The areaVisBorders data.frame contains the
        point lists for each sub-areas and are keyed to the name
        table (\var{areaNamesAbbrsIDs}).
     
        The data structure for each of the following four boundary
        data.frames is:

          seq Key  x      y      hole
            
      \describe{
        \item{seq}{- a numerical sequence number of the boundary
	  points in the data.frame.
         }
        \item{Key}{- the Key field had different uses in each of
	  the four VisBorders structured data.frames. In the
	  areaVisBorders data.frame, the \var{Key} is the unique
	  key for the sub-area as defined in the name table
	  (\var{areaNamesAbbrsIDs}).
          All of the points with the same \var{Key} form one or
	  more polygons and represent boundaries of a sub-area.
	  This allows the package to locate the boundary data for
	  a specific sub-area when needed.

          In the RegVisBorders boundary data.frame, the \var{Key}
	  is the region ID associated with the boundary points
	  (polygons). One or more sub-areas can be linked to an
	  region boundary via the \var{regID} variable in the
	  name table.  The USStatesBG, USSeerBG, UKIrelandBG,and
	  ChinaBG border groups may use of the regions feature.

          In the L3VisBorders, all of the \var{Key} field values
	  area set to a name that represents the entire border
	  group area.  The \var{Key} field is not used in
	  drawning the area's outline. The level 3 boundary
	  outline data.frame is always drawn when the entire
	  geographic area is mapped. If not all of the regions
	  are being mapped, then the L3 boundary is not drawn. 
	 }
        \item{x}{- a numerical value for the \var{x} coordinates
	 of a polygon. The end of the polygon is signaled by an
	 \var{x} value of \var{NA}. The first point in the
	 polygon does not have to be repeated. The polygon draw
	 function used by micromapST will close the figure. An
	 area may be composed of several polygons. Holes in areas
	 are also represented by polygons and are associated with
	 the sub-area.
        }
       \item{y}{- a numerical value for the \var{y} coordinates
	 of a polygon. The end of the polygon is signaled by an
	 \var{y} value of \var{NA}. 
	}
       \item{hole}{- a logical value. If \var{TRUE}, the
	 associated polygon is a hole within the sub-area
	 identified by the \var{Key} field. A hole polygon is
	 always drawn using the maps background color. For this
	 reason, sub-areas containing holes (lakes, rivers, or
	 other sub-areas), must proceed any sub-area in the
	 data.frame that it may overlay with the hole.  For
	 example, if one sub-area "A" is contained within sub-
	 area "B",  sub-area "B" must have a hole where sub-area
	 "A" is located and must preceed sub-area "A" in the
	 VisBorders data.frame.  In this way, sub-area "B" and
	 its hole are drawn before sub-area "A" preventing sub-
	 area "B" hole from overlaying sub-area "A".
        }  
       }
   
      Hole are processed by re-drawing the hole area with the
      current background color. Therefore, any area with holes
      must be in the data.frame prior to any areas that may
      occupy the hole's space in the map.

      The order of the area's boundaries in these files is very
      important to allow correct processing of the holes and any
      areas that overlay holes. Holes are not unpainted polygons,
      but polygons repainted back to the background color. The
      order should be areas with holes must preceed areas that
      overlay hole space. This is required to ensure an area is
      not over-painted by an area's hole

      Each data.frame should be validated before using to make
      sure they are clean and will not generate errors when used
      by \var{micromapST}. 
    }

    Each border group contains the same six data.frames using the
    same six names.  This allows the micromapST package the
    ability to quickly load a particular border group and create
    the requested micromaps.

    See the write up on each included border group for details on
    the specific content of their border group dataset and the
    list of sub-area names, abbreviations, and id that should be
    used to link the data to the boundary information.
   
    The \option{regions} feature and RegVisBorder overlays are
    only supported in the following border groups:
   
    \var{USStatesBG}
    \var{USSeerBG}
    \var{UKIrelendBG}
    \var{ChinaBG}
    
   }
}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}

\keyword{datasets}

