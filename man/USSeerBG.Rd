\name{USSeerBG}
\docType{data}
\alias{USSeerBG}
\title{USSeerBG border group datasets to support use with U.S. 20 Seer areas/registries}
\description{
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call argument is used to specify the border group dataset for the geographical area.  
  The \var{USSeerBG} border group dataset supports creating linked micromaps for the 
  20 Seer registries in the U. S. When the \option{bordGrp} call argument is set 
  to \var{USSeerBG}, the appropriate name table (county names and abbreviations) and 
  the 20 sub-areas (Seer registries) boundary data is loaded in \var{micromapST}.  
  The user's data is then linked to the boundary data via the Seer registry's name, 
  abbreviated, alias match or ID based on the table below.

  The 20 U. S. Seer registries are the accepted registries as of January 2010 
  funded by NCI. 
}
\usage{data(USSeerBG)}
\details{
The \var{USSeerBG} border group dataset contains the following data.frames: 
  \describe{
    \item{areaParms}{ - contains specific parameters for the border group
     }
    \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, numerical 
      identifier and alias matching string for each of the 20 Seer registries.
     }
    \item{areaVisBorders}{ - the boundary point lists for each area.
     }
    \item{L2VisBorders}{ - the boundaries for an intermediate level. For Seer 
      registry border group, L2VisBorders contains the boundaries for the 51
      states and DC in the U. S to help provide a geographical reference of
      the registries to the states.
     }
    \item{RegVisBorders}{ - the boundaries for the 4 U. S. Census regions in 
      the U. S in support of the region feature.
     }
    \item{L3VisBorders}{ - the boundary of the U. S.
      }
   }

The Seer Registries border group contains 20 Seer Registry sub-areas.  
Each registry has a row in the \var{areaNamesAbbrsIDs} data.frame and 
a set of polygons in the areaVisBorders data.frame datasets.

Regions are defined in this border group as the 4 census regions 
in the U. S.   The regions feature is enable. The four census regions 
are: NorthEast, South, MidWest, and West.  The states and Seer registries in each 
region are:
  \tabular{lll}{
      state\tab Seer Registries\tab region\cr
    Alabama\tab <none>\tab  South\cr
    Alaska\tab Alaska Natives\tab West\cr
    Arizona\tab Arizona Natives\tab West\cr
    Arkansas\tab <none>\tab  South\cr
    California\tab California-LA,  \tab West\cr
    \tab California-Other, \tab \cr
    \tab California-SF, \tab \cr 
    \tab California-SJ \tab \cr
    Colorado\tab  <none>\tab West\cr
    Connecticut\tab Connecticut\tab NorthEast\cr
    Delaware\tab <none>\tab South\cr
    District of Columbia\tab <none>\tab South\cr
    Florida\tab <none>\tab South\cr
    Georgia\tab Georgia-Atlanta, \tab South\cr
    \tab Georgia-Other, \tab \cr
    \tab Georgia-Rural\tab \cr
    Hawaii\tab  Hawaii\tab  West\cr
    Idaho\tab <none>\tab West\cr
    Illinois\tab <none>\tab MidWest\cr
    Indiana\tab <none>\tab MidWest\cr
    Iowa\tab Iowa\tab MidWest\cr
    Kansas\tab <none>\tab MidWest\cr
    Kentucky\tab Kentucky\tab South\cr
    Louisiana\tab Louisiana\tab South\cr
    Maine\tab <none>\tab NorthEast\cr
    Maryland\tab <none>\tab South\cr
    Massachusetts\tab <none>\tab NorthEast\cr
    Michigan\tab  Michigan-Detroit\tab MidWest\cr
    Minnesota\tab <none>\tab MidWest\cr
    Mississippi\tab <none>\tab South\cr
    Missouri\tab <none>\tab MidWest\cr
    Montana\tab <none>\tab West\cr
    Nebraska\tab <none>\tab MidWest\cr
    Nevada\tab <none>\tab West\cr
    New Hampshire\tab <none>\tab NorthEast\cr
    New Jersey\tab  New Jersey\tab NorthEast\cr
    New Mexico\tab  New Mexico\tab West\cr
    New York\tab <none>\tab NorthEast\cr
    North Carolina\tab <none>\tab South\cr
    North Dakota\tab <none>\tab MidWest\cr
    Ohio\tab  <none>\tab MidWest\cr
    Oklahoma\tab Oklahoma-Cherokee\tab South\cr
    Oregon\tab <none>\tab West\cr
    Pennsylvania\tab <none>\tab NorthEast\cr
    Rhode Island\tab <none>\tab NorthEast\cr
    South Carolina\tab <none>\tab South\cr
    South Dakota\tab <none>\tab MidWest\cr
    Tennessee\tab <none>\tab South\cr
    Texas\tab <none>\tab South\cr
    Utah\tab Utah\tab West\cr
    Vermont\tab <none>\tab NorthEast\cr
    Virginia\tab <none>\tab South\cr
    Washington\tab Washington-Seattle\tab South\cr
    West Virginia\tab <none>\tab South\cr
    Wisconsin\tab <none>\tab MidWest\cr
    Wyoming\tab <none>\tab West\cr
   }

The L3VisBorders dataset contains the outline of the United States. 

The details on each of these data.frame structures can be found in 
the "bordGrp" section of this document.  The \var{areaNamesAbbrsIDs} 
data.frame provides the linkages to the boundary data for each 
sub-area (registry) using the fullname, abbreviation, and numerical 
identifier for each country to the \var{<statsDFrame>} data based 
on the setting of the \option{rowNames} call argument. 

A column or the data.frame row.names must match one of the types 
of names in the \var{areaNamesAbbrsIDs} data.frame name table. 
If the data row does not match a value in the name table, an warning 
is issued and the data is ignored. If no data is present for a sub-area (registry) 
in the name table, the sub-area (registry) is mapped but not colored.

The following are a list of the names, abbreviations, alias and IDs for each country in the 
\var{USSeerBG} border group.
 \tabular{llllll}{
   Name\tab               ab\tab    alias string\tab      id\tab counties\tab region\cr
   Alaska Natives\tab     AK-NAT\tab  ALASKA NATIVES\tab  18\tab all\tab West\cr
   Arizona Natives\tab    AZ-NAT\tab  ARIZONA NATIVES\tab 20\tab all\tab West\cr 
   California-LA\tab      CA-LA\tab   LOS ANGELES\tab     4\tab  Los Angeles\tab West\cr
   California-SF\tab      CA-SF\tab   SAN FRANCISCO\tab   2\tab  Alameda,  \tab  West\cr
   \tab \tab \tab \tab Contra Costa,\tab \cr
   \tab \tab \tab \tab Marin, \tab \cr
   \tab \tab \tab \tab San Francisco,\tab \cr 
   \tab \tab \tab \tab San Mateo\tab \cr
   California-SJ\tab      CA-SJ\tab   SAN JOSE\tab        3\tab  Montersey \tab West\cr 
   \tab \tab \tab \tab San Benito,\tab \cr
   \tab \tab \tab \tab Santa Clara,\tab \cr 
   \tab \tab \tab \tab Santa Cruz\tab \cr
   California-Other\tab   CA-OTH\tab  CALIFORNIA EXCLUDING\tab 5\tab all other counties\tab West\cr
   Connecticut\tab        CT\tab      CONNECTICUT\tab     1\tab  all\tab NorthEast\cr
   Georgia-Atlanta\tab    GA-ATL\tab  ATLANTA\tab         6\tab  Clayton, Cobb, DeKalb,\tab South\cr 
   \tab \tab \tab \tab Fulton, Gwinnett\tab \cr
   Georgia-Rural\tab      GA-RUR\tab  RURAL GEORGIA\tab   8\tab  Glascock, Greene, Hancock,\tab South\cr 
   \tab \tab \tab \tab Jasper, Jefferson, Morgan,\tab \cr 
   \tab \tab \tab \tab Putnam, Taliaferro, Warren,\tab \cr 
   \tab \tab \tab \tab Washington\tab \cr
   Georgia-Other\tab      GA-OTH\tab  GREATER GEORGIA\tab 7\tab  all other counties\tab South\cr
   Hawaii\tab             HI\tab      HAWAII\tab          9\tab  all\tab West\cr
   Iowa\tab               IA\tab      IOWA\tab            10\tab all\tab MidWest\cr
   Kentucky\tab           KY\tab      KENTUCKY\tab        14\tab all\tab South\cr
   Michigan-Detroit\tab   MI-DET\tab  DETROIT\tab         15\tab Macomb, \tab MidWest\cr
   \tab \tab \tab \tab Oakland, \tab \cr
   \tab \tab \tab \tab Wayne\tab \cr
   New Jersey\tab         NJ\tab      NEW JERSEY\tab      11\tab all\tab NorthEast\cr
   New Mexico\tab         NM\tab      NEW MEXICO\tab      12\tab all\tab West\cr
   Oklahoma-Cherokee\tab  OK-CHE\tab  OKLAHOMA\tab        19\tab Adair, \tab South\cr
   \tab \tab \tab \tab Cherokee, \tab \cr
   \tab \tab \tab \tab Craig, \tab \cr
   \tab \tab \tab \tab Delaware, \tab \cr
   \tab \tab \tab \tab Mayes, \tab \cr
   \tab \tab \tab \tab McIntosh, \tab \cr 
   \tab \tab \tab \tab Muskogee, \tab \cr 
   \tab \tab \tab \tab Nowata,\tab \cr 
   \tab \tab \tab \tab Ottawa, \tab \cr
   \tab \tab \tab \tab Rogers, \tab \cr 
   \tab \tab \tab \tab Seqouyah, \tab \cr 
   \tab \tab \tab \tab Tulsa, \tab \cr 
   \tab \tab \tab \tab Wagnorer, \tab \cr 
   \tab \tab \tab \tab Washington\tab \cr
   Utah\tab               UT\tab      UTAH\tab            16\tab all\tab West\cr
   Washington-Seattle\tab WA-SEA\tab  SEATTLE\tab         17\tab Clallam,  \tab South\cr
   \tab \tab \tab \tab Grays Harbor,\tab \cr
   \tab \tab \tab \tab Island,\tab \cr
   \tab \tab \tab \tab Jefferson,\tab \cr
   \tab \tab \tab \tab King, \tab \cr
   \tab \tab \tab \tab Kitsap, \tab \cr
   \tab \tab \tab \tab Mason, \tab \cr
   \tab \tab \tab \tab Pierce, \tab \cr
   \tab \tab \tab \tab San Juan,\tab \cr 
   \tab \tab \tab \tab Skagit, \tab \cr
   \tab \tab \tab \tab Snohomish, \tab \cr
   \tab \tab \tab \tab Thurston,\tab \cr 
   \tab \tab \tab \tab Whatcom  \tab \cr
  }

The \option{rowNames} = \var{alias} and the \option{regions} = \var{TRUE} 
features are enabled in the \var{USSeerBG} border group.  

The \var{alias} option is designed to allow the package to match the 
registry labels created by the Seer Stat website when exporting 
Seer data for analysis.  The alias match is a "contains" match, so 
the registry field in the user data must "contain" the "alias" values 
listed in the above table.  To help generalize the match, the user's 
registry value is stripped of any punctuation, control characters and 
multiple spaces (blanks, tabs, cr, lf) are reduced to a single blank 
and the string is converted to all upper case. Then the wild card match 
is performed.  

The \option{dataRegionOnly} call parameter (when set to \var{TRUE}) 
instructs the package to only map the 
regions with Seer registers with data. The regions used are the four census 
regions: NorthEast, South, MidWest and West.  The RegVisBorders data.frame 
contains the outline of each of these regions.  For example: if Seer 
registry data is provided for the only the New Mexico, Utah and California 
Registries in the West region, then only the states and regional boundary 
for the West region are drawn. 

The \var{USSeerBG} border group does not contain or support an alternate set 
of abbreviations.  If \option{rowNames} is set to \var{alt_ab}, an warning 
is generated and the standard Seer registry abbreviations are used.

The following steps should be used to export data for \var{micromapST}'s use from the SEER*Stat Website:
 \enumerate{
   \item Log on to the SEER^Stat website.
   \item Create the matrix of results you want in SEER*Stat.
   \item Click on Matrix, Export, Results as Text File 
     (if you created multiple matrices of results, make sure that the 
     one you want to export is highlighted)
   \item In the Matrix Export Options window, click on:
     \enumerate{
       \item Output variables as Labels without quotes
       \item Remove all thousands separators
       \item Output variable names before data
       \item Preserve matrix columns & rename fields
       \item Leave defaults clicked for Line delimiter, Missing Character, 
         and Field delimiter
      }
   \item Change names and locations of text and dictionary files from defaults 
      to the appropriate name and directory location.
  }

To read the resulting text file into R use the \code{read.delim} function with \option{header} = \var{TRUE}.
Follow the \code{read.delim} call with a \code{str} function to verify the data was read correctly.
  \preformatted{
     dataT <- read.delim("c:\datadir\seerstat.txt",header=FALSE)
     str(dataT)
  }

}
\source{NCI}
\references{United States National Cancer Institute Seer Website at www.seer.cancer.gov; 
  Seer Software at seer.cancer.gov/seerstat.; United States Census Bureau, 
  Geography Division. "Census Regions and Divisions of the United States" (PDF). 
  Retrieved 2013-01-10.
 } 
\keyword{datasets}

