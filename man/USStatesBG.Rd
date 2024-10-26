\name{USStatesBG}
\docType{data}
\alias{USStatesBG}
\title{USStatesBG border group datasets to support use with U.S. States amd D.C. Areas}
\description{
 The \var{micromapST} function has the ability to generate linked micromaps for 
 any geographical area.  To specify the geographical area, the \option{bordGrp} 
 call argument is used to specify the border group dataset for the geographical area.  
 When \var{micromapSt} function is used to micromap the U.S. States and DC areas, 
 no border group needs to be specified.  The \var{USStatesBG} border group is the same as 
 the original sub-areas (states and DC) and boundaries used by the all previous versions 
 of the \var{micromapST} package.  By default \var{micromapST} loads the area fullnames, 
 abbreviations, IDs and boundaries files for 50 U. S. states and the District of Columbia
 for the processing of user data and the creation of the requested linked micromap.
}
\usage{data(USStatesBG)}
\details{
The \var{USStatesBG} border group contains in the following data.frames: 
 \describe{
   \item{areaParms}{ - specific parameters associated with this border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, numerical identifier 
    and alias matching string for each of the 51 U. S. States and D.C.}
   \item{areaVisBorders}{ - the boundary point lists for each of the 51 States and D.C..}
   \item{L2VisBorders}{ - the boundaries for the U. S. states and DC.   It is identical to \var{areaVisBorders} data.frame.}
   \item{RegVisBorders}{ - the boundaries for the 4 U. S. census regions}
   \item{L3VisBorders}{ - the boundary of the U.S }
 }

Refer to the section on the border group data.frames for a detailed discussion on
the formats and usage of each of the above data.frame.  

In this border group, there are 51 areas (states and DC) and information and names in the 
areaNamesAbbrIDs data.frame and there boundaries in the areaVisBorders data.frame. 
The L2VisBorders data.frame contains a copy of the areaVisBorders data.frame to allow heavier 
overlaying of the state and DC boundaries during mapping.  
The RegVisBorders data.frame contains the information and boundaries for the 4 U. S. census regions.  
The L3VisBorders dataset contains the information on the boundaries of the U.S.

Alaska and Hawaii are relocated to below California and Alaska is reduced in size by 50%.

The names, abbreviations, id, and assigned U. S. regions used in the areaNamesAbbrsIDs data.frame 
the the 50 U. S. States and DC are as follows: 
\tabular{llll}{
  name\tab           ab\tab  id\tab region\cr
  Alabama\tab        AL\tab   01\tab South\cr
  Alaska\tab         AK\tab   02\tab West\cr
  Arizona\tab        AZ\tab   04\tab West\cr 
  Arkansas\tab       AR\tab   05\tab South\cr
  California\tab     CA\tab   06\tab West\cr
  Colorado\tab       CO\tab   08\tab West\cr
  Connecticut\tab    CT\tab   09\tab NorthEast\cr
  Delaware\tab       DE\tab   10\tab South\cr
  DC\tab             DC\tab   11\tab South\cr
  Florida\tab        FL\tab   12\tab South\cr
  Georgia\tab        GA\tab   13\tab South\cr
  Hawaii\tab         HI\tab   15\tab West\cr
  Idaho\tab          ID\tab   16\tab West\cr
  Illinois\tab       IL\tab   17\tab MidWest\cr
  Indiana\tab        IN\tab   18\tab MidWest\cr
  Iowa\tab           IA\tab   19\tab MidWest\cr
  Kansas\tab         KS\tab   20\tab MidWest\cr
  Kentucky\tab       KY\tab   21\tab South\cr
  Louisiana\tab      LA\tab   22\tab South\cr
  Maine\tab          ME\tab   23\tab NorthEast\cr
  Maryland\tab       MD\tab   24\tab South\cr
  Massachusetts\tab  MA\tab   25\tab NorthEast\cr
  Michigan\tab       MI\tab   26\tab MidWest\cr
  Minnesota\tab      MN\tab   27\tab MidWest\cr
  Mississippi\tab    MI\tab   28\tab South\cr
  Missouri\tab       MO\tab   29\tab MidWest\cr
  Montana\tab        MT\tab   30\tab West\cr
  Nebraska\tab       NE\tab   31\tab MidWest\cr
  Neveda\tab         NV\tab   32\tab West\cr
  New Hampshire\tab  NH\tab   33\tab NorthEast\cr
  New Jersey\tab     NJ\tab   34\tab NorthEast\cr
  New Mexico\tab     NM\tab   35\tab West\cr
  New York\tab       NY\tab   36\tab NorthEast\cr
  North Carolina\tab NC\tab   37\tab South\cr
  North Dakota\tab   ND\tab   38\tab MidWest\cr
  Ohio\tab           OH\tab   39\tab MidWest\cr
  Oklahoma\tab       OK\tab   40\tab South\cr
  Oregon\tab         OR\tab   41\tab West\cr
  Pennsylvania\tab   PA\tab   42\tab NorthEast\cr
  Rhode Island\tab   RI\tab   44\tab NorthEast\cr
  South Carolina\tab SC\tab   45\tab South\cr
  South Dakota\tab   SD\tab   46\tab MidWest\cr
  Tennessee\tab      TN\tab   47\tab South\cr
  Texas\tab          TX\tab   48\tab South\cr
  Utah\tab           UT\tab   49\tab West\cr
  Vermont\tab        VT\tab   50\tab NorthEast\cr
  Virginia\tab       VA\tab   51\tab South\cr
  Washington\tab     WA\tab   53\tab West\cr
  West Virginia\tab  WV\tab   54\tab South\cr
  Wisconsin\tab      WI\tab   55\tab MidWest\cr
  Wyoming\tab        WY\tab   56\tab West\cr
 }

All data must be tagged with the name, abbreviation or id strings to be able to 
find the associated boundaries information.  With the \var{USStatesBG} only, the package
will accept several variations on the full name for DC.  They include:  Washington, D.C., 
District of Columbia, and D. C.  All can be with or without punctuation and upper and lower 
case. When detected, the name is translated to "DC".

The \option{dataRegionsOnly} call parameter can be set to \var{TRUE} to request the package to 
limit the mapping to only states in regional areas where data is being mapped and omit states 
and regions that do not contain data. This allows the caller to focus the micromaps on one of the
four (4) census regions: NorthEast, South, Midwest, or West.

The \option{rowNames} = \var{alias}, \option{rowNames} = \var{alt_ab} are not 
support in the \option{USStatesBG} border group.

}

\source{NIST - Federal Information Processing Standards and U. S. Census website.}
\references{NIST FIPS 6-4 Standards} 
\keyword{datasets}

