\name{SeoulSKoreaBG}
\docType{data}
\alias{SeoulSKoreaBG}
\title{Seoul South Korea border group datasets to support creating micromaps for the 25 districts 
in the Korean city of Seoul.}
\description{
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call argument is used to specify the border group dataset for the geographical area.  
  The \var{SeoulSKoreaBG} border group dataset supports creating linked micromaps for the 
  25 districts in the city of Seoul South Korea. When the \option{bordGrp} call argument is set 
  to \var{SeoulSKoreaBG}, the appropriate name table (county names and abbreviations) and 
  the boundary data for the 25 districts are loaded in \var{micromapST}.  
  The user's data is then linked to the boundary data via the district's name, 
  abbreviated or ID based on the table below.
}
\usage{data(SeoulSKoreaBG)}
\details{
The \var{SeoulSKoreaBG} border group contains the following data.frames:: 
 \describe{
   \item{areaParms}{ - contains specific parameters for the border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, and numerical identifier 
    for the districts in the city of Seoul.}
   \item{areaVisBorders}{ - the boundary point lists for each district in the city of Seoul Korea.}
   \item{L2VisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data is not used and set to L3VisBorders as a place holder.}
   \item{RegVisBorders}{ - the boundaries for an regional level. For this border group, this 
    boundary data is not used and set to L3VisBorders as a place holder.}
   \item{L3VisBorders}{ - the boundary of the city of Seoul South Korea }
 }
 
The Seoul district border group contains 25 district sub-areas.  Each district has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions or L2 boundaries are defined for the Seoul district border group.  The RegVisBorders and 
and L2VisBorders data.frames are set the L3VisBorders data.frame.  The regional feature is disabled.
The L3VisBorders dataset contains 
the outline of the city of Seoul. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (district) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call parameter. 

A column or the data.frame row.names must match one of the types of names in the 
\var{areaNamesAbbrsIDs} data.frame name table. If the data row does not match a value 
in the name table, an warning is issued and the data is ignored. If no data is 
present for a sub-area (district) in the name table, the sub-area is mapped but not colored.
 
The following are a list of the names, abbreviations, and ids for each country in the 
\var{SeoulSKoreaBG} border group.
   \tabular{lll}{
      name\tab ab\tab id\cr
      Dobong\tab DO\tab 11100\cr 
      Dongdaemun\tab DN\tab 11060\cr 
      Dongjak\tab DG\tab 11200\cr 
      Eunpyeong\tab EU\tab 11120\cr 
      Gangdong\tab GA\tab 11090\cr 
      Gangbuk\tab GN\tab 11250\cr 
      Gangnam\tab GG\tab 11230\cr 
      Gangseo\tab GS\tab 11160\cr 
      Geumcheon\tab GE\tab 11180\cr 
      Guro\tab GU\tab 11170\cr 
      Gwanak\tab GW\tab 11210\cr 
      Gwangjin\tab GJ\tab 11050\cr 
      Jongro\tab JO\tab 11010\cr 
      Jung\tab JU\tab 11070\cr 
      Jungnang\tab JN\tab 11020\cr 
      Mapo\tab MA\tab 11140\cr 
      Nowon\tab NO\tab 11110\cr 
      Seocho\tab SE\tab 11220\cr 
      Seodaemun\tab SD\tab 11130\cr 
      Seongbuk\tab SN\tab 11080\cr 
      Seongdong\tab SG\tab 11040\cr 
      Songpa\tab SO\tab 11240\cr 
      Yangcheon\tab YA\tab 11150\cr 
      Yeongdeungpo\tab YE\tab 11190\cr 
      Yongsan\tab YO\tab 11030\cr 
      }
     
The \var{id} field value is the ISO numerical code for the district.

The \option{rowNames} = "alias" or "alt_ab" and the \option{regions} features are not 
supported in the \var{SeoulSKoreaBG} border group.

}

\keyword{datasets}

