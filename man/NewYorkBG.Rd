\name{NewYorkBG}
\docType{data}
\alias{NewYorkBG}
\title{NewYorkBG border group datasets to supports building micromaps for the 62 counties in the state of New York.}
\description{
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call argument is used to specify the border group dataset for the geographical area.  
  The \var{NewYorkBG} border group dataset supports creating linked micromaps for the 
  62 counties in the state of New York. When the \option{bordGrp} call argument is set 
  to \var{NewYorkBG}, the appropriate name table (county names and abbreviations) and 
  the 62 sub-areas (countries) boundary data is loaded in \var{micromapST}.  
  The user's data is then linked to the boundary data via the county's name, 
  abbreviated or ID based on the table below.
}
\usage{data(NewYorkBG)}
\details{
The \var{NewYorkBG} border group dataset contains the following data.frames: 
 \describe{
   \item{areaParms}{ - contains specific parameters for the border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, numerical identifier 
    and alias matching string for each of the 62 counties in New York}
   \item{areaVisBorders}{ - the boundary point lists for each area.}
   \item{L2VisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data.frame is not used and set to L3VisBorders as a place holder.}
   \item{RegVisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data.frame is not used and set to L3VisBorders as a place holder.}
   \item{L3VisBorders}{ - the boundary of the state of New York.}
 }

The New York county border group contains 62 county sub-areas.  Each county has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions are defined in the New York county border group, so the L2VisBorders dataset is not
used and the regions option is disabled.  The L3VisBorders dataset contains 
the outline of the state of New York. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (county) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call argument. 

A column or the data.frame row.names must match one of the types of names in the 
\var{areaNamesAbbrsIDs} data.frame name table. If the data row does not match a value 
in the name table, an warning is issued and the data is ignored. If no data is present 
for a sub-area (county) in the name table, the sub-area (county) is mapped but not colored.

The following are a list of the names, abbreviations, and IDs for each country in the 
\var{NewYorkBG} border group.
 \tabular{lll}{
     name\tab ab\tab id\cr
     Albany\tab ALBA\tab 36001\cr
     Allegany\tab ALLE\tab 36003\cr
     Bronx\tab BRON\tab 36005\cr
     Broome\tab BROO\tab 36007\cr
     Cattaraugus\tab CATT\tab 36009\cr
     Cayuga\tab CAYU\tab 36011\cr
     Chautauqua\tab CHAU\tab 36013\cr
     Chemung\tab CHEM\tab 36015\cr
     Chenango\tab CHEN\tab 36017\cr
     Clinton\tab CLIN\tab 36019\cr
     Columbia\tab COLU\tab 36021\cr
     Cortland\tab CORT\tab 36023\cr
     Delaware\tab DELA\tab 36025\cr
     Dutchess\tab DUTC\tab 36027\cr
     Erie\tab ERIE\tab 36029\cr
     Essex\tab ESSE\tab 36031\cr
     Franklin\tab FRAN\tab 36033\cr
     Fulton\tab FULT\tab 36035\cr
     Genesee\tab GENE\tab 36037\cr
     Greene\tab GREE\tab 36039\cr
     Hamilton\tab HAMI\tab 36041\cr
     Herkimer\tab HERK\tab 36043\cr
     Jefferson\tab JEFF\tab 36045\cr
     Kings\tab KING\tab 36047\cr
     Lewis\tab LEWI\tab 36049\cr
     Livingston\tab LIVI\tab 36051\cr
     Madison\tab MADI\tab 36053\cr
     Monroe\tab MONR\tab 36055\cr
     Montgomery\tab MONT\tab 36057\cr
     Nassau\tab NASS\tab 36059\cr
     New York\tab NEWY\tab 36061\cr
     Niagara\tab NIAG\tab 36063\cr
     Oneida\tab ONEI\tab 36065\cr
     Onondaga\tab ONON\tab 36067\cr
     Ontario\tab ONTA\tab 36069\cr
     Orange\tab ORAN\tab 36071\cr
     Orleans\tab ORLE\tab 36073\cr
     Oswego\tab OSWE\tab 36075\cr
     Otsego\tab OTSE\tab 36077\cr
     Putnam\tab PUTN\tab 36079\cr
     Queens\tab QUEE\tab 36081\cr
     Rensselaer\tab RENS\tab 36083\cr
     Richmond\tab RICH\tab 36085\cr
     Rockland\tab ROCK\tab 36087\cr
     St. Lawrence\tab STLA\tab 36089\cr
     Saratoga\tab SARA\tab 36091\cr
     Schenectady\tab SCHE\tab 36093\cr
     Schoharie\tab SCHO\tab 36095\cr
     Schuyler\tab SCHU\tab 36097\cr
     Seneca\tab SENE\tab 36099\cr
     Steuben\tab STEU\tab 36101\cr
     Suffolk\tab SUFF\tab 36103\cr
     Sullivan\tab SULL\tab 36105\cr
     Tioga\tab TIOG\tab 36107\cr
     Tompkins\tab TOMP\tab 36109\cr
     Ulster\tab ULST\tab 36111\cr
     Warren\tab WARR\tab 36113\cr
     Washington\tab WASH\tab 36115\cr
     Wayne\tab WAYN\tab 36117\cr
     Westchester\tab WEST\tab 36119\cr
     Wyoming\tab WYOM\tab 36121\cr
     Yates\tab YATE\tab 36123\cr
 }

There are no alternate abbreviations or regions assocated with counties in New York border group.

The \var{id} field value is the U. S. state and county FIPS code.

The \option{rowNames} = "alias" or "alt_ab", the \option{regionB} and \option{dataRegionsOnly} 
features are not supported in the \var{NewYorkBG} border group.

}

\keyword{datasets}

