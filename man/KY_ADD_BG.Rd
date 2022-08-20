\name{KY_ADD_BG}
\alias{KY_ADD_BG}
\docType{data}
\title{Kentucky Area Development Districts border group datasets to support creating micromaps 
  at the ADD level within KY}
\description{
 The \var{micromapST} function has the ability to generate linked micromaps for 
 any geographical area.  To specify the geographical area, the \option{bordGrp} 
 call argument is used to specify the border group dataset for the geographical area.  
 When \var{micromapST} function is used to micromap data for Kentucky Area Development Districts,
 instead of the 120 counties. The border group option (\option{bordGrp})
 is set to "KY_ADD_BG".  This instructs micromapST to load the area Name, Abbreviation,
 ID and boundaries files for 15 Kentucky Area Development Districts.
 The datasets contained in the border group are areaNamesAbbrsIDs, areaVisBorders,
 L2VisBorders, and L3VisBorders for the ADDs in Kentucky.
 The user's data is then linked to the boundary data via the ADD's name or  
 abbreviated name based on the table below.
}

\details{
The border group contains the following data.frames:: 
  \describe{
    \item{areaParms}{ - contains specific parameters for the border group}
    \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, and numerical identifier 
     for the counties in the state of Kansas.}
    \item{areaVisBorders}{ - the boundary point lists for each county area in Kansas.}
    \item{L2VisBorders}{ - the boundaries for an intermediate level. For state areas, this 
     boundary data is not used and set to L3VisBorders as a place holder.}
    \item{RegVisBorders}{ - the boundaries for an intermediate level. For state areas, this 
     boundary data is not used and set to L3VisBorders as a place holder.}
    \item{L3VisBorders}{ - the boundary of the state of Kansas }
  }

The Kentucky ADD border group contains 15 districts (sub-areas).  Each ADD has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions are defined for this border group, so the \var{L2VisBorders} and 
\var{RegVisBorders} datasets are not used and the regions feations is disabled.  
The L3VisBorders dataset contains the outline of the state of Kentucky. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (county) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call parameter. 

A column or the data.frame row.names must match one of the types of names in the 
\var{areaNamesAbbrsIDs} data.frame name table. If the data row does not match a value 
in the name table, an warning is issued and the data is ignored. If no data is present 
for a sub-area (ADDs) in the name table, the sub-area (ADDs) is mapped but not colored.

The following are a list of the names, abbreviations, and IDs for each country in the 
\var{KY_ADD_BG} border group.
   \tabular{lll}{
     name\tab              ab\tab id\cr
     Barren River\tab      BR\tab 24001\cr
     Big Sandy\tab         BS\tab 24003\cr
     Bluegrass\tab         BL\tab 24005\cr
     Buffalo Tract\tab     BT\tab 24007\cr
     Cumberland Valley\tab CV\tab 24009\cr
     Fivco\tab             FI\tab 24011\cr
     Gateway\tab           GA\tab 24013\cr
     Green River\tab       GR\tab 24015\cr
     Kentucky River\tab    KR\tab 24017\cr
     KIPDA\tab             KI\tab 24019\cr
     Lake Cumberland\tab   LC\tab 24021\cr
     Lincoln Trail\tab     LT\tab 24023\cr
     Northern Kentucky\tab NK\tab 24025\cr
     Pennyrile\tab         PE\tab 24027\cr
     Purchase\tab          PU\tab 24029\cr
   }
The \var{id} field value is the U. S. state and sequence number for the ADD.

The \option{rowNames} = "alias" or "alt_ab" ( aliases, and alternate abbreviations )
and the \option{regionB} and \option{dataRegionsOnly} ( regions ) features are not 
supported in the \var{KansasBG} border group.

}

\keyword{datasets}

