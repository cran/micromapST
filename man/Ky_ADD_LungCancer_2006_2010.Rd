\name{Ky_ADD_LungCancer_2006_2010}
\alias{Ky_ADD_LungCancer_2006_2010}
\docType{data}
\title{Kentucky Lung Cancer statistics by Area Development Districts.
 - Ky_ADD_LungCancer_2006_2010}
\description{
 This dataset is used in the example of how to create a border group for the Kentucky Area
 Development Districts (ADD) and then generate a linked micromap using the new border group.
 Kentucky has 120 counties which is very difficult to use to create link micromaps with 
 their small maps.  By using the state's ADD areas (aggregation of counties), the state has 
 simplified the presentation of their own statistical data and has provide linked micromaps 
 with an alternative presentation to the 120 counties - 15 ADDs.
 The border group provided in this package to use with this data is the "KY_ADD_BG" datasets.
 
 }
\usage{data(Ky_ADD_LungCancer_2006_2010)}
\details{
The data contains the following fields:
  \itemize{
    \item{ADA Identifier}{ -                     }
    \item{xxx}{ - xxx}
    \item{yyy}{ - yyy}
    \item{zzz}{ - zzz}
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

