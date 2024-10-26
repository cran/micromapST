\name{UtahBG}
\docType{data}
\alias{UtahBG}
\title{UtahBG border group datasets to support creating micromaps for the 
counties in the state of Utah}
\description{
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call parameter is used to specify the border group dataset for the geographical area.  
  The \var{UtahBG} border group dataset is contained within this package and 
  supports creating linked micromaps for the 
  29 counties in the state of Utah. When the \option{bordGrp} call parameter is set 
  to \var{UtahBG}, the appropriate name table (county names and abbreviations) and 
  the 29 sub-areas (countries) boundary data is loaded in \var{micromapST}.  
  The user's data is then linked to the boundary data via the county's name, 
  abbreviation, alternate abbreviation, or ID based on the table below.
}
\usage{data(UtahBG)}
\details{
The \var{UtahBG} border group dataset contains the following data.frames: 
 \describe{
   \item{areaParms}{ - contains specific parameters for the border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, and numerical identifier 
    for the 29 counties in the state of Utah.}
   \item{areaVisBorders}{ - the boundary point lists for each county area in Utah.}
   \item{L2VisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data.frame is not used and set to L3VisBorders as a place holder.}
   \item{RegVisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data.frame is not used and set to L3VisBorders as a place holder.}
   \item{L3VisBorders}{ - the boundary of the state of Utah }
 }

The Utah county border group contains 29 county sub-areas.  Each county has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions are defined in the Utah county border group, so the L2VisBorders dataset is not
used and the regions option is disabled.  The L3VisBorders dataset contains 
the outline of the state of Utah. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (county) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call argument. 

A identified column (\var{idCol}) or the data.frame row.names must match one of the 
types of names in the border group's \var{areaNamesAbbrsIDs} data.frame name table. 
If the data row does not match a value in the name table, an warning is issued and 
the data is ignored. If no data is present 
for a sub-area (county) in the name table, the sub-area is mapped but not colored.

The following are a list of the names, abbreviations, alternate abbreviations and IDs for each country in the 
\var{UtahBG} border group.
\tabular{llll}{
  name\tab           ab\tab alt_ab\tab id\cr
  Beaver\tab         BV\tab   BEA\tab  49001\cr
  Box Elder\tab      BE\tab   BOX\tab  49003\cr
  Cache\tab          CH\tab   CAC\tab  49005\cr 
  Carbon\tab         CA\tab   CAR\tab  49007\cr
  Daggett\tab        DAG\tab  DAG\tab  49009\cr
  Davis\tab          DAV\tab  DAV\tab  49011\cr
  Duchesne\tab       DU\tab   DUC\tab  49013\cr
  Emery\tab          EM\tab   EME\tab  49015\cr
  Garfield\tab       GA\tab   GAR\tab  49017\cr
  Grand\tab          GR\tab   GRA\tab  49019\cr
  Iron\tab           IR\tab   IRO\tab  49021\cr
  Juab\tab           JB\tab   JUA\tab  49023\cr
  Kane\tab           KN\tab   KAN\tab  49025\cr
  Millard\tab        MI\tab   MIL\tab  49027\cr
  Morgan\tab         MO\tab   MOR\tab  49029\cr
  Piute\tab          PT\tab   PIU\tab  49031\cr
  Rich\tab           RH\tab   RIC\tab  49033\cr
  Salt Lake\tab      SL\tab   SAL\tab  49035\cr
  San Juan\tab       SJ\tab   SNJ\tab  49037\cr
  Sanpete\tab        SP\tab   SNP\tab  49039\cr
  Sevier\tab         SV\tab   SEV\tab  49041\cr
  Summit\tab         SU\tab   SUM\tab  49043\cr
  Tooele\tab         TO\tab   TOO\tab  49045\cr
  Uintah\tab         UI\tab   UIN\tab  49047\cr
  Utah\tab           UT\tab   UTA\tab  49049\cr
  Wasatch\tab        WS\tab   WST\tab  49051\cr
  Washington\tab     WA\tab   WSH\tab  49053\cr
  Wayne\tab          WN\tab   WAY\tab  49055\cr
  Weber\tab          WB\tab   WEB\tab  49057\cr
 }
When compiling the information for the \var{UtahBG} border group, it was not 
clear what was the standard accepted abbreviation for each county.  Therefore, two sets of 
abbreviations for each county are included.  The first abbreviation set can be 
referenced by setting the \option{rowNames} call parameter to "ab".  
The second (alternate) abbreviation set can be used by setting
the \option{rowNames} to "alt_ab".

The \var{id} field value is the 5 digit U. S. state and county FIPS code.

The \option{rowNames} = "alias" and the \option{regions} features are not 
supported in the \var{UtahBG} border group.
}

\keyword{datasets}

