\name{MarylandBG}
\docType{data}
\alias{MarylandBG}
\title{MarylandBG border group datasets to support creating micromaps for the 24
counties in the state of Maryland}
\description{
 The \var{micromapST} function has the ability to generate linked micromaps for 
 any geographical area.  To specify the geographical area, the \option{bordGrp} 
 call argument is used to specify the border group dataset for the geographical area.  
 The \var{MarylandBG} border group dataset supports creating linked micromaps for the 
 24 counties in the state of Maryland. When the \option{bordGrp} call argument is set 
 to \var{MarylandBG}, the appropriate name table (county names and abbreviations) and 
 the 24 sub-areas (countries) boundary data is loaded in \var{micromapST}.  
 The user's data is then linked to the boundary data via the county's name, 
 abbreviated or ID based on the table below.
}
\usage{data(MarylandBG)}
\details{
The \var{MarylandBG} border group dataset contains the following data.frames: 
 \describe{
   \item{areaParms}{ - contains specific parameters for the border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, and numerical identifier 
    for the counties in the state of Maryland.}
   \item{areaVisBorders}{ - the boundary point lists for each county area in Maryland.}
   \item{L2VisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data is not used and set to L3VisBorders as a place holder.}
   \item{RegVisBorders}{ - the boundaries for an intermediate level. For this border group, this 
    boundary data is not used and set to L3VisBorders as a place holder.}
   \item{L3VisBorders}{ - the boundary of the state of Maryland }
 }

The Maryland county border group contains 24 county sub-areas.  Each county has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions are defined in the Utah county border group, so the L2VisBorders and RegVisBorders 
data.frames is not used and the \option{dataRegionsOnly} call parameter are is disabled.  
The L3VisBorders dataset contains the outline of the state of Maryland. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (county) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call argument. 

A column or the data.frame row.names must match one of the types of names in the 
\var{areaNamesAbbrsIDs} data.frame name table. If the data row does not match a value 
in the name table, an warning is issued and the data is ignored. If no data is present 
for a sub-area (county) in the name table, the sub-area is mapped but not colored.
 
The following are a list of the names, abbreviations, and ids for each country in the 
\var{MarylandBG} border group.
 \tabular{lll}{
     name\tab ab\tab id\cr
     Allegany\tab AL\tab 24001\cr
     Anne Arundel\tab AA\tab 24003\cr
     Baltimore\tab BL\tab 24005\cr
     Baltimore City\tab BC\tab 24510\cr
     Calvert\tab CV\tab 24009\cr
     Caroline\tab CL\tab 24011\cr
     Carroll\tab CR\tab 24013\cr
     Cecil\tab CC\tab 24015\cr
     Charles\tab CH\tab 24017\cr
     Dorchester\tab DR\tab 24019\cr
     Frederick\tab FR\tab 24021\cr
     Garrett\tab GR\tab 24023\cr
     Harford\tab HR\tab 24025\cr
     Howard\tab HW\tab 24027\cr
     Kent\tab KN\tab 24029\cr
     Montgomery\tab MG\tab 24031\cr
     Prince George's\tab PG\tab 24033\cr
     Queen Anne's\tab QA\tab 24035\cr
     St. Mary's\tab SM\tab 24039\cr
     Somerset\tab SS\tab 24037\cr
     Talbot\tab TB\tab 24041\cr
     Washington\tab WH\tab 24043\cr
     Wicomico\tab WC\tab 24045\cr
     Worcester\tab WR\tab 24047\cr
     }

There are no alternate abbreviations or regions assocated with counties in Maryland border group.

The \var{id} field value is the U. S. state and county FIPS code.

The \option{rowNames} = "alias" or "alt_ab" and the \option{regionB} and \option{dataRegionsOnly} call 
parameters are not supported in the \var{MarylandBG} border group.

}

\keyword{datasets}

