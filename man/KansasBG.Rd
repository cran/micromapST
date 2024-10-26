\name{KansasBG}
\docType{data}
\alias{KansasBG}
\title{KansasBG border group datasets to support creating micromaps for the 105 counties 
in the state of Kansas}
\description{
 The \var{micromapST} function has the ability to generate linked micromaps for 
 any geographical area.  To specify the geographical area, the \option{bordGrp} 
 call argument is used to specify the border group dataset for the geographical area.  
 When \var{micromapST} function is used to micromap data for Kansas County area, the 
 border group option (\option{bordGrp})
 is set to "KansasBG".  This instructs micromapST to load the area Name, Abbreviation,
 ID and boundaries files for Kansas 105 counties.
 The datasets contained in the border group are areaNamesAbbrsIDs, areaVisBorders,
 L2VisBorders, and \cr L3VisBorders for the counties of the state Kansas.
 The user's data is then linked to the boundary data via the county's name, 
 abbreviated name or ID based on the table below.
}
\usage{data(KansasBG)}
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

The Kansas county border group contains 105 county sub-areas.  Each county has a row in the 
areaNamesAbbrsIDs data.frame and a set of polygons in the areaVisBorders data.frame datasets.
No regions are defined in the Kansas county border group, so the \var{L2VisBorders} and 
\var{RegVisBorders} datasets are not used and the regions feations is disabled.  
The L3VisBorders dataset contains 
the outline of the state of Kansas. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area (county) using the fullname, abbreviation,  
and numerical identifier for each country to the \var{<statsDFrame>} data based on the setting 
of the \option{rowNames} call parameter. 

A column or the data.frame row.names must match one of the types of names in the 
\var{areaNamesAbbrsIDs} data.frame name table. If the data row does not match a value 
in the name table, an warning is issued and the data is ignored. If no data is present 
for a sub-area (county) in the name table, the sub-area (county) is mapped but not colored.

The following are a list of the names, abbreviations, and IDs for each country in the 
\var{KansasBG} border group.
   \tabular{lll}{
     name\tab ab\tab id\cr
     Allen\tab AL\tab 20001\cr
     Anderson\tab AN\tab 20003\cr
     Atchison\tab AT\tab 20005\cr
     Barber\tab BA\tab 20007\cr
     Barton\tab BT\tab 20009\cr
     Bourbon\tab BB\tab 20011\cr
     Brown\tab BR\tab 20013\cr
     Butler\tab BU\tab 20015\cr
     Chase\tab CS\tab 20017\cr
     Chautauqua\tab CQ\tab 20019\cr
     Cherokee\tab CK\tab 20021\cr
     Cheyenne\tab CN\tab 20023\cr
     Clark\tab CA\tab 20025\cr
     Clay\tab CY\tab 20027\cr
     Cloud\tab CD\tab 20029\cr
     Coffey\tab CF\tab 20031\cr
     Comanche\tab CM\tab 20033\cr
     Cowley\tab CL\tab 20035\cr
     Crawford\tab CR\tab 20037\cr
     Decatur\tab DC\tab 20039\cr
     Dickinson\tab DK\tab 20041\cr
     Doniphan\tab DP\tab 20043\cr
     Douglas\tab DG\tab 20045\cr
     Edwards\tab ED\tab 20047\cr
     Elk\tab EK\tab 20049\cr
     Ellis\tab EL\tab 20051\cr
     Ellsworth\tab EW\tab 20053\cr
     Finney\tab FI\tab 20055\cr
     Ford\tab FO\tab 20057\cr
     Franklin\tab FR\tab 20059\cr
     Geary\tab GE\tab 20061\cr
     Gove\tab GO\tab 20063\cr
     Graham\tab GH\tab 20065\cr
     Grant\tab GT\tab 20067\cr
     Gray\tab GY\tab 20069\cr
     Greeley\tab GL\tab 20071\cr
     Greenwood\tab GW\tab 20073\cr
     Hamilton\tab HM\tab 20075\cr
     Harper\tab HP\tab 20077\cr
     Harvey\tab HV\tab 20079\cr
     Haskell\tab HS\tab 20081\cr
     Hodgeman\tab HG\tab 20083\cr
     Jackson\tab JA\tab 20085\cr
     Jefferson\tab JF\tab 20087\cr
     Jewell\tab JW\tab 20089\cr
     Johnson\tab JO\tab 20091\cr
     Kearny\tab KE\tab 20093\cr
     Kingman\tab KM\tab 20095\cr
     Kiowa\tab KW\tab 20097\cr
     Labette\tab LB\tab 20099\cr
     Lane\tab LE\tab 20101\cr
     Leavenworth\tab LV\tab 20103\cr
     Lincoln\tab LC\tab 20105\cr
     Linn\tab LN\tab 20107\cr
     Logan\tab LG\tab 20109\cr
     Lyon\tab LY\tab 20111\cr
     Marion\tab MN\tab 20115\cr
     Marshall\tab MS\tab 20117\cr
     McPherson\tab MP\tab 20113\cr
     Meade\tab ME\tab 20119\cr
     Miami\tab MI\tab 20121\cr
     Mitchell\tab MC\tab 20123\cr
     Montgomery\tab MG\tab 20125\cr
     Morris\tab MR\tab 20127\cr
     Morton\tab MT\tab 20129\cr
     Nemaha\tab NM\tab 20131\cr
     Neosho\tab NO\tab 20133\cr
     Ness\tab NS\tab 20135\cr
     Norton\tab NT\tab 20137\cr
     Osage\tab OS\tab 20139\cr
     Osborne\tab OB\tab 20141\cr
     Ottawa\tab OT\tab 20143\cr
     Pawnee\tab PN\tab 20145\cr
     Phillips\tab PL\tab 20147\cr
     Pottawatomie\tab PT\tab 20149\cr
     Pratt\tab PR\tab 20151\cr
     Rawlins\tab RA\tab 20153\cr
     Reno\tab RN\tab 20155\cr
     Republic\tab RP\tab 20157\cr
     Rice\tab RC\tab 20159\cr
     Riley\tab RL\tab 20161\cr
     Rooks\tab RO\tab 20163\cr
     Rush\tab RH\tab 20165\cr
     Russell\tab RS\tab 20167\cr
     Saline\tab SA\tab 20169\cr
     Scott\tab SC\tab 20171\cr
     Sedgwick\tab SG\tab 20173\cr
     Seward\tab SW\tab 20175\cr
     Shawnee\tab SN\tab 20177\cr
     Sheridan\tab SD\tab 20179\cr
     Sherman\tab SH\tab 20181\cr
     Smith\tab SM\tab 20183\cr
     Stafford\tab SF\tab 20185\cr
     Stanton\tab ST\tab 20187\cr
     Stevens\tab SV\tab 20189\cr
     Sumner\tab SU\tab 20191\cr
     Thomas\tab TH\tab 20193\cr
     Trego\tab TR\tab 20195\cr
     Wabaunsee\tab WB\tab 20197\cr
     Wallace\tab WA\tab 20199\cr
     Washington\tab WS\tab 20201\cr
     Wichita\tab WH\tab 20203\cr
     Wilson\tab WL\tab 20205\cr
     Woodson\tab WO\tab 20207\cr
     Wyandotte\tab WY\tab 20209\cr
   }
There are no alternate abbreviations or regions assocated with counties in Kansas.

The \var{id} field value is the U. S. state and county FIPS code.

The \option{rowNames} = "alias" or "alt_ab" and the \option{regionB} and \option{dataRegionsOnly} features are not 
supported in the \var{KansasBG} border group.

}

\keyword{datasets}

