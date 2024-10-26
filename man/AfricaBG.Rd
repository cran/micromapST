\name{AfricaBG}
\docType{data}
\alias{AfricaBG}
\title{AfricaBG border group datasets to support creating
micromaps for the 52 countries in Africa}
\description{
 The \var{micromapST} function has the ability to generate linked
 micromaps for any geographical area.  To specify the geographical
 area, the \option{bordGrp} call argument is used to specify the
 border group dataset for the geographical area. The
 \var{AfricaBG} border group dataset supports creating linked
 micromaps for the 52 countries (sub-areas) on the African
 continent.  When the \option{bordGrp} call argument is set to
 \var{AfricaBG}, the appropriate name table (country names and
 abbreviations) and the 52 sub-areas (countries) boundary data is
 loaded in \var{micromapST}. The user's data is then linked to the
 boundary data via the country's name, abbreviation, alternate
 abbreviation, or ID based on the table below.
}
\usage{data(AfricaBG)} 
\details{
The \var{AfricaBG} border group dataset contains the following
data.frames:
\describe{
  \item{areaParms}{ - contains specific parameters for the
    border group} \item{areaNamesAbbrsIDs}{ - containing the
    names, abbreviations, and numerical identifier
    for the 59 countries in Africa.}
  \item{areaVisBorders}{ - the boundary point lists for each
    country in Africa.} \item{L2VisBorders}{ - the boundaries
    for an intermediate level and is not used in this border
    group and is set to L3VisBorders as a place holder.}
  \item{RegVisBorders}{ - the boundaries for regions in Africa.
    In this implementation of the border group, no regions are
    specified. This data frame is not used and is set to
    L3VisBorders as a place holder.} 
  \item{L3VisBorders}{ - the boundary of the Africa continent.}
}
The Africa continent border group contains 52 country sub-areas.
Each country has a row in the areaNamesAbbrsIDs data.frame and a
set of polygons in the areaVisBorders data.frame datasets. No
regions are defined in the Africa border group, so the
L2VisBorders dataset is not used and the regions option is
disabled. The L3VisBorders dataset contains the outline of the
Africa continent.

The details on each of these data.frame structures can be found
in the "bordGrp" section of this document.  The
\var{areaNamesAbbrsIDs} data.frame provides the linkages to the
boundary data for each sub-area (country) using the fullname,
abbreviation, alternate abbreviation, and numerical identifier
for each country to the \var{<statsDFrame>} data based on the
setting of the \option{rowNames} call argument.

A column or the data.frame row.names must match one of the types
of names in the \var{areaNamesAbbrsIDs} data.frame name table.
If the data row does not match a value in the name table, an
warning is issued and the data is ignored. If no data is present
for a sub-area (country) in the name table, the sub-area is
mapped but not colored.

The following are a list of the names, abbreviations, alternate
abbreviations and IDs for each country in the \var{AfricaBG}
border group.
\tabular{llll}{
  name\tab          ab\tab alt_ab\tab id\cr
  Algeria\tab       ALG\tab  DZ\tab  01\cr 
  Angola\tab        ANG\tab  AO\tab  02\cr 
  Benin\tab         BEN\tab  BJ\tab  03\cr 
  Botswana\tab      BOT\tab  BW\tab  04\cr 
  Burkina Faso\tab  BUF\tab  BF\tab  05\cr 
  Burundi\tab       BUR\tab  BI\tab  06\cr 
  Cameroon\tab      CAM\tab  CM\tab  07\cr 
  Cape Verde\tab    CAP\tab  CV\tab  08\cr 
  Central African Republic\tab CAR\tab CF\tab 09\cr 
  Chad\tab          CHA\tab  TD\tab 10\cr 
  Comoros\tab       COM\tab  KM\tab 11\cr 
  Congo-Brazzaville\tab CNG\tab  CG\tab 12\cr 
  Cote d`Ivoire\tab CDI\tab  CI\tab 13\cr 
  Democratic Republic of Congo\tab ZAI\tab ZR\tab 14\cr 
  Djibouti\tab      DJI\tab  DJ\tab 15\cr 
  Egypt\tab         EGY\tab  EG\tab 16\cr 
  Equatorial Guinea\tab EQG\tab  GQ\tab 17\cr 
  Eritrea\tab       ERI\tab  ER\tab 18\cr 
  Ethiopia\tab      ETH\tab  ET\tab 19\cr 
  Gabon\tab         GAB\tab  GA\tab 20\cr 
  Gambia\tab        GAM\tab  GM\tab 21\cr 
  Ghana\tab         GHA\tab  GH\tab 22\cr 
  Guinea\tab        GIN\tab  GN\tab 23\cr 
  Guinea-Bissau\tab GUB\tab  GW\tab 24\cr 
  Kenya\tab         KEN\tab  KE\tab 25\cr 
  Lesotho\tab       LES\tab  LS\tab 26\cr 
  Liberia\tab       LIB\tab  LR\tab 27\cr 
  Libya\tab         LAJ\tab  LY\tab 28\cr 
  Madagascar\tab    MAD\tab  MG\tab 29\cr 
  Malawi\tab        MAA\tab  MW\tab 30\cr 
  Mali\tab          MAL\tab  ML\tab 31\cr 
  Mauritania\tab    MAU\tab  MR\tab 32\cr 
  Morocco\tab       MOR\tab  MA\tab 33\cr 
  Mozambique\tab    MOZ\tab  MZ\tab 34\cr 
  Namibia\tab       NAM\tab  NA\tab 35\cr 
  Niger\tab         NIG\tab  NE\tab 36\cr 
  Nigeria\tab       NIR\tab  NG\tab 37\cr 
  Rwanda\tab        RWA\tab  RW\tab 38\cr 
  Sao Tome and Principe\tab STP\tab ST\tab 39\cr 
  Senegal\tab       SEN\tab  SN\tab 40\cr 
  Sierra Leone\tab  SIL\tab  SL\tab 41\cr 
  Somalia\tab       SOM\tab  SO\tab 42\cr 
  South Africa\tab  SOU\tab  SA\tab 43\cr 
  Sudan\tab         SUD\tab  SD\tab 44\cr 
  Swaziland\tab     SWA\tab  SZ\tab 45\cr 
  Tanzania\tab      TAN\tab  TZ\tab 46\cr 
  Togo\tab          TOG\tab  TG\tab 47\cr 
  Tunisia\tab       TUN\tab  TN\tab 48\cr 
  Uganda\tab        UGA\tab  UG\tab 49\cr 
  Western Sahara\tab WES\tab WS\tab 50\cr 
  Zambia\tab        ZAM\tab  ZM\tab 51\cr 
  Zimbabwe\tab      ZIM\tab  ZW\tab 52\cr 
}
When this border group was created, there appeared to be no
consistant set of abbreviations for the African countries.
Therefore, the two most commonly found sets of abbreviations are
included as the \var{abbr} and \var{alt_abbr} abbreviation sets.
Set \option{rownames} to "ab" to reference the primary set and
"alt_ab" to reference the second set of abbreviates in the name
table.

The \option{rowNames} = "alias" and the \option{regionB} and
\option{dataRegionsOnly} features are not supported in the
\var{AfricaBG} border group.
}

\keyword{datasets}

