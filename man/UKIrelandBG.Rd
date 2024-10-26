\name{UKIrelandBG}
\docType{data}
\alias{UKIrelandBG}
\title{UKIrelandBG border group datasets contains the boundary information for the United Kingdom and 
  Ireland.}
\description{
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call argument is used to specify the border group dataset for the geographical area.  
  The \var{UKIrelandBG} border group dataset supports creating linked micromaps for the 
  all of the United Kingdom including Northern Ireland and the Isle of Man and Ireland.
  When the \option{bordGrp} call argument is set 
  to \var{UKIrelandBG}, the appropriate name table (county names and abbreviations) and 
  the 219 sub-areas (counties, etc.) boundary data is loaded into \var{micromapST}.  
  The user's data is then linked to the boundary data via the name, abbreviation, or alternate 
  abbreviation for each sub-area (county, etc.).

  The United Kingdom and Ireland information was pulled from the UK and Ireland public web sites 
  in March of 2015.
  
  The UKIreland border group was constructed to provide an area with more than 100 sub-areas for 
  testing \var{micromapST} and enhancing it's ability to handle a large number of sub-areas and
  generate usable linked micromaps.
}
\usage{data(UKIrelandBG)}
\details{
The \var{UKIrelandBG} border group dataset contains the following data.frames: 
  \describe{
    \item{areaParms}{ - contains specific parameters for the border group
     }
    \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, alternate 
      abbreviation, or numerical identifier for each of the United Kingdom or 
      Ireland counties.
     }
    \item{areaVisBorders}{ - the boundary point lists for each sub-area.
     }
    \item{L2VisBorders}{ - the boundaries for an intermediate level. 
      For the United Kingdom and Ireland border group, this boundary point list is 
      not used and is set to equal L3VisBorders data.frame for the border group.
     }
    \item{RegVisBorders}{ - the boundaries for the 4 United Kingdom and Ireland regions
      or realms: England, Wales, Scotland, Northern Ireland, Ireland, and Isle of Man. 
     }
    \item{L3VisBorders}{ - the boundary of the United Kingdom and Ireland area.
      }
   }

The UKIreland border group contains 219 sub-areas (counties, etc.)
Each registry has a row in the \var{areaNamesAbbrsIDs} data.frame and 
a set of polygons in the areaVisBorders data.frame datasets.
Regions are defined in this border group as the 6 country and kingdom regions 
in the UK and Ireland.   The regions feature is enable. The siz (6) regions 
are: England, Scotland, Wales, Northern Ireland, Ireland and Isle of Man. 
The names, abbreviations, alternate abbreviations and IDs for the counties 
in the UKIreland border group are:
  \tabular{lllll}{
      Name\tab ab\tab id\tab alt_ab\tab region\cr
Aberdeen\tab                     GB.ABE\tab 826139\tab ABE\tab SCT\cr 
Aberdeenshire\tab                GB.ABD\tab 826140\tab ABD\tab SCT\cr 
c\tab                     GB.AGY\tab 826171\tab AGY\tab WLS\cr 
Angus\tab                        GB.ANS\tab 826141\tab ANS\tab SCT\cr 
Antrim\tab                       GB.ANT\tab 826113\tab ANT\tab NIR\cr 
Ards\tab                         GB.ARD\tab 826114\tab ARD\tab NIR\cr 
Argyll and Bute\tab              GB.AGB\tab 826142\tab AGB\tab SCT\cr 
Armagh\tab                       GB.ARM\tab 826115\tab ARM\tab NIR\cr 
Ballymena\tab                    GB.BLA\tab 826116\tab BLA\tab NIR\cr 
Ballymoney\tab                   GB.BLY\tab 826117\tab BLY\tab NIR\cr 
Banbridge\tab                    GB.BNB\tab 826118\tab BNB\tab NIR\cr 
Barking and Dagenham\tab         GB.BDG\tab 826001\tab BDG\tab ENG\cr 
Bath and North East Somerset\tab GB.BAS\tab 826002\tab BAS\tab ENG\cr 
Bedfordshire\tab                 GB.BDF\tab 826003\tab BDF\tab ENG\cr 
Belfast\tab                      GB.BFS\tab 826119\tab BFS\tab NIR\cr 
Berkshire\tab                    GB.BRK\tab 826004\tab BRK\tab ENG\cr 
Bexley\tab                       GB.BEX\tab 826005\tab BEX\tab ENG\cr 
Blackburn with Darwen\tab        GB.BBD\tab 826006\tab BBD\tab ENG\cr 
Blaenau Gwent\tab                GB.BGW\tab 826172\tab BGW\tab WLS\cr 
Bournemouth\tab                  GB.BMH\tab 826007\tab BMH\tab ENG\cr 
Brent\tab                        GB.BEN\tab 826008\tab BEN\tab ENG\cr 
Bridgend\tab                     GB.BGE\tab 826173\tab BGE\tab WLS\cr 
Brighton and Hove\tab            GB.BNH\tab 826009\tab BNH\tab ENG\cr 
Bristol\tab                      GB.BST\tab 826010\tab BST\tab ENG\cr 
Bromley\tab                      GB.BRY\tab 826011\tab BRY\tab ENG\cr 
Buckinghamshire\tab              GB.BKM\tab 826012\tab BKM\tab ENG\cr 
Caerphilly\tab                   GB.CAY\tab 826174\tab CAY\tab WLS\cr 
Cambridgeshire\tab               GB.CAM\tab 826013\tab CAM\tab ENG\cr 
Camden\tab                       GB.CMD\tab 826014\tab CMD\tab ENG\cr 
Cardiff\tab                      GB.CRF\tab 826175\tab CRF\tab WLS\cr 
Carlow\tab                       IE.CW\tab  372001\tab CW\tab  IRE\cr 
Carmarthenshire\tab              GB.CMN\tab 826176\tab CMN\tab WLS\cr 
Carrickfergus\tab                GB.CKF\tab 826120\tab CKF\tab NIR\cr 
Castlereagh\tab                  GB.CSR\tab 826121\tab CSR\tab NIR\cr 
Cavan\tab                        IE.CN\tab  372002\tab CN\tab  IRE\cr 
Ceredigion\tab                   GB.CGN\tab 826177\tab CGN\tab WLS\cr 
Cheshire\tab                     GB.CHS\tab 826015\tab CHS\tab ENG\cr 
Clackmannanshire\tab             GB.CLK\tab 826143\tab CLK\tab SCT\cr 
Clare\tab                        IE.CE\tab  372003\tab CE\tab  IRE\cr 
Coleraine\tab                    GB.CLR\tab 826122\tab CLR\tab NIR\cr 
Conwy\tab                        GB.CWY\tab 826178\tab CWY\tab WLS\cr 
Cookstown\tab                    GB.CKT\tab 826123\tab CKT\tab NIR\cr 
Cork\tab                         IE.CO\tab  372004\tab CO\tab  IRE\cr 
Cornwall\tab                     GB.CON\tab 826016\tab CON\tab ENG\cr 
Craigavon\tab                    GB.CGV\tab 826124\tab CGV\tab NIR\cr 
Croydon\tab                      GB.CRY\tab 826017\tab CRY\tab ENG\cr 
Cumbria\tab                      GB.CMA\tab 826018\tab CMA\tab ENG\cr 
Darlington\tab                   GB.DAL\tab 826019\tab DAL\tab ENG\cr 
Denbighshire\tab                 GB.DEN\tab 826179\tab DEN\tab WLS\cr 
Derby\tab                        GB.DER\tab 826020\tab DER\tab ENG\cr 
Derbyshire\tab                   GB.DBY\tab 826021\tab DBY\tab ENG\cr 
Derry\tab                        GB.DRY\tab 826125\tab DRY\tab NIR\cr 
Devon\tab                        GB.DEV\tab 826022\tab DEV\tab ENG\cr 
Donegal\tab                      IE.DL\tab  372005\tab DL\tab  IRE\cr 
Dorset\tab                       GB.DOR\tab 826023\tab DOR\tab ENG\cr 
Down\tab                         GB.DOW\tab 826126\tab DOW\tab NIR\cr 
Dublin\tab                       IE.D\tab   372006\tab D\tab   IRE\cr 
Dumfries and Galloway\tab        GB.DGY\tab 826144\tab DGY\tab SCT\cr 
Dundee\tab                       GB.DND\tab 826145\tab DND\tab SCT\cr 
Dungannon\tab                    GB.DGN\tab 826127\tab DGN\tab NIR\cr 
Durham\tab                       GB.DUR\tab 826024\tab DUR\tab ENG\cr 
Ealing\tab                       GB.EAL\tab 826025\tab EAL\tab ENG\cr 
East Ayrshire\tab                GB.EAY\tab 826146\tab EAY\tab SCT\cr 
East Dunbartonshire\tab          GB.EDU\tab 826147\tab EDU\tab SCT\cr 
East Lothian\tab                 GB.ELN\tab 826148\tab ELN\tab SCT\cr 
East Renfrewshire\tab            GB.ERW\tab 826149\tab ERW\tab SCT\cr 
East Riding of Yorkshire\tab     GB.ERY\tab 826026\tab ERY\tab ENG\cr 
East Sussex\tab                  GB.ESX\tab 826027\tab ESX\tab ENG\cr 
Edinburgh\tab                    GB.EDH\tab 826150\tab EDH\tab SCT\cr 
Eilean Siar\tab                  GB.ELS\tab 826151\tab ELS\tab SCT\cr 
Enfield\tab                      GB.ENF\tab 826028\tab ENF\tab ENG\cr 
Essex\tab                        GB.ESS\tab 826029\tab ESS\tab ENG\cr 
Falkirk\tab                      GB.FAL\tab 826152\tab FAL\tab SCT\cr 
Fermanagh\tab                    GB.FER\tab 826128\tab FER\tab NIR\cr 
Fife\tab                         GB.FIF\tab 826153\tab FIF\tab SCT\cr 
Flintshire\tab                   GB.FLN\tab 826180\tab FLN\tab WLS\cr 
Galway\tab                       IE.G\tab   372007\tab G\tab   IRE\cr 
Glasgow\tab                      GB.GLG\tab 826154\tab GLG\tab SCT\cr 
Gloucestershire\tab              GB.GLS\tab 826030\tab GLS\tab ENG\cr 
Greenwich\tab                    GB.GRE\tab 826031\tab GRE\tab ENG\cr 
Gwynedd\tab                      GB.GWN\tab 826181\tab GWN\tab WLS\cr 
Hackney\tab                      GB.HCK\tab 826032\tab HCK\tab ENG\cr 
Halton\tab                       GB.HAL\tab 826033\tab HAL\tab ENG\cr 
Hammersmith and Fulham\tab       GB.HMF\tab 826034\tab HMF\tab ENG\cr 
Hampshire\tab                    GB.HAM\tab 826035\tab HAM\tab ENG\cr 
Haringey\tab                     GB.HRY\tab 826036\tab HRY\tab ENG\cr 
Harrow\tab                       GB.HRW\tab 826037\tab HRW\tab ENG\cr 
Hartlepool\tab                   GB.HPL\tab 826038\tab HPL\tab ENG\cr 
Havering\tab                     GB.HAV\tab 826039\tab HAV\tab ENG\cr 
Herefordshire\tab                GB.HEF\tab 826040\tab HEF\tab ENG\cr 
Hertfordshire\tab                GB.HRT\tab 826041\tab HRT\tab ENG\cr 
Highland\tab                     GB.HLD\tab 826155\tab HLD\tab SCT\cr 
Hillingdon\tab                   GB.HIL\tab 826042\tab HIL\tab ENG\cr 
Hounslow\tab                     GB.HNS\tab 826043\tab HNS\tab ENG\cr 
Inverclyde\tab                   GB.IVC\tab 826156\tab IVC\tab SCT\cr 
Isle of Wight\tab                GB.IOW\tab 826044\tab IOW\tab ENG\cr 
Islington\tab                    GB.ISL\tab 826045\tab ISL\tab ENG\cr 
Kensington and Chelsea\tab       GB.KEC\tab 826046\tab KEC\tab ENG\cr 
Kent\tab            GB.KEN\tab 826047\tab KEN\tab ENG\cr 
Kerry\tab           IE.KY\tab 372008\tab KY\tab IRE\cr 
Kildare\tab         IE.KE\tab 372009\tab KE\tab IRE\cr 
Kilkenny\tab        IE.KK\tab 372010\tab KK\tab IRE\cr 
Kingston upon Hull\tab   GB.KHL\tab 826048\tab KHL\tab ENG\cr 
Kingston upon Thames\tab GB.KTT\tab 826049\tab KTT\tab ENG\cr 
Lambeth\tab         GB.LBH\tab 826050\tab LBH\tab ENG\cr 
Lancashire\tab      GB.LAN\tab 826051\tab LAN\tab ENG\cr 
Laoighis\tab        IE.LS\tab  372011\tab LS\tab IRE\cr 
Larne\tab           GB.LRN\tab 826129\tab LRN\tab NIR\cr 
Leicester\tab       GB.LCE\tab 826052\tab LCE\tab ENG\cr 
Leicestershire\tab  GB.LEC\tab 826053\tab LEC\tab ENG\cr 
Leitrim\tab         IE.LM\tab  372012\tab LM\tab IRE\cr 
Lewisham\tab        GB.LEW\tab 826054\tab LEW\tab ENG\cr 
Limavady\tab        GB.LMV\tab 826130\tab LMV\tab NIR\cr 
Limerick\tab        IE.LK\tab  372013\tab LK\tab IRE\cr 
Lincolnshire\tab    GB.LIN\tab 826055\tab LIN\tab ENG\cr 
Lisburn\tab         GB.LSB\tab 826131\tab LSB\tab NIR\cr 
London\tab          GB.LND\tab 826056\tab LND\tab ENG\cr 
Longford\tab        IE.LD\tab  372014\tab LD\tab IRE\cr 
Louth\tab           IE.LH\tab  372015\tab LH\tab IRE\cr 
Luton\tab           GB.LUT\tab 826057\tab LUT\tab ENG\cr 
Magherafelt\tab     GB.MFT\tab 826132\tab MFT\tab NIR\cr 
Manchester\tab      GB.MAN\tab 826058\tab MAN\tab ENG\cr 
Mayo\tab            IE.MO\tab  372016\tab MO\tab IRE\cr 
Meath\tab           IE.MH\tab  372017\tab MH\tab IRE\cr 
Medway\tab          GB.MDW\tab 826059\tab MDW\tab ENG\cr 
Merseyside\tab      GB.MSY\tab 826060\tab MSY\tab ENG\cr 
Merthyr Tydfil\tab  GB.MTY\tab 826182\tab MTY\tab WLS\cr 
Merton\tab          GB.MRT\tab 826061\tab MRT\tab ENG\cr 
Middlesbrough\tab   GB.MDB\tab 826062\tab MDB\tab ENG\cr 
Midlothian\tab      GB.MLN\tab 826157\tab MLN\tab SCT\cr 
Milton Keynes\tab   GB.MIK\tab 826063\tab MIK\tab ENG\cr 
Monaghan\tab        IE.MN\tab  372018\tab MN\tab IRE\cr 
Monmouthshire\tab   GB.MON\tab 826183\tab MON\tab WLS\cr 
Moray\tab           GB.MRY\tab 826158\tab MRY\tab SCT\cr 
Moyle\tab           GB.MYL\tab 826133\tab MYL\tab NIR\cr 
Neath Port Talbot\tab GB.NTL\tab 826184\tab NTL\tab WLS\cr 
Newham\tab            GB.NWM\tab 826064\tab NWM\tab ENG\cr 
Newport\tab           GB.NWP\tab 826185\tab NWP\tab WLS\cr 
Newry and Mourne\tab  GB.NYM\tab 826134\tab NYM\tab NIR\cr 
Newtownabbey\tab      GB.NTA\tab 826135\tab NTA\tab NIR\cr 
Norfolk\tab           GB.NFK\tab 826065\tab NFK\tab ENG\cr 
North Ayrshire\tab    GB.NAY\tab 826159\tab NAY\tab SCT\cr 
North Down\tab        GB.NDN\tab 826136\tab NDN\tab NIR\cr 
North East Lincolnshire\tab GB.NEL\tab 826066\tab NEL\tab ENG\cr 
North Lanarkshire\tab GB.NLK\tab 826160\tab NLK\tab SCT\cr 
North Lincolnshire\tab GB.NLN\tab 826067\tab NLN\tab ENG\cr 
North Somerset\tab    GB.NSM\tab 826068\tab NSM\tab ENG\cr 
North Yorkshire\tab   GB.NYK\tab 826069\tab NYK\tab ENG\cr 
Northamptonshire\tab  GB.NTH\tab 826070\tab NTH\tab ENG\cr 
Northumberland\tab  GB.NBL\tab 826071\tab NBL\tab ENG\cr 
Nottingham\tab      GB.NGM\tab 826072\tab NGM\tab ENG\cr 
Nottinghamshire\tab GB.NTT\tab 826073\tab NTT\tab ENG\cr 
Offaly\tab          IE.OY\tab 372019\tab OY\tab IRE\cr 
Omagh\tab           GB.OMH\tab 826137\tab OMH\tab NIR\cr 
Orkney Islands\tab  GB.ORK\tab 826161\tab ORK\tab SCT\cr 
Oxfordshire\tab     GB.OXF\tab 826074\tab OXF\tab ENG\cr 
Pembrokeshire\tab   GB.PEM\tab 826186\tab PEM\tab WLS\cr 
Perthshire and Kinross\tab GB.PKN\tab 826162\tab PKN\tab SCT\cr 
Peterborough\tab    GB.PTE\tab 826075\tab PTE\tab ENG\cr 
Plymouth\tab        GB.PLY\tab 826076\tab PLY\tab ENG\cr 
Poole\tab           GB.POL\tab 826077\tab POL\tab ENG\cr 
Portsmouth\tab      GB.POR\tab 826078\tab POR\tab ENG\cr 
Powys\tab           GB.POW\tab 826187\tab POW\tab WLS\cr 
Redbridge\tab       GB.RDB\tab 826079\tab RDB\tab ENG\cr 
Redcar and Cleveland\tab GB.RCC\tab 826080\tab RCC\tab ENG\cr 
Renfrewshire\tab    GB.RFW\tab 826163\tab RFW\tab SCT\cr 
Rhondda, Cynon, Taff\tab GB.RCT\tab 826188\tab RCT\tab WLS\cr 
Richmond upon Thames\tab GB.RIC\tab 826081\tab RIC\tab ENG\cr 
Roscommon\tab       IE.RN\tab 372020\tab RN\tab IRE\cr 
Rutland\tab         GB.RUT\tab 826082\tab RUT\tab ENG\cr 
Scottish Borders\tab GB.SCB\tab 826164\tab SCB\tab SCT\cr 
Shetland Islands\tab GB.ZET\tab 826165\tab ZET\tab SCT\cr 
Shropshire\tab      GB.SHR\tab 826083\tab SHR\tab ENG\cr 
Sligo\tab           IE.SO\tab  372021\tab SO\tab IRE\cr 
Somerset\tab        GB.SOM\tab 826084\tab SOM\tab ENG\cr 
South Ayrshire\tab  GB.SAY\tab 826166\tab SAY\tab SCT\cr 
South Gloucestershire\tab GB.SGC\tab 826085\tab SGC\tab ENG\cr 
South Lanarkshire\tab GB.SLK\tab 826167\tab SLK\tab SCT\cr 
South Yorkshire\tab GB.SYK\tab 826086\tab SYK\tab ENG\cr 
Southampton\tab     GB.STH\tab 826087\tab STH\tab ENG\cr 
Southend-on-Sea\tab GB.SOS\tab 826088\tab SOS\tab ENG\cr 
Southwark\tab       GB.SWK\tab 826089\tab SWK\tab ENG\cr 
Staffordshire\tab   GB.STS\tab 826090\tab STS\tab ENG\cr 
Stirling\tab        GB.STG\tab 826168\tab STG\tab SCT\cr 
Stockton-on-Tees\tab GB.STT\tab 826091\tab STT\tab ENG\cr 
Stoke-on-Trent\tab  GB.STE\tab 826092\tab STE\tab ENG\cr 
Strabane\tab        GB.STB\tab 826138\tab STB\tab NIR\cr 
Suffolk\tab         GB.SFK\tab 826093\tab SFK\tab ENG\cr 
Surrey\tab          GB.SRY\tab 826094\tab SRY\tab ENG\cr 
Sutton\tab          GB.STN\tab 826095\tab STN\tab ENG\cr 
Swansea\tab         GB.SWA\tab 826189\tab SWA\tab WLS\cr 
Swindon\tab         GB.SWD\tab 826096\tab SWD\tab ENG\cr 
Telford and Wrekin\tab GB.TFW\tab 826097\tab TFW\tab ENG\cr 
Thurrock\tab        GB.THR\tab 826098\tab THR\tab ENG\cr 
Tipperary\tab       IE.TA\tab  372022\tab TA\tab  IRE\cr 
Torbay\tab          GB.TOB\tab 826099\tab TOB\tab ENG\cr 
Torfaen\tab         GB.TOF\tab 826190\tab TOF\tab WLS\cr 
Tower Hamlets\tab   GB.TWH\tab 826100\tab TWH\tab ENG\cr 
Tyne and Wear\tab   GB.TWR\tab 826101\tab TWR\tab ENG\cr 
Vale of Glamorgan\tab GB.VGL\tab 826191\tab VGL\tab WLS\cr 
Waltham Forest\tab  GB.WFT\tab 826102\tab WFT\tab ENG\cr 
Wandsworth\tab      GB.WND\tab 826103\tab WND\tab ENG\cr 
Warrington\tab      GB.WRT\tab 826104\tab WRT\tab ENG\cr 
Warwickshire\tab    GB.WAR\tab 826105\tab WAR\tab ENG\cr 
Waterford\tab       IE.WD\tab  372023\tab WD\tab  IRE\cr 
West Dunbartonshire\tab GB.WDU\tab 826169\tab WDU\tab SCT\cr 
West Lothian\tab    GB.WLN\tab 826170\tab WLN\tab SCT\cr 
West Midlands\tab   GB.WMD\tab 826106\tab WMD\tab ENG\cr 
West Sussex\tab     GB.WSX\tab 826107\tab WSX\tab ENG\cr 
West Yorkshire\tab  GB.WYK\tab 826108\tab WYK\tab ENG\cr 
Westmeath\tab       IE.WH\tab  372024\tab WH\tab  IRE\cr 
Westminster\tab     GB.WSM\tab 826109\tab WSM\tab ENG\cr 
Wexford\tab         IE.WX\tab  372025\tab WX\tab  IRE\cr 
Wicklow\tab         IE.WW\tab  372026\tab WW\tab  IRE\cr 
Wiltshire\tab       GB.WIL\tab 826110\tab WIL\tab ENG\cr 
Worcestershire\tab  GB.WOR\tab 826111\tab WOR\tab ENG\cr 
Wrexham\tab         GB.WRX\tab 826192\tab WRX\tab WLS\cr 
York\tab            GB.YOR\tab 826112\tab YOR\tab ENG\cr 
Isle of Man\tab     IM\tab     833000\tab IMN\tab IMN\cr 
}

When compiling the abbreviations for this border group, multiple sets of
abbreviations were found.  The two most common sets are included in 
the border group as "ab" and "alt_ab" types of \option{rowNames}.

The L3VisBorders dataset contains the outline of the UK and Ireland. 

The details on each of these data.frame structures can be found in 
the "bordGrp" section of this document.  The \var{areaNamesAbbrsIDs} 
data.frame provides the linkages to the boundary data for each 
sub-area (registry) using the fullname, abbreviation, alternate abbreviation
and numerical identifier for each county/provence to the \var{<statsDFrame>} 
data based on the setting of the \option{rowNames} call argument. 

A column or the data.frame row.names must match one of the types 
of names in the \var{areaNamesAbbrsIDs} data.frame name table. 
If the data row does not match a value in the name table, an warning 
is issued and the data is ignored. If no data is present for a sub-area 
in the name table, the sub-area is mapped but not colored.

The \option{dataRegionsOnly} call parameter instructs the package to only map the 
regions with Seer registers with data. The regions are the four census 
regions: England, Scotland, Wales, Isle of Man, Northern Irelend and Ireland.

}
\source{NCI}
\references{????  Retrieved 2013-01-10.
 } 
\keyword{datasets}

