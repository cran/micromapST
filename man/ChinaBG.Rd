\name{ChinaBG}
\docType{data}
\alias{ChinaBG}
\title{ChinaBG border group datasets to support creating micromaps for the 32 providences
and municipalities in the country of Republic of China}
\description{  
  The \var{micromapST} function has the ability to generate linked micromaps for 
  any geographical area.  To specify the geographical area, the \option{bordGrp} 
  call argument is used to specify the border group dataset for the geographical area.  
  The \var{ChinaBG} border group dataset supports creating linked micromaps for the 
  34 provinces, special administrative regions, metropolitan areas in the China. 
  When the \option{bordGrp} call argument is set 
  to \var{ChinaBG}, the appropriate name table  (sub area names and abbreviations) and 
  the 34 sub-areas (provinces, SAR, cities, etc.) boundary data is loaded in \var{micromapST}.  
  The user's data is then linked to the boundary data via the county's name, 
  abbreviated name or ID based on the table below.
}
\usage{data(ChinaBG)}
\details{
The \var{ChinaBG} border group dataset contains the following data.frames: 
 \describe{
   \item{areaParms}{ - contains specific parameters for the border group}
   \item{areaNamesAbbrsIDs}{ - containing the names, abbreviations, and numerical identifier 
    for the providences and municipalities of China.}
   \item{areaVisBorders}{ - the boundary point lists for each area in China.}
   \item{L2VisBorders}{ - the boundaries for an intermediate level.  This level
    is not used in this border group.}
   \item{RegVisBorders}{ - the boundaries for an regional level for China. This set of boundaries
    are used in conjunction with the regions call parameter.}
   \item{L3VisBorders}{ - the boundary of the country of China.}
 }
For the China border group, there are 34 county sub-areas listed in the 
areaNamesAbbrsIDs and areaVisBorders datasets.  The \var{L2VisBorders} dataset
is not used and is set to the \var{L3VisBorders} dataset as a placeholder.
The \var{RegVisBorders} dataset represents the 
6 regions of China in the \var{ChinaBG} border group.  
The L3VisBorders dataset contains 
the outline of the country of China. 

The details on each of these data.frame structures can be found in the "bordGrp" section 
of this document.  The \var{areaNamesAbbrsIDs} data.frame provides the linkages 
to the boundary data for each sub-area using the fullname,
abbreviation, alternate abbreviation, and numerical identifier for each country 
to the \var{<statsDFrame>} data based on the setting of the \option{rowNames} call argument.  
A column or the data.frame row.names must match one
of the types of names in the \var{areaNamesAbbrsIDs} data.frame name table.  
If the data row does not match a value in the name table, an warning is issued 
and the data is ignored. If no data is present for a sub-area (county) 
in the name table, the sub-area is mapped but not colored.

The following are a list of the names, abbreviations, alternate abbreviations, ids,
and the region for each county, province or metro area in the \var{ChinaBG} border group.
\tabular{lllll}{
  name\tab ab\tab alt_ab\tab id\tab region\cr
  Anhui\tab AH\tab CN.AH\tab 34\tab Huadong\cr
  Beijing\tab BJ\tab CN.BJ\tab 11\tab Huabei\cr
  Chongqing\tab CQ\tab CN.CQ\tab 50\tab Xinan\cr
  Fujian\tab FJ\tab CN.FJ\tab 35\tab Huadong\cr
  Gansu\tab GS\tab CN.GS\tab 62\tab Xibei\cr
  Guangdong\tab GD\tab CN.GD\tab 44\tab Zhongnan\cr
  Guangxi\tab GX\tab CN.GX\tab 45\tab Zhongnan\cr
  Guizhou\tab GZ\tab CN.GZ\tab 52\tab Xinan\cr
  Hainan\tab HI\tab CN.HA\tab 46\tab Zhongnan\cr
  Hebei\tab HE\tab CN.HB\tab 13\tab Huabei\cr
  Heilongjiang\tab HL\tab CN.HL\tab 23\tab Dongbei\cr
  Henan\tab HA\tab CN.HE\tab 41\tab Zhongnan\cr
  Hubei\tab HB\tab CN.HU\tab 42\tab Zhongnan\cr
  Hunan\tab HN\tab CN.HN\tab 43\tab Zhongnan\cr
  Jiangsu\tab JS\tab CN.JS\tab 32\tab Huadong\cr
  Jiangxi\tab JX\tab CN.JX\tab 36\tab Huadong\cr
  Jilin\tab JL\tab CN.JL\tab 22\tab Dongbei\cr
  Liaoning\tab LN\tab CN.LN\tab 21\tab Dongbei\cr
  Nei Mongol\tab NM\tab CN.NM\tab 15\tab Huadong\cr
  Ningxia Hui\tab NX\tab CN.NX\tab 64\tab Xibei\cr
  Qinghai\tab QH\tab CN.QH\tab 63\tab Xibei\cr
  Shaanxi\tab SN\tab CN.SA\tab 61\tab Xibei\cr
  Shandong\tab SD\tab CN.SD\tab 37\tab Huadong\cr
  Shanghai\tab SH\tab CN.SH\tab 31\tab Huadong\cr
  Shanxi\tab SX\tab CN.SX\tab 14\tab Huabei\cr
  Sichuan\tab SC\tab CN.SC\tab 51\tab Xinan\cr
  Tianjin\tab TJ\tab CN.TJ\tab 12\tab Huabei\cr
  Xinjiang Uygur\tab XJ\tab CN.XJ\tab 65\tab Xibei\cr
  Xizang\tab XZ\tab CN.XZ\tab 54\tab Xinan\cr
  Yunnan\tab YN\tab CN.YN\tab 53\tab Xinan\cr
  Zhejiang\tab ZJ\tab CN.ZJ\tab 33\tab Buadong\cr
  Hong Kong\tab HK\tab CN.HK\tab 81\tab Zhongnan\cr
  Macao\tab MC\tab CN.MC\tab 82\tab Zhongnan\cr
  Taiwan\tab TW\tab CN.TW\tab 71\tab Huadong\cr
 }

The \var{ChinaBG} supports two sets of abbreviations for each county 
(province or metro area). No consistant source was found when the border 
group was originally created.  Therefore, the two most common sets
are included.  The first abbreviation set can be referenced by setting 
the \option{rowNames} call argument to "ab".  The second (alternate) 
abbreviation set can be used by setting the \option{rowNames} to "alt_ab".

The \option{rowNames} = "alias" features are not supported in the 
\var{ChinaBG} border group.
}

\keyword{datasets}

