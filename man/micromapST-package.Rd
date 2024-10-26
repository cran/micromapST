\name{micromapST-package}
\alias{micromapST-package}
\docType{package}
\title{A graphics package to easily and quickly create linked micromaps for a specified geographic collection of areas.}
\description{
 The micromapST package provides a means of creating multiple column
 graphics representing data for a collection of geographic areas. The originally micromapST was limited
 to creating linked micromaps for each US 50 state and the District of Columbia. With this 
 release, the package has been updated to be able to create linked micromaps for 
 any collection of areas that is defined in a border group dataset (see details). 
 Each area's graphical element is linked to a small map by means of color.
}
\details{
    \tabular{ll}{
      Package:  \tab micromapST\cr
      Type:     \tab Package\cr
      Version:  \tab 3.0.4\cr
      Date:     \tab 2024-10-26\cr
      License:  \tab GPL-2\cr
      LazyLoad: \tab no\cr
    } 
The package uses the following R released packages: stringr, RColorBrewer, graphics,
stats, grDevices, and labeling.  When the package is download, these support packages 
should also be loaded.  If not, please install these packages before 
loading micromapST.

Linked micromap plots link statistical graphs to an organized set of small maps, thus
adding geographical context to the graphs and data.  The \var{micromapST} package has been 
expanded to be able to use boundary data from any collection of geographic areas through 
the use of border group datasets.  Each border group dataset contains five R objects that define
the boundaries, run parameters and name, abbreviation, and ID relationships for a geographic 
region.  When a border group is specified in the function call, the associated dataset is loaded and
the five R objects become the key data structures used by micromapST to create the linked 
micromaps.  The five R objects are:
\itemize{
  \item \var{areaParms} - run parameters for this border group
  \item \var{areaNamesAbbrsIDs} - the area name, abbreviation and numeric ID associates for each area.
  \item \var{areaVisBorders} - the boundary data for each area, indexed by the area abbreviation
  \item \var{L2VisBorders} - the boundary data for overlaying boundaries within the areas. 
  \item \var{RegVisBorders} - the boundary data for a region of areas. 
  \item \var{L3VisBorders} - the boundary data for the entire collection of areas. 
}
The currently the supported sets of border groups 
contain in this package are:
\itemize{
  \item \var{USStatesBG} - The 50 U.S. states and the District of Columbia
  \item \var{USSeerBG} - The 20 U.S. Seer Areas within the U.S.
  \item \var{KansasBG} - The 105 counties in the state of Kansas  
  \item \var{MarylandBG} - The 24 counties of Maryland 
  \item \var{NewYorkBG} - The 62 counties in the state of New York  
  \item \var{UtahBG} - The 29 counties of Utah
  \item \var{SeoulSKoreaBG} - The 25 districts in the city of Seoul.
  \item \var{UKIrelandBG} - The 218 counties in UK and Ireland, for testing only.
  \item \var{ChinaBG} - The 32 provinces, municipalities, autonomous regions, and Special 
  Administrative divisions of China.
  \item \var{AfricaBG} - The 52 countries of the African continent.
}
The default border group is \var{USStatesBG} to allow users of previous releases 
of \var{micromapST} to run without changes to their code.  
To support users of 
the test release of \var{micromapSEER}, a function \var{micromapSEER} is included.
The function \var{micromapSEER} sets up required parameters and then calls \var{micromapST}
with the border group set to \var{USSeerBG} to create the same linked micromaps as 
the \var{micromapSEER} package created.  All of the call parameters are the same.
The two packages were merged to allow \var{micromapSEER} users to benefit from  
new features and fixes are released under \var{micromapST}.

Additional border groups will be added over time.  The user may also create their own border 
group for use with \var{micromapST} (see paper on creating \var{micromapST} border groups.)\cr
The entire micromap is created to fits on a single page.  
The page may be portrait or landscape and can range from an 8.5 x 11 up to a 11 to 17 page. 
Areas are grouped into panels from 3 to 5
areas each based on the sort variable, with the median-valued area set off 
in a separate panel in the middle of the page. If the median panel contains more than 
1 area, a full link micromap panel is generated.  Otherwise a single line representing 
the area is drawn and the median area is highlighted in the panels above and below the median.
In the case of the U.S. Map and 51 states, there are 5 panels of 5 areas (states) above the 
median and 5 panels of 5 areas (states) below the median row.

The U.S. Seer Registry data may be groups of 9, 11, 13, 17 or 18 registries. 
Number of registries per panel and number of panels are dynamically setup 
based on the number of registries involved in the micromap.  There are a 
variety of glyphs the caller can specify for each column of the micromap: 
the US map with areas colored, a list of registry names or abbreviations 
(the default) and one or more statistical graphics. The order of
these panels is specified by the caller. 

The statistical glyphs implemented in this version are plots of dots,
dots with significant, dots with confidence intervals, 
dots and intervals based on Standard Error, horizontal bars, arrows, 
time series with or without confidence bands, 
horizontal stacked (segmented (SEGBAR), normalized (NORMBAR) or centered (CTRBAR)) 
bar charts, scatter plots and boxplots.  The layout of the linked micromap plot is defined
by the \var{panelDesc} data.frame that is passed to the \var{micromapST} function.

If the micromap cannot fit on one page, warnings are generated and the function 
is stopped.  It is suggested the caller increase the size of the page (graphic space)
being used to compensate.

The U.S. map of states and areas used by the \var{USStatesBG} and \var{USSeerBG} 
border groups are generalized boundary map, based on Mark Monmonier's
visibility map. These maps are simplified to maximize the color areas 
shown for each state and to minimize the length of the boundary lines 
while still allowing identification of each area.  At some future time, all
border groups should have their boundaries characterized to enhance the 
linked micromap's readability.

One of the biggest enhancements in this version of \var{micromapST}, is support 
for other geographic areas. This has been added using border groups.  
Each border group data set contains the unique collection of  
information, run parameters, names, abbreviations, and boundary 
information for the geographic region.  The package contains some pre-made 
border groups and the package user is encourage gather the required information
and create their own border group.   This is not a small task and 
required boundary file manipulation to reduce the complexity of 
the boundaries and researching to identify a suitable list of names, 
abbreviations and ID for each sub-area within the desired geographical area. 
The author is working on a guideline and a step by step procedure 
to help the user create their own border groups.  
This release includes border group to re-create the original link micromaps 
in earlier versions of \var{micromapST} for the 50 U. S. States and DC and micromapSEER
for the 20 NCI Seer Registries.  Several other test versions of border groups 
have been included in this release.  The complete list of border groups included are:
\itemize{
   \item{\var{USStatesBG}} - the default border group to continue generating 
    the original linked micromaps in earlier versions of micromapST.
   \item{\var{USSeerBG}} - a border group to support creating linked micromaps 
    for NCI. A function micromapSEER is included to support earlier coding.  
    This function automatically pulls in the USSeerBG border group.
   \item{\var{KansasBG}} - a border group to support the generation of 
    linked micromaps for the 105 counties in the state of Kansas.  
   \item{\var{MarylandBG}} - a border group to support the generation of 
    linked micromaps for the 23 counties and the city of Baltimore in 
    the state of Maryland.
   \item{\var{NewYorkBG}} - a border group to support the generation of 
    linked micromaps for the 62 counties in the state of New York.
   \item{\var{UtahBG}} - a border group to support the generation of linked 
    micromaps for the 29 counties in the state of Utah.
   \item{\var{ChinaBG}} - a border group to support the generation of linked 
    micromaps for the 35 provinces, special administrative regions, and 
    autonomous regions of China.
   \item{\var{UKIrelandBG}} - a border group for testing a large number of 
    sub-areas (counties) in multiple regions.  The package currently supports 
    up to 110.  This border group is designed to help in testing and developing 
    support for up to 218 sub areas (counties, etc.)
   \item{\var{SeoulSKoreaBG}} - a border group to support the generation 
    of linked micromaps for the 25 districts within the city of Seoul South Korea.
   \item{\var{AfricaBG}} - a border group to support creation of linked 
    micromaps for 52 countries of Africa.  This border group is for a continent 
    with the sub-areas being countries.
 }
The following datasets have also been included for use in many examples:
\itemize{
   \item{\var{Educ8thData}} - Education Data from an 8th grade survey.
   \item{\var{TSdata}} - Sample time series data.
   \item{\var{statePop2010}} - State Population data for 2010.
   \item{\var{wflung00and95}} - White Female Lung cancer data for 2000 and 1995.
   \item{\var{wflung00and95US}} - White Female Lung cancer data totals for 2000 and 1995.
   \item{\var{wflung20cnty}} - White Female Lung cancer sample data by county for 2000.
   \item{\var{wmlung5070}} - White Male Lung cancer sample data for 1950 and 1970.
   \item{\var{wmlung5070US}} - White Male Lung cancer sample data for 1950 and 1970 totals.
}
Refer to the chapter on each border group for definitions on the Names, 
Abbreviations, and IDs used in the border group to link the user data 
to the boundary information to draw the micromap maps.

The sort order of the rows (areas) is based one of the statistical data 
columns as specified by the user.  Correlation between multiple statistical 
columns can be judged visually by comparing the pattern of one column's 
values from top to bottom of the page with that of the sorted column.  
Spatial clusters of states with similar values of the sorting
variable can be identified on the small maps that are linked to 
the graphics by color.

A area linked micromap plot is generated by 4 steps:

  \preformatted{
  # load the package  

  library(micromapST)

  # Read, create or collect your data into a data.frame.
  
  statsDFrame <- data.frame(a row per area, 
                            column per variable to be plotted, 
                            row.names set to the area names 
                            or abbreviations)

  # now set up a data frame that defines the labels, 
  #       panel and page layout

  panelDesc<-data.frame(...)

  # specify the data source, panelDesc, sorting variable and 
  #       order, and call the stateMicromap function

  micromapST(statsDFrame, panelDesc, title=c("title1","title2"), 
             details=list(options=values))
  }

The package contains a set of examples of how to produce linked area micromaps.
The datasets used in each example are provided to help you learn how 
to use micromapSEER.

As of release 2.0.1 of the package, a new function has been added.  The 
\var{BuildBorderGroup} function provide assistance to the user in building 
their own border groups from shape file and a name table as needed for 
geographic areas not covered by areas included in the package. Review the 
documentation on the \var{BuildBorderGroup} function for more details.

There are four primary sources for boundary data:  
\itemize{
 \item{U. S. Census Bureau} for U. S. states and territories 
   (\url{https://www.census.gov/}) (2021 - Cartographic Boundary Files) ;
 \item{GADM (Global Administrative area Database)} for all of the 
   countries and sub-divisions in the world (\url{https://gadm.org}) 
   (V4.1, July 16, 2022 release, older versions are available.);
 \item{DIVA-GIS (\url{https://diva-gis.org})} (January 2012); and 
 \item{local governments}.
} 
All of the boundary data (shape files or json format) are free for public 
use.  Care must be taken to ensure your data matches the boundary areas 
available in each collection. Data location IDs and boundaries change 
over time as areas are split, merged, added, or eliminated. 
The BuildBorderGroup function tries to clean up and repair the boundary 
data as best as it can. Always inspect the boundary data and make sure 
its usable for your application and is valid.

With the announcement of the support retirement of maptools, rgeos, 
rgdal, and effectively sp and spdep, this package had to be upgraded 
to continue to be useful by it's users. V3.0.0 completes that enhancement.
}

\author{
Daniel B. Carr \email{dcarr@gmu.edu} and 
James B Pearson, Jr \email{jbpearson353@gmail.com}, 
with contributions from Linda Pickle\cr
Maintainer: "James B. Pearson Jr." \email{jbpearson353@gmail.com}\cr 
Package compiled by "James B, Pearson, Jr." \email{jbpearson353@gmail.com}
}

\references{
  Daniel B. Carr and Linda Williams Pickle, Visualizing Data Patterns with 
  Micromaps, CRC Press, 2010\cr
  Linda Williams Pickle, James B. Pearson Jr., Daniel B. Carr (2015), 
  micromapST: Exploring and Communicating Geospatial Patterns in US State Data.,
  Journal of Statistical Software, 63(3), 1-25., 
  \url{https://www.jstatsoft.org/v63/i03/}
}

\note{
The packaged is tuned to work with an area 7.5" wide and 10.5" high. 
Testing has shown it works well with portrait or landscape orientation 
and areas up to 11" x 17".

The examples in this package the output is directed to a PDF file for best 
clarity and resolution. File types of SVG, PNG, JPEG or TIFF can also be used.   
If the output is directed to a window, it is suggested a 
\code{windows( 7.5, 10.5, xpinch=72, ypinch=72, pointsize=9 )} command 
is used to set up the window to best display the linked micromap. 
The results will vary based on the resolution of the monitor being used.
}
