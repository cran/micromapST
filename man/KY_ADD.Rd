\name{KY_ADD}
\alias{KY_ADD}
\docType{data}
\title{Kentucky County to Area Development Districts Conversion Table} 
\description{
 One of the examples uses the 15 Area Develoment Districts in Kentucky
 instead of the 120 counties to create linked micromap with micromapST.
 This file contains the data table of the name of the ADD for each 
 county in Kentucky and is used to show how counties can be aggregated 
 into a small number of units to better work with linked micromaps.
}

\details{
 The conversion table, a data.frame, has the following columns 
   \describe{
     \item{ADD_Name}{ - The area development district's name as a character 
       string.}
     \item{County}{ - The KY County name as a character string.}
     \item{ID}{ - The numerical value assigned to the KY county.}
   }

 There are 120 counties in Kentucky and 15 area development districts (ADDs).

 The following are a list of the names (ADD_Names), abbreviations, and IDs 
 for each ADD in the conversion table and in the var{KY_ADD_BG} border group.
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
 These field values for the ADDs are assigned to the ADD in the \var{KY_ADD_BG} 
 border group name table when the border group is created.
}

\keyword{datasets}
\keyword{Kentucky}
\keyword{aggregate}

