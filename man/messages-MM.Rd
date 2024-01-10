\name{messages-MM}
\alias{messages-MM}
\docType{data}
\title{micromapST Generated Warning and Error Messages-micromapST}

\description{
  micromapST verifies as much of its operation and the data provided
  by the user to try and identify and document to the user problems
  before they cause an R warning or error exception and throw a R
  like cryptic message. Each message, warning and error message is
  documented with the general form of the message and a friendly
  explanation and advice on what may be wrong and how to fix the
  issue identified.

  The micromapST messages all start with "***" to help quickly find
  them in the warnings() logs and general output.

  The general format of the messages is:
  \preformatted{

    ***XXXX NNNNNN text of message
  
      or
    
    ***XXXX NNNNNN CC text of message
    }
  where the XXXX is the message alphanumeric identifier, NNNNNN is
  the name of the function or glyph issuing the message and CC is
  the glyph graphic column number in the \var{panelDesc} structure.

  The four alphanumeric message identifier following the "***" is a
  unique message identifer to help find the explanation in this
  document and to help discuss problems over the email more
  accurately. It's always best to include the log of the preparation
  and execution of micromapST as well as the data used when
  requesting help.

  The first two digits/characters of the message identifier
  indicates which logical segment of the package generated the
  message:
  \describe{
    \item{01xx}{Main package initialization, startup, call argument
       validation, user data validation and \var{panelDesc}
       structure and contents validation. 
      }
    \item{02xx}{panelDesc and glyph parameters and data validation,
      processing and graphic generation
      }
    \item{03xx}{Border Group data and structure validation and
      setup,
      }
    \item{04xx}{Internal Operation
      }
    \item{05xx}{Informational Messages
      }
   }
  The second field of the message contains the name of the section
  of code generating the message.  If a glyph generated the message,
  the name of the glyph (type=xxx) and the panel column number are
  displayed to help you focus on the parameters and data that may
  need to be inspected.  Each message contains specific information
  (like \var{panelDesc} named list, checked value, etc.) to help
  identify the cause of the message and help lead the user to a fix.

  The remainder of the message contain a text explanation of the
  issue and a list of parameters: glyph column number/name, data row
  or column, variable name, and value related to the issue.
  
  The abbreviation in most messages identifies the processing step 
  in the package the message was produced:
  \describe{
   \item{CARG}{Call Arguments-Parameters
    \describe{
     \item{<blank>}{General call parameter messages}
     \item{-RNC}{Row Name Column}
     \item{-RN}{Row Name type}
     \item{-AS}{Ascend sort order parameter}
     \item{-TL}{Title parameter}
     \item{-PN}{PlotName parameter}
     \item{-GP}{Group Pattern parameter}
     \item{-SC}{Scaling parameter}
     \item{-SL}{Staggered Label parameter}
     \item{-CS}{Column Size parameter}
     \item{-DF}{Statistics Data Frame parameter or content}
     \item{-PD}{Panel Description data frame parameter or content}
     \item{-SORT}{Sort parameters}
    }
   }
   \item{Alias}{Use of the Alias location ID feature}
   \item{BGBD}{Border Group Directory parameter}
   \item{BGBN}{Border Group Name parameter} 
   \item{BGBG}{Border Group data set content}
   \item{BDATA}{General Border Group boundary data}
   \item{COLORS}{color call parameter}
   \item{DETS}{details call parameter}
   \item{PDCOL}{Panel Description dataframe columns. These messages relate 
     to the columns within the panel description data frame 
     rather than the glyph itself.}
   \item{PANEL}{Panel Description dataframe general issues}
   \item{DMP & INB}{Internal issues}
   \item{<glyph name>}{Messages related to a specific glyph graphic.  
     The <glyph name> would be one 
     of the following: dot, dotconf, dotse, dotsignif, ctrbar, normbar, segbar, 
     map, mapcum, mapmedian, maptail, id, arrow, bar, boxplot, scatdot, ts, or tsconf.
   }
  }
}
\details{
 \bold{Conventions:}
    The user provides the data.frames for the statsDFrame and
  panelDesc structures.  In this document, there variables are
  represented by the generic terms of \var{statsDFrame} and
  \var{panelDesc}.  In the messages produced by the package, these
  values are replaced with the names of the user variables provided
  in the micromapST call.

  The following values are also substituted with the real run values
  at time of execution:
  \describe{
    \item{<sortVarCol>}{List of column names/numbers used in the
      \option{sortVar} call parameter. Used to identify invalid
      \var{statsDFrame} column names and numbers used in the
      \option{sortVar} call parameter.
     }
    \item{<PDvarName>}{panelDesc variable name. The panelDesc
      variables include \var{lab1}, \var{lab2}, \var{lab3},
      \var{lab4}, \var{refVal}, \var{refTxt}, \var{col1},
      \var{col2}, \var{col3}, \var{type}, and \var{panelData}. }
    \item{<PDcol>}{panelDesc glyph column number. This helps
      identify which graphic column (glyph) is being processing when
      the error/warning is signaled. The far left column number is
      1.
     }
    \item{<PDvarList?}{a list of variables from the panelDesc
      data.frame. Used in messages to identify one or more
      \var{panelDesc} variables that are invalid.
     }
    \item{<detVar>}{detailed list variable name
     }
    \item{<lastVarName>}{last variable name used.  Used to locate a
      value or absents of a value in a list by using the last good
      variable name.
     }
    \item{<bordGrp>}{a character string of the name of the border
      group being referenced. The border group names does not
      include the .RData or .rda extension used on the data file.
     }
    \item{<detVarName>}{detailed list variable name 
     }
    \item{<method>}{The labeling method requested for generating
      Axis labels.
     }
    \item{<list of unused sub-areas>}{A list of the sub-areas
      (counties, etc.) that did not have data.
     }
    \item{<listRowNames>}{list of row.names (sub-areas)
     }
    \item{<litrowNamesCol>}{The literal rowNamesCol value submitted
      by the caller.
     }
    \item{<class(xxx)>}{The class of the variable names "xxx"
     }
    \item{<number items>}{Number of items in list or vector.
     }
    \item{<invalid name list>}{List of invalid names that were
      unexpected.
     }
    \item{<list>}{a general list of options, variables, or names.
     }
    \item{<statsDFrame>}{The name of the data.frame used as the
      first call parameter.  This data.frame is used for the
      statsDFrame and provides the numeric values for the plots.
     }
    \item{<panelDesc>}{The name of the data.frame serving as the
      panelDesc panel column description.
     }
    \item{<gName>}{The name of the glyph being drawn.
     }
    \item{<glyphName>}{The name of the glyhp begin drawn.
     }
    \item{<sDFName>}{The name of statistics data.frame for the
      linked micromap.
     }
    \item{<pdColNum>}{The panelDesc data.frame column number (glyph
      column).
     }
    \item{<dupList>}{Is the list of duplicate entries in a list.
     }
    \item{<rowNumbers>}{References the row number in the statsDFrame
      data.frame. 
     }
    \item{<list of rows>}{Is a list of rows related to the message.
     }
    \item{<rowNumbers>}{One or more row numbers related to the
      message.
     }
    \item{<NotUsedNames>}{Identifies a not used name from name
      table.
     }
    \item{<future>}{Reserved for future usage.
     }
   }
  
  The following is a listing of all micromapST generated messages and 
  a description of possible causes and solutions.
  
\describe{
  \item{01yx}{\bold{Initialization, Setup and Call argument
     validation,} \var{panelDesc} and User data validation. where y
     indicates the specific argument or area.
     
    \describe{
      \item{01Zx}{\bold{General Operation ("Z"):}
      
        \describe{
          \item{01Z0}{\bold{CARG Key call arguments are missing,
            NULL, wrong type, or NA, Execution stopped.}
          
             The micromapST function requires the \var{statsDFrame}
	     and \var{panelDesc} data frames be provided.  This is
	     the minimum set of call parameters required. If either
	     or both parameters are missing (the first two in the
	     function call), the execution of the function is
	     stopped. 
	   }
          \item{01Z8}{\bold{CARG Warnings have been found in the 
             parameters and data. Package continues, but results are 
             unpredictable. Review log and fix errors.}
          
             The micromapST function has detected warning in the 
             parameters and data.  The function will continue
             but the results may be unpredictable.
             Review the errors and warnings, correct the causes 
             and re-run. 
	   }
          \item{01Z9}{\bold{CARG Errors have been found in parameters 
             and data. Please review program log and fix problems. 
             Packaged stopped.}
          
             The micromapST function has detected errors in the 
             data and call parameters that prevent the function from
             running.  Correct the identified error and re-run.
           }
         }  
       }     
  
      \item{010x}{\bold{\var{statsDFrame} call argument (1st argument) (CARG-DF):}
      
        \describe{
          \item{0101}{\bold{CARG-DF First argument (\var{statsDFrame}) 
             is missing or NULL or not a data.frame.}
             
             The first calling argument/parameter must be the
	     \var{statsDFrame} data frame containing the data to be
	     graphed. If the first argument/parameter is missing or NULL.
	     Provide the correctly built \var{statsDFrame} and re-
	     run. The data structure can not be a list, tibble, matrix, 
	     array or simple vector variable.
            }
          \item{0102}{\bold{CARG-DF The First argument (\var{statsDFrame}) is not a 
             data.frame class.}
             
             The \var{statsDFrame} variable holding the data for each area
             (county, district, etc.) in a border group is not a data.frame structure.
             Each column represents a data variable to micromapST.  Each row represents
             an area being mapped and charted.  The area location ids may be located 
             as the data.frame row.names or in a data.frame column itself.
	     The panelDesc structure uses the col1, col2, and col3 columns to specify
	     which column in the \var{statsDFrame} should be used for the glyph.
	     The sortVar and rowNamesCol call parameters use the statsDFrame column
	     name or number to identify how to sort the graphics or where the area name is 
	     located in the data.
	   }
          \item{0103}{\bold{CARG-DF The \var{<sDFName>} \var{statsDFrame} data.frame 
             must have at least 1 or 2 columns.}
          
             The <sDFName> data.frame provided as the \var{<statsDFrame>} must have 
             at least one column or two if the rowNameCol call parameter is specified. 
             One data column must be provided and if the row.names feature in the 
             data.frame is not used for the location id, then one additional column 
             is required and the rowNameCol must be specified.
            }
          \item{0104}{\bold{CARG-DF There are duplicate entries in the \var{statsDFrame} 
             data.frame. Duplicate entires are ignored.}
             
             The \var{statsDFrame} data frame contains rows with duplicate names. 
             Only the first row is used, duplicates are ignored.  Check the 
             area name values in the data frame and correct as needed to 
             make sure each row represents a unique area. The following message provides
             a list of the duplicate sub-areas. ***
           }
          \item{0105}{\bold{CARG-DF The duplicate rows are: <listRowNames> }
          
             This message provides a list of the duplicate area names, 
             abbreviations or IDs in the \var{statsDFrame} data frame. Correct the duplicate 
             entries and re-run micromapST. *** 
           }
          \item{0106}{\bold{CARG-DF The following row in the \var{<sDFName>} statsDFrame  
             data.frame are not found in the name table:}
          
             See continued message (0107) for details
           }
          \item{0107}{\bold{CARG-DF     <listRowNames> }\cr
             This is a two line error message.  The area names provided 
             in \var{statsDFrame} data.frame must match the names in the 
             border group name table. This warning provides a list of the 
             araa names not found in the name table. Please select the 
             correct border group or correct the area names in the 
             data.frame and re-run micromapST. Unless requested, 
             the function will stop.
           }
          \item{0108}{\bold{CARG-DF The rows not matched to boundaries 
            will be removed and not mapped.}\cr
             If a row of data does not match an entry in the name table, the 
             data row in the \var{statsDFrame} will be deleted.  Check the 
             location id used on the data row to make sure it matches the 
             type of names you selected in the name table ("Full", "Ab", "ID", 
             etc.)  If the row's loc id does not match, correct and re-run.
            }
          \item{0109}{\bold{CARG-DF Data row names in the ,sDFName," data.frame 
             must match the location ids in the name table. Call stopped.}\cr
             If \option{ignoreNoMatch} is FALSE and a data row does not have 
             an entry in the boundary name table then the package can not 
             continue to run. All data must have valid names or abbreviations 
             associated with each row that has sub-area boundaries in the 
             border group. Execution is stopped. Verify the area identifiers 
             in the supplied data, correct and retry.
           }
         }
       }

      \item{011x}{\bold{panelDesc call argument (2nd argument) (CARG-PD):}
      
        \describe{
          \item{0111}{\bold{CARG-PD The second call argument, the panelDesc 
             structure, is missing or NULL.}\cr
             The second call argument/parameter is the \var{panelDesc} 
             data.frame. Without this data frame, micromapST does not know 
             which glyphs to draw or where the supporting data is located 
             in the \var{statsDFrame} data.frame. The data frame is missing, 
             NULL, NA or is not a data.frame structure.   The data structure 
             can not be a list, tibble, matrix or array variable.  Correct 
             and re-run micromapST. 
           }
          \item{0112}{\bold{"CARG-DF The second argument, the panelDesc 
            structure, is not a data.frame.}\cr
            The second argument in the \var{micromapST} call must be the panelDesc 
            structure as a data.frame. No other type of variable is acceptable.  
            Please correct the structure and retry.
           } 
          \item{0113}{\bold{CARG-PD The following named lists in the 
            \var{<pDName>} panelDesc data.frame are not valid: 
            \var{<PDVarList>} }\cr
            The \var{panelDesc} variables \var{col1}, \var{col2}, 
            and \var{col3} identify the columns in the \var{statsDFName} 
            data.frame that contain data for each area to be used by 
            the glyph for the column.  The column number or name 
            provided in the \var{panelDesc} structure is not a valid 
            \var{statsDFrame} column number (out of range) or name.   
            This error message provides a list of the invalid column 
            numbers or names. 
           }
          \item{0114}{\bold{CARG-PD The required \var{type} named list 
            is missing in the \var{<pDName>} panelDesc data.frame.}\cr
            The \var{panelDesc} list named \var{type} does not exist in the 
            \var{panelDesc} data frame. This named list is required to 
            identify the type of glyph to be generated for the specific 
            column. Inspect the \var{panelDesc} data.frame, check for 
            spelling and correct the source of the problem and re-run.
             
            Example: \var{panelDesc}=list(type=c("map","dot","bar"), ... ) 
           }
          \item{0115}{\bold{CARG-PD The \var{<panelDesc>} \var{type} named 
            list contains one or more invalid glyph name(s): \var{<pdTypeList>}.}\cr
            The type named list in the \var{<panelDesc>} panelDesc data frame 
            contains invalid glyph names.  The invalid names are listed in 
            this error message. The valid glyph names are:  map, mapcum, 
            mapmedian, maptail, id, arrow, bar, boxplot, dot, dotse, 
            dotconf, dotsignif, segbar, normbar, ctrbar, ts, tsconf, 
            and scatdot.  
           }
         }
       }
      \item{012x}{\bold{call argument column name/number verification 
         (CARG-xxx):}\cr
         When a call argument/parameter contains a column name or number
         referencing the \var{<statsDFrame>} data.frame, the arguments 
         values are checked during the initialization of the micromapST 
         processing. The following errors may be detected and the function 
         stopped.
        
         The call arguments/parameters that contain column names/numbers are:
         \preformatted{
               rowNamesCol      RNC
               sortVar          SORT
         }
         The abbreviated name (\var{<callArgAbbr>}) listed  above for the 
         call argument/parameter (\option{<callArg>}) is included in the message 
         to assist in identifying and correcting the problem. 
        \describe{
           \item{0123}{\bold{CARG-\option{<callArgAbbr>} A column name in the 
             \option{<callArg>} call argument does not exist in the 
             \var{<sDFName>} data.frame: \var{<value>}}\cr
             One or more of the column names in the \option{<callArg>} 
             call argument/parameter does not exist in the 
             \var{<statsDFrame>} data.frame. The bad value is displayed 
             in the \var{<value>} part of the message. Verify that the 
             column name exists in the \var{<statsDFrame>} data.frame 
             and re-run.
           }
          \item{0124}{\bold{CARG-\option{<callArgAbbr>} The \option{<callArg>} 
             call argument is empty.  Argument ignored.}\cr
             The \option{<callArg>} value was found to have a length of zero
             - empty. The argument can not be checked and is ignored.
           }
          \item{0126}{\bold{CARG-\option{<callArgName>} The call parameter 
            contains more than one value. Only the first value will be used.} \cr
            The call parameter <callArgName> can only use a single value. 
            More than one value was specified in the function call. 
            Only the first value will be used. 
           }
.        }
       }
      \item{015x}{\bold{\option{bordGrp} and \option{bordDir} parameter (BGBx):}
    
        \describe{
          \item{0150}{\bold{BGBD The directory specified in the bordDir call 
             parameter is not a valid character string.}
             Check the bordDir path name and make sure it is valid.
             }
          \item{0151}{\bold{BGBD The directory specified in the \option{bordDir} 
             argument/parameter does not exist. Value=\var{<bordDir>}}
          
             The path specified does not exist, Processing is stopped.  Correct the path 
             name and re-run. Check to make sure the slashes in the path name are correct 
             and that the path exists.
           }
          \item{0152}{\bold{BGBN The value provided as the bordGrp name is not 
            character. Fix and rerun.}
          
             The bordGrp call parameter must be a character string of the border 
             group name (and it's file.) If it is not the name of border group 
             provided with the package, it must be the name of the border group 
             file (without the .rda) that is located in the bordDir path.
           }
          \item{0153}{\bold{BGBN When the \option{bordDir} parameter is set to NULL, 
             the \option{bordGrp} must be one contain in the package. \var{<backageName>} }

             When the \option{bordDir} call parameter is not provied, or set 
             to NULL or NA, the value of the \option{bordGrp} parameter 
             must be one of the internal border groups provided with the package.  
             The border groups provided with the package are: 
             \describe{
               \item{USStatesBG}{U.S. States and the District of Columbia}
               \item{USSeerBG}{The 18 U.S. Seer Registry areas}
               \item{KansasBG}{The counties of the state of Kansas}
               \item{MarylandBG}{The counties of the state of Maryland}
               \item{NewYorkBG}{The counties of the state of New York}
               \item{UtahBG}{The counties of the state of Utah}
               \item{UKIrelandBG}{The provinces, counties and cities of the 
                 United Kingdom and Ireland}
               \item{ChinaBG}{The provinces, special administration areas, 
                 metropolitan cities of China}
               \item{SeuolSKBG}{The districts of the South Korean city of Seoul}
              }
             If you are using a custom or private border group, then the 
             \option{bordDir} parameter must be provide to specify the file 
             directory containing the border group .rda file.
             
             See the appropriate documention section for details on each border 
             group contained in this package.
            }
           \item{0154}{\bold{BGBN The \option{bordGrp} parameter has not been 
             specified and is required when the \var{bordDir} is provided.}
          
             When the \option{bordDir} call argument/parameter is specified, 
             the package will load a users or private border group into the package.  
             Therefore, the \option{bordGrp} call argument/parameter MUST
             be specified to identify the file in the directory to load.  Verify 
             the \option{bordGrp} filename is present and exists in 
             the \option{bordDir} directory and re-run. Execution was stopped.
           }
           \item{0155}{\bold{BGBN The \option{bordGrp} filename must have an ".rda" 
             or ".RData" file extension.}
          
             When private border group files are created, they must be saved 
             as ".rda" or ".RData" type "R" files.  Please re-create your private 
             border group file and re-run. Execution is stopped.
           }
          \item{0156}{\bold{BGBN The \option{bordGrp} file in the \option{bordDir} 
             directory does not exist.}
          
             The package attempted to open the \option{bordGrp} file in the 
             \option{bordDir} directory, but the file does not exist.  If the 
             \option{bordGrp} filename was specified without a file extension, 
             the package appended an ".rda" file extension.  Verify the \option{bordGrp} 
             filename exists in the \option{bordDir} directory and re-run. 
             Execution is stopped..
           }
          }
         }
        \item{01Mx}{\bold{Boundary datasets:}
         \describe{   
          \item{01M0}{\bold{BGBG System error encountered when loading the border group. 
             See error message:}\cr
             The package attempted to load the user/private border group.  
             The R system load() function failed and returned an error message.  
             The error message is logged following this message using the same 
             message indicator.  Inspect the file and resolve the R system load() error
             mesage and re-run.  Execution was stopped.
             
             The second like of error message details the loading error issued by R.
           }
          \item{01M1}{\bold{The areaNamesAbbrsIDs (Name Table) is missing from 
            the border group.}\cr
            The border group must contain the name table (areaNamesAbbrsIDs) 
            to be useful.  Determine why the name table is missing and retry.
           }
          \item{01M2}{\bold{The areaVisBorders boundary data set is missing 
            from the border group.}\cr
            The border group must contain the boundary data for all of the areas 
            in the name table. In this case, the areaVisBorders data.frame containing 
            the area boundary point is missing. Determine why the table is 
            missing and retry.
           }
          \item{01M3}{\bold{The areaVisBorders boundary data set has a NULL value 
            from the border group.}\cr
            The border group must contain the boundary data for all of the areas 
            in the name table.  In this case, the areaVisBorders data.frame containing 
            the area boundary point has a NULL value.
            Determine why the areaVisBorders data set is NULL, repair and retry.
           }
         \item{01M4}{\bold{The L3VisBorders boundary dataset is missing from the Border Group. }\cr
            The Level 3 (map outline) boundary table is an optional boundary points 
            data.frame and is missing from the border group.  The package will disable 
            drawing the L3 borders.
           }
          \item{01M5}{\bold{The L2Borders is set to be drawn, but no L2 border data is present. 
             L2 Feature disabled.}\cr
            The L2VisBorders boundary point data.frame is missing from the border 
            group when the L2Feature is enabled.  The L2Feature will be disabled.  
            If you require the L2 boundaries 
            determine why the data.frame is missing and retry.
           }
          \item{01M6}{\bold{The RegBorders is set to be drawn, border data is not present. 
             Reg Feature disabled.}\cr
            The RegVisBorders boundary point data.frame is missing from the border 
            group when the RegFeature is enabled.  The RegFeature will be disabled. 
            dataRegionsOnly options is also disabled.  If you require the Reg boundaries 
            determine why the data.frame is missing and retry.
           }
          \item{01M7}{\bold{The L3VisBorders is NULL. Ploting the L3 outline is disabled.}\cr
            The L3 boundary data found in the border group is empty.  The drawing of the L3 
            outline boundary has been disabled.  This may be a normal situation and only 
            needs attention if the L3 boundary data was expected to be in the border group.  
            Correct the problem and recreate the border group.
           } 
          \item{01M8}{\bold{BGBN After loading <BorderGroupName> border group, the 
            following optional objects are missing: <list of data.frames>}\cr
            After loading the border group specified in the function call 
            (<BorderGroupName>), the following optional objects (data.frames)
            were missing in the border group:  <list of data.frames>.  
            These are optional and any use of them has been disabled.
            If you need one of these boundary data.sets, contact the border 
            group builder and determine why these data.frames were excluded.  
            The optional boundary data.frames are:  L3VisBorders, L2VisBorders, 
            and RegVisBorders.
           }
          \item{01M9}{\bold{BGBN After loading <BorderGroupName> border group, the following 
            critical objects are missing: <list of data.frames>}\cr
            After loading the border group specified in the function call 
            (<BorderGroupName>), the following critical objects (data.frames)
            were missing in the border group:  <list of data.frames>.  
            These boundary data.frames are required for the micromapST package
            to run.  The execution of the package has been stopped. 
            Contact the border group builder and determine why these data.frames 
            were excluded.  The optional boundary data.frames are:  areaVisBorders, 
            areaNamesAbbrsIDs, and areaParms.
           }
          \item{01MA}{\bold{L2Border was requested, but the Name Table does not have 
            a L2_ID column.  Disable drawing of L2.}\cr
            The border group indicates a Level 2 boundary should be drawn where 
            appropriate.  However, the name table does not have the L2_ID column 
            to like the Level 2 boundaries to the basic areas.  Correct the border 
            group's name table, rebuild the border group and try again.
           }
          \item{01MB}{\bold{Regional boundaries have been requested, but the 
            Name Table has no regID column. The feature is disabled.}\cr
           The border group indicates the Regional boundary and the regional mapping 
           feature has been requested, but the Name Table has no regID column.  
           This feature is disabled.         
          }
         }
        }
      
      \item{017x}{\bold{\option{rowNamesCol} call argument (CARG-RNC):}/cr      
         To be able to link up the \var{statsDFrame} rows to areas in the border 
         group and its boundary information, the \var{statsDFrame} must provide 
         a column containing the sub-area names, abbreviations or IDs that match 
         the name table entries in the border group.  The values can be supplied 
         via the row.names on the data frame or in a column in the data frame.  
         The \option{rowNamesCol} call parameter allows the user to specify 
         which \var{statsDFrame} column to use as the link to the border group name 
         table. The rowNames argument/parameter is used to specify if the link is 
         the name, abbreviation or ID of the sub-area.
         
        \describe{
         \item{0171}{\bold{CARG-RNC The row names in column \option{rowNamesCol} 
           of the statsDFrame data.frame contains duplicates.  Only one row per area 
           is permitted.  Duplicate rows are: <list of rows>.  The rowNamesCol will be 
           ignored and the data.frame row.names values will be used.}\cr          
           The \option{rowNamesCol} value specifies a column with duplicate area names.
           There can only be one row per area. The rowNamesCol call parameter is 
           ignored and the data.frames row.names information is attempted to be used.    
          }
         \item{0172}{\bold{CARG-RNC The \option{rowNamesCol} argument value must 
             have a length = 1. Only first value used.}\cr          
             The \option{rowNamesCol} value must have a length of 1 and should be a 
             simple vector. Only one column can be specified to contain the area 
             names (row names.)  If more then one value is provided, only the 
             first value in the data will be used.    
           }
          \item{0173}{\bold{CARG The rowColNames is NA. The row.names values on 
            the data.frame will be used.}\cr          
             The \option{rowNamesCol} value provided is NA and cannot be used. 
             Instead the data.frame row.names information will be used.
           }
         \item{0174}{\bold{CARG The rowColNames is NA. The row.names values on 
            the data.frame will be used.}\cr             
             The rowColNames value is NA.  It must be a valid statsDFrame column 
             name.  The data in the column must contact data that matches one of 
             the location IDs in the Name Table. When no rowColNames is specified, 
             the row.names values on the statsDFrame data.frame are used 
             as the location ID. 
           }
          \item{0175}{\bold{CARG The rowColNames value specified does not exist 
            in the data.frame. The row.names values on the data.frame will be used.}\cr 
            The data.frame column name specified in the \option{rowColNames} call 
            parameter does not exist in the data.frame.  To continue executing, 
            the data.frames row.names information is used.
           }
         }
       }

      \item{018x}{\bold{\option{sortVar} and \option{ascend} call arguments (CARG-AS):}

         The \option{sortVar} calling argument/parameter specifies the \var{statsDFrame} 
         column numbers and/or names to be used to sort the data and the rows 
         presented in the linked micromaps.  The sorting is executed in the order 
         of the columns listed in the numeric or character vector provided 
         via the \option{sortVar} argument/parameter. The column numbers and names are 
         verified before the sorting is attempted.  All references must be valid. 
         The default sort uses the sub-area names as spedified in the \option{rowNames} 
         argument/parameter.
         
         The \option{ascend} flag indicates which direction the sorting will be done.  
         \option{ascend} = TRUE requests ascending order.  \option{ascend} = FALSE 
         requests descending order.
         
        \describe{
          \item{0181}{\bold{CARG-SV The \option{sortVar} parameter is not a numerical 
            or character vector variable.  Matrix, arrays, data.frames and tibbles 
            are not supported.  Will use the default of alpha sort on area names.
            }

            The \option{sortVar} call argument/parameter must be a numeric or 
            character vector with a length less than with 3 values. The values 
            must be valid indexes or names of columns in the statsDFrame 
            data.frame supplied by the user.  Only a simple vector is supported.
            Matrixes, arrays, data.frame and tibbles are not supported.
           }
          \item{0186}{\bold{CARG-AS The \option{ascend} parameter is not a
            logical variable.  Must be TRUE or FALSE.}

            The \option{ascend} call argument/parameter must be a logical 
            vector with a length of 1. Vectors with multiple values are 
            not allowed.  Numeric, Integer or Character vectors are not supported.
           }
         }
       }
       
      \item{019x}{\bold{\option{rowNames} call argument and \option{alias} name 
        matching feature (CARG-RN & ALIAS):}\cr      
         The \option{rowNames} call argument/parameter specifies what type of sub-area 
         name/string should be used to match the sub-area name in the 
         \var{statsDFrame} data frame, boxplot and time series data with the 
         border group's  name table. The valid rowNames values are: 
         \option{full}, \option{ab}, \option{alt_ab}, \option{id}, and \option{alias}.  
         The default is \option{ab}.
         
         When \option{alias} is specified, the package attempt to link the 
         \var{<statsDFrame>} data rows to the boundary data using a partial match 
         between the sub-area names in the \var{<statsDFrame>} data.frame and the 
         \var{alias} column in the name table.  This feature was implemented 
         to handle special cases were the data source does not use the regularly used
         names or abbreviations for the sub-areas.   See the discussion on the 
         USSeerBG border group for more details and an example.  At this time, 
         only the USSeerBG border group makes use of this feature.
      
        \describe{
          \item{0190}{\bold{CARG-RN Invalid \option{rowNames} argument value.  
            The value must be \option{ab}, \option{alt_ab}, \option{id}, 
            \option{alias}, or \option{full}. The default of \option{ab} 
            will be used.}

            The \option{rowNames} call argument/parameter contains an invalid value.  
            The default of \option{ab} is used.
           }
          \item{0191}{\bold{CARG-RN rowNames=\option{alias} is not supported for 
            this bordGrp. The default of \option{ab} will be used.}

             The rowNames value of \option{alias} is only supported in border groups 
             containing the an alias column in the name table and have the 
             border group areaParm variable \var{enableAlias} set to TRUE.
             The only border group supplied with the package that supports 
             the alias name matching is the the USSeerBG border group.  
           }
          \item{0192}{\bold{CARG-RN rowNames=\option{seer} is only supported for 
            the USSeerBG bordGrp. The default of \option{ab} will be used.}

             Select the correct border group or use another rowNames value.
           }
          \item{0193}{\bold{The rowNames parameter is NULL, NA or Missing. 
             The default of 'ab' will be used.}\cr
             rowNames must be provided to define the type of location id to use.
             The default is "ab".  The other options are "full", "alias", and "alt_ab".
           }  
          \item{0195}{\bold{ALIAS Alias Names(s) in the data no not match the 
            name table for the area. The unmatched data rows are: <listRowNames>}

             The rowNames value of \option{alias} was specified.  During partial 
             wildcard matching of the "alias" column in the border group name table 
             with the data column in the \var{statsDFrame} data frame specified 
             by the \option{rowNamesCol} call argument/parameter, one or more data rows 
             did not successfully match the name table.  The <listRowNames> 
             part of the message lists the data row values that did not match.   
             Review the \var{statsDFrame} values used for sub-area names and the 
             documentation of the alias strings provided in the selected border 
             group and make adjustments as required.
           }
          \item{0196}{\bold{ALIAS Sub-area names in the data have duplicate 
            name in rows: <listRowNames>  Only one row per area is permitted.}

             There are multiple rows in the \var{statsDFrame} that match a single 
             sub-area in the border group name table. Only the first matching 
             data row will be used.  The <listRowNames> section 
             of the message contains a list of the duplicate rows.  Correct row names 
             to eliminate any duplicates and re-run.
           }
           \item{0197}{\bold{The rowNames parameter is not a character string.}\cr
             The rowNames parameter must be provided and must be a character vector.  If 
             it is not, the function is terminated.
           }
         }
       }
       
      \item{01Ax}{\bold{\option{title} call argument (CARG-TL):}\cr
         The \option{title} calling argument/parameter is used to specify a one or 
         two line title for the linked micromap. If not \option{title} is provided, 
         the titles are left empty - blank.
         
        \describe{
          \item{01A0}{\bold{CARG-TL The \option{title} argument contains more 
            than 2 items. Only the first two will be used.}\cr
             The \option{title} call argument/parameter contains more that 
             2 character strings.  Only two title lines are supported.  
             Only the first two values (lines) are used.
           }
          \item{01A1}{\bold{CARG-TL The typeof/class of the \option{title} parameter 
            is not character. Only character vectors are supported. The \option{title} 
            argument is ignored.}\cr
            The type and class of the \option{title} call argument/parameter must be 
            a character vector. Numeric, integer and logical vectors are not supported.
           }
          \item{01A2}{\bold{CARG-TL The \option{title} argument/parameter is empty.  
             Recommend providing a title for the linked micromap.}\cr
             The \option{title} call argument/parameter is empty (length of 0). 
             It's recommend a set of titles be specified for the linked micromap.  
             Please provide a \option{title} 
             character vector to help identify the linked micromap and re-run.
           }
         }
       }
       
      \item{01Bx}{\bold{\option{plotNames} call argument (CARG-PL):}\cr
         The \option{plotNames} calling argument/parameter specifies the type of area 
         name to be used in the ID glyph. The allowed values are \option{ab} and \option{full}.
         
        \describe{
          \item{01B0}{\bold{CARG-PN Invalid \option{plotNames} argument value. The value 
             must be \option{ab} or \option{full}.}\cr
             The value of the \option{plotNames} call argument/parameter is not valid, 
             The value must be \option{ab} or \option{full}. The default value of 
             \option{ab} is used. Check the spelling and re-run.
           }
          \item{01B2}{\bold{CARG-PN Invalid plotNames argument value. The value must 
            be 'ab' or 'full'. The default of 'ab' will be used.}\cr
            The \option{plotNames} argument value must be 'ab' (abbreviation) 
            or 'full' (full name). The default of 'ab' will be used.  
            If the data contails full names, correct and re-run.
           }
         }
       }

      \item{01Cx}{\bold{\option{grpPattern} call argument (CARG-GP):}\cr
         The package creates a pattern of how many sub-area rows will be placed 
         in each glyph group row based on the total number of sub-area with data.  
         The user can override this generated pattern by providing their own 
         pattern in the form of an integer vector.  Each entry represents one glyph 
         group row.  The maximum value for an entry is 5. The values must be in 
         decending order toward the median sub-area and must sum to the number 
         of sub-areas in the data.
         For example:  grpPattern = c(5,5,4,3,4,5,5) for 31 sub-areas.  
         If these rules are not followed, the following
         messages are generated and the package created pattern is used.
      
        \describe{
          \item{01C0}{\bold{CARG-GP The \option{grpPattern} call parameter must be 
             an integer vector.}\cr
             The vector provided must be an numeric or integer vector. The package 
             created pattern is used.  Correct and re-run.
            }
          \item{01C1}{\bold{CARG-GP The total of the rows per group must equal 
            the number of rows in the \var{<statsDFrame>} data.frame. 
            grpPattern ignored.}\cr
            
            The sum of the sub-area rows in the grpPattern vector is not equal 
            to the number of sub-areas present in the \var{statsDFrame} data.frame.  
            For example, if the \var{<statsDFrame>} data.frame has 51 rows after 
            duplicates are removed, then the sum of the grpPattern vector must be 51.
            A grpPattern of c(5,5,5,5,5,1,5,5,5,5,5) would be valid, but 
            c(4,4,4,4,4,1,4,4,4,4,4) would be invalid. The package will use a 
            calculated pattern.  Correct and re-run.
           }      
          \item{01C2}{\bold{CARG-GP The \option{grpPattern} call parameter vector 
            must be <= 5 (rows per group).  A value of <x> was found.}\cr
             
             Each element in the grpPattern vector represents the number of sub-area 
             rows per glyph group/row.  The maximum number of sub-area per group is 5.
             The package will use a calculated pattern.  Correct and re-run.
           }      
          \item{01C3}{\bold{CARG-GP The \option{grpPattern} call parameter is not 
             properly ordered. The number of rows per group must be in desending 
             order to the median sub-area.}\cr
             The number of rows per glyph group must be arranged in descending 
             order from the ends to the median sub-area row in the middle of the list.  
             For example:  grgPattern = c(5,5,4,3,4,5,5).  
             A grpPattern of c(3,4,5,5,4,3) is acceptable.
             The package will created and use a calculated pattern.  Correct and re-run.
           }  
          \item{01C4}{\bold{CARG-GP The one of the values in the grpPattern call 
             parameter is non-numeric or an NA.  grpPattern ignored.}\cr
             The grpPattern is a vector of integers specifying the number of areas in 
             each group/row or perceptional row.  One of the values in the vector 
             is a non-numeric value or an NA.  Correct and re-run.
           }           
         }
       }

      \item{01Dx}{\bold{\option{axisScale} call argument (axisScale & CARG-SC):}\cr
         The \option{axisScale} calling argument/parameter is a single value character 
         vector that specifies the type of axis scaling the micromapST should 
         use on the glyphs.  The acceptable values are:
         
        \describe{
          \item{o}{original axis labeling provided by the pretty function. 
             Scaling is done in the same way as previous releases of micromapST.
           }
          \item{e}{labeling is done using the extended algorithm from the 
             labeling package.
           }
          \item{s}{labeling is done using the extended algorithm and label 
             scaling by applying a multiplier of hundreds, thousands, etc 
             and a subtitle used to identify the multiplier units.
             \preformatted{
          Example:
            before:      1000  2000  3000  4000
                                  
            after:            in hundreds
                          1     2     3     4
              }                      
           }
          \item{sn}{labeling is done using the extended algorithm and each 
             label is scaled by applying a multiplier of hundreds, thousands, etc. 
             and a character identifying the scaling units postpended to the label.
             \preformatted{
          Example:
             before:    0    5000  10000  15000  20000
              
             after:     0     5K    10K    15K    20K
              }
           }
         }
         \option{axisScale} must be a character vector with a single item.
         
        \describe{
          \item{01D0}{\bold{CARG-SC The \option{axisScale} argument is invalid, 
             \option{axisScale} can only be set to \option{o}, \option{e}, \option{s}, 
             or \option{sn}. The default value of \option{e} will be used.}

             The \option{axisScale} call argument/parameter does not contain a valid value.  
             It must be \option{o}, \option{e}, \option{s}, or \option{sn}. 
             The default of \option{e} will be used.
           }
          \item{01D1}{\bold{CARG-SC The \option{axisScale} argument is not a character 
             vector of length 1.}

             The \option{axisScale} call argument/parameter must be a character vector 
             containing only one value. Numerical, integer and logical vectors are not 
             supported.  Only simple vector structures are support. Correct the value 
             assigned with \option{axisScale} and re-run.
           } 
          \item{01D3}{\bold{CARG-AX The value for axisMethod internal variable is 
             out of range 1-5 : <locAxisMethod>}

             The \option{axisScale} call argument/parameter is translated into the axisMethod 
             variable internally.  The value should be between 1 and 6.  If out of range
             an internal programming problem has occured.
           }
         }
       }
       
      \item{01Ex}{\bold{\option{staggerLab} call argument (CARG-SL):}
      
        \describe{
          \item{01E0}{\bold{CARG-SL The \option{staggerLab} argument is not a logical value. 
             Setting \option{staggerLab} to FALSE.}

             The \option{staggerLab} parameter must be a TRUE or FALSE value. If it is not 
             a logical variable, the call parameter is ignored and the default is used.
             ***check***
           }
          \item{01E4}{\bold{CARG-SL The maxAreasPerGrp argument is not a numeric value. 
             Setting to the default of 5.}

             The \option{maxAreasPerGrp} argument must be a numeric value from 1 to 5.
             The default value is 5.
           }
          \item{01E5}{\bold{CARG-SL The maxAreasPerGrp call parameter is not 5 or 6. 
             Value set to 5.}

             The \option{maxAreasPerGrp} argument must be either 5 or 6.  At the 
             current time only 5 is supported and will enforced.  
           }
         }
       }
       
      \item{01Fx}{\bold{\option{colSize} call argument (CARG-CS):}
        
        This feature is not implemented in the version. The argument name is reserved 
        for future use and development. 
       \describe{
         \item{01F1}{\bold{CARG-CS The \option{colSize} parameter in \var{pDName} 
           contains NA values in the colSize column: <BadValues>. Values must be numeric 
           and > 0.} 
           The columns in the panelDesc data.frame (\var{panelDesc}) are not value width 
           numbers.  <BadValues> lists the values in the column.  The width of the glyph
           column will be calculated.  Correct if needed and re-run.
          }
         \item{01F2}{\bold{CARG-CS The \option{colSize} parameter in \var{pDName} has 
           values for fixed width glyphs. Value(s): <BadValues>.  Value(s) are ignored 
           and set to NA.}
           The \option{colSize} parameter in the panelDesc data.frame cannot be used 
           to assign the column width on a glyph column that has already been assign 
           a column width. The value can be NA, "", " " or 0.  If any other 
           value that is not numeric, it is invalid. The value is ignored.  
           <BadValues> contains the list of values that will be ignored.
          }
         \item{01F3}{\bold{CARG-CS The \option{colSize} parameter in <pDName> does not 
           contain numeric values : <BadValues> } 
           The <pDName> panelDesc data.frame has an a non-numeric value, character, 
           logical, or a numeric with a value of "Inf" in the \option{colSize} 
           parameter column.  The <BadValues> list identifies the problem.  
           The value is ignored.  Correct and re-run.
          }
         \item{01F4}{\bold{CARG-CS The \option{colSize} entries in \var{pDName} are 
           out of range ( <= 0 or > 200 ). Values are: <BadValues>}
           The one or more values specified for \option{colSize} in the \var{pDName} 
           data.frame are out of range. The \option{colSize} value will be ignored.
          }
         \item{01F5}{\bold{CARG-CS The reviewed \option{colSize} parameter in 
          \var{pDName} has bad values (reported above) and have been replaced 
          by the mean of the good values: 
           <meanColSize>,  Bad Values: <BadValues> }

           The "colSize" panelDesc parameter row as a bad value, see message <BadValues>.
          }
         \item{01F6}{\bold{CARG-CS The \option{colSize} parameter in \var{pDName} contains 
             no useful information and will be ignored.}
            The information provided in the colSize argument in the panelDesc data.frame has no 
            usable values or information.  The argument will be ignored.
          }
        }
       }
      \item{01Gx}{\bold{Regional Features:}
       \describe{
         \item{01G0}{\bold{CARG-RB The regionsB call argument is not a logical variable. 
           The default of FALSE will be used.}\cr
           The regionB call parameter must be TRUE or FALSE.  FALSE indicates the 
           region boundaries should not be drawn, while TRUE indicate the should 
           be drawn as required.
          }
         \item{01G5}{\bold{ARG-DRO The dataRegionsOnly call argument is not a logical variable. 
           The default of FALSE will be used.}\cr
           The dataRegionsOnly call parameter must be TRUE or FALSE.  FALSE indicates the 
           all regions will be used and drawn.  TRUE indicates only the regions with areas 
           with data in the statsDFrame data.frame will be drawn.  Regions the do not have 
           areas with data will not be drawn. This is a means of including multiple regions 
           in a border group and only drawn sub-regions as required by the data.
          }
        }
       } 
     \item{01Kx}{\bold{\option{colors} call argument (COLORS):}
      
        \describe{
          \item{01K0}{\bold{COLORS An invalid single value is provided for the 
            \option{colors} argument. It must be 'BW', 'greys', or 'grays'. 
            The argument is ignored.}

             In the micromapST function call, a single value was provided for 
             the \option{colors} parameter.  The value must be \var{BW}, 
             \var{greys}, or \var{grays}.  If not, the parameter is ignored.
           }
          \item{01K1}{\bold{COLORS The colors vector has the incorrect number of items. 
             It must have 1 or 24 items. <number items> provided.}

             In the micromapST function call, a single value or a vector 24 values 
             can be supplied. If a different number of values are provided, 
             the parameter is ignored and the default colors are used.
           }
          \item{01K2}{\bold{COLORS The colors vector type is invalid.  It must be 
            a character vector.}

             In the micromapST function call, the \option{colors} parameter 
             contains non-character values. The \option{colors} contained in 
             the vector must be character strings identifying the colors 
             to be used for the 24 color parameters used by micromapST.   
             Numerical values related to the current color palette are 
             not acceptable.
           }
         }
       }

      \item{01Nx}{\bold{\option{details} call argument (DETS):}
         
         The \option{details} call argument/parameter contains a named list 
         of micromapST parameters values the user wants to override or change. 
         Most of the parameters relate to minor functional variations in 
         how the glyphs are drawn. Several other control the details on how 
         the graphic page is layed out and should not be changed under normal 
         situations. Changes of these variable can cause unpredictable results.
      
         Future: The \var{panelDesc} data frame has been extended to support 
         glyph parameter modifications on a per column per glyph basis and is 
         the preferred way of working with the glyphs variables. Any changes 
         made via the \option{details} parameter are global changes and will 
         effect all of the glyphs.
         
        \describe{
          \item{01N0}{\bold{DETS The <tag> does not have a valid value: <value>  
            Check type <method> used.}

             The \option{details} call argument/parameter must be a named 
             list structure and the type of each named variable on the 
             list must match the master table. If it is not, \option{details} 
             call parameter is ignored.
           }
          \item{01N1}{\bold{DETS The \option{details} parameter is not a list.}

             The \option{details} call argument/parameter must be a list structure. 
             If it is not, \option{details} value is ignored.
           }
          \item{01N2}{\bold{DETS variable: <name> not found in master variable list. 
             Name is not valid, skipped.}
          
             All of the variables used in the \option{details} call 
             argument/parameter name list must be on the master detail 
             variable list.  See the documentation section on the micromapDefSets 
             and the micromapSTSetDefaults for the variable description. 
             If the variable is not present, it will be deleted and ignored.
            }
          \item{01N3}{\bold{DETS Zero length variable name found in the 
            \option{details} list after the \var{<lastVarName>} variable.}

             This error frequently occurs when the \option{details} parameter 
             list contain two commas and no name/value. The position of the 
             two commas is preceded by the <lastVarName> variable in the list.  
             The empty variable/list will be ignored, but please remove the extra 
             comma for future runs.
            }
           \item{01NA}{\bold{DETS The \var{Id.Dot.pch} variable is can only be 
             set to a value in the range from 1 to 25.  Using the default of 22.}

             The \var{IdDotpch} variable is the symbol code for the character preceeding 
             the labels in the id glyph. Only pch values of 1 to 25 are supported..  
             The variable is ignored and the default character value of 22 is used. 
           }
         }
         Over time each more \option{details} variable will be checked and additional 
         error message will be provided.
         
         *** What other \option{details} variable should be validated?
         
         The \var{panelDesc} is being enhanced to allow appropriate variables 
         to be defined on a glyph graphic column basis. As 
         this feature becomes available the documentation will be updated.
       }
     }
   }

  \item{02xx}{\bold{panelDesc and glyphs data messages:}
  
     As micromapST setups to create the linked micromaps, each glyphs preforms additional validate 
     on the \var{panelDesc} variables and the associated \var{statsDFrame} data.

     The message identifier (4 alphanumeric) in the message is followed by the glyph name (type) 
     and the \var{panelDesc} panel column number (1 through "N").  This helps the user quickly 
     identify which glyphs column in the \var{panelDesc} definition is related to and 
     therefore the data in the \var{statsDFrame}.  The text of the message with specific information 
     to pin point the problem.

     For the map, mapcum, mapmedian, maptail and id glyphs there are no additional data 
     from the user and no additional validation is required.

     For the arrow, bar, dot, dotsignif, dotse, dotconf, ctrbar, normbar, segbar, and scatdot 
     glyphs uses data in the \var{statsDFrame} data.frame and use the \var{col1}, \var{col2}, and \var{col3} named 
     lists for the panel column to identify the \var{statsDFrame} columns with the required data. 
     The \var{col1}, \var{col2}, and \var{col3} pointers are validated and the data in the \var{statsDFrame} data.frame 
     is inspected based on the type of glyph requested.  The messages use the 3rd digit 
     to identify which \var{panelDesc} variable was validated.  1 = \var{col1}, 2 = \var{col2}, 3 = \var{col3}. 
     The name of the \var{panelDesc} named list is also included in the message.

     For the segmented stacked bar glyphs (CTRBAR, SEGBAR, NORMBAR), \var{col1} and \var{col2} specify 
     the first and last \var{statsDFrame} column of the contiguous set of columns to use for 
     the data for the segment values/sizes.  The message related to the inspection 
     of these columns use the 3rd digit of the message number to indicate the relative column number, 
     1 being the first column of data up to a maximum of 9 for the last column of data.  
     The column name is also included in the messages to help.
     
     The following is a summary of the usage of the \var{col1}, \var{col2}, and \var{col3} variables by each glyph: 
     
     \bold{Variable Usage by glyph:}
         \tabular{lllll}{
           glyph\tab \tab col1\tab col2\tab col3\cr
           dot\tab =\tab dotvalue\tab -\tab -\cr
           bar\tab =\tab bar length/value\tab -\tab -\cr
           
           arrow\tab =\tab start value\tab end value\tab -\cr
           dotsignif\tab =\tab dot value\tab p_value\tab -\cr
           dotse\tab =\tab dot value\tab standard error\tab -\cr
           scatdot\tab =\tab x coordinates\tab y coordinates\tab -\cr
           
           ctrbar\tab =\tab first data column in set\tab last data column in set\tab -\cr
           normbar\tab =\tab first data column in set\tab last data column in set\tab -\cr
           segbar\tab =\tab first data column in set\tab last data column in set\tab -\cr
           
           dotconf\tab =\tab dot value\tab lower confidence\tab upper confidence\cr
          }
      
          The \var{col1}, \var{col2}, and \var{col3} variables are not used 
          by the map, mapcum, maptail, mapmedian, id, boxplot, ts, and tsconf glyphs.


     The 3rd character in the message identifier \var{panelDesc} data column (\var{col1}, 
     \var{col2}, or \var{col3}) pointer related to the warning message.  The glyph graphic 
     column number associated with the warning is provided in the <cc> text of the message.

    \describe{
      \item{02yx}{\bold{\var{panelDesc} parameter checks for \var{panelDesc} variables:}
      
         This top level checks for the values provided in the \var{col1}, \var{col2}, \var{col3} variables are done by the internal
         function CheckColx and include:
         
        \describe{
          \item{a)}{If does not exist, create dummy vector to for variable
           }
          \item{b)}{Verify type of vector is numeric, integer, or character.  All other types not permitted.
           }
          \item{c)}{Fill empty string items with "NA" where appropriate.
           }
         }
         The rest of the checking is done in the glyphs based on which variables needed.
         
         *** Note, the use of <PDvar> appears to be a duplication of the \var{xx} variable in the message, is this true?
         The following warnings relate to the \var{panelDesc} variables in general:
         
        \describe{
          \item{0205}{\bold{ <gNameList> The length of the glyph type list is 
             different the length of the variables list.}
             The number of glyph graphs indicated in the panelDesc$Type column is different than 
             the number of data columns for each glyph variable (col1, col2, etc.)  Even if 
             the data column is not used by the glyph, the variable columns all must be the same
             length as the panelDesc$Type column.
           }
          \item{020A}{\bold{ <gName> <pdColNum> The first column name/number (<stColName1>) must 
             proceed the last column name/number (<stColName2>) in the <sDFName> data frame.}
             ***  More Information to be provided later. ***
           }
          \item{020B}{\bold{ <gName> <pdColNum> The first column name/number (<stColName1>) must 
             proceed the last column name/number (<stColName2>) in the <sDFName> data frame.}
             ***  More Information to be provided later. ***
           }
          \item{02y0}{\bold{ <gName> <pdColNum> The first column name/number (<stColName1>) must 
             proceed the last column name/number (<stColName2>) in the <sDFName> data frame.}
             ***  More Information to be provided later. ***
           }
          \item{02y1xx}{\bold{ <gName> <pdColNum> The number of segments is <value>. It must be 
             between 2 and 9. If over 9, only the first 9 will be used.}
             ***  More Information to be provided later. ***
           }
          \item{02y1}{\bold{PDCOL <glyph> cc The column name/number of <PDColOrig> in <PDVar>
             does not exist in the statsDFname data.frame.
             }
             The column name or number of <PDColOrig> in <PDVar> does not exist as a column name in the 
             statsDFrame data.frame, is a negative or zero valued integer, is not an integer, 
             or is an integer with a value greater
             then the number of columns in the statsDFrame data.frame. 
           }
         }
         It appears the "y" is the col"y" indicator and "cc" is the glyph column number.
         If so, the "y" is redundent.  
        
         The following warning deal with the column name or number specified 
         in the \var{panelDesc} data.frame variables \var{col1}, \var{col2}, and \var{col3}:
         
        \describe{
          \item{02y4}{\bold{<glyph> cc No \var{<statsDFrame>} column was specified in \var{<PDVarName>} 
             in the \var{<panelDesc>} panelDesc data.frame. A data column name/number is required. \var{<usage>}}

             The <glyph> function for column \var{<cc>} requires a valid \var{<statsDFrame>} data.frame column be specified 
             in the \var{panelDesc} \var{<PDVarName>} variable. The value is missing. Correct the \var{<PDVarName>} for column \var{cc}
             and re-run.
           }
          \item{02y5}{\bold{<glyph> cc The required panelDeac variable \var{<pdVarName>} is missing from 
             the \var{<pdDFname>} data frame. \var{<usage>}}

             When using the "<glyph>" in glyph column "cc", requires the \var{<pdVarName>} variable 
             to locate the data in the \var{<statsDFrame>} data frame.  This is general related 
             to the \var{col1}, \var{col2} and \var{col3} variable lists in the \var{panelDesc} 
             data frame.  Check the glyph descriptions and make sure all of the panelDesc variables 
             required for the glyph are provided. 
           }
          \item{02y6}{\bold{<glyph> cc The specified column name or number in \var{<pdVarName>} panelDesc 
             variable (<stColName>) does not exist in \var{<statsDFrame>} data frame or is out of range. \var{<usage>}}
 
             The <glyph> determined the \var{statsDFrame} column name or number provided in the \var{<pdVarName>} does not exist or is out of range. 
             If a column name was provided, it did not match any column names in the \var{<statsDFrame>} data frame.
             If a column number was provided, it has a value < 0 or greater than the number of columns in the \var{<statsDFrame>} data frame.
             Verify the column name or number in the panelDesc column variable and re-run. 
            }
           \item{02y7}{\bold{<glyph> cc The data provided in the \var{<stColName>} column of the \var{<statsDFrame>} 
             data frame contains one or more non-numeric entries. \var{<usage>}}

             For the glyph <glyph> in panel column \var{cc}, the data provided in the \var{<stColName>} are character vectors and 
             one or more values contain non-numeric characters.  This may result in an improper translation to numerics.
             The entries are treated as NA values.  Inspect the data in column \var{<stColName>} and correct.
           }
          \item{02y8}{\bold{<glyph> cc The data provided in the \var{<stColName>} column in the 
             \var{<statsDFrame>} data frame contains one or more entries that could not be 
             converted to numeric values. \var{<usage>}}

             For the glyph \var{<glyph>} in column \var{cc}, an attempt was made to convert 
             the character data in the \var{<stColName>} column of the \var{<statsDFrame>} 
             to numeric values.  
             One or more entries did not convert correctly resulting in missing values.  
             Inspect the data in the \var{<stColName>} column of the \var{<statsDFrame>} data frame and correct. 
            }
           \item{02y9}{\bold{<glyph> cc The \var{<stColName>} data column in \var{<statsDFrame>} 
             data frame is not a character or numeric vector. \var{<usage>}}

             The \var{<stColName>} data column in the \var{<statsDFrame>} is not a character or numeric column.  The glyph requires
             numerical values to generate the glyph.  Inspect the data and correct.
             Logical. List and Matrix type data is not supported.  If the column is character vectors, the character vectors must only
             contains numerical images and must be able to be converted to numerics to draw the glyph.  Inspect the data column and correct.
            }
          \item{02yA}{\bold{<glyph> cc The \var{<stColName>} data column in \var{<statsDFrame>} 
             data frame does not contain any numerical data. No rows will be drawn. \var{<usage>}}

            The \var{<statsDFrame>} data frame column \var{<stColName>} does not contain ANY 
            numerical data to permit drawing of the requested glyph.  Inspect the data column and re-run.
           }
          \item{02yB}{\bold{<glyph> cc The \var{<pdVarName>} data column in \var{<statsDFrame>} data frame contains 
             all missing values (NA) and cannot be graphed. \var{<usage>}}

             In the <glyph> for column \var{cc}, the data column in the user provided \var{statsDFrame} pointed to by the 
             \var{<PDVarName>} \var{panelDesc} variable contains a \var{NA} value.  The glyph for that data row will not be drawn.
             If this is an error, correct the data in the \var{statsDFrame} data.frame and re-run.
             ***check***
           }
          \item{02yC}{\bold{<glyph> cc The row numbers with missing data are: <list of row names>}\cr
             The <list of row names> indicates which rows in the \var{statsDFrame}
             data.frame provided by the user is missing data values, are NA, or "".
             Inspect the supplied data and correct.

             In the <glyph> for column \var{cc}, the data column in the user provided 
             \var{statsDFrame} pointed to by the \var{<PDVarName>} \var{panelDesc} variable 
             contains a \var{NA} value.  The glyph for that data row will not be drawn.
             If this is an error, correct the data in the \var{statsDFrame} data.frame and re-run.
             ***check***
           }
         \item{02yD}{\bold{<glyph> cc The \var{<stColName>} data column in \var{<statsDFrame>} 
             data frame does not contain any data. Data vector has length of zero. \var{<usage>}}

            The \var{<statsDFrame>} data frame column \var{<stColName>} has a length of zero. 
            If the \var{<statsDFrame>} is properly constructed with at least one sub-area, 
            then this should not happen.  If this error occurs, check the \var{<statsDFrame>} 
            to make sure it has not been corrupted and has rows for each sub-area being 
            graphed and mapped. 
           }
        }            

         The following warning related to the data pointed to by the \var{panelDesc} \var{col1}, 
         \var{col2}, and \var{col3} variable in the \var{statsDFrame} data.frame.  
         The "y" indicates which panelDesc col (1, 2, or 3) 
         is being referenced.
         
        \describe{    
           \item{02yC}{\bold{<glyph> cc The \var{<stColName>} column in \var{<statsDFrame>} data frame 
             contains one or more missing values. \var{<usage>}}
       
             ***  More Information to be provided later. ***
             Description needed.  **checked** arrow, bar, dot, dotsignif, dotconf, 
             ***check***
            }
          }
         
         The following warnings are specific to the dot significance glyph:
        \describe{
           \item{022P}{\bold{<glyph> cc One or more P_Value data entries in \var{<stColName>} are missing.}

             The data in the \var{<statsDFrame>} pointed to for use by the dotsignif glyph (\var{col2}) 
             for the significance P Value is not in the range of 0 to 1 probability.  Review the data 
             and correct the values to a range of 0 to 1. This is only used by the DOTSIGNIF glyph.
           }
          \item{022Q}{\bold{<glyph> cc One or more P_Value data entries in \var{<stColName>} are out of the range.}

             The data in the \var{<statsDFrame>} pointed to for use by the dotsignif glyph (\var{col2}) 
             for the significance P Value is not in the range of 0 to 1 probability.  Review the data 
             and correct the values to a range of 0 to 1. This is only used by the DOTSIGNIF glyph.
           }
         }
         
         The following special case that do not reference a particular panelDesc column 
         variable. These are generated by the stacked bar glyphs with regard to the \var{<statsDFrame>}
         data and the panelDesc col1 and col2 specifications.:
        \describe{
           \item{020A}{\bold{<glyph> cc The first column name/number (\var{<stColName>}) must proceed the last column 
             name/number (\var{stColName}>) in the \var{<statsDFrame>} data frame.}

             In segbar, normbar and ctrbar stacked bar glyphs, the user provides the location of the first 
             and last data column in the \var{<statsDFrame>} data frame.  Each data column between 
             the first and last columns are used to create the stacked bar glyph.  The processing is 
             done from lower column number to higher column number. Therefore, the first column must 
             preceed the last column in the data frame. That is must have a lower column number or 
             a column name that preceeds the last column name in the data frame. 
           }
          \item{020B}{\bold{<glyph> cc The number of segments is <numSegs>. It must be between 2 and 9. If over 9, 
             only the first 9 will be used.}

             Stacked bar glyphs use 2 to 9 data columns from the \var{statsDFrame} data.frame to create 
             segmented stacked bar graphs (CTRBAR, SEGBAR, NORBAR).  Each data column represents the length
             of one segment and is validated for the glyph prior to use.  
             If only 1 segment is specified, no stacked bar glyph is drawn.  
             If more than 9 segments exists, only the first 9 will be used 
             in the segbar, normbar and ctrbar glyph.
           }
          \item{020C}{\bold{<glyph> cc The number of segments is <numSegs>  At least 2 data columns must be defined.}

             ****Duplicate of 020B
             Used by segbar, normbar, ctrbar glyphs.
             ***check***
             ***  More Information to be provided later. ***
           }
          \item{020D}{\bold{<glyph> cc The data provided cannot be centered around the value of <center value>.}

             The ctrbar glyph is designed to center the stacked bars around the value of zero.  The data provided is
             all higher than or lower than zero.  A future feature is planned to allow the center value for the ctrbar
             glyphs to be specified. Until then the center value is set to 0 (zero).
             Used by ctrbar glyphs.
             ***check***
             
           }
         }

         
        \bold{BoxPlot specific processing messages (boxplot data list):}
         
        \describe{
           \item{02B1}{\bold{BOXPLOT cc The \var{panelData} value of \var{<pDataValue>} in the 
             \var{<panelData>} data frame does not exist or is not accessible.}
 	
             The \var{panelData} variable in the \var{<panelDesc>} data frame is used to pass completed 
             data structures to glyphs that cannot be contained in the \var{<statsDFrame>} data frame.  
             In this case, the variable name provided \var{<pDataValue>} does not exist or 
             is not accessible by the package.  Make sure the name is spelled correctly and exists 
             in the .GlobalEnv (calling) environment. This message is used by the BOXPLOT, 
             TS and TSCONF glyphs. **checked** boxplot, 
            }
           \item{02B3}{\bold{BOXPLOT cc The \var{<pdDataName>} data for the boxplot is not is list.}

              The data structure must be a list (one per area) of boxplot data lists.  With the name
              of each area the attribute or name of each list entry.
            }
           \item{02B4}{\bold{BOXPLOT cc The \var{<pdDataName>} structure does not have any name attributes 
              for the boxplot data.}

              No names, no way to link to the boundary data. Each list in this structed must 
              be named using the area's name that matches the name table.
            }
           \item{02B5}{\bold{BOXPLOT cc The \var{<pdDataName>} boxplot data is not a valid 
              structure. Must contain 6 boxplot sub lists.}

              The \var{<pdDataName>} structure must contain the 6 data lists for each area. 
              Less than 6 or greater than 6 lists were found in the area's list structure. 
              Correct the structure of the list and retry.
            }
           \item{02B6}{\bold{BOXPLOT cc The \var{<pdDataName>} boxplot data does not contain 
              all of the lists of boxplot function output. Invalid structure.}

              The \var{<pdDataName>} structure must contain the following 6 names list for 
              each area: stats, names, out, group, n, and conf. If these named lists are 
              not found, the package considers the data structure invalid and will not 
              attempt to draw the boxplot glyph.  Verify the \var{<pdDataName>} is the correct 
              variable and was properly created by the boxplot function. 
            }
           \item{02B7}{\bold{BOXPLOT cc In the \var{<PDDataName>} boxplot data, the '$name' 
              named list contains one or more missing values (NA).}
              
              The '$name' list is used to link the boxplot data to the boundary data. If the data 
              cannot be match with a area, the areas glyph row will not be drawn.  Check the 
              boxplot data and ensure each individual boxplot has a valid sub-area name associated 
              with it. 
            }
           \item{02B8}{\bold{BOXPLOT cc There are duplicate sets of boxplot data in <boxnam> for 
              the same area. Only the first one will be used.}
              
              The '$name' list in the boxplot data contain duplicates sub-area names. Only one 
              boxplot can be drawn per area. The first set of data will be used to draw 
              the boxplot.  Any other data with the same name will be ignored.  If a area does 
              not have any boxplot data, that area's glyph will be omitted. 
            }
           \item{02BA}{\bold{BOXPLOT cc The $stats matrix in the <boxnam> boxplot data does 
              not have 5 values per area list is not the same length as the $name list.}

              Need description 
            }
           \item{02BB}{\bold{BOXPLOT cc The $stats matrix in the \var{<pcDataName>} boxplot 
              data must \var{<bpNumNames>}  elements.}
              
              ***  More Information to be provided later. ***
            }
           \item{02BC}{\bold{BOXPLOT cc The $stats matrix in the \var{<pcDataName>} boxplot data 
              has missing values. Areas with missing values will not be drawn.}

              ***  More Information to be provided later. ***
            }
           \item{02BD}{\bold{BOXPLOT cc The sub-area names/abbrevations in the \var{<pdDataName>} 
              boxplot data $names values do not match the border group names: \var{<nameList>}.}

              ***  More Information to be provided later. ***
            }
           \item{02BE}{\bold{BOXPLOT cc 02BE BOXPLOT cc There are one or more of rows in the 
              \var{<statsDFrame>} that does not have matching boxplot data, 
              (\var{<pdDataName>}) entries.}

              ***  More Information to be provided later. ***
            }
           \item{02BF}{\bold{BOXPLOT cc 02BE BOXPLOT cc The missing sub-areas are: \var{<AreaList>}}

              ***  More Information to be provided later. ***
            }
         }
         For the time series (TS) and time series with confidence band (TSCONF), 
         data for the graphics is provided through a separate data structure via the panelData 
         column in the \var{panelDesc} data.frame.  The \var{statsDFrame} data.frame is not used and 
         the \var{colx} indexes are not used. 
         
        \describe{
        
          \item{02D1}{\bold{<glyphs> cc The \var{panelData} value of \var{<pDataValue>} in 
            the \var{<panelData>} data frame does not exist or is not accessible.}
	
 	    The \var{panelData} variable in the \var{<panelDesc>} data frame is used to 
 	    pass completed data structures to glyphs that cannot be contained in the 
 	    \var{<statsDFrame>} data frame.  In this case, the variable name provided 
 	    \var{<pDataValue>} does not exist or is not accessible by the package.  
	    Make sure the name is spelled correctly and exists in the .GlobalEnv 
	    (calling) environment. This message is used by the BOXPLOT, TS and TSCONF glyphs. 
          }
         }
         
         The following warnings are issued during the processing of the sub-area identifiers 
         in the \var{<statsDFrame>} data frame and the border group name table.
         
        \describe{
           \item{02P3}{\bold{<glyph> cc There are no links in the \var{<PDDataName>} data 
             to the following \var{statsDFrame} areas: <list of \var{statsDFrame} areas.}

             Sub-areas in the \var{statsDFrame} do not have data in \var{<PDDataName>}.
              ***  More Information to be provided later. ***
            }
           \item{02P4}{\bold{<glyph> cc The following area links in \var{<PDDataName>} 
             data do not link to \var{<statsDFrame>} areas and will not be used: <listOfNames>}

              Areas in the \var{<PDDataName>} do not have links to the \var{<statsDFrame>} data.frame.
              ***  More Information to be provided later. ***
            }
           \item{02P5}{\bold{<glyph> cc There are no sub-area links provided in \var{<PDDataName>}. Cannot link 
              data to the \var{statsDFrame} data.frame.}

              \var{<PDDataName>} does not have values to link to \var{<statsDFrame>} or the name table.
              ***  More Information to be provided later. ***
            }
           \item{02P6}{\bold{<glyph> cc There are duplicate sub-areas in the \var{<PDDataName>} 
              structure. Only the first match will be used.}

              \var{<PDDataName>} has duplicate entries.
              ***  More Information to be provided later. ***
            }
         }

         Only validate panelData entries for boxplot, ts and tsconf glyphs.  For boxplot, 
         datatype must be list.  For ts and tsconf, datatype must be 3 dimensional array. 
 
         Implementation: \var{statsDFrame} and the sorted link table drives the glyph generation.  
         Must do this to match up with the other columns.  If no data is in \var{<PDDataName>} 
         for the \var{statsDFrame} link, no glyphs is drawn for the row. 
   
         If duplicates exist in \var{<PDDataName>} only the first will be used.
 
         If extra entries exist in \var{<PDDataName>} that don't match the \var{statsDFrame}, 
         even if they match the name table, they are not used.
 
     
        \bold{TS and TSConf specific processing:}
        
        \describe{   
           \item{02T1}{\bold{TSxxxx-15 cc The variable name specified in the panelData row 
             does not exist.}

             The name of the array containing the area information, the series and the point 
             information (X, Y, HighY and LowY) values does not exist.  The glyph will not 
             be drawn.  Check for a misspelled variable or incorrect name and retry.
            }
           \item{02T2}{\bold{TSxxxx-02 cc The data structured passed in the panelData field 
              is not an array. Structure name = <dataNam>.}

               The data structure provided the additional information to construct the time series
               graphs MUST be a 3 dimension array.  Any other type of structure is not acceptable.
               
            }
           \item{02T3}{\bold{TSxxxx-03 cc The time serial array's 1st dimension is not <numRows> areas 
              to match statsDFrrame. It is <dimDArr[1]>.}
              
              The time series array does have enough 1st dimensional values to match the number of rows
              in the statsDFrame and the Name Table. 
            }
           \item{02T4}{\bold{TSxxxx-04 cc The time serial array's 2nd dimension must have at least 2 values 
              (time periods 2 to "n"). It is <dimDArr[2]>.}

              The time series must cover at least 2 or more time periods.  So, the 2nd dimension 
              must have a dimension of at least 2.
            }
           \item{02T5}{\bold{TSxxxx-05 cc The time series array's 3rd dimension is not 4 values for TSConf 
              (X, Y, LowY and HighY). It is <dimDArr[3]>.}

              See note on message "02TA" below.
            }
           \item{02T6}{\bold{TSxxxx-06 cc The time series array's 3rd dimension must be 2 or 4. It is <dimDArr[3]>. 
              (TS: X & Y, TSCONF: X, Y, LowY, HighY.)}

              See notes on message "02TA" below.
            }
           \item{02T7}{\bold{TSxxxx-10 cc rowNames on array do not match area ID list. The bad 
              area IDs are: <List of Name Table Keys>.}                                     

              In processing the time series array, several location IDs cannot be found
              in the Name Table.  They are listed in the <List of Name Table Keys> string.
              Determine the reason for the incorrect values, correct and retry.
            }
           \item{02TA}{\bold{TSxxxx-12 cc The time series array\'s 3rd dimension must be at 
              least 2.  It is <number>.}
              
              The 1st dimension represents the geographical areas of the Time Service.
              The 2nd dimension represents the time of the observation (0 to "N").  The area's
              points are sorted by the X value provided in the 3rd dimension.
              The 3rd dimension of the array are is the observed value on the graph for the 
              area at the time.  For standard TS graphs this is x and y values on the chart.
              For a TS-conf graph, the 3rd dimension contains the x and y values and the 
              confidence interval HighY and Low Y values.  Correct the content of the 3rd 
              dimension and retry.
            }
           \item{02TB}{\bold{TSxxxx-14 cc The time series array does not have rownames (location IDs)
              assigned to the 1st dimension. Data cannout be paired up with areas.}
              
              The row.names tied to the 1st dimension of the Time Series array are required
              to be able to link each time series with it geographic area.  Make sure 
              the values used for the 1st Dimension match the select type of location IDs in 
              the Name Table.
           
            }
         }
       }
     }
   }

  \item{04xx}{\bold{panel layout calculations:}
  
    \describe{
      \item{042x}{\bold{column sizing calculations:}
      
        \describe{
          \item{0420}{\bold{PANEL Calculated column widths is less than minimum <colSizeMin> 
            inches - too many columns specified.}

              ***  More Information to be provided later. ***
           }
          \item{0421}{\bold{PANEL Column width is to small to be useful, Package stopped.}

              ***  More Information to be provided later. ***
           }
         }
       }
      \item{043x}{\bold{row sizing calculations:}
      
        \describe{
          \item{0430}{\bold{PANEL panelLayout - The calculated width of <calculated-width> 
             is too large for the available space of <space-width>.}

              ***  More Information to be provided later. ***
           }
          \item{0431}{\bold{PANEL panelLayout - The calculated GrpRow Height is too small 
             to be used.}

              ***  More Information to be provided later. ***
           }
          \item{0432}{\bold{PANEL panelLayout - The calculated GrpRow Height is <GrpRowHeight> inches.  
             The minimum size limit is: <minimum row height>.}

              ***  More Information to be provided later. ***
           }
         }
       }
      \item{045x}{\bold{panel functions:}
      
        \describe{
          \item{0450}{\bold{PANEL panelLengthen - invalid vector length. < 2}

              ***  More Information to be provided later. ***
           }
          \item{0451}{\bold{PANEL panelSelect - Dimension error. Program error - index 'i' 
             or 'j' is out of bounds.}

              ***  More Information to be provided later. ***
           }
          \item{0452}{\bold{PANEL panelSelect - Bad label region name.  Must be left, right, 
             top or bottom.}

              ***  More Information to be provided later. ***
           }
         }
       }
      \item{049x}{\bold{Internal Package Messages:}
      
        \describe{
          \item{0490}{\bold{DMP Error in axisMethod selection in Dot and DotSignif code.}

              ***  More Information to be provided later. ***
           }
          \item{0491}{\bold{INB is.between.r The r range value is not a vector with length of 2. 
            FALSE returned.}

             Function is.between.r --- Need description **checked**
              ***  More Information to be provided later. ***
           }
         }
       }
     }
   }

  \item{05xx}{\bold{Informational:}
  
    \describe{
      \item{050x}{\bold{Panel Messages:}
      
        \describe{
          \item{0501}{\bold{PANEL Number of parameters overlaid = <numOverlaid.}

              ***  More Information to be provided later. ***
           }
         }
       }
     }
   }
 } 
}
\author{
Jim Pearson, StatNet Consulting, LLC, Gaithersburg, MD 
}
\keyword{messages}
\keyword{datasets}

