\name{messages-BG}
\alias{messages-BG}
\docType{data}							 
\title{micromapST Generated Warning and Error Messages - BuildBorderGroup}
\description{
  The BuildBorderGroup function verifies the call parameters and the 
  incoming data.  It then produces the data structures required
  by micromapST to draw maps for custom geographic areas.  
  The user must provide a shape file and a name table for the function
  to product the user's custom border group dataset. 
  This chapter documents the error and warning messages
  created by the BuildBorderGroup function when a problem or error 
  is discovered before R has a chance to abort the execution of the 
  function.
  
  Like micromapST messages, BuildBorderGroup messages all start with 
  "***" to help quickly find them in the warnings() logs and general output.

  The general format of the messages is:
  \preformatted{

    ***XXXX text of message
  }
  where the XXXX is the unque four alphanumeric message identifier of 
  the message. This chapter attempts to provide insight into the cause
  of the message and possible ways to resolve the problem.
  
  Messages with numbers ranging from 3000 to 3999.
}
\details{
  \bold{Conventions:}
  The user provides the shape file and the name table to the function.
  The messages attempt to provide specific information on the 
  problem detected.  So, parts of the message will be modified 
  to provide more specific information.  In the messages below <name> type
  fields are used to identify the variable information in the messages.
  
  Any values in the message that are replaced by more specific information
  are shown as <namefield>.  The explanation for the message and suggested
  means of resolution will reference the <namefield> variable to help
  define what the message is associated with in the operation of the 
  BuildBorderGroup funcition.
  
  The following is a listing of all micromapST generated messages and a 
  description of possible causes and solutions.
  
 \describe{
   \item{3xxx}{\bold{BuildBorderGroup Messages:} 
    \describe{
     \item{312x}{\bold{checkPointRestart}
      \describe{
        \item{3120}{\bold{The checkPointReStart call parameter is not a logical value.}
 
         The "checkPointReStart" call parameter must be a logical variable with a value of TRUE or FALSE.
         The default value is FALSE.
         }
        \item{3122}{\bold{Check Point Restart has been requested.  Check point files\cr
          will be read from folder: <name table directory>/Checkpoint directory.}
 
          The "checkPointReStart" call parameter was set to TRUE and the BuildBorderGroup function will attempt
          a restart using the <name table directory> and the "Checkpoint" subdirectory to locate
          the check point files.  If valid, the files will be reloaded and the BuildBorderGroup 
          process will be continued to completion.
         }
       }
      }
     \item{313x}{\bold{General Call Parameter}
      \describe{
        \item{3130}{\bold{Require call parameters are missing : <list of parameters> - execution stopped.}
       
         BuildBorderGroup requires several call parameters to run. The <list of parameters> string 
         presents to operate properly.  The minimum set of call parameters is:
         "NameTableFile", "BorderGroupName", and "ShapeFile".  If no directories are specified either the 
         value of the "NameTableDir" will be used or the current working directory.  The function must have
         enough information to locate the initial name table (.csv or spreadsheet format) and the shape file(s)
         (.shp, .shx, .dbf)
         }
       }
      } 
     \item{315x}{\bold{BorderGroup Files}
      \describe{
       \item{3150}{\bold{BorderGroup directory specified in the call parameter does not exist. 
         Value= <BorderGroupName>.}\cr
         The BorderGroupDir parameter in the function call specifies a non-existing directory. 
         Recheck the "<BorderGroupDir>" character string to see if it is correct and accessible.
        }
       \item{3154}{\bold{BorderGroupName is not a character string. Value= <BorderGroupName>.}\cr
         The BorderGroupName parameter is not a character string that can be used as the border
         group name.  The value must be a character string that is acceptable as a filename by 
         operating system.  Inspect and correct.
        }
       }
      }
   
     \item{320x}{\bold{NameTable Files}
      \describe{
       \item{3202}{\bold{NameTable directory specified in the NameTableDir call\cr
                  parameter does not exist. Value= <NameTableDir>}\cr
         The name table directory provided in the function call does not exist. Check the 
         "<NameTableDir>" character string and make sure it exists and is accessible.
        }
       \item{3204}{\bold{NameTableFile parameter has not been provided. Value= <NameTableFile>}\cr
         The NameTableFile parameter specifying the initial name table filename is not present.
         The parameter must be specified.
        }
       \item{3205}{\bold{File extension on name table filename is missing - .csv added.}\cr
         The extension on the name table filename is missing. The extension of ".csv" was 
         added to attempt to open the file.  If this is not correct, please correct the
         filename specified in the "NameTableFile" call parameter.
        }
       \item{3206}{\bold{The NameTable file is not a .csv, Excel, or R .RDA format.}\cr
         The initial name table file must be a .csv, Microsoft Excel (.xls or .xlsx) or an R .rda formated
         file.  Correct and retry.
        }
       \item{3208}{\bold{The NameTableLink call parameter is not a character string. Fix and rerun.}
         The NameTableLink call parameter must be a character string that can be used to reference 
         a data.frame column read in from a .csv, spreadsheet or R .rda file.  Please inspect the link name
         and make sure it is a valid column in the initial name table.
        }
       }
      }
    \item{321x and 322x}{\bold{Shape File and Link}
     \describe{
      \item{3212}{\bold{Shape File parameter has not been provided.}\cr
        The filename of the shape file has not been provided in the BuildBorderGroup function call.
        Review the function call and make sure the ShapeFile parameter is present and identifies
        the base filename of the shape file to be used.  The extensions of .shp, .shx, or .dbf do not
        have to be specified.  The base filename will be used as the "layer" to be read.
       }
      \item{3213}{\bold{The Shape File call parameter is not a SPDF or character variable.}\cr
        The ShapeFile call parameter can be either the filename of the shape file or the name of 
        a SpatialPolygonsDataFrame to be used.  Make sure the filename or the SPDF name are valid and 
        can be accessed.
       }
      \item{3214}{\bold{Shape file directory specified in the ShapeFileDir 
                    call parameter does not exist. Value= <ShapeFileDir>}\cr
        The directory specified in the ShapeFileDir call parameter does not exist.
        Check the "<ShapeFileDir>" value provided and make sure it is valid and access is permitted.
        When no value is provided, the directory specified for the NameTableDir will be used.
        If no NameTableDir is proivded, the current working directory is used.
       }
      \item{3216}{\bold{Shape file (dir & name) does not exist. Value= <xS>}\cr
        The path to the shape file (directory and name) does not exist.  The function
        attempted to access the directory and filename provided using no extension and the ".shp" 
        extension and could not find the shape file.  Check the path "<xS>" and make sure
        it is valid and can be accessed.
       }
      \item{3220}{\bold{The ShapeLinkName call parameter is missing. The default value of 'NAME' 
        will be used.}\cr
        For the shape file polygons to be used by micromapST, a linkage between the name table and shape file
        must be established.  The ShapeLinkName call parameter is used to identify the shape file variable 
        the function can use to match data in the name table.  The ShapeLinkName call parameter is missing.
        The function will attempt to use the default value of "NAME" to find a variable in the shape file.
        If this is not correct, provide the ShapeLinkName parameter with a value of the correct variable.
       }
      \item{3221}{\bold{ShapeLinkName is not a character string. Value= <ShapeLinkName>.}
        The value in the ShapeLinkName call parameter is not a character string that can 
        be used to access a variable in the shape file header.  Review the "<ShapeLinkName>"
        character string and correct.
       }
     }
    } 
    \item{323x & 324x}{\bold{Map Headers and parameters}
     \describe{
      \item{3230}{\bold{The MapHdr parameter does not contain character }\cr
        The MapHdr call parameter does not contain character strings.  Inspect and correct.  The default of 
        c("","Areas") will be used.
       }
      \item{3232}{\bold{The MapHdr parameter must be a simple vector type. }\cr
        The MapHdr call parameter must be a simple vector type variable.  Data.frames, lists, tibbles, and 
        more advanced structures are not allowed.  Correct and retry.
       }
      \item{3234}{\bold{The MapHdr parameter has zero or more than 2 elements. Only the first 2 will be used.}\cr
        The MapHdr call parameter is present, but must consist of 2 elements (values).  If more than 2 values
        are provided, then only the first 2 are used.
       }
      \item{3236}{\bold{It is suggested the max length of the MapHdr strings be }\cr
        The MapHdr values (2) should be less than 16 character to keep the map type glyph columns 
        from becoming too wide.  Consider shortening the character strings.
       }
      \item{3242}{\bold{The MapMinH parameter does not contain numeric value. Default Value is used.}\cr
        The MapMinH (Map Minimum Height) parameter is provided, but does not contain a numeric value.
        The default value of 1" is used.  
       }
      \item{3244}{\bold{The MapMinH minimum height value is out of range (0.4 to 2.5 inch). The default will be used.}\cr
        The MapMinH parameter value must be between 0.4 and 2.5 inches to be usable.  The default of 1" is used.
       }
      \item{3246}{\bold{The MapMaxH parameter does not contain numeric value. Default Value is used.}\cr
        The MapMaxH parameter value must be a numeric value.  The default value of 1.75 inches will be used. 
       }
      \item{3248}{\bold{The MapMaxH maximum height value is out of range (1 to 2.5 inches). The default will be used.}\cr
        MapMaxH default is 1.75"
       }
      \item{3249}{\bold{The MapMinH value must be less than the MapMaxH value. Will swap values.}\cr
        The value of the MapMinH must be smaller than the value for MapMaxH.  The values are swapped so
        that the MapMinH is less than MapMaxH.
       }
     }
    } 
    \item{325x}{\bold{ID Header and Parameters}
     \describe{
      \item{3250}{\bold{The IDHdr parameter does not contain character strings for use }\cr
        The IDHdr call parameter must be a character string to be used for the ID glyph column 
        headers.  The default header of the border group name and "Areas" will be used.
       }
      \item{3252}{\bold{The IDHdr parameter must be a simple vector type. }\cr
        The IDHdr parameter value must be a simple one dimensional vector with 2 value, like the MapHdr
        parameters. The default of the border group name and "Areas" will be used.
       }
      \item{3254}{\bold{The IDHdr parameter has more than 2 elements. Only the first 2 will be used.}\cr
        The IDHdr parameter has more than 2 values (elements) in the vector.  Only the first 2 values 
        will be used for the IDHdr headers.
       }
      \item{3256}{\bold{It is suggested the max length of the IDHdr strings be 12 characters.}\cr 
        To keep the ID glyph column from becoming too wide, it is recommended the IDHdr string be limited
        to less than 12 characters each.
       }
      }  
     }
     \item{326x}{\bold{Reduction Percentage Parameter}
      \describe{
       \item{3266}{\bold{The ReducePC parameter must be a numeric value. The default of 1.25 \% will be used.}\cr
         The ReducePC (Reduce Percentage) parameter is not a numeric value.  The default value of 1.25\% will be used. 
         This parameter indicates how much of the original spatial information will be kept when the geometry 
         is simplified by the rmapshaper package.
        }
       \item{3267}{\bold{The ReducePC parameter is not a simple vector. The default value of 1.25 \% will be used.}\cr
         The ReducePC parameter can only be a single value. The default value of 1.25 \% will be used.
        }
       \item{3268}{\bold{The ReducePC parameter has more than one value. Only the first value will be used.}\cr
	 More than one value is provided with the ReducePC parameter.  Only the first value will be used.		
        }
       \item{3269}{\bold{The value of ReducePC is out of range (0.01 to 100 percent). The default value of 1.25 \% will be used.}\cr
         The value of the ReducePC parameter must be between 0.01 and 100 percent.  It is out of range.  The default value of
         1.25 \% will be used.
        }
      }
     }
     \item{327x}{\bold{proj4 Parameter}
      \describe{
       \item{3272}{\bold{Error processing proj4 parameter. The provided proj4 call parameter will be ignored.}\cr
         The projection string provided on the proj4 call parameter is not valid or can not be process by R.  The proj4
         call parameter is ignored.
        }
       \item{3274}{\bold{The proj4 call parameter does not have +units=m, changing string to meters.}\cr
         The proj4 call parameter projection does not have a component of +units=m to set the projection to meters as 
         required.  Since this projection will be the final projection of the map, the +units has been changed to "m" 
         to force the final projection to be in meters.
        }
       \item{3276}{\bold{The proj4 call parameter specifies a long/lat projection.\cr
                            proj4: <proj4>\cr
                            The final projection can't be a longlat projection.\cr
                            The proj4 parameter is ignored and a AEA projection will be created.}\cr
         The proj4 call parameter specifies a long/lat projection (see <proj4> for details). 
         The final projection of the map for linked micromaps should not be long/lat.  Therefore,
         a Albers Equal Area projection is calculated based on the centroid of the map with secondary
         latitudes 1/4 the maps height above and below the centroid.
       }
      }
     }
     \item{329x}{\bold{LabelCex Call Parameter}
      \describe{
       \item{3296}{\bold{The LabelCex parameter must be a numeric value. The default of 0.25 will be used.}\cr
         The value of the LabelCex call parameter must be numeric.  The default of 0.25 will be used instead.
        }
      \item{3297}{\bold{ The LabelCex parameter must be a simple vector. The default value of 0.25 will be used.}\cr
         The LabelCex parameter value is not a single value vector but is a more complex data type. The default value of 0.25 will
         be used.
        }
      \item{3298}{\bold{The LabelCex parameter has more than one value. Only the first value will be used.}\cr
        The LabelCex parameter contains more than one value. Only the first value will be used.
        }
      \item{3299}{\bold{The value of LabelCex is out of range (0.05 to 10). The default value of 0.25 will be used.}\cr
        The LabelCex parameter value must be between 0.05 and 10.  It is out of range and the default value of 0.25 will
        be used.
        }
      }
     }
     \item{330x}{\bold{Shape File Processing and Call Parameter}
      \describe{
       \item{3300}{\bold{The Shape file SPDF structure was passed to the function in the call.}\cr
         This message is an informational message to document the shape file (as a SpatialPolygonsDataFrame) was passed to 
         the function instead of passing the directory and name of the shape file.
        }
       \item{3301}{\bold{Reading shape file from dir: <SFDir>  file: <SFName>}\cr
         The function is going to read the shape file from the directory <SFDir> and layer name <SFName> using rgdal\'s readOGR function
         with verbose = TRUE.
        }
       \item{3310}{\bold{The proj4string field in the Shape File is empty, set to <OrigLongLat>}\cr
         The proj4string slot in the SpatialPolygonsDataFrame for the shape file is empty.  The projection for the shape file is
         assumped to be a long/lat projection and will be set to a long/lat projection.
        }
       \item{3321}{\bold{Checking Shape Link Name Column: <ShapeLinkName>}\cr
         Checking to make sure the shape file data.frame header contain a variable by the name of <ShapeLinkName> as specified 
         on the function call.
        }
       \item{3324}{\bold{The ShapeLinkName provided: <ShapeLinkName> does not exist in the shapefile data.}\cr
         The shape file variable <ShapeLinkName> does not exist in the data.frame header.  Check for the existence of the variable
         and for possible spelling errors and return.
        }
       \item{3325}{\bold{Shape file link variable name is valid, values will be clean up and stored in variable X__Link.}\cr
        The specified Shape File link variable name was found in the SpatialPolygonsDataFrame.  Its contents will
        be cleaned up and saved in variable X__Link for later use.
        }
      }
     }
     \item{350x to 355x}{\bold{Name Table General Issues}
      \describe{
       \item{3500}{\bold{Name Table Link column is: <NameTableLink>}\cr
         The function will use the <NameTableLink> column to pair up the name table rows with the collections of polygons in the shape file.       
        }
       \item{3510}{\bold{The Name Table was read from: <NameTableType> <NameTablePath>}\cr
         The initial name table file will be read from <NameTablePath> containing the directory and filename.  The name table file type
         is <NameTableType>.
        }
       \item{3512}{\bold{The NameTable in the .rda file is not a data.frame. Please correct and retry.}\cr
         The initial name table can be constructed and saved in the R .rda format file.  When read, the contents
         of the file is not a data.frame structure and can't be used.  Research the cause and correct.
        }
       \item{3514}{\bold{There are more than one data.frame in the .rda file provided for the NameTable. 
         Provide only one data.frame table in the .rda and retry.}\cr
         When the .rda file was opened for use as the name table data.frame, more than one was found.  The .rda should only 
         contain one data.frame for use as the name table.
        }
       \item{3521}{\bold{The Name Table has no columns of data.}\cr
         The name table as read, does not contain any data columns or meaningful column labels.  Research and correct. 
        }
       \item{3522}{\bold{The Name Table has no rows or areas.}\cr
         The name table as read, does not contain any area rows.  There must be one row per area in the map.  Research and correct. 
        }
       \item{3532}{\bold{The column specified in the NameTableLink calling parameter 
              does not exist in the loaded Name Table.}\cr
         The value specified in the NameTableLink call parameter is not the name of a column in the read initial name table.  If the "Link" column
         exists, it will be used as the NameTableLink column to match the name table areas to the shape file polygons.
        }
       \item{3542}{\bold{At least one of the following columns must be present in the 
         NameTable file: <OneCol>  \cr Please correct the spreadsheet and try again.}\cr
         One of the columns named in <OneCol> must exist in the name table.   If none of the columns exist, then the initial 
         name table is not valid and needs additional work to permit the building of a border group.
        }
       \item{3550}{\bold{The following columns are not needed and will be deleted from the Name Table:}\cr
         Extra columns were found in the name table.  These columns are listed below and will be deleted.  Check for typos to make
         sure all of the required information is retained.
        }
      }
     }
     \item{356x, 357x, 358x}{\bold{Name Table Name, Abbr, ID, Alias and Alt_Abr data errors}
      \describe{
       \item{3562}{\bold{The <inx> column contains duplicate entries. Correct and retry.}\cr
         The column identified by <inx> was inspected and found to contain duplicate values (entries).  Duplicates are
         not allowed in location ID columns that must be unique for each row.  Research the values in column <inx>
         and correct.
        }
       \item{3564}{\bold{The <inx> column contains NA or blank values that are not allowed.}\cr
         The column identified by <inx> contains blank values or NA values.  These values are not permitted in 
         location ID type columns.  Research and correct the values.
        }
       \item{3572}{\bold{The Abbr column in the Name Table is not included. Will attempt to backfill 
          it from other information.}\cr
          The Abbr location id should be supplied in the name table whenever possible.  If it does not 
          exist, it will be created using other information provided in the the name table.
       }
      \item{3573}{\bold{The Name Table Abbr field is persent - no backfill required.}\cr
          The name table abbr field was present.  Processing continues normally.
       }
      \item{3574}{\bold{Some of the Name Table Abbr values are longer than 5 characters. It is 
         recommended the Abbr values be keep short.}\cr
         It is recommended the character string values in the Abbr column be kept to 5 characters or less. This keeps the 
         location ID fields in the data smaller and less work for the preparer and keeps the ID glyph abbr label narrower to more statistics
         type glyphs to be presented.
       }
      \item{3576}{\bold{None of the columns needed are present. The Link and one of the Name, Abbr, 
           and ID column should have been there. This should never happen with the previous checks.}\cr
         If this error message occurs, then something has happened during the processing.  This situation should have
         been caught earlier.   Research the name table and make sure all required columns are present.
        }
      \item{3582}{\bold{Checking ID column in the name table to make sure the values are numeric with leading zeros.}\cr
        While the ID values of location ids are numeric, a lot of them have leading zero.  The IDs are check to ensure
        they are numeric, then converted to character format and leading zeros added to help in later comparisons.
       }
      \item{3584}{\bold{The ID column is not present. A numerical sequence number has been used to fill the column.}\cr
        No ID column is present in the name table.  An ID column consisting of a sequence of 1 to "n" is used.
       }
      \item{3586}{\bold{The ID data column is not all numeric values. Values will be assigned.}\cr
        Not all of the ID values are numeric or valid.  The rows with bad ID values will be replaced with 
        new values.
       }
      \item{3592}{\bold{The Name Table in the <inxRN> area row and in the <inx> column \cr
        is not numeric and has a bad value of: <WrkVal> Value set to zero. Fix and retry.}\cr
        Each row in the name table represents an area in the map.  In the name table column <inx> on 
        row named <inxRN> there is a bad value. It is either non-numeric with a value of <WrkVal>.  The 
        value is set to zero for this run.  Research and correct the value.
       }
      \item{3594}{\bold{Data in row: <inxRN> for <inx> parameter <WrkVal2> is out of range. (<low> to <high>)}\cr
        The data value in the row named <inxRN> for column <inx> has a value <WrkVal2> that is out of range.  The acceptable
        range is from <low> to <high>.   Correct and rerun.
       }
      }
     }
     \item{363x}{\bold{Map Labels and Coordinates}
      \describe{
       \item{3630}{\bold{A value in MapL is not a character variable in the following rows:\cr
               <NameTableRows>\cr  Please correct and rerun.}\cr
         In the name table the MapL columns has values that are not characters in the rows listed
         in <NameTableRows>.  Correct the values to character type labels and rerun.
        }
       \item{3631}{\bold{If the MapL column is present with a label, then MapX and MapY must be 
            present. Correct and rerun,}\cr
          In the name table, the MapL column is present to provide a couple of labels for moved areas on the map.
          However, the coordinates of the MapL label (MapX and MapY) are not present.  Add the MapX and MapY columns indicating
          the coordinates to be used for the MapL label.  The MapX and MapY coordinates must be in the coordinates system
          used by the original shape file.
        }
       \item{3632}{\bold{MapL is present and there are no MapX coordinates. Correct and rerun.}\cr
          The MapL label column is present, but the MapX value of the coordinates of the label has been 
          omitted.  Update the name table and rerun.  
        }
       \item{3633}{\bold{MapL is present and there are no MapY coordinates. Correct and rerun.}\cr
          The MapL label column is present, but the MapY value of the coordinates of the label has been 
          omitted.  Update the name table and rerun.
        }
       \item{3634}{\bold{The MapL label for <NTN> should be 3 or less characters.  Please correct and rerun.}\cr
          The MapL label for <NTN> has more than 3 characters.  To work best on the very small map used in linked micromaps, 
          it is strongly suggested to use no more then a 3 character label.
        }
       \item{3635}{\bold{No MapLabel content - processing skipped.}\cr
          The old MapLabel column in the name table has been retired in favor of the MapL, MapX, and MapY name table 
          columns.   If none of these columns are present, then the processing and validation of the Map Label information 
          is skipped.  No map labels will be drawn.
        }
       \item{3636}{\bold{Some of the items in the MapLabel entry for <inxRN> are NA or blanks. Will be ignored.}\cr
	  The retire MapLabel value for the <inxRN> row is either NA or blank(s).   This value will be ignored and no 
	  Map Label will be drawn for this area.
        }
       \item{3637}{\bold{The MapLabel value for <inxRN> is not valid. Must be a character string 
          with three values separated by commas. The value is ignored.}\cr
 
        *** More detail to be added ***
        }
       \item{3638}{\bold{The label value in the MapLabel entry for <inxRN> is > 3 char.  
           Only first 2 characters used.}\cr
          The MapLabel label entry at name table row <inxRN> is > 3 characters.  Only the 
          first 2 characters are used for the label.  If needed, correct the label and re-run.
        }
       \item{3639}{\bold{One of the coordinates in the MapLabel entry for <inxRN> is not a number. 
          <xMapX> or <xMapY>}\cr
         The coordinates (X and Y) for the Map Label at name table row <inxRN>, one or both are not 
         numeric.  The values are <xMapX> and <xMapY>.  The values are ignored.  If required, correct
         the coordinates and re-run.
        }
       \item{3640}{\bold{One of the MapLabel coordinates for <inxRN> are out of range. Entry ignored.}\cr               
         One or both of the MapX or MapY coordinates at name table row <inxRN> are out of range and 
         will be ignored.  Correct and re-run.
        }
      }
     }
     \item{371x}{\bold{Proj4 projection processing}
      \describe{
       \item{3711}{\bold{The protection provided in the shape files does not have +units=m. Will modify 
            and setup for re-projection to change to meters.}\cr

       The BuildBorderGroup function depends on the projection units being meters.  Therefore,
       the projection found in the shape file will be modified to "+units=m" and the projection
       will be transformed at the end of processing.
       }
      \item{3712}{\bold{Found +units=m in proj4string of non-longlat projection in the shape file.}\cr

       There will not be any re-projection of the map done later.  Unless the proj4 call parameter
       is present.
       }
      \item{3718}{\bold{The proj4 value character vector.  Must be a valid proj4 argument 
            character string to be converted.}\cr

       In the convertPROJ4 subfunction, the proj4 value passed to the function is not a valid
       character string.  Subfunction fails.  The subfunction is called when the proj4 call parameter
       is validated.
       }
      }
     } 
     \item{372x & CleanGeo}{\bold{Boundary data clean up}
      \describe{
       \item{3720}{\bold{Cleaning up geometry in shape file using cleangeo inspect and correction.}\cr
         This is an informational message indicating the shape file geometry will be passed through
         the CleanGeo package for inspection and corrections.  The log will indicate any issues identified.
        }
       \item{cleanGeo1}{\bold{Unknown advanced cleaning method. Accepted values: 'POLYGONATION', 'BUFFER'}\cr
         The function is set up to use the "POLYGONATION" method.  This message should not be seen unless 
         there is a programming error.  The POLYGONATION method always selected.
        }
       \item{cleanGeo2}{\bold{Cleaning orphaned holes at index <s>, <x>}\cr
         Progress report from CleanGeo related to orphaned holes in the spatial structure.  
         No action is required. The issue will be resolved by CleanGeo.
        }
       \item{cleanGeo3}{\bold{Checking geometry validity at index <s>, <x>}\cr
         Progress report from CleanGeo acknowledging a valid check of the shape file's geometry 
         with in the structure.  CleanGeo was inspecting a geometry item at <s> and <x> in the 
         structure. No action required.
       }
      \item{cleanGeo4}{\bold{Catched MAPTOOLS warning <s>, <msg>}\cr
        CleanGeo monitor calls to maptools functions. If a warning is returned, it is documented by <s> and <msg> 
        in this message.  Review the warnings in the maptools documentation and make any needed corrections 
        if possible.
       }
      \item{cleanGeo5}{\bold{Catched MAPTOOLS error <s>, <msg>}\cr
        CleanGeo monitor calls to maptools functions. If a error is returned, it is documented by <s> and <msge>
        in this message.  Review the errors in the maptools documentation and 
        and make any needed correctioins if possible.
       }
      \item{cleanGeo6}{\bold{Cleaning geometry at index <s>, <x>, report.msg}\cr
        CleanGeo reports on the progress of the cleaning effort.  More details can be found
       in the report.msg file for the cleaning done at <s> and <x>.
       }
      \item{cleanGeo7}{\bold{Removing false polygon at index <s>, <x>}\cr
        CleanGeo has removed a false polygon in the spatial structure at <s> and <x>.  The 
        process then continues.  No action is required.
       }
      }
     }
     \item{375x}{\bold{Matching Shape File to Name Table}
      \describe{
       \item{3750}{\bold{Comparing shape file to name table links}\cr
          The link values in the shape file identified variable are being compared to 
          the values in the name table identified column. If there are any
          polygons that do not have a matching name table entry, the polygon(s) will
          be deleted.
        }
       \item{3752}{\bold{The following Shape File areas are not in Name Table:\cr
                 <List of Shape File areas not in Name Table>\cr
               The areas will be dropped.}\cr
        Any area (polygon) in the shape file that does not have a row in the Name Table, will 
        be dropped from the final border group map.
        }
       \item{3754}{\bold{Deleting the following polygons from the shape file:\cr
                      <List of areas to be deleted>}\cr
         This message reports the list of shape file areas to be deleted.
        }
      }
     }
     \item{376x}{\bold{Linking Name Table to Shape File}
      \describe{
       \item{3760}{\bold{Comparing the link values to tie the name table to the shape file.}\cr
         This is the reverse comparison to comparing shape file links to the name table links.
         If a name table area/row does not have a matching shape file set of polygons, then
         the functions execution will be halted.  Either provide the polygon(s) for the 
         area included in the name table or remove the name table entry.
        }
       \item{3762}{\bold{The following Name Table areas do not have boundaries in the ShapeFile:\cr
		<List of Name Table Rows without polygons>\cr
                Correct and retry.}\cr
        The BuildBorderGroup function can not continue if there is not an area(set of polygons) in 
        the shape file for the Name Table row.  Execution stopped.
        }
       \item{3768}{\bold{The length of Name Table and the number of areas in the shape file are different.}\cr
         Execution Stopped.  At this point in the processing previous messages would have stopped or modified
         the flow.  If the sizes of Name Table and Shape File are different at this time, it's a 
         programming error.
        }
      }
     }
     \item{377x}{\bold{Shape File Simplification}
      \describe{
       \item{3770}{\bold{Simplifying the shape file boundary data with rmapshaper.}\cr
         This message is a progress report to indicate the processing has called rmapshaper to 
         simplify the spatialpolygon data.frame (shape file) to the ReducePC specification 
         with a weighting of 0.9.  Check the results to see if the map is over simplified or not
         simplified enough.  Change the ReducePC call parameter and rerun until the map becomes usable.
        }
      }
     }
     \item{379x}{\bold{Area Size Inspection and Name Table Modifications}
      \describe{
       \item{3793}{\bold{The following areas may be too small (<0.03\%): <ListOfAreas>}\cr
         In general, if an polygon is less than 0.03% of the total map surface, when it is 
         shaded for linked micromaps, it may not be visible to the graphs users.  The list
         of areas that may be too small are provided in the "<ListOfAreas>" string
         in the message.  Review the list of areas and a plot of the map and determine
         if the areas should be enlarged or moved or manually manipulated to ensure their 
         shading can be seen easily.  checkPointReStart call parameter allows the user to 
         intercept the shape file before the VisBorders data.frame format datasets are created
         to make custom changes.
        }
       \item{3798}{\bold{Info:No modifications are required to map.}\cr
         The name table did not contain any shift, scale or rotate parameters for any areas 
         in the maps.  No modifications were done.
        }
      }
     }
     \item{38xx}{\bold{Final processing and transformation}
      \describe{
       \item{3800}{\bold{Transforming projection of Shape file and label points.}\cr
         This is a progress message to indicate, the function is about to perform any
         requested or needed project transformations on the shape file and the label points in the 
         name table.  If the resulting projection is not correct, check the value provided
         via the proj4 call parameter and whether the default AEA projection based on the center
         of the map is correct.
        }
       \item{3811}{\bold{Using user provided projection: <proj4>}\cr
         This message outputs the projection string provided by the BuildBorderGroup caller via the 
         proj4 calling parameter to help document the border group.
        }
       \item{3812}{\bold{Re-transforming shape file using original projection, with +unit= changed to meters.}\cr
         The original projection provided on the shape file did not have the +unit= proj4/6 parameter set 
         to meters.  BuildBorderGroup assumes the projection of the shape file just prior to the conversion
         of the boundaries to the VisBorders format is "meters".  The proj4 call parameter or the projection
         contained in the shape file was inspected and a new projection created with the +unit= paremeter set to
         "m" was created and used in the maps transformation before the VisBorders data.frames are created.
        }
       \item{3813}{\bold{Projecting shape file using created AEA projection.}\cr
         Since the shape file is a long/lat projection and no projection was provided via the proj4 call parameter,
         the BuildBorderGroup function will calculate an Albers Equal Area projection based on the centroid
         of the mapped area and secondary latitudes at 1/4 the map's height above and below the center latitute.
         This results in all of the areas being presented with equal area (or shading area).  
        }
       \item{3816}{\bold{Info:No transformation was done to the map.}\cr
         The shape file contains a non-long/lat projection with +unit=m.  Therefore, no projection 
         transformation will be done on the map.
        }
       \item{3817}{\bold{Info:Transform Map Label points.}\cr
         The drawing points for the map labels (when present) must be transformed in the same manner
         as the boundary points.  This progress message indicates the transformation is about to be done.
        }
       \item{3818}{\bold{Info:Transformation of Map Label completed.}\cr
         The transformation of the map label drawing coordinates is done.
        }
       \item{3819}{\bold{The length of name table and the number of areas in the shape file are different.}\cr
         The number of areas listed in the name table and the number of areas with polygons in the shape file
         are different.  Either the name table has more entried than the shape file or the shape file has 
         more areas then the name table.  Correct and re-run.
        }
       \item{3823}{\bold{The following areas may be too small (<0.03\%):
        <Names of areas the may be too small>}\cr

        If an area is too small, the user may not be able to see the shading in the 
        area and can't link the geographic location to the data displayed in the 
        other glyphs.  The list of areas contained in <Names of areas that may be too small> should
        be reviewed to determine how they can be enlarged to ensure their shading will be seen.
       }
      }
     }
     \item{39xx}{\bold{checkPointRestart and Wrapup}
      \describe{
       \item{3910}{\bold{The Checkpoint - Folder: <CkptPath>}\cr
         The directory used to save the check point files is <CkptPath>.
        }
       \item{3912}{\bold{Checkpoint - Name Table: <NTCkpt>  NTCkptcsv: <NTCkptcsv>}\cr
         The check point Name Table data.frame is save as a data.frame in the <NPCkpt> file and as a .csv 
         file in the <NTCkptcsv> file.
        }
       \item{3913}{\bold{Checkpoint - Shape File: <SFCkpt>  SFCkptRDA: <SFCkptRDA>}\cr
         The check point image of the shape file is saved in ERSI Shapefile format in the <SFCkpt> shape files and 
         as a SpatialPolygonsDataFrame in the <SFCkptRDA> RDA file.
        }
       \item{3914}{\bold{BuildBorderGroup has completed the write of the check point files to disk
        for possible editing and restart.  They are located in the following directory: 
         <Check Point Directory>}\cr
         The border group dataset has been successfully written to directory included in this message and is 
         ready for use, reuse, or editing.
        }
       \item{3915}{\bold{The check point Shape File for the border group is saved to: <Shape File Name>}\cr
         This message identifies the filename of the saved Check Point Shape File being used to build this
         border group.  It can be editted, but must be returned to the same directory with the same 
         name for the "checkPointRestart" logic to restart the border group building process.
        }
       \item{3916}{\bold{After editing, the results must be saved back to the same directory and filename.}\cr
         The check point shape file can be manually edited to make areas more visible in the small micromap. 
         Once the modifications are completed by any external GIS or polygon editor, the final shape file
         must be save in it's original directory and under the original filename.  If you want to save 
         a copy of the shape file producted by the BuildBorderGroup function, save it before making modifications.
        }
       \item{3917}{\bold{Checkpoint - areaParms image: <APCkpt>}\cr
          A check point file for the areaParms data.frame has been written to the /CheckPoint directory using 
          the file name of <APCkpt>.
        }
       \item{3920}{\bold{Check Point Restart Process Initiated.}\cr
         This is an informational message letting the user know the checkPointReStart process has been 
         initiated and all of the working files have been reloaded into the function.
        }
       \item{3921}{\bold{No Border Group or Name Table directory provides. Cannot find restart files. STOP.}
         The name table and border group directories provided for the checkPointReStart could not be located.
         Make sure to provide the same directories and file names used in the original BuildBorderGroup 
         function call.
        }
       \item{3922}{\bold{NameTable directory: <NameTableDir>}\cr
         The name table directory being used for the check point restart is <NameTableDir>.
        }
       \item{3930}{\bold{Creating the 4 micromapST boundary layers (area, L2, L3, and Regions).}\cr
         The shape file will be copied to the area, L2, Regions, and L3 shape file images and 
         merge based on the spaces layed out in the name table for L2 spaces, Regional spaces, and the
         outline of the entire map (L3).
        }
       \item{3940}{\bold{Completed conversion to VisBorders format.}\cr
         The conversion of the shape files to micromapST VisBorders has been completed by the function. The boundary sets
         will be plotted if requested for inspection, saved to the check point directory and then combined into 
         the border group dataset.
        }
       \item{3953}{\bold{Writing an images of each Border Group data.frame for <BGBase> }\cr
         A single R .rda file will be written for each of the 6 data.frames included in the border group <BGBase>.
         The can be found in the border group directory under the names of <BGBase>_<name of data.frame>.rda.
        }
       \item{3955}{\bold{ Border Group Created - Successfully.}\cr
         The writing of the border group dataset has been successful.  The border group is now ready for use.
        }
       \item{3969}{\bold{Border Group: <finalBGroup> is done.}\cr
         The process of gathering the information, validating it, editing it, and converting in to a format
         for micromapST has been completed and the dataset written to disk.
        }
       \item{3970}{\bold{Generate scaled example of test maps - Before Conversion.}\cr
         Message not used.
        }
       \item{3980}{\bold{The proj4 value character vector.  Must be a valid proj4 argument character string to be converted.}\cr
         The projection character string provided on the proj4 call parameter is not a valid.  It can not be used
         to specify a projection for use by R. At least make it complient with PROJ4 speciications. Correct and re-run.
        }
       \item{3985}{\bold{AdjPolygons - Polygons level value is not a 'Polygons' structure.}\cr
         In processing the SpatialPolygonsDataFrame image of the shape file, the "Polygons"  level below
         the "polygons" level is not a valid SpatialPolygons structure and can not be processed.  Execution
         is halted.   Review the shape file or SpatialPolygonsDataFrame structure and make sure it is correct.
        }
       \item{3999}{\bold{Errors have been found and noted above.  Execution stopped. Please fix problem(s) and retry.}\cr

        *** More detail to be added ***
        }
      }
     }
     \item{\bold{Report messages}}{
      \describe{
       \item{\bold{PUBLICATION INFORMATION FOR NAME TABLE IN BORDER GROUP}}{
         The border group has been created.  To make sure the user can use the same location ids that were
         used in the name table, the following table is printed to provide documentation on what 
         location IDs are available to the user when they are assemblying their data.  Each column present 
         in the name table is listed:  Name, Abbr, ID, Alias, Alt_Abbr.  These are the primary columns used by 
         the data gatherer.
        }
       \item{\bold{Name Table Modifications and Map Label Values}}{
         If Map Labels and name table modifications were specified for any area in the name table,
         The MapL, MapX, MapY, Xoffset, Yoffset, Scale and Rotate columns of the name table are 
         listed in this section for later reference.  If neither Map Labels or modifications were used, this 
         section of the report is not outputed.
        }
       \item{\bold{Name Table Map Label Values}}{
         If only Map Labels were implemented in the name table and not name table modifications, 
         only the values for the MapL, MapX, and MapY name table columns are displayed in this section of the report.
        }
       \item{\bold{Name Table Map Modifications Values}}{
         If no Map Labels were specified in the name table, but name table modification were specified, then 
         this section of the report will list only the name table modification values for future reference.
        }
       \item{\bold{Name Table Layer 2 and Regional Values}}{
         If the name table contains Layer 2 and Regional space information for use by micromapST, then this 
         section of the report will be outputed listing the L2_ID, L2_ID_Name, regID, and regName name table
         column information for later use.  If the L2 and regional information is not present this section
         of the report is not displayed.
        }
       \item{\bold{Name Table Layer 2 Values}}{
         If only the Layer 2 information is present in the name table, then the L2_ID and L2_ID_Names name
         table columns will be displayed in thei report for reference.
        }
       \item{\bold{Name Table Regional Values}}{
         If only the regional space information is present in the name table, then the regID and regName name
         columns will be displayed in the report for reference.
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

