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
   \item{3xxx}{\bold{BuildBorderGroup Messages:}\cr
    \describe{
     \item{310x}{\bold{Debug Call parameter:}\cr
      \describe{
       \item{3100}{\bold{debug call parameter is not a numeric value. 
         The default value of <default debug> will be used.}\cr
         
         The value provided for the debug call parameter is not a 
         numeric value.  It must a number between 0 and 65535.  
         The debug parameter will be set to the default value of 0.
       }
      }
     }
     \item{312x}{\bold{checkPointRestart}\cr
      \describe{
       \item{3120}{\bold{The checkPointReStart call parameter is not a 
         logical value.}\cr
 
         The checkPointReStart call parameter must be a logical variable 
         with a value of TRUE or FALSE. The default value is FALSE.
       }
       \item{3122}{\bold{Check Point Restart has been requested.  Check 
          point files\cr will be read from folder: 
          <name table directory>/Checkpoint directory.}
 
          The checkPointReStart call parameter was set to TRUE and 
          the BuildBorderGroup function will attempt a restart using the 
          <name table directory> and the Checkpoint subdirectory to locate 
          the check point files.  If valid, the files 
          will be reloaded and the BuildBorderGroup process 
          will be continued to completion.
       }
      }
     }
     \item{313x}{\bold{General Call Parameter}\cr
      \describe{
        \item{3130}{\bold{Require call parameters are missing : 
            <list of parameters> - execution stopped.}\cr
       
         BuildBorderGroup requires several call parameters to run. The 
         <list of parameters> string presents to operate properly.  
         The minimum set of call parameters is: NameTableFile, BorderGroupName, 
         and ShapeFile.  If no directories are specified either the 
         value of the NameTableDir will be used or the current working 
         directory. The function must have enough information to locate 
         the initial name table (.csv or spreadsheet format) and the shape file(s) 
         (.shp, .shx, .dbf)
       }
      }
     } 
     \item{314x}{\bold{Name Table Directory}\cr
      \describe{
       \item{3141}{\bold{NameTableDir call parameter is missing or NULL. Correct 
        and re-run.}\cr
        The NameTableDir parameter must be a character string that provide 
        the directory path information to the directory containing 
        the Name Table file.
       }
       \item{3142}{\bold{NameTableDir call parameter is an 'NA', Empty or not 
         a character string. Value= <NameTableDir>  Correct and re-run.}\cr
         The NameTableDir parameter must contain a character string 
         with length > 1 of an existing path to the directory that contains 
         the Name Table File.  The value was found to be NA, ""(empty), or a 
         non-character variable.  Correct the incorrect value and retry.
       }
       \item{3143}{\bold{NameTableDir value specified does not exist. 
         Value= <NameTableDir>}\cr
         The character string provided as the NameTableDir value is not a 
         valid path to the directory containing the Name Table File.  
         The path provided does not exist.
       }
      }
     }
     \item{315x}{\bold{BorderGroup Files}
      \describe{
       \item{3150}{\bold{BorderGroup directory specified in the call parameter 
         does not exist. It will be created. Value= <BorderGroupName>.}\cr
         The BorderGroupDir parameter in the function call specifies a 
         non-existing directory. 
         The path provided will be used to create the BorderGroupDir directory.
       }
       \item{3152}{\bold{The required BorderGroupName call parameter is missing.}\cr
         The BorderGroupName parameter must be provided to be able to create 
         the final border group data set.  Provide the required parameter and 
         rerun the function.
       }
       \item{3154}{\bold{BorderGroupName is a 'NA', Empty or is not a character 
         string. Value= <BorderGroupName>.}\cr
         The BorderGroupName parameter is not a character string or has a value 
         of NA. Neither can be used as the border group name.  The value must be 
         a character string that is acceptable as a filename by operating system.  
         Inspect and correct.
       }
       \item{3156}{\bold{BorderGroup directory specified in the call parameter is NA, 
         Empty or not a character vector. The parameter will be ignored and the 
         NameTable directory used. Value=<BorderGroupDir>
         }
         The provided Border Group Directory is found to be NA, ""(empty), 
         or a non-character variable.  The parameter is ignored and the 
         Name Table Directory is used instead. If the Border Group Directory 
         was meant to point to a different location, correct 
         the parameter and retry.
       }  
      }
     }
     \item{320x}{\bold{NameTable Files}\cr
      \describe{
       \item{3202}{\bold{NameTableFile parameter has not been provided. 
         Execution Stopped.}\cr
         The name table filename for the name table data has not been 
         provided. Add the appropriate NameTableFile call parameter.
       }
       \item{3204}{\bold{The name table passed to function on NameTableFile 
         parameter is not a valid variable type: <class of NameTable data>}\cr
         The class of the NameTableFile parameter must be a data.frame or 
         a character string. It was found to be <class of NameTable data>.  
         Correct and rerun.
       }
       \item{3206}{\bold{The NameTableFile parameter is set to 'NA', requires 
         a valid file name or structure.}\cr
         The name table file parameter is set to a NA value.  It must be a 
         valid data.frame or a character string indicating the name of the 
         name table file in the name table directory.
       }
       \item{3207}{\bold{The NameTableFile parameter is empty. A name table 
          structure or filename must be provided.}\cr
          For BuildBorderGroup to function, it must have the name or structure 
          of a valid Name Table.  Without this, the border group can not build 
          a name table or complete the construction of a border group dataset.  
          Check on how to build a name table or check for typos in the 
          Name Table File value.
       }
       \item{3208}{\bold{The NameTableLink call parameter is not a character 
         string. Fix and rerun.}\cr
         The NameTableLink call parameter must be a character string that can 
         be used to reference a data.frame column read in from a .csv, spreadsheet 
         or R .rda and .RData files. Please inspect the link name and make sure 
         it is a valid column in the initial name table.
       }
       \item{3209}{\bold{  The NameTable file is not a .csv, Excel, 
         or R .RDA format.}\cr 
         The file containing the initial NameTable data must be a .csv, 
         Excel spreadsheet or R .rda file.  Each column must be one of the data 
         columns required to construct the name table. (see name table section.)
       }
      }
     } 
     \item{321x}{\bold{Name Table Link}\cr
      \describe{
       \item{3212}{\bold{The NameTableLink call parameter is an 'NA', empty 
        (\'\' or \' \') or not a character string. Fix and rerun.}\cr
        The NameTableLink identifies the name table column that will be 
        used to link the name table to the information and polygons in the 
        shape file structure.  It was found have a value of 'NA', 
        Empty (\'\' or \' \') or was not a character type value. 

        The NameTableLink call parameter must be a character string that 
        can be used to reference a data.frame column read in from a .csv, 
        spreadsheet or R .rda file.  Please inspect the link name and make 
        sure it is a valid column in the initial name table.
       }
      } 
     } 
     \item{322x}{\bold{Shape File and Directory}\cr
      \describe{
       \item{3220}{\bold{Shape File parameter has not been provided or is NA.}\cr
        The filename of the shape file has not been provided in the 
        BuildBorderGroup function call. Review the function call and make sure 
        the ShapeFile parameter is present and identifies
        the base filename of the shape file to be used.  
        The extensions of .shp, .shx, or .dbf do not have to be specified.  
        The base filename will be used as the layer to be read. 
        The ShapeFileDir is used as the 'dsn' value on the st_read call.
       }
       \item{3221}{\bold{ShapeFile is a sf structure passed as a call parameter.}\cr
        The ShapeFile call parameter is a sf structure.
       }
       \item{3222}{\bold{ShapeFile is a SPDF structure passed as a call parameter.}\cr
        The ShapeFile call parameter is a SPDF structure.
       }
       \item{3223}{\bold{The ShapeFile call parameter is being used to pass 
        a full spatial structure to the function. However, the structure 
        must be a SPDF or a sf class. The data was: <ShapeFile class>}\cr
        The ShapeFile call parameter must be a character string, 
        sf structure or SPDF structure. The class of the ShapeFile value 
        is <ShapeFile class> and is not usable. Fix and retry.
       }
       \item{3224}{\bold{Shape file directory specified in the ShapeFileDir 
        call parameter does not exist. Value= <ShapeFileDir>}\cr
        The directory specified in the ShapeFileDir call parameter does not exist.
        The value was <ShapeFileDir>. Make sure the character string is a valid 
        path to the directory containing the Shape File and that access is 
        permitted. When no value is provided, the directory specified for the 
        NameTableDir will be used. If no NameTableDir is provided, 
        the current working directory is used.
       }
       \item{3225}{\bold{Shape file (dir & name) does not exist. 
        Value= <ShapeFileDir & ShapeFile value>}\cr
        The path to the shape file (directory and name) does not exist.  
        The either the directory and/or the shape file filename does not exist, 
        is not accessible or does not reference valid ERSI Shapefile collection 
        containing a x.shp file.  The extension of .shp is added to the 
        path to validate the directory and filename for ESRI Shapefiles 
        collections.  Only the base filename is required in the ShapeFile 
        filename. If present it is removed when the shape file is read. 
        Check the path <ShapeFileDir & ShapeFile value> and make sure 
        it is valid and can be accessed.
       }
       \item{3227}{\bold{The ShapeFile call parameter is an NA, empty('' or ' '), 
        or not a character string. The filename of the Shapefile must be specified.}\cr
        The provided ShapeFile name has a value of NA, is empty ('' or ' '), 
        or is not a character string.  A valid filename must be provide for the 
        ShapeFile name.  Correct and retry.
       }
      }
     } 
     \item{323x}{\bold{ShapeLinkName parameter}
      \describe{   
       \item{3230}{\bold{The ShapeLinkName call parameter is missing. The default 
        value of NAME will be used.}\cr
        For the shape file polygons to be used by micromapST, a linkage between 
        the name table and shape file must be established.  The ShapeLinkName 
        call parameter is used to identify the shape file variable the function 
        can use to match data in the name table. The ShapeLinkName call 
        parameter is missing. The function will attempt to use the 
        default value of NAME to find a variable in the shape file. 
        If this is not correct, provide the ShapeLinkName parameter 
        with a value of the correct variable.
       }
       \item{3232}{\bold{ShapeLinkName value is NA.  The default of NAME 
       will be used.}\cr
        The value in the ShapeLinkName call parameter is an NA value. 
        It must be a character string identifying the Shape File data 
        column/variable to be used to link the polygons to the 
        name table rows.
       }
       \item{3234}{\bold{ShapeLinkName is not a character string. 
        Value= <ShapeLinkName>.}\cr
        The value in the ShapeLinkName call parameter is not a character 
        string that can be used to access a variable in the shape file header.  
        Review the <ShapeLinkName> character string and correct.
       }
      }
     } 
     \item{324x}{\bold{Map Headers and parameters}
      \describe{
       \item{3240}{\bold{The MapHdr parameter does not contain character strings 
        for use as the column headers. The MapHdr parameter will be ignored. }\cr
        The MapHdr call parameter does not contain character strings.  
        Inspect and correct.  The default of c("","Areas") will be used.
       }
       \item{3242}{\bold{The MapHdr parameter must be a simple vector type. }\cr
        The MapHdr call parameter must be a simple vector type variable with 
        1 or 2 elements.  Data types of Data.frames, lists, tibbles, and other 
        advanced structures are not allowed.  Correct and retry.
       }
       \item{3244}{\bold{The MapHdr parameter has zero or more than 2 elements. 
        Only the first 2 will be used.}\cr
        The MapHdr call parameter contains more that 2 elements. \cr
        For example: c('hdr1','hdr2','hdr3') Only the first 2 elements will be used.
       }
       \item{3246}{\bold{It is suggested the max length of the MapHdr strings 
        be 16 characters. }\cr
        The MapHdr values (2) should be less than 16 character to keep the 
        map type glyph columns from becoming too wide.  Consider shortening 
        the character strings.
       }
      }
     } 
     \item{325x}{\bold{Map Minimum and Maximum Height}
      \describe{
       \item{3251}{\bold{The MapMinH parameter does not contain numeric value. 
        Default Value is used.}\cr
        The MapMinH (Map Minimum Height) parameter is provided, but does not 
        contain a numeric value.  The default value of 1 inch is used. 
       }
       \item{3252}{\bold{The MapMinH minimum height value is out of range 
        (0.4 to 2.5 inch).  The default will be used.}\cr
        The MapMinH parameter value must be between 0.4 and 2.5 inches to be usable.  
        The default of 1 inch is used.
       }
       \item{3254}{\bold{The MapMaxH parameter does not contain numeric value. 
        Default Value is used.}\cr
        The MapMaxH (Map Maximum Height) parameter is provided, but does not contain 
        a numeric value. The default value of 1.75 inches is used. 
       }
       \item{3255}{\bold{The MapMaxH maximum height value is out of range 
        (1 to 2.5 inches). The default will be used.}\cr
        The MapMaxH - value must be in the range from 1 to 2.5 inches. The default 
        value of 1.75 inches will be used.
       }
       \item{3257}{\bold{The MapMinH value must be less than the MapMaxH value. 
        Will swap values.}\cr
        The MapMaxH value must higher than the MapMinH value.  The values are swapped.
       }
      }
     }  
     \item{326x}{\bold{ID Header and Parameters}
      \describe{
       \item{3261}{\bold{The IDHdr parameter does not contain character strings 
        for use as the column headers.}\cr
        The IDHdr call parameter must be a character string to be used 
        for the ID glyph column headers.  The default header of the border group 
        name and Areas will be used.
       }
       \item{3262}{\bold{The IDHdr parameter must be a simple vector type.}\cr
        The IDHdr parameter value must be a simple one dimensional vector 
        with 2 value, like the MapHdr parameters. The default of the border 
        group name and "Areas" will be used.
       }
       \item{3264}{\bold{The IDHdr parameter has more than 2 elements. Only 
        the first 2 will be used.}\cr
        The IDHdr parameter has more than 2 values (elements) in the vector.  
        Only the first 2 values will be used for the IDHdr headers.
       }
       \item{3266}{\bold{It is suggested the max length of the IDHdr strings be 
        12 characters.}\cr 
        To keep the ID glyph column from becoming too wide, it is recommended 
        the IDHdr string be limited to less than 12 characters each.
       }
      }  
     } 
     \item{327x}{\bold{Reduction Percentage Parameter}
      \describe{
       \item{3272}{\bold{The ReducePC parameter must be a numeric value. The default 
        of 1.25 \% will be used.}\cr
        The ReducePC (Reduce Percentage) parameter is not a numeric value.  The 
        default value of 1.25\% will be used. This parameter indicates how much 
        of the original spatial information will be kept when the geometry 
        is simplified by the rmapshaper package.
       }
       \item{3274}{\bold{The ReducePC parameter is not a simple vector. The default 
        value of 1.25 \% will be used.}\cr
        The ReducePC parameter can only be a single value. The default value 
        of 1.25 \% will be used.
       }
       \item{3276}{\bold{The ReducePC parameter has more than one value. Only the 
        first value will be used.}\cr
        More than one value is provided with the ReducePC parameter.  
        Only the first value will be used.		
       }
       \item{3278}{\bold{The value of ReducePC is out of range (0.01 to 100 \%). 
        The default value of 1.25 \% will be used.}\cr
        The value of the ReducePC parameter must be between 0.01 and 100 percent.  
        It is out of range.  The default value of 1.25 \% will be used.
       }
      }
     }  
     \item{328x}{\bold{LabelCex Call Parameter}
      \describe{
       \item{3282}{\bold{The LabelCex parameter must be a numeric value. The default 
        of 0.25 will be used.}\cr
        The value of the LabelCex call parameter must be numeric.  The default 
        of 0.25 will be used instead.
       }
       \item{3284}{\bold{ The LabelCex parameter must be a simple vector. The default 
        value of 0.25 will be used.}\cr
        The LabelCex parameter value is not a single value vector but is a more 
        complex data type. The default value of 0.25 will be used.
       }
       \item{3286}{\bold{The LabelCex parameter has more than one value. Only the 
        first value will be used.}\cr
        The LabelCex parameter contains more than one value. Only the first 
        value will be used.
       }
       \item{3288}{\bold{The value of LabelCex is out of range (0.05 to 10). The default 
        value of 0.25 will be used.}\cr
        The LabelCex parameter value must be between 0.05 and 10.  It is out of range 
        and the default value of 0.25 will be used.
       }
      }
     } 
     \item{330x}{\bold{proj4 Parameter}
      \describe{
       \item{3301}{\bold{No projection provided in the shapefile or the proj4 
        call parameter,will be set to a Long/Lat projection.}\cr
        No projection was found in the shapefile or structure provided the 
        function or in the proj4 call parameter.  The boundary data is 
        assumed to be longitude/latitude values and the projection
        in the spatial structure will be set to a Long/Lat projection.
       } 
       \item{3302}{\bold{The proj4 call parameter set to NA or Empty('') or 
        is not a character string. The parameter will be ignored.}\cr
        The proj4 call parameter must be a valid character string in the 
        proj4 projection syntax. A value of NA is not acceptable and the 
        proj4 call parameter will be ignored.
       } 
       \item{3304}{\bold{3304 The proj4 call parameter is not a valid 
        character string or 'crs' structure for a projection. The proj4 parameter 
        will be ignored. The default AEA projection will be used in needed.}\cr
        The function was unable to process the provided proj4 projection character 
        string using the st_crs function.  The parameter will be ignored.  
        A generic AEA projection based on the center of the map and its height 
        will be used to transform the map for use in the link micromap.
       }       
       \item{3305}{\bold{Invalid proj4 parameter value provided. Parameter 
        will be ignored.}\cr
        When trying to process the proj4 projection string using the 
        sf package st_crs function, a error was raised. The proj4 string 
        can not be process properly and will be ignored.
       }
       \item{3306}{\bold{The proj4 call parameter specifies a long/lat   
        projection.\cr
        proj4: <proj4>\cr
        The final projection can't be a longlat projection.\cr
        The proj4 parameter is ignored and a AEA projection will be created.}\cr
        The proj4 call parameter specifies a long/lat projection (see <proj4> 
        for details). The final projection of the map for linked micromaps 
        should not be long/lat.  Therefore, an Albers Equal Area projection 
        is calculated based on the centroid of the map with secondary
        latitudes 1/4 the maps height above and below the centroid.
       }
       \item{3308}{\bold{The proj4 call parameter does not have +units=m, 
        changing string to meters.}\cr
        The proj4 call parameter projection does not have a component of +units=m 
        to set the projection to meters as required.  Since this projection 
        will be the final projection of the map, the +units has been changed 
        to m to force the final projection to be in meters.
       }
      }
     } 
     \item{331x & 332x}{\bold{Shape File Processing and Call Parameter}
      \describe{
       \item{3310}{\bold{The Shape file SPDF structure was passed to the function 
        in the call.}\cr
        This message is an informational message to document the shape file 
        (as a SpatialPolygonsDataFrame) was passed to the function instead 
        of passing the directory and name of the shape file.
       }
       \item{3311}{\bold{Reading shape file from dir: <SFDir>  file: <SFName>}\cr
        The function is going to read the shape file from the directory <SFDir> 
        and layer name <SFName> using rgdal\'s readOGR function
        with verbose = TRUE.
       }
       \item{3312}{\bold{The sf st_read can not import the shapefile as specified. 
        The errors reported were: <res1> <res2>}\cr
         
        In the attempt to read the shapefile, the function attempts to use 
        the dsn= parameter on the st_read for the directory to the shape files 
        and the layer= parameter to specify the specific shape file.  
        This is one of two modes the st_read function can use depending on the 
        driver it selects.  If this mode does not succeed, the function
        will attempt to use the mode where the layer= is not used and 
        the dsn= parameter must contain the entire directory and filename, 
        minus the extentions.  If either are successful, this message is 
        generated and the two error messages are reported in <res1> 
        and <res2>.  Review the error messages and correct the directory 
        name used, the filename referenced and make sure both exist.
         
       }  
       \item{3314}{\bold{Spatial Driver found in boundary data file read 
        was <SReadDriver>.}\cr
        The <SReadDriver> was identified during the st_read of the Shape File.
       }
       \item{3315}{\bold{The shape file structure was passed to the function 
        in the call.}\cr
        The type of data passed to the function in the ShapeFile parameter 
        indicates the user is passing a spatial structure rather than a filename.  
        The type of structure will be validated.
       }
       \item{3317}{\bold{The spatial structure passed to the function via the 
        ShapeFile parameter is not a SPDF or a sf full structure.  
        Please correct and try again.}\cr
        If a spatial structure is passed to the BuildBorderGroup function, 
        it must be a SPDF or sf structure containing the data information.  
        The data information is required to be able to match the 
        polygons in the structure to the rows in the name table.
       }
       \item{3320}{\bold{The projection field in the shapefile is empty, 
        set to <OrigLongLat>}\cr
        The proj4string slot in the SpatialPolygonsDataFrame for the shape 
        file is empty. The projection for the shape file is assumped to be 
        a long/lat projection and will be set to a long/lat projection of <OrigLongLat>.
       }
       \item{3325}{\bold{Checking Shape Link Name Column: <ShapeLinkName>}\cr
        Checking to make sure the shape file data.frame header contain a variable by 
        the name of <ShapeLinkName> as specified 
        on the function call.
       }
       \item{3326}{\bold{The ShapeLinkName provided: <ShapeLinkName> does not exist in 
        the shape file data.}\cr
        The shape file variable <ShapeLinkName> does not exist in the data.frame header. 
        Check for the existence of the variable and for possible spelling errors and return.
       }
       \item{3327}{\bold{Shape file link variable name is valid, values will be 
        cleaned up and stored in variable X__Link.}\cr
        The specified Shape File link variable name was found in the 
        SpatialPolygonsDataFrame.  Its contents will be cleaned up and saved 
        in variable X__Link for later use.
       }
      }
     }  
     \item{350x to 355x}{\bold{Name Table General Issues}
      \describe{
       \item{3510}{\bold{The Name Table was read from: <NameTableType> 
        <NameTablePath>}\cr
        The initial name table file will be read from <NameTablePath> 
        containing the directory and filename.  The name table file type 
        is <NameTableType>.
       }
       \item{3511}{\bold{The Name Table Path provided to read the Name Table 
         does not exist. Value = <NameTablePath>.
          }
        The Name Table Directory and Filename to read the Name Table does 
        not exist. Please check the Name Table file's location and try again.  
        The Name Table must be accessible to run the BuildBorderGroup function.
       }
       \item{3512}{\bold{The NameTable in the .rda file is not a data.frame. 
        Please correct and retry.}\cr
        The initial name table can be constructed and saved in the R .rda 
        format file.  When read, the contents of the file is not a data.frame 
        structure and cant be used.  Research the cause and correct.
       }
       \item{3514}{\bold{There are more than one data.frame in the .rda 
        file provided for the NameTable. Provide only one data.frame table 
        in the .rda and retry.}\cr
        When the .rda file was opened for use as the name table data.frame, 
        more than one was found.  The .rda should only contain one data.frame 
        for use as the name table.
       }
       \item{3215}{\bold{Name Table Link column is: <NameTableLink>}\cr
        The function will use the <NameTableLink> column to pair up the name 
        table rows with the collections of polygons in the shape file.       
       }
       \item{3521}{\bold{The Name Table has no columns of data.}\cr
        The name table as read, does not contain any data columns or meaningful column 
        labels. Research and correct. 
       }
       \item{3522}{\bold{The Name Table has no rows or areas.}\cr
        The name table as read, does not contain any area rows.  There must be one 
        row per area in the map. Research and correct. 
       }
       \item{3532}{\bold{The column specified in the NameTableLink calling parameter 
        does not exist in the loaded Name Table.}\cr
        The value specified in the NameTableLink call parameter is not the name of a column 
        in the read initial name table.  If the Link column exists, it will be used as 
        the NameTableLink column to match the name table areas to the shape file polygons.
       }
       \item{3542}{\bold{At least one of the following columns must be present in the 
        NameTable file: <List of Column Names>\cr Please correct the spreadsheet and 
        try again.}\cr
        One of the columns named in <List of Column Names> must exist in the name table.   
        If none of the columns exist, then the initial name table is not valid and needs 
        additional work to permit the building of a border group.
       }
       \item{3550}{\bold{The following columns are not needed and will be deleted from the 
        Name Table:}\cr
        Extra columns were found in the name table.  These columns are listed below and 
        will be deleted.  Check for typos to make sure all of the required information 
        is retained.
       }
      }
     }  
     \item{356x to 359x}{\bold{Name Table Name, Abbr, ID, Alias and Alt_Abr data errors}\cr
      \describe{
       \item{3562}{\bold{The <inx> column contains duplicate entries. Correct and retry.}\cr
        The column identified by <inx> was inspected and found to contain duplicate values 
        (entries).  Duplicates are not allowed in location ID columns that must be unique 
        for each row.  Research the values in column <inx> and correct.
       }
       \item{3564}{\bold{The <inx> column contains NA or blank values that are not 
        allowed.}\cr
        The column identified by <inx> contains blank values or NA values.  These values 
        are not permitted in location ID type columns.  Research and correct the values.
       }
       \item{3572}{\bold{The Abbr column in the Name Table is not included. 
        Will attempt to backfill it from other information.}\cr
        The Abbr location id should be supplied in the name table whenever 
        possible.  If it does not exist, it will be created using other 
        information provided in the the name table.
       }
       \item{3573}{\bold{The Name Table Abbr field is persent - no backfill 
        required.}\cr
        The name table abbr field was present.  Processing continues normally.
       }
       \item{3574}{\bold{Some of the Name Table Abbr values are longer than 5 
        characters. It is recommended the Abbr values be keep short.}\cr
        It is recommended the character string values in the Abbr column be kept 
        to 5 characters or less. This keeps the location ID fields in the data 
        smaller and less work for the preparer and keeps the ID glyph abbr 
        label narrower to more statistics type glyphs to be presented.
       }
       \item{3576}{\bold{None of the columns needed are present. The Link and 
        one of the Name, Abbr, and ID column should have been there. This should 
        never happen with the previous checks.}\cr
        If this error message occurs, then something has happened during 
        the processing.  This situation should have been caught earlier.   
        Research the name table and make sure all required columns are present.
       }
       \item{3582}{\bold{Checking ID column in the name table to make sure the 
        values are numeric with leading zeros.}\cr
        While the ID values of location ids are numeric, a lot of them have 
        leading zero.  The IDs are check to ensure they are numeric, then 
        converted to character format and leading zeros added to help in 
        later comparisons.
       }
       \item{3584}{\bold{The ID column is not present. A numerical sequence number 
        has been used to fill the column.}\cr
        No ID colomn is present in the name table.  An ID column consisting of 
        a sequence of 1 to n is used.
       }
       \item{3586}{\bold{The ID data column is not all numeric values. Values 
        will be assigned.}\cr
        Not all of the ID values are numeric or valid.  The rows with bad 
        ID values will be replaced with new values.
       }
       \item{3590}{\bold{Name Table L2_ID_Name column contains row(s) with NAs 
        or '' values. L2 feature disabled.}\cr
        All of the rows in the Name Table L2_ID and L2_ID_Name columns must 
        have values for the L2 Feature to operate correctly.  Inspect these 
        columns in the name table provided and correct.
       }
       \item{3592}{\bold{Name Table L2_ID column contains row(s) with NAs 
        or '' values. L2 feature disabled.}\cr
        All of the rows in the Name Table L2_ID and L2_ID_Name columns 
        must have values for the L2 Feature to operate correctly.  
        Inspect these columns in the name table provided and correct.
       }
        \item{3596}{\bold{Name Table regName column contains row(s) with NAs or '' values. Region feature disabled.}\cr
        All of the rows in the Name Table regID and regName columns must have values for the 
        Region Feature to operate correctly.  Inspect these columns in the name table provided and correct.
       }
       \item{3598}{\bold{Name Table regID column contains row(s) with NAs or '' values. Region feature disabled.}\cr
        All of the rows in the Name Table regID and regName columns must have values for the 
        Region Feature to operate correctly.  Inspect these columns in the name table provided and correct.
       }
       \item{3599}{\bold{The Region Feature has been disabled. The number of regions defined 
        is either 1 or is equal to the number of areas.}\cr
        For the Region feature to operate, there must be more than 1 region and less than the number
        areas.  If equal to 1 or equal to the number areas in the map, the Region function has no 
        value and is disabled.
       }
      }
     } 
     \item{362x}{\bold{Name Table Modification Parameters}
      \describe{
       \item{3622}{\bold{The Name Table in the <inxRN> area row and in the <inx> column
        is not numeric and has a bad value of: <WrkVal>. Value set to the default. Fix and retry.}\cr
        The data value in the row named <inxRN> for column <inx> has a value <WrkVal2> that is 
        a non-numeric value.  Correct and rerun.
       }
       \item{3624}{\bold{Data in row: <inxRN> for <inx> parameter <WrkVal2> is out of range. 
        (<low> to <high>)}\cr
        The data value in the row named <inxRN> for column <inx> has a value <WrkVal2> that is 
        out of range.  The acceptable range is from <low> to <high>.   Correct and rerun.
       }
      }
     } 
     \item{363x and 364x}{\bold{Map Labels and Coordinates}
      \describe{
       \item{3630}{\bold{The MAPL column in the name table is not character data. Labeling 
        will not be done.}\cr
        The map label must be a character string value.  Labeling for the row will not be done.
       } 
       \item{3631}{\bold{If the MapL column is present with label(s), then MapX and MapY 
        must be present. One or the other is missing. Labeling is not done.}\cr
        For each Map Label in the name table, there must be a valid MapX and MapY value 
        specifying the location on the map to draw the label.  If all three are not present,
        the label for that area will not be drawn.       
       } 
       \item{3632}{\bold{MapL value is present and there are no valid MapX coordinates. 
        Labeling will not be done.}\cr
        A map label for an area is present, but the MapX coordinate is missing or invalid.
        The label will not be drawn. Correct the MapX and possibly the MapY values.       
       } 
       \item{3633}{\bold{MapL is present and there are no valid MapY coordinates. 
        Labeling will not be done.}\cr
        A map label for an area is present, but the MapY coordinate is missing or invalid.
        The label will not be drawn. Correct the MapY and possibly the MapX values.       
       } 
       \item{3634}{\bold{The MapL label - <label> - for area <area name> should be 3 or less characters 
        to be usable.}\cr
        It is recommended the map label string be shortened to 3 character or less to reduce 
        the space required to draw the label.
       } 
       \item{3635}{\bold{No MapLabel content - processing skipped.}\cr
        The retired MapLabel column in the Name Table does not have any values.  Converting
        the MapLabel column into the MapL, MapX, and MapY columns will be skipped.       
       } 
       \item{3636}{\bold{Some of the items in the MapLabel entry for <name table row> 
        are NA or blanks. Will be ignored.}\cr
        The MapLabel entry for the <name table row> is blank or NA and can not be used.
        The value is ignored.
       } 
       \item{3637}{\bold{The MapLabel value for <name table row> is not valid. Must be a 
        character string with three values separated by commas. The value is ignored.}\cr
        The character string in the MapLabel field must consist of three values separated by 
        commas. The first value is a character string (the label) and the other two columns
        are numbers representing the X and Y coordintates to draw the label.  The X and Y 
        coordinates are in the units and origin of the original shape file.
       } 
       \item{3638}{\bold{The label value in the MapLabel entry for <name table row> is > 3 char.
        Only first 2 characters will be used.}\cr
        The recommend length of the map labels is 2-3 character. One or more of the labels are 
        greater than 3 character.  Adjust the length of the map labels.
       } 
       \item{3639}{\bold{One of the coordinates in the MapLabel entry for <name table row>
        is/are not a number. <MapX> or <MapY>}\cr
        Check the MapX and MapY coordinates values, they must be numerical values.
       } 
       \item{3640}{\bold{One of the MapLabel coordinates for <name table row> are out of range. 
        Entry ignored.}\cr
        The values for the MapX and MapY coordinates must be within the range of the space covered 
        by the defined map in its original projection. If the values are outside of the map's area, 
        this error will appear.  Inspect the coordinates and adjust as needed.
       } 
      }     
     }  
     \item{371x}{\bold{Shape File Projection}
      \describe{
       \item{3711}{\bold{The projection provided in the Shape File does not have +units=m,
        modify and setup for re-projection to change to meters.}\cr
        The final projection must be in meters.  The projection in the Shape File or 
        structure is not meters. A new projection based on the original is created 
        with +units=m.  This projection will be used for the final transformation of the map.
        }
       \item{3712}{\bold{Found +units=m in projection string of non-longlat projection 
        in the shape file.}\cr
        The projection in the shapefile is not a long-lat projection and it is already in units of
        meters.  No modifications are required and no transformation will be needed.
        }
       } 
      } 
     \item{372x}{\bold{Boundary data clean up}\cr
      \describe{
       \item{3720}{\bold{Cleaning up polygons in spatial structure.}\cr
        The spatial structure will be passed through sf st_is_valid and st_make_valid 
        functions to clean up the polygon geometries.
        }
       \item{3722}{\bold{Shape File contains invalid polygons.  The indexes and associate reasons are\:}\cr
        This is an informational message indicating the shape file geometry will be passed through
        the sf package validation and make_valid function to clean up the geometry.  S2 functions
        are not used.
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
       \item{3764}{\bold{Some of the polygons in the Shape file still do not belong to 
        areas in the name table. Polygon(s) are ignored.}\cr
        Not every polygon is linked to an area in the Name Table.  These polygons will 
        remain in the map, but will be ignored.  If multple ignored polygons share the 
        same boundary, the boundary may not be drawn.
       }
      }
     } 
     \item{377x}{\bold{Shape File Simplification}\cr
      \describe{
       \item{3770}{\bold{Simplifying the shape file boundary data with rmapshaper.}\cr
        This message is a progress report to indicate the processing has called rmapshaper to 
        simplify the spatialpolygon data.frame (shape file) to the ReducePC specification 
        with a weighting of 0.9.  Check the results to see if the map is over simplified or not
        simplified enough.  Change the ReducePC call parameter and rerun until the map becomes usable.
       }
       \item{3772}{\bold{rmapshaper parameters before simplification : Keep= <MS_Keep> 
        Weight= <MS_Weighting>}\cr
        The rmapshaper ms_simplify function parameters for the MS_Keep and MS_Weighting 
        are listed to document the amount of boundary reduction and smoothing.  See the 
        rmapshaper ms_simplify documentation for more details.
       }
       \item{3774}{\bold{rmapshaper processing completed.}\cr
        The rmapshaper ms_simplify function has completed its work and has returned a 
        modified spatial structure.
       }
       \item{3778}{\bold{Map area aggregation completed.}\cr
        After the rmapshaper ms_simplify completes its work, the polygons in the spatial
        structure are aggregated into multipolygons to create one rows per area
        in the sfc structure.  This message signals the aggregation step has been 
        completed.  The number of features in the sf structure are now equal to the 
        number of areas in the name table.
       }
      }
     }  
     \item{379x}{\bold{Area Size Inspection and Name Table Modifications}\cr
      \describe{
       \item{3793}{\bold{The following areas may be too small (<0.03\%): <ListOfAreas>}\cr
        In general, if an polygon is less than 0.03% of the total map surface, when it is 
        shaded for linked micromaps, it may not be visible to the graphs users.  The list
        of areas that may be too small are provided in the <ListOfAreas> string
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
    \item{38xx}{\bold{Name Table Modifications}\cr
     \describe{
      \item{3801}{\bold{Identifying neighbors for each area.}\cr
       The function is now searching and identifying the neighbors for each area in the map.
       This information is used in the color selection algorithm to keep two areas that 
       share the same boundary line from being colored the same color in the examples 
       and any future feature that is added to micromapST.
      }
      \item{3804}{\bold{The list of neighbors for each area are in the name table 'NB' column.}\cr
       The neighbor list is stored in the 'NB' column of the name table temporarily.  This will be
       save across the checkpoint restart for future use.
      }
      \item{3810}{\bold{Info:No modifications are required to map.}\cr
       The name table did not contain any modifications for areas in the map.
      }
      \item{3811}{\bold{Area: <area key> will be adjusted using the <modification list> values.}\cr
       <area key> area has parameters to have shifts, scaling and/or rotate modification done to the 
       boundaries.
      }
      \item{3812}{\bold{Xoffset: <Xoffset>  Yoffset: <Yoffset>  Scale: <Scale>  Rotate: <Rotate>  in radians: <radians>}\cr
       The name table contains the following values for each of the possible modification for the 
       identified area in # 3811.
      }
      \item{3813}{\bold{Re-inserting area polygons for <area key>}\cr
       The area <area key> has been modified and is being re-inserted into the map and 
       neighboring areas.
      }
      \item{3814}{\bold{plot of original area before modifications in blue:}\cr
       A screen plot was drawn of the area before modification with blue lines.
      }
      \item{3815}{\bold{plot of main area after modifications in green:}\cr
       A screen plot was drawn of the area after the modification with green lines.
      }
      \item{3818}{\bold{Original neighbor boundaries in magenta for <neighbor area>}\cr
       A screen plot was drawn for an neighboring area to the modified area before  
       adjustments were made with magenta lines.
      }
      \item{3819}{\bold{Trimmed neighbor boundaries in seagreen for <neighbor area>}\cr
       A screen plot was drawn of an neighboring area to the modified area after trimming
       adjustments were made with seagreen lines.
      }
      \item{3822}{\bold{Name Table modificiations to Shape file are completed.}\cr
       All of the name table specified area modification has been completed.
      }
     } 
    }  
    \item{384x}{\bold{Area Coverage Analysis}
     \describe{
      \item{3843}{\bold{These area coverage estimates were done after the name table 
       modifications. The area's coverage may have be increased or decreased from the 
       original Shape file.  This coverage review is done against the current area's 
       coverage after all modifications.  If an area's coverage is less than 0.057\% 
       of the total map coverage of <TotalArea> * 0.00057 m^2 may not be large enough 
       to be visible in a in a linked micromap graphic.  The area(s) that should 
       be reviewed is(are):}\cr
      }
      \item{3844}{\bold{<list of areas that may be undersized.>}
       This message provide a list of the areas that may not be visible when 
       willed with the link color in the linked micromap.  Investigate and adjust 
       the area size as needed.
      }
      \item{3845}{\bold{All of the area appear to be big enough to show the shading 
       in a linked micromap.}\cr
       Verify all of the areas can be seen when the linked micromap color shades the 
       area.
      }
     }
    } 
    \item{386x}{\bold{Final processing and transformation}\cr
     \describe{
      \item{3860}{\bold{Transforming projection of Shape file and label points.}\cr
       This is a progress message to indicate, the function is about to perform any
       requested or needed project transformations on the shape file and the label points in the 
       name table.  If the resulting projection is not correct, check the value provided
       via the proj4 call parameter and whether the default AEA projection based on the center
       of the map is correct.
      }
      \item{3861}{\bold{Using user provided projection: <proj4>}\cr
       This message outputs the projection string provided by the BuildBorderGroup caller via the 
       proj4 calling parameter to help document the border group.
      }
      \item{3862}{\bold{Re-transforming shape file using original projection, with +unit= changed to meters.}\cr
       The original projection provided on the shape file did not have the +unit= proj4/6 parameter set 
       to meters.  BuildBorderGroup assumes the projection of the shape file just prior to the conversion
       of the boundaries to the VisBorders format is meters.  The proj4 call parameter or the projection
       contained in the shape file was inspected and a new projection created with the +unit= paremeter set to
       m was created and used in the maps transformation before the VisBorders data.frames are created.
      }
      \item{3863}{\bold{Projecting shape file using created AEA projection.}\cr
       Since the shape file is a long/lat projection and no projection was provided via the proj4 call parameter,
       the BuildBorderGroup function will calculate an Albers Equal Area projection based on the centroid
       of the mapped area and secondary latitudes at 1/4 the map's height above and below the center latitute.
       This results in all of the areas being presented with equal area (or shading area).  
      }
      \item{3865}{\bold{Transformations completed.}\cr
       All of the transformation and projection modifications have been completed.
      }
      \item{3866}{\bold{Info:No transformation was done to the map.}\cr
       No transformations were required on the final map.  The original projection is still 
       in use.
      }
      \item{3868}{\bold{The proj4 value character vector is not a valid projection.
       Must be a value acceptable to st_crs(). proj4 is ignored.}\cr
       Verify the proj4 call parameter is correct and has the right syntax for the proj4 projection
       specification.  The proj4 parameter will be ignored.
      }
     }
    }  
    \item{387x}{\bold{Generating Test Plot}
     \describe{
      \item{3872}{\bold{The length of name table and the number of areas in the shape file are different.}\cr
       The number of areas listed in the name table and the number of areas with polygons in the shape file
       are different.  Either the name table has more entried than the shape file or the shape file has 
       more areas then the name table.  Correct and re-run.
      }
     }
    }  
    \item{391x}{\bold{checkPointRestart Writes}\cr
     \describe{
      \item{3910}{\bold{The checkpoint files will be store in the Checkpoint directory : <CkptPath>}\cr
       The directory used to save the check point files is <CkptPath>.
      }
      \item{3912}{\bold{Checkpoint -  Name Table RDA Filename: <NTCkpt>   Name Table CSV 
       Filename: <NTCkptcsv>}\cr
       The check point Name Table data.frame is save as a data.frame (.rda) in the <NPCkpt> file 
       and as a .csv file in the <NTCkptcsv> file.
      }
      \item{3913}{\bold{Checkpoint - The shape file spatial data is written to: <SFCkpt>.
        as a set of ESRI Shapefiles.  The RDA image spatial data is in: <SFCkptRDA>}\cr
       The check point image of the shape file is saved in ERSI Shapefile format in the 
       <SFCkpt> shape files and as a SpatialPolygonsDataFrame in the <SFCkptRDA> RDA file.
      }
      \item{3915}{\bold{Checkpoint - areaParms data.frame in: <APPCkpt> as <APCkpt>}\cr
       The control areaParms dataset has been checkpointed in the <APPCkpt> directory
       as the <APCkpt> filename.
      }
      \item{3917}{\bold{BuildBorderGroup has completed writing of the checkpoint files to disk 
        for possible editing and restart. They are located in the following directory : 
        <Check Point Directory>}\cr
       The border group dataset has been successfully written to directory included in this 
       message and is ready for use, reuse, or editing.
      }
      \item{3918}{\bold{The check point Shape File for the border group is saved to: <Shape File Name>}\cr
       This message identifies the filename of the saved Check Point Shape File being used to build this
       border group.  It can be editted, but must be returned to the same directory with the same 
       name for the checkPointRestart logic to restart the border group building process.
      }
      \item{3919}{\bold{After editing, the results must be saved back to the same directory and filename.}\cr
       The check point shape file can be manually edited to make areas more visible in the small micromap. 
       Once the modifications are completed by any external GIS or polygon editor, the final shape file
       must be save in it's original directory and under the original filename.  If you want to save 
       a copy of the shape file producted by the BuildBorderGroup function, save it before making modifications.
      }
     }
    }
    \item{392x}{\bold{Checkpoint Reload to Continue}
     \describe{
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
      \item{3923}{\bold{Using the BorderGroup directory: <BorderGroupDir> to locate the 
       checkpoint files.}\cr
       The name table directory was not provided. Since the target output directory was the 
       BorderGroupDir path, the BorderGroupDir call parameter will be used to locate the 
       checkpoint folder and files.
      }
      \item{3925}{\bold{Reading areaParms dataset: <areaParm.rda file>}\cr
       The restart logic in the checkpoint feature is loading the areaParms.rda
       file from the <areaParms.rda file> path.
      }
      \item{3926}{\bold{Reading shape file: dsn=<Shapefile Directory>  layer=<Shapefile base name>}\cr
       The restart logic in the checkpoint feature is loading the saved shape file from the 
       checkpoint folder <ShapFile Directory> and the layer is the <ShapeFile base name>.  These are
       the checkpoint folder locations not the original Shapefile directory and file parameters.
      }
      \item{3927}{\bold{Reading NameTable: <NT filename>}\cr
       The restart logic in the checkpoint feature is loading the checkpointed version
       of the name table (areaNamesAbbrsIDs) from the checkpoint folder.
      }
      \item{3928}{\bold{The Shapefile has been modified and the 'X__Key' variable has been removed.\cr
       Rerun the BuildBorderGroup function to restore the variable.\cr 
       Then do not remove the variable when editing the shape file.}\cr
       BuildBorderGroup places a couple of special variable in the shape file to allow the polygons to be 
       matched up with the areas in the name table.  It appears the 'X__Key' variable as been removed.
       Editing is permitted, but none of the 'X__' variables added by the BuildBorderGroup should be 
       removed.  Rerun BuildBorderGroup to restore the variables, do the required editing, and then 
       do a \option{checkPointReStart} = TRUE to build the micromapST boundary files.
      }
     }
    } 
    \item{3Axx}{\bold{Creating micromapST boundaries files}\cr
     \describe{
      \item{3A02}{\bold{The shape file variables have been editted. The 'X__Key' variables are 
       missing. Redo the edits and do not delete the 'X__Key' variable.}\cr
       During the editing of the Shape file, the 'X__Key' variable must be kept the same to allow the 
       Name Table to maintain a link between the area location ids and the associated polygons.
       Reedit the Shape File and ensure the X__Key variable is untouched.
      }
      \item{3A04}{\bold{The shape file row.names in the spatial structure do not match
       the 'X__Key' variable values. Investigate and correct cause. row.names are reset to the 
       'X__Key' values.}\cr
       The contents of the X__Key variable/column. If they are changed, the tie between the 
       Name Table and the spatial data is broken.  Reedit the Shape file and return the X__Key column
       to its original value written when the checkpoint file was created.
      }
      \item{3A22}{\bold{Invalid polygon found in <area key>  area #:<nz> id:<z>}\cr
       Message is issued by the BuildVisBorders function as it is process the spatial 
       data and converting it into the data.frame format for micromapST drawing. The message
       indicates an invalid polygon was identified for the <area key> at row number <nz> with the 
       id of <z>.  Identify the bad polygon and fix its geometry.
      }
      \item{3A28}{\bold{Completed conversion to VisBorders format.}\cr
       All of the boundary data for the individual areas, Layer 2 boundaries (if requested), regional
       layer boundaries (if requested), and the map outline have been converted to the data.frame 
       point format required by micromapST to draw the miniture maps.
      }
      \item{3A30}{\bold{Creating the 4 micromapST boundary layers (area, L2, L3, and Regions).}\cr
       The shape file will be copied to the area, L2, Regions, and L3 shape file images and 
       merge based on the spaces layed out in the name table for L2 spaces, Regional spaces, and the
       outline of the entire map (L3).
      }
      \item{3A40}{\bold{Completed conversion to VisBorders format.}\cr
       The conversion from spatial structure to micromapST's data.frame format has been completed.
      }
      \item{3A53}{\bold{Writing an images of each Border Group data.frame for <BGBase> }\cr
       A single R .rda file will be written for each of the 6 data.frames included in the 
       border group <BGBase>. The can be found in the border group directory under the 
       names of <BGBase>_<name of data.frame>.rda.
      }
      \item{3A55}{\bold{ Border Group Created - Successfully.}\cr
       The writing of the border group dataset has been successful.  The border group is now 
       ready for use.
      }
      \item{3A60}{\bold{Summary build report of names, abbr, id and other data is written 
       to <RepOutFileName> }\cr
       Each border group needs documentation to provide the user to let them know what the list of 
       Names, Abbrs, IDs, Aliases, and Alt_Abr location IDs are available in the Name Table for the 
       space mapped by border group.  A copy of the key information from the Name Table is printed for 
       use in this documentation in a text file in the border group directory under the name of 
       <RepOutFileName>.
      }
      \item{3A69}{\bold{Border Group: <finalBGroup> is done.}\cr
       The process of gathering the information, validating it, editing it, and converting 
       in to a format for micromapST has been completed and the dataset written to disk.
      }
      \item{3980}{\bold{The proj4 value character vector is not a valid projection. Must be a value 
       acceptable to st_crs. proj4 is ignored.}\cr
       The projection character string provided on the proj4 call parameter is not a valid.  
       It can not correctly processed the sf function st_crs to be used as a projection.
       Make it complient with PROJ4 speciications. Correct and re-run. (convertProj4)
      }
      \item{3985}{\bold{AdjPolygons - Polygons level value is not a Polygons structure.}\cr
       In processing the SpatialPolygonsDataFrame image of the shape file, the Polygons  
       level below the polygons level is not a valid SpatialPolygons structure and 
       can not be processed.  Execution is halted.   Review the shape file or 
       SpatialPolygonsDataFrame structure and make sure it is correct.
      }
      \item{3997}{\bold{The number of areas in the map is 1 or less. A border group can 
       not be made.}\cr
       The number of areas in the name table and the shapefile must be more than 1 polygon or area to 
       be able to build a border group.  Check the name table and shapefile contents.
      }
      \item{3999}{\bold{Errors have been found and noted above.  Execution stopped. 
       Please fix problem(s) and retry.}\cr
       Inspect the previous messages and errors to identify the cause of problems that stopped 
       the execution of the function.  Fix the problem and rerun.
      }
     }   
    }
    
   }  
  }
  \item{\bold{REPORT MESSAGES}}{
   \describe{
    \item{\bold{PUBLICATION INFORMATION FOR NAME TABLE IN BORDER GROUP}}{
     The border group has been created.  To make sure the user can use the same location ids that were
     used in the name table, the following table is printed to provide documentation on what 
     location IDs are available to the user when they are assemblying their data.  Each column present 
     in the name table is listed:  Name, Abbr, ID, Alias, Alt_Abbr.  These are the primary columns used by 
     the data gatherer.
    }
    \item{\bold{Name Table Regional Values}}{
     If only the regional space information is present in the name table, then 
     the regID and regName name columns will be displayed in the report for reference.
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
     If only the Layer 2 information is present in the name table, then the L2_ID and 
     L2_ID_Names name table columns will be displayed in thei report for reference.
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

