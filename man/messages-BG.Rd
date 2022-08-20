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
  
  The following values are also substituted with the real run values
  at time of execution:
  \describe{
    \item{<saaaaal>}{To be defined later.
     }
    \item{<Paaaae>}{To be defined later.
     }
   }
  
  The following is a listing of all micromapST generated messages and a 
  description of possible causes and solutions.
  
 \describe{
   \item{3xxx}{\bold{BuildBorderGroup Messages:} 
    \describe{
     \item{312x}{\bold{checkPointRestart}
      \describe{
        \item{3120}{\bold{The checkPointReStart call parameter is not a logical value.}
 
          *** More detail to be added ***
         }
        \item{3122}{\bold{Check Point Restart has been requested.  Check point files\cr
          will be read from folder: <name table directory>/Checkpoint directory.}
 
         *** More detail to be added ***
         }
       }
      }
     \item{313x}{\bold{General Call Parameter}
      \describe{
        \item{3130}{\bold{Require call parameters are missing : <list of parameters> - execution stopped.}
       
         *** More details to be added ***
         }
       }
      } 
     \item{315x}{\bold{BorderGroup Files}
      \describe{
       \item{3150}{\bold{BorderGroup directory specified in the call parameter does not exist. Value= <BorderGroupName>.}

        *** More detail to be added ***
        }
       \item{3154}{\bold{BorderGroupName is not a character string. Value= <BorderGroupName>.}
 
        *** More detail to be added ***
        }
       }
      }
   
     \item{320x}{\bold{NameTable Files}
      \describe{
       \item{3202}{\bold{NameTable directory specified in the NameTableDir call\cr
                  parameter does not exist. Value= <NameTableDir>}

        *** More detail to be added ***
        }
       \item{3204}{\bold{NameTableFile parameter has not been provided. Value= <NameTableFile>}

        *** More detail to be added ***
        }
       \item{3205}{\bold{File extension on name table filename is missing - .csv added.}

        *** More detail to be added ***
        }
       \item{3206}{\bold{The NameTable file is not a .csv, Excel, or R .RDA format.}
 
        *** More detail to be added ***
        }
       \item{3208}{\bold{The NameTableLink call parameter is not a character string. Fix and rerun.}

        *** More detail to be added ***
        }
       }
      }
    \item{321x and 322x}{\bold{Shape File and Link}
     \describe{
      \item{3212}{\bold{Shape File parameter has not been provided.}

       *** More detail to be added ***
       }
      \item{3213}{\bold{The Shape File call parameter is not a SPDF or character variable.}
 
       *** More detail to be added ***
       }
      \item{3214}{\bold{Shape file directory specified in the ShapeFileDir call parameter does not exist. Value= <ShapeFileDir>}
 
       *** More detail to be added ***
       }
      \item{3216}{\bold{Shape file (dir & name) does not exist. Value= <xS>}

       *** More detail to be added ***
       }
      \item{3220}{\bold{The ShapeLinkName call parameter is missing. The default value of 'NAME' will be used.}

       *** More detail to be added ***
       }
      \item{3221}{\bold{ShapeLinkName is not a character string. Value= <ShapeLinkName>.}

       *** More detail to be added ***
       }
     }
    } 
    \item{323x & 324x}{\bold{Map Headers and parameters}
     \describe{
      \item{3230}{\bold{The MapHdr parameter does not contain character }

       *** More detail to be added ***
       }
      \item{3232}{\bold{The MapHdr parameter must be a simple vector type. }

       *** More detail to be added ***
       }
      \item{3234}{\bold{The MapHdr parameter has zero or more than 2 elements. Only the first 2 will be used.}

       *** More detail to be added ***
       }
      \item{3236}{\bold{It is suggested the max length of the MapHdr strings be }

       *** More detail to be added ***
       }
      \item{3242}{\bold{The MapMinH parameter does not contain numeric value. Default Value is used.}
  
       *** More detail to be added ***
       }
      \item{3244}{\bold{The MapMinH minimum height value is out of range (0.4 to 2 inch). The default will be used.}

       *** More detail to be added ***
       }
      \item{3246}{\bold{The MapMaxH parameter does not contain numeric value. Default Value is used.}

       *** More detail to be added ***
       }
      \item{3248}{\bold{The MapMaxH maximum height value is out of range (0.4 to 2 inches). The default will be used.}

       *** More detail to be added ***
       }
      \item{3249}{\bold{The MapMinH value must be less than the MapMaxH value. Will swap values.}

       *** More detail to be added ***
       }
     }
    } 
    \item{325x}{\bold{ID Header and Parameters}
     \describe{
      \item{3250}{\bold{The IDHdr parameter does not contain character strings for use }
 
       *** More detail to be added ***
       }
      \item{3252}{\bold{The IDHdr parameter must be a simple vector type. The MapHdr values will be used.}

       *** More detail to be added ***
       }
      \item{3254}{\bold{The IDHdr parameter has more than 2 elements. Only the first 2 will be used.}

       *** More detail to be added ***
       }
      \item{3256}{\bold{It is suggested the max length of the IDHdr strings be 12 characters.} 

       *** More detail to be added ***
       }
      }  
     }
     \item{326x}{\bold{Reduction Percentage Parameter}
      \describe{
       \item{3266}{\bold{The ReducePC parameter must be a numeric value. The default of 10\% will be used.}

        *** More detail to be added ***
        }
       \item{3267}{\bold{The ReducePC parameter must be a simple vector. The default value of 10\% will be used.}

        *** More detail to be added ***
        }
       \item{3268}{\bold{The ReducePC parameter has more than one value. Only the first value will be used.}

        *** More detail to be added ***
        }
       \item{3269}{\bold{The value of ReducePC is out of range (0.75 to 95 percent). The default value of 10\% will be used.}

        *** More detail to be added ***
        }
      }
     }
     \item{327x}{\bold{proj4 Parameter}
      \describe{
       \item{3272}{\bold{Error processing proj4 parameter. The provided proj4 call parameter will be ignored.}

        *** More detail to be added ***
        }
       \item{3274}{\bold{The proj4 call parameter does not have +units=m, changing string to meters.}
 
        *** More detail to be added ***
        }
       \item{3276}{\bold{The proj4 call parameter specifies a long/lat projection.\cr
                            proj4: <proj4>\cr
                            The final projection can't be a longlat projection.\cr
                            The proj4 parameter is ignored and a AEA projection will be created.}

        *** More detail to be added ***
       }
      }
     }
     \item{329x}{\bold{LabelCex Call Parameter}
      \describe{
       \item{3296}{\bold{The LabelCex parameter must be a numeric value. The default of 0.25 will be used.}

        *** More detail to be added ***
        }
      \item{3297}{\bold{ The LabelCex parameter must be a simple vector. The default value of 0.25 will be used.}

        *** More detail to be added ***
        }
      \item{3298}{\bold{The LabelCex parameter has more than one value. Only the first value will be used.}

        *** More detail to be added ***
        }
      \item{3299}{\bold{The value of LabelCex is out of range (0.05 to 10). The default value of 0.25 will be used.}

        *** More detail to be added ***
        }
      }
     }
     \item{330x}{\bold{Shape File Processing and Call Parameter}
      \describe{
       \item{3300}{\bold{The Shape file SPDF strucure was passed to the function in the call.}

        *** More detail to be added ***
        }
       \item{3310}{\bold{The proj4string field in the Shape File is empty, set to <OrigLongLat>}

        *** More detail to be added ***
        }
       \item{3324}{\bold{The ShapeLinkName provided: <ShapeLinkName> does not exist in the shapefile data.}

        *** More detail to be added ***
        }
      }
     }
     \item{351x to 355x}{\bold{Name Table General Issues}
      \describe{
       \item{3512}{\bold{The NameTable in the .rda file is not a data.frame. Please correct and retry.}
 
        *** More detail to be added ***
        }
       \item{3514}{\bold{There are more than one data.frame in the .rda file provided for the NameTable. 
        Provide only one data.frame table in the .rda and retry.}
 
        *** More detail to be added ***
        }
       \item{3521}{\bold{The Name Table has no columns of data.}
 
        *** More detail to be added ***
        }
       \item{3522}{\bold{The Name Table has no rows or areas.}
 
        *** More detail to be added ***
        }
       \item{3532}{\bold{The column specified in the NameTableLink calling parameter 
              does not exist in the loaded Name Table.}
 
        *** More detail to be added ***
        }
       \item{3542}{\bold{At least one of the following columns must be present in the 
         NameTable file: <OneCol>  \cr Please correct the spreadsheet and try again.}

        *** More detail to be added ***
        }
       \item{3550}{\bold{The following columns are not needed and will be deleted from the Name Table:}

        *** More detail to be added ***
        }
       \item{3558}{\bold{None of the columns needed are present. The Link and one of the Name, Abbr, 
           and ID column should have been there. This should never happen withthe previous checks.}

        *** More detail to be added ***
        }
      }
     }
     \item{356x, 357x, 358x}{\bold{Name Table Name, Abbr, ID, Alias and Alt_Abr data errors}
      \describe{
       \item{3562}{\bold{The <inx> column contains duplicate entries. Correct and retry.}

        *** More detail to be added ***
        }
       \item{3564}{\bold{The <inx> column contains NA or blank values that are not allowed.}

        *** More detail to be added ***
        }
       \item{3571}{\bold{The Addr column in the Name Table is not included. Will attempt to backfill 
          it from other information.}

       *** More detail to be added ***
       }
      \item{3573}{\bold{The Name Table Abbr field is persent - no backfill required.}

       *** More detail to be added ***
       }
      \item{3574}{\bold{Some of the Name Table Abbr values are longer than 5 characters. It is 
         recommended the Abbr values be keep short.}
 
       *** More detail to be added ***
       }
      \item{3576}{\bold{None of the columns needed are present. The Link and one of the Name, Abbr, 
        and ID column should have been there.  This should never happen withthe previous checks.}

       *** More detail to be added ***
       }
      \item{3582}{\bold{Checking ID column in the name table to make sure the values.}

       *** More detail to be added ***
       }
      \item{3584}{\bold{The ID column is not present. A numerical sequence number has been used to fill the column.}

       *** More detail to be added ***
       }
      \item{3586}{\bold{The ID data column is not all.}

       *** More detail to be added ***
       }
      \item{3592}{\bold{The Name Table in the <inxRN> area row and in the <inx> column \cr
        is not numeric and has a bad value of: <WrkVal> Value set to zero. Fix and retry.}

       *** More detail to be added ***
       }
      \item{3594}{\bold{Data in row: <inxRN> for <inx> parameter <WrkVal2> is out of range. (<low> to <high>)}

       *** More detail to be added ***
       }
      }
     }
     \item{363x}{\bold{Map Labels and Coordinates}
      \describe{
       \item{3630}{\bold{A value in MapL is not a character variable in the following rows:\cr
               <NameTableRows>\cr  Please correct and rerun.}

        *** More detail to be added ***
        }
       \item{3631}{\bold{If the MapL column is present with a label, then MapX and MapY must be 
            present. Correct and rerun,}

        *** More detail to be added ***
        }
       \item{3632}{\bold{MapL is present and there are no MapX coordinates. Correct and rerun.}

        *** More detail to be added ***
        }
       \item{3633}{\bold{MapL is present and there are no MapY coordinates. Correct and rerun.}

        *** More detail to be added ***
        }
       \item{3634}{\bold{The MapL label for <NTN> should be 3 or less characters.  Please correct and rerun.}

        *** More detail to be added ***
        }
       \item{3635}{\bold{No MapLabel content - processing skipped.}
 
        *** More detail to be added ***
        }
       \item{3636}{\bold{Some of the items in the MapLabel entry for <inxRN> are NA or blanks. Will be ignored.}
 
        *** More detail to be added ***
        }
       \item{3637}{\bold{The MapLabel value for <inxRN> is not valid. Must be a character string 
          with three values separated by commas. The value is ignored.}
 
        *** More detail to be added ***
        }
       \item{3638}{\bold{The label value in the MapLabel entry for <inxRN> is > 3 char.  
           Only first 2 characters used.}

        *** More detail to be added ***
        }
       \item{3639}{\bold{One of the coordinates in the MapLabel entry for <inxRN> is not a number. 
          <xMapX> or <xMapY>}

        *** More detail to be added ***
        }
       \item{3640}{\bold{One of the MapLabel coordinates for <inxRN> are out of range. Entry ignored.}               

        *** More detail to be added ***
        }
      }
     }
     \item{371x}{\bold{Proj4 projection processing}
      \describe{
       \item{3711}{\bold{The protection provided in the shape files does not have +units=m. Will modify 
            and setup for re-projection to change to meters.}

       The BuildBorderGroup function depends on the projection units being meters.  Therefore,
       the projection found in the shape file will be modified to "+units=m" and the projection
       will be transformed at the end of processing.
       }
      \item{3712}{\bold{Found +units=m in proj4string of non-longlat projection in the shape file.}

       There will not be any re-projection of the map done later.  Unless the proj4 call parameter
       is present.
       }
      \item{3718}{\bold{The proj4 value character vector.  Must be a valid proj4 argument 
            character string to be converted.}

       In the convertPROJ4 subfunction, the proj4 value passed to the function is not a valid
       character string.  Subfunction fails.  The subfunction is called when the proj4 call parameter
       is validated.
       }
      }
     } 
     \item{372x & CleanGeo}{\bold{Boundary data clean up}
      \describe{
       \item{3720}{\bold{Cleaning up geometry in shape file using cleangeo inspect and correction.}

        *** More detail to be added ***
        }
       \item{cleanGeo1}{\bold{Unknown advanced cleaning method. Accepted values: 'POLYGONATION', 'BUFFER'}

        Internal Error, POLYGONATION method always selected.
        }
       \item{cleanGeo2}{\bold{Cleaning orphaned holes at index <s>, <x>}

        Progress report from CleanGeo on orphaned holes in the spatial structure.  No action should be required.
        }
       \item{cleanGeo3}{\bold{Checking geometry validity at index <s>, <x>}

        Progress message from CleanGeo acknowledging valid geometry with in the structure. No action required.
       }
      \item{cleanGeo4}{\bold{Catched MAPTOOLS warning <s>, <msg>}

       CleanGeo monitor calls to maptools functions. If a warning is returned, it is documented.  Review
       the warning in the maptools documentation and correct is possible.
       }
      \item{cleanGeo5}{\bold{Catched MAPTOOLS error <s>, <msg>}

       CleanGeo monitor calls to maptools functions. If a error is returned, it is documented.  Review
       the warning in the maptools documentation and correct is possible.
       }
      \item{cleanGeo6}{\bold{Cleaning geometry at index <s>, <x>, report.msg}

       CleanGeo reports on the progress of the cleaning effort.  More details can be found
       in the report.msg file.
       }
      \item{cleanGeo7}{\bold{Removing false polygon at index <s>, <x>}

       CleanGeo has removed a false polygon in the spatial structure and is continuing processing.
       }
      }
     }
     \item{375x}{\bold{Matching Shape File to Name Table}
      \describe{
       \item{3752}{\bold{The following Shape File areas are not in Name Table:\cr
                 <List of Shape File areas not in Name Table>\cr
               The areas will be dropped.\cr}

        Any area (polygon) in the shape file that does not have a row in the Name Table, will 
        be dropped from the final border group map.
        }
       \item{3754}{\bold{Deleting the following polygons from the shape file:\cr
                      <List of areas to be deleted>}
                      
        This message reports the list of shape file areas to be deleted.
        }
      }
     }
     \item{376x}{\bold{Linking Name Table to Shape File}
      \describe{
       \item{3760}{\bold{Comparing the link values to tie the Name Table to the shape file.}

        *** More detail to be added ***
        }
       \item{3762}{\bold{The following Name Table areas do not have boundaries in the ShapeFile:\cr
		<List of Name Table Rows without polygons>\cr
                Correct and retry.}

        The BuildBorderGroup function can not continue if there is not an area(set of polygons) in 
        the shape file for the Name Table row.  Execution stopped.
        }
       \item{3868}{\bold{The length of Name Table and the number of areas in the shape file are different.}

         Execution Stopped.  At this point in the processing previous messages would have stopped or modified
         the flow.  If the sizes of Name Table and Shape File are different at this time, it's a 
         programming error.
        }
      }
     }
     \item{38xx}{\bold{Final processing and transformation}
      \describe{
       \item{3823}{\bold{The following areas may be too small (<-0.03\%):
        <Names of areas the may be too small>}

        If an area is too small, the user may not be able to see the shading in the 
        area and can't link the geographic location to the data displayed in the 
        other glyphs.
       }
      \item{3840}{\bold{Info:No modifications are required to map.}

       *** More detail to be added ***
       }
      \item{3866}{\bold{Info:No transformation was done to the Map Label points.}

       *** More detail to be added ***
       }
      \item{3867}{\bold{Info:Transform Map Label points.}

       *** More detail to be added ***
       }
      \item{3868}{\bold{Info:Transformation of MapLabel completed.}

       *** More detail to be added ***
       }
      }
     }
     \item{39xx}{\bold{checkPointRestart and Wrapup}
      \describe{
       \item{3914}{\bold{The check point Shape File for the border group is saved to
                 <CkptPath> directory.}

        When a check point is done, images of the name table and shape file are written to disk into
        the directory indicated by <CkptPath>.
        }
       \item{3914}{\bold{BuildBorderGroup has completed write the check point files for possible editing: 
        <Check Point Directory>}

        The border group dataset has been successfully written to directory included in this message and is 
        ready for use, reuse, or editing.
        }
       \item{3915}{\bold{The check point Shape File for the border group is saved to: <Shape File Name>}
       
        This message identifies the filename of the saved Check Point Shape File being used to build this
        border group.  It can be editted, but must be returned to the same directory with the same 
        name for the "checkPointRestart" logic to restart the border group building process.
        }
       \item{3916}{\bold{After editing, the results must be save back to the same directory and filename.}

        The check point shape file can be manually edited to make areas more visible in the small micromap. 
        Once the modifications are completed by any external GIS or polygon editor, the final shape file
        must be save in it's original directory and under the original filename.  If you want to save 
        a copy of the shape file producted by the BuildBorderGroup function, save it before making modifications.
        }
        \item{3920}{\bold{No Border Group or Name Table directory provides. Cannot find restart files. STOP.}
      
        *** More detail to be added ***
        }
       \item{3925}{\bold{AdjPolygons - wPolygons value is not a 'Polygons' structure.}

        *** More detail to be added ***
        }
       \item{3970}{\bold{Generate scaled example of test maps - Before Conversion.}

        *** More detail to be added ***
        }
       \item{3999}{\bold{Errors have been found and noted.  Execution must be stop. Fix problems and retry.}

        *** More detail to be added ***
        }
      }
     }
   }
  }
 } 
}  
\keyword{messages}
\keyword{datasets}

