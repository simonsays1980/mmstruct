## Copyright (C) 2013 Lars Simon Zehnder
#
# This file is part of mmstruct.
#
# mmstruct is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# mmstruct is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mmstruct. If not, see <http://www.gnu.org/licenses/>.

### ========================================================================
### The mmTRACE Class
### ------------------------------------------------------------------------
.mmTRACE  <- setClass("mmTRACE",
                      representation(.Start     = "POSIXct",
                                     .End       = "POSIXct",
                                     .NSEC      = "integer",
                                     .NCOMP     = "integer",
                                     .Enhanced  = "logical",
                                     .Cleaned   = "logical"
                                     ),
                      contains  = c("mmDATASTRUCT"),
                      prototype(.Start      = as.POSIXct(Sys.time()),
                                .End        = as.POSIXct(Sys.time()),
                                .NSEC       = as.integer(0),
                                .NCOMP      = as.integer(0),
                                .Enhanced   = as.logical(FALSE),
                                .Cleaned    = as.logical(FALSE)
                                )
)

### -------------------------------------------------------------------------
### Constructors
### -------------------------------------------------------------------------
"mmTRACE" <- function(path, ...) 
{
    dtrace <- fread(path, ...)
    .mmTRACE(.Table = dtrace)
}

## --------------------------------------------------------------------------
## Column names are already in standard 'mmstruct' format from wrds.
"mmTRACE.default" <- function(path, format = "YYYYmmdd", ...)
{
    timer       <- proc.time()
    options(warn = -1)
    cat("Read data ...")
    dtrace      <- fread(path, ...)
    cat("done\n")
    cat("Read ", nrow(dtrace), " rows and ", ncol(dtrace), " columns\n")
    cat("Time elapsed for reading: ")
    elapsed     <- proc.time() - timer
    elapsed     <- format(elapsed, digits = 2)
    cat(elapsed[[3]], "sec\n")
    timer.prepare   <- proc.time()
    options(warn = 0)
    cat("Prepare data.table ...")
    keep        <- .check.mmTRACE(names(dtrace))
    if (sum(keep, na.rm = TRUE) < length(keep)) {
        dtrace  <- dtrace[,which(keep == FALSE) := NULL]
    }
    dtrace      <- dtrace[, TM_STMP := mmstruct.dtformat(TRD_EXCTN_DT, TRD_EXCTN_TM, 
                                                         FinCenter = "America/New_York",
                                                         format = "YYYYmmdd",
                                                         type = "any2gmt")]                                     
    if ("ASCII_RPTD_VOL_TX" %in% names(dtrace)) {
        setkey(dtrace, ASCII_RPTD_VOL_TX)
        dtrace  <- dtrace[ASCII_RPTD_VOL_TX == "5MM+", ASCII_RPTD_VOL_TX := "5e+6"]
        dtrace  <- dtrace[ASCII_RPTD_VOL_TX == "1MM+", ASCII_RPTD_VOL_TX := "1e+6"]
        dtrace  <- dtrace[, ASCII_RPTD_VOL_TX := as.integer(ASCII_RPTD_VOL_TX)]
    }
    start.dt    <- dtrace[, min(TM_STMP, na.rm = TRUE)]
    end.dt      <- dtrace[, max(TM_STMP, na.rm = TRUE)]
    if ("BOND_SYM_ID" %in% names(dtrace)) {
        nsec    <- dtrace[, NROW(unique(BOND_SYM_ID))]
    } else {
        if ("CUSIP_ID" %in% names(dtrace)) {
            nsec    <- dtrace[, NROW(unique(CUSIP_ID))]
        } else {            
            nsec    <- as.integer(1)
        }
    }
    if ("COMPANY_SYMBOL" %in% names(dtrace)) {
        ncomp   <- dtrace[, NROW(unique(COMPANY_SYMBOL))]
    } else {
        ncomp   <- as.integer(1)
    }
    .nrows      <- dim(dtrace)[1]
    .ncols      <- dim(dtrace)[2]
    .Enhanced   <- "TRD_RPT_DT" %in% names(dtrace)
    cat("done\n")
    cat("Time elapsed for preparation: ")
    elapsed     <- proc.time() - timer.prepare
    elapsed     <- format(elapsed, digits = 2)
    cat(elapsed[[3]], "sec\n")
    cat("Total time elapsed: ")
    elapsed     <- proc.time() - timer
    elapsed     <- format(elapsed, digits = 2)
    cat(elapsed[[3]], "sec\n")
    cat("Creating 'mmTRACE' object ...")
    cat("done\n")
    .mmTRACE(.FinCenter = "America/New_York", .NROWS = .nrows, .NCOLS = .ncols, 
             .Table = dtrace, .Start = start.dt, .End = end.dt,.NSEC = nsec, 
             .NCOMP = ncomp, .Enhanced = .Enhanced, .Cleaned = FALSE)
}
    
### -------------------------------------------------------------------------
### Getters and Setters
### -------------------------------------------------------------------------
setMethod("getStart", "mmTRACE", 
          function(object)
          {
              return(object@.Start)
          }
)
## -
## No Setter as this slot should not be manipulated by users.
## -

setMethod("getEnd", "mmTRACE", 
          function(object)
          {
              return(object@.End)
          }
)
## -
## No Setter as this slot should not be manipulated by users.
## -

setMethod("getNSEC", "mmTRACE", 
          function(object)
          {
              return(object@.NSEC)
          }
)
## -
## No Setter as this slot should not be manipulated by users.
## -

setMethod("getNCOMP", "mmTRACE",
          function(object)
          {
              return(object@.NCOMP)
          }
)

setMethod("isEnhanced", "mmTRACE",
          function(object, verbose = FALSE) {
              if ( hasTRD_RPT_DT( object ) ) {
                  return(object@.Enhanced)
              } else {
                  if ( verbose ) {
                      stop( paste( "'mmTRACE' object does not carry ",
                                  "data from the Enhanced TRACE dataset.",
                                  sep = "" ) )
                  } else {
                      return( object@.Enhanced )
                  }
              }
          }
)

## -
## No Setter as this slot should not be manipulated by users.
## -

setMethod("isCleaned", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ( object@.Cleaned ) {
                  return( object@.Cleaned )
              } else {
                  if ( verbose ) {
                      stop( paste("'mmTRACE' object is not cleaned ",
                                  "up.", sep = "" ) )
                  } else {
                      return( object@.Cleaned )
                  }
              }

          }
)

setReplaceMethod("setCleaned", "mmTRACE",
          function(object, value)
          {
              object@.Cleaned <- value
              return(object)
          }
)

### ------------------------------------------------------------------------
### Show
### ------------------------------------------------------------------------
setMethod("show", "mmTRACE", 
          function(object) 
          {
              name <- ifelse(length(object@.Name) > 0, object@.Name, 'mmTRACE')
              cat(paste("Object '", name, "'\n", sep = ""))
              cat("     class       :", class(object), "\n")
              cat("     .Created    :", format(object@.Created, usetz = TRUE), "\n")
              cat("     .Modified   :", format(object@.Modified, usetz = TRUE), "\n")
              cat("     .Enhanced   :", object@.Enhanced, "\n")
              cat("     .Cleaned    :", object@.Cleaned, "\n")
              cat("     .NROWS      :", object@.NROWS, "\n")
              cat("     .NCOLS      :", object@.NCOLS, "\n")
              cat("     .Table      : Object of class", 
                  class(object@.Table)[1], "\n")
              cat("     .Start      :", format(object@.Start, usetz = TRUE), "\n")
              cat("     .End        :", format(object@.End, usetz = TRUE), "\n")
              cat("     .FinCenter  :", format(object@.FinCenter), "\n")
              cat("     .NSEC       :", object@.NSEC, "\n")
              cat("     .NCOMP      :", object@.NCOMP, "\n")
          }
)

### ------------------------------------------------------------------------
### Has
### ------------------------------------------------------------------------
setMethod("hasBOND_SYM_ID", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("BOND_SYM_ID" %in% names(object@.Table)) {
                  return(TRUE)                  
              } else {
                  if (verbose) {
                      stop(paste("No BOND SYMBOL security identifier: ",
                                 "'mmTRACE' object has no ",
                                 "column named 'BOND_SYM_ID'.",
                                 sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasCUSIP_ID", "mmTRACE", 
          function(object, verbose = FALSE)
          {
              if ("CUSIP_ID" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No CUSIP security identifier: ",
                                 "'mmTRACE' object has no column ",
                                 "named 'CUSIP_ID'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasCOMPANY_SYMBOL", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("COMPANY_SYMBOL" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No COMPANY identifier: ",
                                 "'mmTRACE' object has no column ",
                                 "named 'COMPANY_SYMBOL", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)
setMethod("hasTRD_EXCTN_DT", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRD_EXCTN_DT" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No trade execution date: ",
                                 "'mmTrace' object has no column named ",
                                 "'TRD_EXCTN_DT'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasTRD_EXCTN_TM", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRD_EXCTN_TM" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No trade execution time: ",
                                 "'mmTRACE' object has no column named ",
                                 "'TRD_EXCTN_TM'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasMSG_SEQ_NB", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("MSG_SEQ_NB" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No message sequence number: ",
                                 "'mmTRACE' object has no column named ",
                                 "'MSG_SEQ_NB'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasORIG_MSG_SEQ_NB", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("ORIG_MSG_SEQ_NB" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No original message sequence number: ",
                                 "'mmTRACE' object has no column named ",
                                 "'ORIG_MSG_SEQ_NB'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasTRC_ST", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRC_ST" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No trade status: ",
                                 "'mmTRACE' object has no column named ",
                                 "'TRC_ST'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasASOF_CD", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("ASOF_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No as-of trade identifier: ",
                                 "'mmTRACE' object has no column named ",
                                 "'ASOF_CD'.", sep = ""))
                  }
              }              
          }
)

setMethod("hasSALE_CNDTN_CD", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("SALE_CNDTN_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No sale condition: ",
                                 "'mmTRACE' object has no column named ",
                                 "'SALE_CNDTN_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasSALE_CNDTN2_CD", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("SALE_CNDTN2_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No second modifier: ",
                                 "'mmTRACE' object has no column named ",
                                 "'SALE_CNDTN2_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasAGU_QSR_ID", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("AGU_QSR_ID" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No give-up flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'AGU_QSR_ID'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasBUY_CPCTY_CD", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("BUY_CPCTY_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No buy capacity: ",
                                 "'mmTRACE' object has no column named ",
                                 "'BUY_CPCTY_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasCMSN_TRD", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("CMSN_TRD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No commission trade flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'CMSN_TRD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasCNTRA_MP_ID", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("CNTRA_MP_ID" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No contra-party identifier: ",
                                 "'mmTRACE' object has no column named ",
                                 "'CNTRA_MP_ID'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasDAYS_TO_STTL_CT", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("DAYS_TO_STTL_CT" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No days to settlement variable: ",
                                 "'mmTRACE' object has no column named ",
                                 "'DAYS_TO_STTL_DT'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasDISSEM_FL", "mmTRACE",
          function(object, verbose = FALSE) 
          {
              if ("DISSEM_FL" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No dissemination flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'DISSEM_FL'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasENTRD_VOL_QT", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("ENTRD_VOL_QT" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No volume variable: ",
                                 "'mmTRACE' object has no column named ",
                                 "'ENTRD_VOL_QT'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasRPTD_PR", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("RPTD_PR" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No reported price: ",
                                 "'mmTRACE' object has no column named ",
                                 "'RPTD_PR'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasRPTD_SIDE_CD", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("RPT_SIDE_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No buy/sell flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'RPT_SIDE_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasSCRTY_TYPE_CD", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("SCRTY_TYPE_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No security type identifier: ",
                                 "'mmTRACE' object has no column named ",
                                 "'SCRTY_TYPE_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasSELL_CPCTY_CD", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("SELL_CPCTY_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No sell capacity: ",
                                 "'mmTRACE' object has no column named ",
                                 "'SELL_CPCTY_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasSPCL_TRD_FL", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("SPCL_TRD_FL" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No special trade flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'SPCL_TRD_FL'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasTRDG_MKT_CD", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRDG_MKT_CD" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No market identifier: ",
                                 "'mmTRACE' object has no column named ",
                                 "'TRDG_MKT_CD'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasTRD_RPT_DT", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRD_RPT_DT" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No trade report date: ",
                                 "'mmTRACE' object has no column named ",
                                 "'TRD_RPT_DT'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasTRD_RPT_TM", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("TRD_RPT_TM" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No trade report time: ",
                                 "'mmTRACE' object has no column named ",
                                 "'TRD_RPT_TM'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)

setMethod("hasWIS_FL", "mmTRACE",
          function(object, verbose = FALSE)
          {
              if ("WIS_FL" %in% names(object@.Table)) {
                  return(TRUE)
              } else {
                  if (verbose) {
                      stop(paste("No when-issued flag: ",
                                 "'mmTRACE' object has no column named ",
                                 "'WIS_FL'.", sep = ""))
                  } else {
                      return(FALSE)
                  }
              }
          }
)


### ------------------------------------------------------------------------
### Cleanup
### ------------------------------------------------------------------------
# - Amihud price impact measure and turnover measure ot on the same data
# - Changes to Variable Definition Effective February 6, 2012:
#       (Report. Cancellation, Correction) -> (T, C, W) -> (G, H, I)
setMethod("cleanUp", "mmTRACE",
         function(object, sec = NULL, comp = NULL, excl.after = FALSE, 
                  stats = FALSE, stats.ctrl = c("comp", "sec"))                   
         {
             stats.ctrl <- match.arg(stats.ctrl)
             .check.args.cleanUp.mmTRACE(object, sec, comp, excl.after, 
                                         stats.ctrl)
             sec            <- unlist(unique(as.character(sec)))
             comp           <- unlist(unique(as.character(comp)))
             if (hasBOND_SYM_ID(object)) {
                 .raw.cleanUp.BONDSYMID.mmTRACE(object, sec, comp, excl.after, 
                                                stats, stats.ctrl)
             } else if (hasCUSIP_ID(object)) {
                 .raw.cleanUp.CUSIPID.mmTRACE(object, sec, comp, excl.after, 
                                              stats, stats.ctrl)
             } else {
                 stop(paste("No security identifier: 'mmTrace' object ",
                            "needs either BOND_SYM_ID or CUSIP_ID to ",
                            "identifiy securities.", sep = ""))
             }
         }
)

setMethod("cleanUpEnhanced", "mmTRACE",
          function(object, sec = NULL, comp = NULL, excl.ID.bydate = TRUE,
                   excl.ID.allbuy = TRUE, excl.XD = TRUE, 
                   excl.further = list( excl.whenissued = TRUE,
                                       excl.nonsecondary = TRUE,
                                       excl.special = TRUE,
                                       excl.equitylinked = TRUE,
                                       excl.nssettlement = TRUE,
                                       excl.nocash = TRUE,
                                       excl.commission = TRUE,
                                       excl.giveup = TRUE ),
                   stats = FALSE, stats.ctrl = c("comp", "sec") )
          {
              stats.ctrl <- match.arg(stats.ctrl)
              ## TODO check Args
              sec       <- unlist( unique( as.character(sec) ) )
              comp      <- unlist( unique( as.character(comp) ) )
              isEnhanced(object, TRUE)
              if ( hasBOND_SYM_ID( object ) ) {
                  .raw.cleanUp.enhanced.BONDSYMID.mmTRACE( object, sec, comp, excl.ID.bydate, 
                                                           excl.ID.allbuy, excl.XD, excl.further,
                                                           stats, stats.ctrl )                                                                                                                  
              } else if ( hasCUSIP_ID( object )) {

              } else {
                  stop(paste("No security identifier: 'mmTrace' object ",
                            "needs either BOND_SYM_ID or CUSIP_ID to ",
                            "identifiy securities.", sep = ""))
              }                 
          }
)

### ------------------------------------------------------------------------
### Get company list
### ------------------------------------------------------------------------
setMethod("getCompanies", "mmTRACE",
          function(object) 
          {
              if ("COMPANY_SYMBOL" %in% names(object@.Table)) {
                  comp.list <- object@.Table[, list(unique(COMPANY_SYMBOL))]
                  setnames(comp.list, names(comp.list)[1], "mmTRACE: Companies")
                  return(comp.list)
              } else {
                  stop(paste("Slot @.Table does not contain a column named ",
                             "'COMPANY_SYMBOL'."))
              }
          }
)

### ------------------------------------------------------------------------
### Get security list 
### ------------------------------------------------------------------------
setMethod("getSecurities", "mmTRACE", 
          function(object, addComp = TRUE)
          {
              if (addComp && "COMPANY_SYMBOL" %in% names(object@.Table)) {
                  if ("BOND_SYM_ID" %in% names(object@.Table)) {
                      sec.list  <- object@.Table[, unique(BOND_SYM_ID),
                                                 by = COMPANY_SYMBOL]
                      setnames(sec.list, names(sec.list)[2], 
                               "mmTRACE: Securities (BOND_SYM_ID)")
                  } else if ("CUSIP_ID" %in% names(object@.Table)) {
                      sec.list  <- object@.Table[, unique(CUSIP_ID),
                                                 by = COMPANY_SYMBOL]
                      setnames(sec.list, names(sec.list)[2], 
                               "mmTRACE: Securities (CUSIP_ID)")
                  } else {
                      stop(paste("Slot @.Table does neither contain a column ",
                                  "named 'BOND_SYM_ID' nor a column named ",
                                 "'CUSIP_ID'.", sep = ""))
                  }
              } else {
                  if ("BOND_SYM_ID" %in% names(object@.Table)) {
                      sec.list  <- object@.Table[, list(unique(BOND_SYM_ID))]
                      setnames(sec.list, names(sec.list)[1], 
                               "mmTRACE: Securities (BOND_SYM_ID)")
                  } else if ("CUSIP_ID" %in% names(object@.Table)) {
                      sec.list  <- object@.Table[, list(unique(CUSIP_ID))]
                      setnames(sec.list, names(sec.list)[1], 
                               "mmTRACE: Securities (CUSIP_ID)")
                  } else {
                      stop(paste("Slot @.Table does neither contain a column named ",
                                 "'BOND_SYM_ID' nor a column named 'CUSIP_ID'.",
                                 sep = ""))
                  }
              }
              return(sec.list)
          }
)

### ------------------------------------------------------------------------
### Get TRACE data summary
### ------------------------------------------------------------------------
setMethod("getVolumeSummary", "mmTRACE", 
          function(object, from, to, by = c("sec", "comp"), aggregate,
                   crit = c("SUM", "MEAN", "MEDIAN", "STD", "MAX", "MIN"),
                   quantile, quantile.crit = c("SUM", "MEAN", "MEDIAN", 
                                               "STD", "MAX", "MIN"))
          {
              ## check from, to
              if (missing(from)) {
                  from  <- object@.Start
              } else {
                  if (!inherits(from, "POSIXct")) {
                      stop(paste("Unknown argument: 'from' and 'to' ",
                                 "have to be of class 'POSIXct'. See ",
                                 "?as.POSIXct for how to create 'POSIXct' ", 
                                 "objects.", sep = ""))                  
                  }
              }
              if (missing(to)) {
                  to  <- object@.Start
              } else {
                  if (!inherits(from, "POSIXct")) {
                      stop(paste("Unknown argument: 'from' and 'to' ",
                                 "have to be of class 'POSIXct'. See ",
                                 "?as.POSIXct for how to create 'POSIXct' ", 
                                 "objects.", sep = ""))                  
                  }
              }
            
              ## match by argument
              by <- match.arg(by)
              ## aggregate must be any function used for aggregation
              if (!missing(aggregate)) {
                  if (!exists(as.character(substitute(aggregate)), mode = "function")) {                           
                      stop(paste("Unknown function in argument ",
                                 "'aggregate': '", aggregate,
                                 "' is not defined", sep = ""), 
                           call. = FALSE)
                  }
                  crit <- match.arg(crit)
              }
              ## quantile.crit determines the variable quantiles
              ## should be added for
              if (!missing(quantile)) {
                  quantile <- as.integer(quantile)
                  if (length(quantile) > 1 || quantile < 2) {
                      stop(paste("Unknown argument: number of quantiles ",
                                 "in argument 'quantile' must be an ",
                                 "integer value greater 2", sep = ""))
                  }
                  quantile.crit <- match.arg(quantile.crit)
              }
              if (hasBOND_SYM_ID(object)) {
                  .volumesummary.BONDSYMID.mmTRACE(object, from, to, by, 
                                                   aggregate, crit, 
                                                   quantile, quantile.crit)
              } else if (hasCUSIP_ID(object)) {
                  .volumesummary.CUSIPID.mmTRACE(object, from, to, by, 
                                                 aggregate, crit, 
                                                 quantile, quantile.crit)
              } else {
                  stop(paste("Missing identifier: slot @.Table ",
                             "in 'mmTRACE' object has neither a ",
                             "a column named 'BOND_SYM_ID' nor a ",
                             "a column named 'CUSIP_ID'.", sep = ""))
              }
          }
)
### ========================================================================
### ========================================================================
### Private functions.
### Thee functions are not exported to the user.
### ------------------------------------------------------------------------

### ========================================================================
### I. Checking 
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
### 1. .check.mmTRACE
### Checks the names of the input data file if names are con-
### form with the standard wrds column names for the TRACE
### data
### @param names.vec    Vector with column names 
### @see ?names
### ------------------------------------------------------------------------
".check.mmTRACE" <- function(names.vec) 
{
    avail <- names.vec %in% TRACE.names 
    if (sum(avail, na.rm = TRUE) == 0) {
        stop(paste("Unknown column names. 'mmTRACE' default constructor ",
                   "needs column names similar to the predefined standard.",
                   sep = ""))
    } 
    if (length(names.vec) > length(TRACE.names)) {
        warning(paste("Unknown column names. 'mmTRACE' default ",
                      "constructor. needs column names similar to the ",
                      "predefined standard.", sep = ""))
        return(avail)
    }
}

### -----------------------------------------------------------------------
### 2. .check.args.cleanUp.mmTRACE
### Checks the arguments for the cleanUp algorithm of the TRACE data. The 
### algorithm needs a full 'data.table' object with predefined column names.
### The names must at least cover 'BOND_SYM_ID' or 'CUSIP_ID' to identify 
### different securities in the sample. Furthermore the names must cover
### 'TRD_EXCTN_DT' and 'TRD_EXCTN_TM' to identify the trade times. Further,
### 'MSG_SEQ_NB' to identify a message on a certain day and 'TRC_ST' to 
### determine the status of a message. The identifiers must be either 
### 'T', 'C', 'W' or 'G', 'H', 'I' for data disseminated after Feb. 6th,
### 2012. 
### If provided, 'sec' must be a character vector containing security IDs, 
### 'comp' must be a character vector containing company IDs and 
### 'excl.outside' can contain either 'A' or 'Z' corresponding to the Second
### sale identifier 'SALE_CNDTN2_CD' in the WRDS data. 
### @param  object          mmTRACE object
### @param  sec             character vector with security IDs (either CUSIP_ID or 
###                         BOND_SYM_ID
### @param  comp            character vector with company IDs (COMPANY_SYMBOL) 
### @param  excl.outside    character vector filled either by 'A', 'Z' or
###                         both
### @see cleanUp()
### -----------------------------------------------------------------------
".check.args.cleanUp.mmTRACE"   <- function(obj, sec, comp, excl.after, stats.ctrl) 
{
    if (dim(obj@.Table)[1] == 0) {
        stop(paste("No data. Slot @.Table in 'mmTRACE' object is empty. ",
                   "For cleanUp the algorithm needs data.", sep = ""))
    } else {
        hasTRD_EXCTN_DT(obj, verbose = TRUE)
        hasTRD_EXCTN_TM(obj, verbose = TRUE)
        hasMSG_SEQ_NB(obj, verbose = TRUE)
        hasORIG_MSG_SEQ_NB(obj, verbose = TRUE)
        hasTRC_ST(obj, verbose = TRUE)
        hasASOF_CD(obj, verbose = TRUE)
        if (excl.after) {
            hasSALE_CNDTN2_CD(obj, verbose = TRUE)
        }
    }
    if (!is.null(sec) && any(!as.character(sec) %in% unlist(c(getSecurities(obj))))) {
            stop(paste("Unknown argument. Argument 'sec' contains symbols ",
                       "not contained in the data.", sep = ""))
    }     
    if (!is.null(comp) && any(!as.character(comp) %in% unlist(c(getCompanies(obj))))) {
            stop(paste("Unknown argument. Argument 'comp' contains symbols ",
                       "not contained in the data.", sep = ""))
    }
}

### ========================================================================
### II. Algorithms 
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
### 1. .raw.cleanUp.<BONDSYMID, CUSIPID>.mmTRACE
### Raw code applying the clean-up algorithm proposed by Dick-Nielsen (2009).
### All duplicates and reversals are deleted from the dataset for the 
### specified companies 'comp' and the specified securities 'sec'. 
### 'excl.outside' indicates if trades outside the market hours and outside
### trading sequence should be deleted as well.
### @param obj              'mmTRACE' object
### @param sec              character vector containing securities (if empty,
###                         ALL)
### @param  comp            character vector containing company symbols (if 
###                         empty, ALL)
### @param  excl.outside    character vector containing exclusion symbols (
###                         either 'A' or 'Z' or both)
### @see ?cleanUp, .check.args.cleanUp.mmTRACE
### ------------------------------------------------------------------------
".raw.cleanUp.BONDSYMID.mmTRACE"  <- function(obj, sec, comp, excl.after,
                                              stats, stats.ctrl)
{
    cat("Remove duplicates ...")
    timer <- proc.time()
    ## Prepare statics if true

    ## Extract list of securities by company and securities
    sec.list <- getSecurities(obj)
    setnames(sec.list, names(sec.list)[2], "SEC")
    if (length(sec) > 0) {
        setkey(sec.list, "SEC")
        sec.list <- sec.list[SEC %in% sec]
    } 
    if (length(comp) > 0 && hasCOMPANY_SYMBOL(obj)) {
        setkey(sec.list, "COMPANY_SYMBOL")
        sec.list <- sec.list[COMPANY_SYMBOL %in% comp]
    }
    sec.list <- sec.list$SEC    
    if (stats) {
        stats.id <- switch(stats.ctrl,
                           "comp"   = "COMPANY_SYMBOL",
                           "sec"    = "BOND_SYM_ID")       
        n.stats <- obj@.Table[BOND_SYM_ID %in% sec.list, NROW(MSG_SEQ_NB), by = stats.id]
        setkeyv(n.stats, stats.id)
    } else {
        dim.all <- obj@.Table[BOND_SYM_ID %in% sec.list, NROW(MSG_SEQ_NB)]
    }
    
   
    ## STEP 1: REMOVE DUPLICATES
    ## remove duplicates identified via TM_STMP and MSG_SEQ_NB and security
    ## identifier (either CUSIP_ID or BOND_SYM_ID)
    setkey(obj@.Table, NULL)
    DT <- unique(obj@.Table[BOND_SYM_ID %in% sec.list])
    if (stats) {
        n.duplicates <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats      <- n.stats[n.duplicates]
        setkeyv(n.stats, stats.id)
        rm(n.duplicates)
    } else {
        dim.duplicates <- dim(DT)[1]
    }
    cat("done\n")
    elapsed <- proc.time() - timer
    elapsed <- format(elapsed, digits = 2)
    cat("Time elapsed for removing duplicates: ")
    cat(elapsed[[3]], "secs\n")
    timer.reversal <- proc.time() 
    cat("Remove reversals ...")    
    ## STEP 2: REMOVE REVERSALS AND CHECK FOR AS_OF
    ## ASOF_CD: R   Reversal
    ##          A   As-of 
    ## Cancel a reversal and its original message always and 
    ## keep always an as_of message.
    setkey(DT, "BOND_SYM_ID", "TM_STMP", "ASCII_RPTD_VOL_TX", "YLD_PT", "RPTD_PR")
    DT <- DT[!DT[ASOF_CD == "R"][, list(BOND_SYM_ID, TM_STMP, ASCII_RPTD_VOL_TX, YLD_PT, RPTD_PR)]]
    elapsed <- proc.time() - timer.reversal
    elapsed <- format(elapsed, digits = 2)
    if (stats) {
        n.reversal  <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats     <- n.stats[n.reversal]
        setkeyv(n.stats, stats.id)
        rm(n.reversal)
    } else {
        dim.reversals <- dim(DT)[1]
    }
    cat("done\n")
    cat("Time elapsed for removing reversals: ")
    cat(elapsed[[3]], "secs\n")
    timer.cancellations <- proc.time()
    cat("Remove cancellations and check for corrections ...")
    ## STEP 3: REMOVE CANCELLATIONS AND CHECK FOR CORRECTIONS
    ## TRADE CANCELLATION: All original messages AS WELL AS cancellation messages 
    ## must be deleted. 
    ## Compare the messages via MSG_SEQ_NB, TM_STMP and BOND_SYM_ID (alternatively 
    ## CUSIP_ID). The latter is needed as different Bonds could still have the 
    ## same MSG_SEQ_NB on the same TM_STMP.
    
    setkey(DT, "MSG_SEQ_NB", "TM_STMP", "BOND_SYM_ID")
    ## Since February 2nd, 2012 TRC_ST got different values. This is first been
    ## checked and in case of values in the table before this date two 
    ## independent algorithms are run and data is merged afterwards.
    feb <- as.POSIXct("2012-02-06", tz = "America/New_York")
    if (any(DT[, TM_STMP < feb])) {        
        DT.before   <- obj@.Table[TM_STMP < feb]
        DT.after    <- obj@.Table[TM_STMP >= feb] 
        ## TRC_ST:  T   Trade Report
        ##          C   Trade Cancellation
        ##          W   Trade Correction
        ## Cancel all messages in case TRC_ST == "C" and cancel
        ## only original message in case TRC_ST == "W".

        ## CANCELLATIONS
        DT.before   <- DT[!DT[TRC_ST == "C"][, list(ORIG_MSG_SEQ_NB, 
                                                    TM_STMP, 
                                                    BOND_SYM_ID)]]
        DT.before   <- DT[!DT[TRC_ST == "C"]]                                                        
        ## CORRECTIONS
        DT.before   <- DT[!DT[TRC_ST == "W"][, list(ORIG_MSG_SEQ_NB, 
                                                    TM_STMP,
                                                    BOND_SYM_ID)]]
        if (any(DT[,TM_STMP >= feb])) {
            ## TRC_ST:  G   Trade Report
            ##          H   Trade Cancellation
            ##          I   Trade Correction
            ## Cancel all messages in case TRC_ST == "H" and cancel
            ## only original message in case TRC_ST == "I".

            ## CANCELLATIONS
            DT.after    <- DT[!DT[TRC_ST == "H"][, list(ORIG_MSG_SEQ_NB,
                                                        TM_STMP,
                                                        BOND_SYM_ID)]]
            DT.after    <- DT[!DT[TRC_ST == "H"]]
            ## CORRECTIONS                                                      
            DT.after    <- DT[!DT[TRC_ST == "I"][, list(ORIG_MSG_SEQ_NB,
                                                        TM_STMP,
                                                        BOND_SYM_ID)]]
            ## MERGE 
            DT  <- rbind(DT.before, DT.after)            
        } 
    } else {
        ## TRC_ST:  G   Trade Report
        ##          H   Trade Cancellation
        ##          I   Trade Correction
        ## Cancel all messages in case TRC_ST == "H" and cancel
        ## only original message in case TRC_ST == "I".

        ## CANCELLATIONS
        DT  <- DT[!DT[TRC_ST == "H"][, list(ORIG_MSG_SEQ_NB,
                                            TM_STMP,
                                            BOND_SYM_ID)]]
        DT  <- DT[!DT[TRC_ST == "H"]]                                                                        
        DT  <- DT[!DT[TRC_ST == "I"][, list(ORIG_MSG_SEQ_NB,
                                            TM_STMP,
                                            BOND_SYM_ID)]]
    }
    if (stats) {        
        n.corrections   <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats         <- n.stats[n.corrections]
        setkeyv(n.stats, stats.id)
        rm(n.corrections)
    } else {
        dim.corrections <- dim(DT)[1]
    }
    elapsed <- proc.time() - timer.cancellations
    elapsed <- format(elapsed, digits = 2)
    cat("done\n")
    cat("Time elapsed for cancellations and corrections: ")
    cat(elapsed[[3]], "secs\n")
    ## IF ASKED FOR: REMOVE ALL TRADES REPORTED OUTSIDE TRADING HOURS    
    ## Again, here come of relevance the changes effective since February
    ## 6th, 2012.
    if (excl.after) {
        cat("Remove messages after market hours ...")        
        timer.after <- proc.time()        
        if (any(DT[,TM_STMP < feb])) {
            DT.before <- DT[TM_STMP < feb]          
            DT.after  <- DT[TM_STMP >= feb]            
            ## SALE_CNDTN2_CD:  A   Trades reported outside market hours
            ##                  Z   Sold out of sequence (reported late)
            setkey(DT.before, "SALE_CNDTN2_CD")
            DT.before <- DT.before[!DT.before[J("A", "Z")]]

            ## SALE_CNDTN2_CD   T   Reported after market hours
            ##                  Z   Reported late
            ##                  U   Reported late, after market hours
            setkey(DT.after, "SALE_CNDTN2_CD")
            DT.after <- DT.after[!DT.after[J("T", "Z", "U")]]
            DT <- rbind(DT.before, DT.after)
        } else {
            ## SALE_CNDTN2_CD   T   Reported after market hours
            ##                  Z   Reported late
            ##                  U   Reported late, after market hours
            setkey(DT, "SALE_CNDTN2_CD")
            DT <- DT[!DT[J("T", "Z", "U")]]
        }
        if (stats) {
            n.after <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
            rm(n.after)
            setkeyv(n.stats, stats.id)
            n.stats <- n.stats[n.after]
        } else {
            dim.after <- dim(DT)[1]
        }
        elapsed <- proc.time() - timer.after
        elapsed <- format(elapsed, digits = 2)
        cat("done\n")
        cat("Time elapsed for removing after market hour messages: ")
        cat(elapsed[[3]], "secs\n")
    }
    cat("Updating 'mmTRACE' object ...")
    if (stats) {
        n.stats[, Duplicates := list(V1 - V1.1)]
        n.stats[, Reversals  := list(V1.1 - V1.2)]
        n.stats[, CanOrCorr  := list(V1.2 - V1.3)]
        if (excl.after) {
            n.stats[, Outside := list(V1.3 - V1.4)]
            n.stats[, V1.4 := NULL]
        }
        n.stats[, V1    := NULL]
        n.stats[, V1.1  := NULL]
        n.stats[, V1.2  := NULL]
        n.stats[, V1.3  := NULL]
        n.stats[, Total := Duplicates + Reversals + CanOrCorr]
    } else {
        duplicates  <- dim.all - dim.duplicates
        reversals   <- dim.duplicates - dim.reversals
        canorcorr   <- dim.reversals - dim.corrections
        total       <- duplicates + reversals + canorcorr
        if (excl.after) {
            after   <- dim.corrections - dim.after
            total   <- total + after
        }
    }
    obj@.Table <- DT
    rm(DT)
    obj@.Modified   <- Sys.time()
    obj@.Cleaned    <- TRUE
    obj@.NROWS      <- dim(obj@.Table)[1]
    obj@.NCOLS      <- dim(obj@.Table)[2]
    obj@.NSEC       <- obj@.Table[, NROW(unique(BOND_SYM_ID))]
    obj@.NCOMP      <- obj@.Table[, NROW(unique(COMPANY_SYMBOL))]
    obj@.Start      <- obj@.Table[, min(TM_STMP, na.rm = TRUE)]
    obj@.End        <- obj@.Table[, max(TM_STMP, na.rm = TRUE)]
    elapsed         <- proc.time() - timer
    elapsed         <- format(elapsed, digits = 2)
    cat("done\n")
    if (!stats) {
        c.stats     <- format(c(duplicates, reversals, canorcorr, total),
                              big.mark = ",", scientific = FALSE, 
                              justify = "right", width = 15)
        cat("Found  :", c.stats[1], " duplicates\n")
        cat("       :", c.stats[2], " reversals\n")
        cat("       :", c.stats[3], " cancellations and corrections\n")
        if (excl.after) {
            a.stats <- format(c(after, total + after), big.mark = ",",
                              scientific = FALSE, justify = "right", 
                              width = 15)
            cat("       :", a.stats[1], " after market hour messages\n")        
            cat("Total  :", a.stats[2], " deleted messages\n")
        } else {
            cat("Total  :", c.stats[4], " deleted messages\n")
        }
        cat("Total time elapsed: ")
        cat(elapsed[[3]], "secs\n")
        return(obj)
    } else {
        return(list(TRACE = obj, stats = n.stats))
    }
}

".raw.cleanUp.CUSIPID.mmTRACE"  <- function(obj, sec, comp, excl.after,
                                              stats, stats.ctrl)
{
    cat("Remove duplicates ...")
    timer <- proc.time()
    ## Prepare statics if true

    ## Extract list of securities by company and securities
    sec.list <- getSecurities(obj)
    setnames(sec.list, names(sec.list)[2], "SEC")
    if (length(sec) > 0) {
        setkey(sec.list, "SEC")
        sec.list <- sec.list[SEC %in% sec]
    } 
    if (length(comp) > 0 && hasCOMPANY_SYMBOL(obj)) {
        setkey(sec.list, "COMPANY_SYMBOL")
        sec.list <- sec.list[COMPANY_SYMBOL %in% comp]
    }
    sec.list <- sec.list$SEC    
    if (stats) {
        stats.id <- switch(stats.ctrl,
                           "comp"   = "COMPANY_SYMBOL",
                           "sec"    = "BOND_SYM_ID")       
        n.stats <- obj@.Table[CUSIP_ID %in% sec.list, NROW(MSG_SEQ_NB), by = stats.id]
        setkeyv(n.stats, stats.id)
    } else {
        dim.all <- obj@.Table[CUSIP_ID %in% sec.list, NROW(MSG_SEQ_NB)]
    }
    
   
    ## STEP 1: REMOVE DUPLICATES
    ## remove duplicates identified via TM_STMP and MSG_SEQ_NB and security
    ## identifier (either CUSIP_ID or BOND_SYM_ID)
    setkey(obj@.Table, NULL)
    DT <- unique(obj@.Table[CUSIP_ID %in% sec.list])
    if (stats) {
        n.duplicates <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats      <- n.stats[n.duplicates]
        setkeyv(n.stats, stats.id)
        rm(n.duplicates)
    } else {
        dim.duplicates <- dim(DT)[1]
    }
    cat("done\n")
    elapsed <- proc.time() - timer
    elapsed <- format(elapsed, digits = 2)
    cat("Time elapsed for removing duplicates: ")
    cat(elapsed[[3]], "secs\n")
    timer.reversal <- proc.time() 
    cat("Remove reversals ...")    
    ## STEP 2: REMOVE REVERSALS AND CHECK FOR AS_OF
    ## ASOF_CD: R   Reversal
    ##          A   As-of 
    ## Cancel a reversal and its original message always and 
    ## keep always an as_of message.
    setkey(DT, "CUSIP_ID", "TM_STMP", "ASCII_RPTD_VOL_TX", "YLD_PT", "RPTD_PR")
    DT <- DT[!DT[ASOF_CD == "R"][, list(CUSIP_ID, TM_STMP, ASCII_RPTD_VOL_TX, YLD_PT, RPTD_PR)]]
    elapsed <- proc.time() - timer.reversal
    elapsed <- format(elapsed, digits = 2)
    if (stats) {
        n.reversal  <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats     <- n.stats[n.reversal]
        setkeyv(n.stats, stats.id)
        rm(n.reversal)
    } else {
        dim.reversals <- dim(DT)[1]
    }
    cat("done\n")
    cat("Time elapsed for removing reversals: ")
    cat(elapsed[[3]], "secs\n")
    timer.cancellations <- proc.time()
    cat("Remove cancellations and check for corrections ...")
    ## STEP 3: REMOVE CANCELLATIONS AND CHECK FOR CORRECTIONS
    ## TRADE CANCELLATION: All original messages AS WELL AS cancellation messages 
    ## must be deleted. 
    ## Compare the messages via MSG_SEQ_NB, TM_STMP and CUSIP_ID (alternatively 
    ## BOND <- SYM_ID). The latter is needed as different Bonds could still have the 
    ## same MSG_SEQ_NB on the same TM_STMP.
    
    setkey(DT, "MSG_SEQ_NB", "TM_STMP", "CUSIP_ID")
    ## Since February 2nd, 2012 TRC_ST got different values. This is first been
    ## checked and in case of values in the table before this date two 
    ## independent algorithms are run and data is merged afterwards.
    feb <- as.POSIXct("2012-02-06", tz = "America/New_York")
    if (any(DT[, TM_STMP < feb])) {        
        DT.before   <- obj@.Table[TM_STMP < feb]
        DT.after    <- obj@.Table[TM_STMP >= feb] 
        ## TRC_ST:  T   Trade Report
        ##          C   Trade Cancellation
        ##          W   Trade Correction
        ## Cancel all messages in case TRC_ST == "C" and cancel
        ## only original message in case TRC_ST == "W".

        ## CANCELLATIONS
        DT.before   <- DT[!DT[TRC_ST == "C"][, list(ORIG_MSG_SEQ_NB, 
                                                    TM_STMP, 
                                                    CUSIP_ID)]]
        DT.before   <- DT[!DT[TRC_ST == "C"]]                                                        
        ## CORRECTIONS
        DT.before   <- DT[!DT[TRC_ST == "W"][, list(ORIG_MSG_SEQ_NB, 
                                                    TM_STMP,
                                                    CUSIP_ID)]]
        if (any(DT[,TM_STMP >= feb])) {
            ## TRC_ST:  G   Trade Report
            ##          H   Trade Cancellation
            ##          I   Trade Correction
            ## Cancel all messages in case TRC_ST == "H" and cancel
            ## only original message in case TRC_ST == "I".

            ## CANCELLATIONS
            DT.after    <- DT[!DT[TRC_ST == "H"][, list(ORIG_MSG_SEQ_NB,
                                                        TM_STMP,
                                                        CUSIP_ID)]]
            DT.after    <- DT[!DT[TRC_ST == "H"]]
            ## CORRECTIONS                                                      
            DT.after    <- DT[!DT[TRC_ST == "I"][, list(ORIG_MSG_SEQ_NB,
                                                        TM_STMP,
                                                        CUSIP_ID)]]
            ## MERGE 
            DT  <- rbind(DT.before, DT.after)            
        } 
    } else {
        ## TRC_ST:  G   Trade Report
        ##          H   Trade Cancellation
        ##          I   Trade Correction
        ## Cancel all messages in case TRC_ST == "H" and cancel
        ## only original message in case TRC_ST == "I".

        ## CANCELLATIONS
        DT  <- DT[!DT[TRC_ST == "H"][, list(ORIG_MSG_SEQ_NB,
                                            TM_STMP,
                                            CUSIP_ID)]]
        DT  <- DT[!DT[TRC_ST == "H"]]                                                                        
        DT  <- DT[!DT[TRC_ST == "I"][, list(ORIG_MSG_SEQ_NB,
                                            TM_STMP,
                                            CUSIP_ID)]]
    }
    if (stats) {        
        n.corrections   <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats         <- n.stats[n.corrections]
        setkeyv(n.stats, stats.id)
        rm(n.corrections)
    } else {
        dim.corrections <- dim(DT)[1]
    }
    elapsed <- proc.time() - timer.cancellations
    elapsed <- format(elapsed, digits = 2)
    cat("done\n")
    cat("Time elapsed for cancellations and corrections: ")
    cat(elapsed[[3]], "secs\n")
    ## IF ASKED FOR: REMOVE ALL TRADES REPORTED OUTSIDE TRADING HOURS    
    ## Again, here come of relevance the changes effective since February
    ## 6th, 2012.
    if (excl.after) {
        cat("Remove messages after market hours ...")        
        timer.after <- proc.time()        
        if (any(DT[,TM_STMP < feb])) {
            DT.before <- DT[TM_STMP < feb]          
            DT.after  <- DT[TM_STMP >= feb]            
            ## SALE_CNDTN2_CD:  A   Trades reported outside market hours
            ##                  Z   Sold out of sequence (reported late)
            setkey(DT.before, "SALE_CNDTN2_CD")
            DT.before <- DT.before[!DT.before[J("A", "Z")]]

            ## SALE_CNDTN2_CD   T   Reported after market hours
            ##                  Z   Reported late
            ##                  U   Reported late, after market hours
            setkey(DT.after, "SALE_CNDTN2_CD")
            DT.after <- DT.after[!DT.after[J("T", "Z", "U")]]
            DT <- rbind(DT.before, DT.after)
        } else {
            ## SALE_CNDTN2_CD   T   Reported after market hours
            ##                  Z   Reported late
            ##                  U   Reported late, after market hours
            setkey(DT, "SALE_CNDTN2_CD")
            DT <- DT[!DT[J("T", "Z", "U")]]
        }
        if (stats) {
            n.after <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
            rm(n.after)
            setkeyv(n.stats, stats.id)
            n.stats <- n.stats[n.after]
        } else {
            dim.after <- dim(DT)[1]
        }
        elapsed <- proc.time() - timer.after
        elapsed <- format(elapsed, digits = 2)
        cat("done\n")
        cat("Time elapsed for removing after market hour messages: ")
        cat(elapsed[[3]], "secs\n")
    }
    cat("Updating 'mmTRACE' object ...")
    if (stats) {
        n.stats[, Duplicates := list(V1 - V1.1)]
        n.stats[, Reversals  := list(V1.1 - V1.2)]
        n.stats[, CanOrCorr  := list(V1.2 - V1.3)]
        if (excl.after) {
            n.stats[, Outside := list(V1.3 - V1.4)]
            n.stats[, V1.4 := NULL]
        }
        n.stats[, V1    := NULL]
        n.stats[, V1.1  := NULL]
        n.stats[, V1.2  := NULL]
        n.stats[, V1.3  := NULL]
        n.stats[, Total := Duplicates + Reversals + CanOrCorr]
    } else {
        duplicates  <- dim.all - dim.duplicates
        reversals   <- dim.duplicates - dim.reversals
        canorcorr   <- dim.reversals - dim.corrections
        total       <- duplicates + reversals + canorcorr
        if (excl.after) {
            after   <- dim.corrections - dim.after
            total   <- total + after
        }
    }
    obj@.Table <- DT
    rm(DT)
    obj@.Modified   <- Sys.time()
    obj@.Cleaned    <- TRUE
    obj@.NROWS      <- dim(obj@.Table)[1]
    obj@.NCOLS      <- dim(obj@.Table)[2]
    obj@.NSEC       <- obj@.Table[, NROW(unique(CUSIP_ID))]
    obj@.NCOMP      <- obj@.Table[, NROW(unique(COMPANY_SYMBOL))]
    obj@.Start      <- obj@.Table[, min(TM_STMP, na.rm = TRUE)]
    obj@.End        <- obj@.Table[, max(TM_STMP, na.rm = TRUE)]
    elapsed         <- proc.time() - timer
    elapsed         <- format(elapsed, digits = 2)
    cat("done\n")
    if (!stats) {
        c.stats     <- format(c(duplicates, reversals, canorcorr, total),
                              big.mark = ",", scientific = FALSE, 
                              justify = "right", width = 15)
        cat("Found  :", c.stats[1], " duplicates\n")
        cat("       :", c.stats[2], " reversals\n")
        cat("       :", c.stats[3], " cancellations and corrections\n")
        if (excl.after) {
            a.stats <- format(c(after, total + after), big.mark = ",",
                              scientific = FALSE, justify = "right", 
                              width = 15)
            cat("       :", a.stats[1], " after market hour messages\n")        
            cat("Total  :", a.stats[2], " deleted messages\n")
        } else {
            cat("Total  :", c.stats[4], " deleted messages\n")
        }
        cat("Total time elapsed: ")
        cat(elapsed[[3]], "secs\n")
        return(obj)
    } else {
        return(list(TRACE = obj, stats = n.stats))
    }
}

".raw.cleanUp.enhanced.BONDSYMID.mmTRACE"  <- function(obj, sec, comp, excl.ID.bydate,
                                                       excl.ID.allbuy, excl.XD, excl.further,
                                                       stats, stats.ctrl)
{
    ## TODO: Check for SALE_CNDTN2_CD (out of sequence, etc.) -> further
    cat("Remove cancellations and check for corrections ...")
    timer <- proc.time()

    ## Extract list of securities by company and securities
    sec.list <- getSecurities( obj )
    setnames( sec.list, names( sec.list )[2], "SEC" )
    if ( length( sec ) > 0 ) {
        setkey( sec.list, "SEC" )
        sec.list <- sec.list[SEC %in% sec]
    } 
    if ( length( comp ) > 0 && hasCOMPANY_SYMBOL( obj ) ) {
        setkey(sec.list, "COMPANY_SYMBOL")
        sec.list <- sec.list[COMPANY_SYMBOL %in% comp]
    }
    sec.list <- sec.list$SEC   
    DT <- obj@.Table[BOND_SYM_ID %in% sec.list]
    if ( stats ) {
        stats.id <- switch( stats.ctrl,
                           "comp"   = "COMPANY_SYMBOL",
                           "sec"    = "BOND_SYM_ID" )       
        n.stats <- DT[, NROW( MSG_SEQ_NB ), by = stats.id]
        n.stats[, Raw := V1]
        setkeyv( n.stats, stats.id )
    } else {
        dim.raw <- DT[, NROW( MSG_SEQ_NB )]
    }
    ## STEP 1: REMOVE CANCELLATIONS AND CHECK FOR CORRECTIONS
    ## Uses the enhanced information 'TRD_RPT_DT", i.e. reporting date.
    ## Setting key to these variables compares in a merge three given
    ## variables in the input with these variables in the same ordering. 
    setkey( DT, "BOND_SYM_ID", "MSG_SEQ_NB", "TRD_RPT_DT") 
    ## (1.1) FIND ORIGINAL TRADES FOR TRADE CANCELLATIONS AND CORRECTIONS
    DT <- DT[!DT[TRC_ST == "W", list( BOND_SYM_ID, 
                                      ORIG_MSG_SEQ_NB,
                                      TRD_RPT_DT )]]
    if ( stats ) {
        n.corrections <- DT[, NROW( MSG_SEQ_NB ), by = stats.id]
        n.stats      <- n.stats[n.corrections]
        n.stats[, Corrections := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv( n.stats, stats.id )
        rm( n.corrections )
    } else {
        dim.corrections <- dim(DT)[1]
    }

    DT <- DT[!DT[TRC_ST == "C", 
                 list(BOND_SYM_ID, 
                      ORIG_MSG_SEQ_NB, 
                      TRD_RPT_DT)]]
    ## (1.2) REMOVE CANCELLATIONS
    DT <- DT[!DT[TRC_ST == "C"]]
    if (stats) {
        n.cancellations <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats      <- n.stats[n.cancellations]
        n.stats[, Cancellations := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv(n.stats, stats.id)
        rm(n.cancellations)
    } else {
        dim.cancellations <- dim(DT)[1]
    }
    cat("done\n")
    elapsed <- proc.time() - timer
    elapsed <- format(elapsed, digits = 2)
    cat("Time elapsed for removing cancellations and checking for corrections: ")
    cat(elapsed[[3]], "secs\n")
    timer.reversal <- proc.time() 
    cat("Remove reversals ...")    

    ## --------------------------------------------------------------------------
    ## STEP 2: REMOVE REVERSALS    
    setkey(DT, "BOND_SYM_ID", "TM_STMP", "RPTD_PR", "ENTRD_VOL_QT", 
           "RPT_SIDE_CD", "TRD_EXCTN_DT")
    ## Due to switching symbols in the CNTRA_MP_ID (possibly errors) the 
    ## seven-way match suggested in Asquith, Covert and Pathak (2013)  
    ## is not applied. In contrast to Dick-Nielsen (2013) the RPT_SIDE_CD
    ## flag is added as another variable for matching. Errors were not 
    ## detected in this variable. 
    ## 
    ## Most inner data.table: we get all reversals and the four values
    ## by which it is matched in the upcoming merge plus the TRD_RPT_DT.
    ##
    ## Second inner data.table: Merge all reversals in regard to bond symbol,
    ## time stamp, reported price, entired volume quoted, sell side and 
    ## and trade execution date vs. reported date, with the whole data.table. 
    ## Take only the first match, such that exactly one trade or one as-of 
    ## is matched with one reversal. This is important as otherwise one 
    ## reversal could match more than one trade or as-of in case there are less
    ## rows in the most inner table. Further exclude rows that do not match
    ## and roll over (NOCF) the trade execution date if it does not yet match
    ## the reporting date (execution dates must be smaller than the reported
    ## date on reversals and the carry forward ensures, that they can get
    ## matched; further the time stamp assures, that the same date and time
    ## in trade execution is aware; this makes decreases the set of matching 
    ## trades entirely and enables a correct matching) Do the merge first only
    ## for the whole data.table of trades and afterwards for the whole data.table
    ## with as-ofs.
    ##
    ## Delete then all entries resulting from the merge of inner tables. 
    ## Further roll again the trade execution date forward, otherwise the
    ## trade or as-of to be deleted cannot be found. The NOCF does not 
    ## influence the whole data.table, as the matching rows get deleted.
    ## (2.1) DELETE TRADES BY REVERSALS
    DT <- DT[!DT[ASOF_CD != "R" & ASOF_CD != "A"][DT[ASOF_CD == "R", list(BOND_SYM_ID, 
                                                                          TM_STMP, 
                                                                          RPTD_PR, 
                                                                          ENTRD_VOL_QT, 
                                                                          RPT_SIDE_CD,
                                                                          TRD_RPT_DT)], 
          roll = Inf, nomatch = 0, mult = "first"], roll = Inf]
    if ( stats ) {
        n.rev.trades    <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats         <- n.stats[n.rev.trades]
        n.stats[, ReversedTrades := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv(n.stats, stats.id)
        rm(n.rev.trades)
    } else {
        dim.rev.trades <- dim(DT)[1]
    }
    ## (2.2) DELETE AS-OFS BY REVERSALS
    DT <- DT[!DT[ASOF_CD == "A"][DT[ASOF_CD == "R", list(BOND_SYM_ID,
                                                         TM_STMP,
                                                         RPTD_PR,
                                                         ENTRD_VOL_QT,
                                                         RPT_SIDE_CD,
                                                         TRD_RPT_DT)],
          roll = Inf, nomatch = 0, mult = "first"], roll = Inf]
    if ( stats ) {
        n.rev.asof      <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats         <- n.stats[n.rev.asof]
        n.stats[, ReversedAsOfs := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]        
        setkeyv(n.stats, stats.id)
        rm(n.rev.asof)
    } else {
        dim.rev.asof <- dim(DT)[1] 
    }
    ## (2.3) DELETE REVERSALS 
    DT <- DT[!DT[ASOF_CD == "R"]]
    elapsed <- proc.time() - timer.reversal
    elapsed <- format(elapsed[[3]], digits = 2)
    if ( stats ) {
        n.reversal  <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats     <- n.stats[n.reversal]
        n.stats[, Reversals := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv(n.stats, stats.id)
        rm(n.reversal)
    } else {
        dim.reversals <- dim(DT)[1]
    }
    cat("done\n")
    cat("Time elapsed for removing reversals: ")
    cat(elapsed, "secs\n")
    timer.agency <- proc.time()

    if (excl.XD) {
        ## (2.4) REMOVE DELAYED REVERSALS AND DELAYED DISSEMINATIONS
        DT <- DT[!DT[ASOF_CD == "X" | ASOF_CD == "D"]]
        if ( stats ) {
            n.delayed   <- DT[, NROW( MSG_SEQ_NB ), by = stats.id]
            n.stats     <- n.stats[n.delayed]
            n.stats[, Delayed := V1 - V1.1]
            n.stats[, V1:= V1.1]
            n.stats[, V1.1 := NULL]
            setkeyv( n.stats, stats.id )
            rm( n.delayed )            
        }            
    } 

    ## ------------------------------------------------------------------------------
    ## STEP 3: REMOVE AGENCY TRADES
    cat( "Remove agency trades ...")
    setkey( DT, "BOND_SYM_ID", "TM_STMP", "RPTD_PR", "ENTRD_VOL_QT" )
    ## A usual principal transaction produces one message in the system. In
    ## contrast an agency transaction produces three: one dealer sell, one
    ## dealer buy and one customer buy. Two of these three have to be deleted.
    ## 
    ## Matching is done via the frequencies of volumes as in Dick-Nielsen (2013).
    ## The trades must have the same quantity, time_stamp, bond symbol and price.
    ##
    ## Reports with volume frequencies greater 1 and both SELL_CPCTY_CD == "A" or
    ## BUY_CPCTY_CD == "A" are deleted from the sample. It remains the inter-dealer
    ## trade with SELL_CPCTY_CD == "A" or BUY_CPCTY_CD == "A". 
    DT <- DT[!DT[DT[SELL_CPCTY_CD == "A"], .N][N > 1]]
    DT <- DT[!DT[DT[BUY_CPCTY_CD == "A"], .N][N > 1]]
    if (stats) {        
        n.agency        <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
        n.stats         <- n.stats[n.agency]
        n.stats[, AgencyTrades := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv(n.stats, stats.id)
        rm(n.agency)
    } else {
        dim.agency <- dim(DT)[1]
    }
    elapsed <- proc.time() - timer.agency
    elapsed <- format( elapsed, digits = 2 )
    cat( "done\n" )
    cat( "Time elapsed for removing agency trades: " )
    cat( elapsed[[3]], "secs\n" )
    timer.id <- proc.time()
    ## -----------------------------------------------------------------------------
    ## STEP 4: DELETE DUPLICATES FROM INTER-DEALER TRADES
    cat( "Remove duplicates from inter-dealer buy trades ..." )
    
    setkey( DT, "BOND_SYM_ID", "TM_STMP", "RPTD_PR", "ENTRD_VOL_QT", 
           "TRC_ST", "ASOF_CD","CNTRA_MP_ID" )
    ## Following Asquith, Covert and Pathal (2013) all buy-side inter-dealer
    ## trades are eliminated. 
    ## 
    ## First, all matching inter-dealer buy messages are deleted using
    ## among the regular message variables a time stamp. As it could be
    ## that dealers put in different times for the trade the data sample
    ## is cleaned in a second step on the basis of the trade execution 
    ## date only. 
    ## Asquith, Covert and Pathal (2013) apply a conservative approach an
    ## delete in a third step all remaining not matched inter-dealer buy
    ## trades. Here, it is followed their approach.
    ## (4.1) REMOVE ALL MATCHING INTER-DEALER BUY TRADES MATCHING ON TM_STMP
    DT <- DT[!DT[RPT_SIDE_CD == "B"][DT[RPT_SIDE_CD == "S"], 
                                     nomatch = 0, mult = "first"][
                                     CNTRA_MP_ID == "D" & RPT_SIDE_CD == "B"]]
    if ( stats ) {
        n.interd.dt     <- DT[, NROW( MSG_SEQ_NB ), by = stats.id] 
        n.stats         <- n.stats[n.interd.dt]
        n.stats[, IDbyDateTime := V1 - V1.1]
        n.stats[, V1 := V1.1]
        n.stats[, V1.1 := NULL]
        setkeyv( n.stats, stats.id )
        rm( n.interd.dt )
    } 
    cat("done\n") 
    if ( excl.ID.bydate ) {
        ## (4.2) REMOVE ALL INTER-DEALER BUY TRADES MATCHING ON TRD_EXCTN_DT
        cat("Remove all duplicates from inter-dealer buy trades using the date only ...")
        setkey(DT, "BOND_SYM_ID", "TRD_EXCTN_DT", "RPTD_PR", "ENTRD_VOL_QT",
               "TRC_ST", "ASOF_CD", "CNTRA_MP_ID")
        DT <- DT[!DT[RPT_SIDE_CD == "B"][DT[RPT_SIDE_CD == "S"], 
                                         nomatch = 0, mult = "first"][
                                         CNTRA_MP_ID == "D" & RPT_SIDE_CD == "B"]]
        if ( stats ) {
            n.interd.d      <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
            n.stats         <- n.stats[n.interd.d]
            n.stats[, IDbyDate := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 := NULL]
            setkeyv(n.stats, stats.id)
            rm(n.interd.d)
        }
        cat("done\n")
        if ( excl.ID.allbuy ) {
            ## (4.3) REMOVE ALL INTER-DEALER BUY TRADES THAT COULD NOT BE MATCHED
            cat("Remove all remaining inter-dealer buy trades ...")
            DT <- DT[!DT[RPT_SIDE_CD == "B" & CNTRA_MP_ID == "D"]]
            if ( stats ) {
                n.interd.ab     <- DT[, NROW(MSG_SEQ_NB), by = stats.id]
                n.stats         <- n.stats[n.interd.ab]
                n.stats[, IDAllBuys := V1 - V1.1]
                n.stats[, V1 := V1.1]
                n.stats[, V1.1 := NULL]
                setkeyv(n.stats, stats.id)
                rm(n.interd.ab)
            }
            cat("done\n")
        }
    }
    if ( !stats ) {
        dim.interd <- dim( DT )[1]
    }
    elapsed <- proc.time() - timer.id
    elapsed <- format(elapsed[[3]], digits = 2)
    cat( "Time elapsed to remove inter dealer trades: " )
    cat(elapsed, "secs\n")

    ## ----------------------------------------------------------------------------
    ## STEP 5: REMOVE ADDITIONAL TRADES
    if ( any( unlist( excl.further ) ) ) {
        timer.further <- proc.time()
        cat( "Remove further reports ..." )
        setkey(DT)        
    }
    if ( excl.further$excl.whenissued ) {
        ## (5.1) REMOVE WHEN ISSUED TRADES
        DT <- DT[!DT[WIS_FL == "Y"]]
        if ( stats ) {
            n.whenissued    <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.whenissued]
            n.stats[, WhenIssued := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.whenissued)
        }
    }
    if ( excl.further$excl.nonsecondary ) {
        ## (5.2) REMOVE NON-SECONDARY MARKET TRADES
        DT <- DT[!DT[TRDG_MKT_CD %in% c("S2", "P1", "P2")]]
        if ( stats ) {
            n.nonsecondary  <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.nonsecondary] 
            n.stats[, NonSecond := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.nonsecondary)
        }
    }
    if ( excl.further$excl.special ) {
        ## (5.3) REMOVE TRADES UNDER SPECIAL CIRCUMSTANCES
        DT <- DT[!DT[SPCL_TRD_FL == "Y"]]
        if ( stats ) {
            n.special       <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.special] 
            n.stats[, SpecialFl := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.special)
        }
    } 
    if ( excl.further$excl.equitylinked ) {
        ## (5.4) REMOVE TRADES IN EQUITY-LINKED NOTES
        DT <- DT[!DT[SCRTY_TYPE_CD == "C"]]
        if ( stats ) {
            n.equitylinked  <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.equitylinked] 
            n.stats[, EquityLinked := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.equitylinked)
        }       
    } 
    if (excl.further$excl.nssettlement) {
        ## (5.5) REMOVE TRADES WITH NON-STANDARD SETTLEMENT
        DT <- DT[!DT[DAYS_TO_STTL_CT > 6]]
        if ( stats ) {
            n.nssettlement  <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.nssettlement] 
            n.stats[, NSSettlement := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.nssettlement)
        }       
    }
    if ( excl.further$excl.nocash ) {        
        ## (5.6) REMOVE TRADES THAT ARE NOT CASH SALES
        DT <- DT[!DT[SALE_CNDTN_CD != "C"]]
        if ( stats ) {
            n.nocash        <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.nocash] 
            n.stats[, NoCash := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.nocash)
        }       
    }
    if ( excl.further$excl.commission ) {
        ## (5.7) REMOVE COMMISSION TRADES 
        DT <- DT[!DT[CMSN_TRD == "Y"]]
        if ( stats ) {
            n.commission    <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.commission] 
            n.stats[, Commission := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv( n.stats, stats.id )
            rm( n.commission )
        }
    }
    if ( excl.further$excl.giveup ) {
        ## (5.8) REMOVE TRADES WITH AN AUTOMATIC GIVE-UP
        ## An automatic give-up occurs either if a clearing firm executes a trade
        ## with one of its correspondents (one-sided) ot if an alternative trading
        ## system/electronic communication network (ATS/ECN) matches a Buy and a 
        ## Sell order on its system, creating a 'locked-in' trade between the two 
        ## parties.
        ## A = Automatic Give-Up (AGU)
        ## Q = Qualified Service Representative (QSR)
        ## BLANK = Regular
        DT <- DT[!DT[AGU_QSR_ID %in% c("A", "Q")]]
        if ( stats ) {
            n.giveup        <- DT[, NROW(MSG_SEQ_NB), by = stats.id] 
            n.stats         <- n.stats[n.giveup] 
            n.stats[, AUGandQSR := V1 - V1.1]
            n.stats[, V1 := V1.1]
            n.stats[, V1.1 :=  NULL]
            setkeyv(n.stats, stats.id)
            rm(n.giveup)
        }
    }
    if ( any( unlist(excl.further) ) ) {
        if ( !stats ) {
            dim.further <- dim( DT )[1]
        }
        elapsed <- proc.time() - timer.further
        elapsed <- format(elapsed[[3]], digits = 2)        
        cat( "done\n" )
        cat("Time elapsed to remove further reports: ")
        cat(elapsed, "secs\n")        
    }
    ## ---------------------------------------------------------------------------
    ## STEP 6 (OPTIONAL): MAKE STATISTICS
    cat("Updating 'mmTRACE' object ...")
    if (stats) {
        n.stats[, Total := Raw - V1]
        n.stats[, V1    := NULL]
    } else {
        corrections     <- dim.raw - dim.corrections
        cancellations   <- dim.corrections - dim.cancellations
        reversed.trades <- dim.cancellations - dim.rev.trades
        reversed.asof   <- dim.rev.trades - dim.rev.asof
        reversals       <- dim.rev.asof - dim.reversals
        agency          <- dim.reversals - dim.agency
        interdealer     <- dim.agency - dim.interd
        further         <- dim.interd - dim.further
        total           <- dim.raw - dim.further
    }
    obj@.Table <- DT
    rm(DT)
    obj@.Modified   <- Sys.time()
    obj@.Enhanced   <- TRUE
    obj@.Cleaned    <- TRUE
    obj@.NROWS      <- dim(obj@.Table)[1]
    obj@.NCOLS      <- dim(obj@.Table)[2]
    obj@.NSEC       <- obj@.Table[, NROW(unique(BOND_SYM_ID))]
    obj@.NCOMP      <- obj@.Table[, NROW(unique(COMPANY_SYMBOL))]
    obj@.Start      <- obj@.Table[, min(TM_STMP, na.rm = TRUE)]
    obj@.End        <- obj@.Table[, max(TM_STMP, na.rm = TRUE)]
    elapsed         <- proc.time() - timer
    elapsed         <- format(elapsed[[3]], digits = 2)
    cat("done\n")
    if (!stats) {
        c.stats     <- format( c( corrections, cancellations, reversed.trades, 
                                  reversed.asof, reversals, agency, interdealer, 
                                  further, total),
                               big.mark = ",", scientific = FALSE, 
                               justify = "right", width = 15 )
        cat( "-----------------------------------------------------------------------\n" )
        cat( "      SUMMARY                         \n")
        cat( "--------------------------\n")
        cat( "Found  :", c.stats[1], " corrections\n" )
        cat( "       :", c.stats[2], " cancellations\n" )
        cat( "       :", c.stats[3], " reversed trades\n" )
        cat( "       :", c.stats[4], " reversed as-of trades\n" )
        cat( "       :", c.stats[5], " reversals\n" )
        cat( "       :", c.stats[6], " agency transactions\n" )
        cat( "       :", c.stats[7], " inter-dealer transactions\n" )
        cat( "       :", c.stats[8], " further removals specified in 'excl.further'\n" )
        cat( "Total  :", c.stats[4], " deleted messages\n\n" )
                
        cat( "Total time elapsed: " )
        cat( elapsed, "secs\n" )
        cat( "-----------------------------------------------------------------------\n" )
        return( obj )
    } else {
        cat( "Total time elapsed: " )
        cat( elapsed, " secs\n" )
        return( list( TRACE = obj, stats = n.stats ) )
    }
}

### ------------------------------------------------------------------------
### 2. .raw.cleanUp.<BONDSYMID, CUSIPID>.mmTRACE
### Raw code applying the clean-up algorithm proposed by Dick-Nielsen (2009).
### All duplicates and reversals are deleted from the dataset for the 
### specified companies 'comp' and the specified securities 'sec'. 
### 'excl.outside' indicates if trades outside the market hours and outside
### trading sequence should be deleted as well.
### @param obj              'mmTRACE' object
### @param sec              character vector containing securities (if empty,
###                         ALL)
### @param  comp            character vector containing company symbols (if 
###                         empty, ALL)
### @param  excl.outside    character vector containing exclusion symbols (
###                         either 'A' or 'Z' or both)
### @see ?cleanUp, .check.args.cleanUp.mmTRACE
### ------------------------------------------------------------------------
".volumesummary.BONDSYMID.mmTRACE" <- function(object, from, to, by,
                                               aggregate, crit, quantile,
                                               quantile.crit)
{
    by <- switch(by,
                 "sec"  = quote(list(BOND_SYM_ID)),
                 "comp" = quote(list(COMPANY_SYMBOL, BOND_SYM_ID))
                 )
    setkeyv(object@.Table, cols = c(paste0(by)[-1], "TM_STMP")) 
    ## First create variables
    DT <- object@.Table[, list(SUM = sum(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                               MEAN = mean(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                               MEDIAN = median(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                               STD = sd(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                               MAX = max(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                               MIN = min(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE)
                               ), by = by]
    ## Check for aggregate
    ## 'by' is in case of argument 'byi == "comp"' in the function call
    ## an expression of a list with two elements. Therefore if 'length(by)
    ## > 2' the user chose "comp".   
    if (!missing(aggregate) && eval(length(by)) > 2) {        
        usercall <- expression(do.call(aggregate, list(get(crit), na.rm = TRUE)))
        ## Only for ordering functions remaining variables can be 
        ## carried on. Note though that for median the number of 
        ## could decrease due to medians over odd number of securities
        ## for a certain company. As then no '==' match can be given
        ## 'which' returns NA and the row will be deleted from the
        ## summary.
        ## Further, if more than one match is found all matches are 
        ## returned.
        if (aggregate %in% c("max", "min", "median")) {
            DT <- DT[, .SD[which(get(crit) == eval(usercall))], 
                     by = list(COMPANY_SYMBOL)]
        } else if (aggregate %in% c("mean", "sd", "var")) { 
            cnames <- colnames(DT)[-c(1, 2)]
            clist <- vector("list", length(cnames))
            for (i in 1:length(cnames)) {
                clist[[i]] <- parse(text = paste0(cnames[i], "=", aggregate, "(", 
                                                  cnames[i], ",na.rm = TRUE)"))
            }
            DT <- DT[, lapply(clist, eval, envir = .SD), by = list(COMPANY_SYMBOL)]
            setnames(DT, colnames(DT)[-1], cnames)
        } else {
            expr <- parse(text = paste0(crit, "=", aggregate, "(", crit, ")"))
            DT <- DT[, list(eval(expr, envir = .SD)), by = COMPANY_SYMBOL]
            setnames(DT, "V1", crit)
        }
    } 
    if (!missing(quantile)) {
        breaks <- seq(0, quantile)/quantile
        ## If quantiles are not unique (very often for 'MIN'), they are 
        ## made unique. The result are less quantiles than preferred
        ## by the user. 
        DT[, QUANTILE := cut(get(quantile.crit), unique(quantile(get(quantile.crit), 
                                                          breaks)),
                             include.lowest = TRUE, labels = FALSE)]
    }
    return(DT)
}

".volumesummary.CUSIPID.mmTRACE" <- function(object, from, to, by,
                                               aggregate, crit, quantile,
                                               quantile.crit)
{
    by <- switch(by,
                 "sec"  = quote(list(CUSIP_ID)),
                 "comp" = quote(list(COMPANY_SYMBOL, CUSIP_ID))
                 )
    setkeyv(object@.Table, cols = c(paste0(by)[-1], "TM_STMP")) 
    ## First create variables
    DT <- object@.Table[TM_STMP >= from & TM_STMP < to, 
                        list(SUM = sum(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                             MEAN = mean(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                             MEDIAN = median(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                             STD = sd(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                             MAX = max(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE),
                             MIN = min(as.numeric(ASCII_RPTD_VOL_TX), na.rm = TRUE)
                             ), by = by]
    ## Check for aggregate
    ## 'by' is in case of argument 'byi == "comp"' in the function call
    ## an expression of a list with two elements. Therefore if 'length(by)
    ## > 2' the user chose "comp".   
    if (!missing(aggregate) && eval(length(by)) > 2) {        
        usercall <- expression(do.call(aggregate, list(get(crit), na.rm = TRUE)))
        ## Only for ordering functions remaining variables can be 
        ## carried on. Note though that for median the number of 
        ## could decrease due to medians over odd number of securities
        ## for a certain company. As then no '==' match can be given
        ## 'which' returns NA and the row will be deleted from the
        ## summary.
        ## Further, if more than one match is found all matches are 
        ## returned.
        if (aggregate %in% c("max", "min", "median")) {
            DT <- DT[, .SD[which(get(crit) == eval(usercall))], 
                     by = list(COMPANY_SYMBOL)]
        } else if (aggregate %in% c("mean", "sd", "var")) { 
            cnames <- colnames(DT)[-c(1, 2)]
            clist <- vector("list", length(cnames))
            for (i in 1:length(cnames)) {
                clist[[i]] <- parse(text = paste0(cnames[i], "=", aggregate, "(", 
                                                  cnames[i], ",na.rm = TRUE)"))
            }
            DT <- DT[, lapply(clist, eval, envir = .SD), by = list(COMPANY_SYMBOL)]
            setnames(DT, colnames(DT)[-1], cnames)
        } else {
            expr <- parse(text = paste0(crit, "=", aggregate, "(", crit, ")"))
            DT <- DT[, list(eval(expr, envir = .SD)), by = COMPANY_SYMBOL]
            setnames(DT, "V1", crit)
        }
    } 
    if (!missing(quantile)) {
        breaks <- seq(0, quantile)/quantile
        ## If quantiles are not unique (very often for 'MIN'), they are 
        ## made unique. The result are less quantiles than preferred
        ## by the user. 
        DT[, QUANTILE := cut(get(quantile.crit), unique(quantile(get(quantile.crit), 
                                                          breaks)),
                             include.lowest = TRUE, labels = FALSE)]
    }
    return(DT)
}


















