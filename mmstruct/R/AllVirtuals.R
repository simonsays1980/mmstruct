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
### The mmSTRUCT VIRTUAL Class
### ------------------------------------------------------------------------
.mmDATASTRUCT <- setClass("mmDATASTRUCT",
                      representation("VIRTUAL",
                                     .Name      = "character",
                                     .Created   = "POSIXct",
                                     .Modified  = "POSIXct",
                                     .FinCenter = "character",
                                     .NROWS     = "integer",
                                     .NCOLS     = "integer",
                                     .Table     = "data.table"),
                      prototype(.Name       = character(),
                                .Created    = as.POSIXct(Sys.time()),
                                .Modified   = as.POSIXct(Sys.time()),
                                .FinCenter  = "GMT",
                                .NROWS      = as.integer(0),
                                .NCOLS      = as.integer(0),
                                .Table      = data.table()
                                )
)

### ------------------------------------------------------------------------
### Getters and Setters 
### ------------------------------------------------------------------------
setMethod("getName", "mmDATASTRUCT",
          function(object) 
          {
              return(object@.Name)
          }
)

setReplaceMethod("setName", signature(object    = "mmDATASTRUCT", 
                                      value     = "character"),
                 function(object, value) 
                 {
                     object@.Name   <- value
                     return(object)
                 }
)

setMethod("getCreated", "mmDATASTRUCT",
          function(object)
          {
              return(object@.Created)
          }
)
## -
## No setter as this slot should not be manipulated by the user.
## -

setMethod("getModified", "mmDATASTRUCT",
          function(object)
          {
              return(object@.Modified)
          }
)

setReplaceMethod("setModified", signature(object    = "mmDATASTRUCT",
                                          value     = "POSIXct"),
                 function(object, value) 
                 {
                     object@.Modified   <- value
                     return(object)
                 }
)

setMethod("getFinCenter", "mmDATASTRUCT",
          function(object) {
              return(object@.FinCenter)
          }
)

setReplaceMethod("setFinCenter", signature(object  = "mmDATASTRUCT",
                                    value   = "character"),
          function(object, value) {
              object@.FinCenter <- value
              return(object)
          }
)

setMethod("getNROWS", "mmDATASTRUCT",
          function(object)
          {
              return(object@.NROWS)
          }
)
## -
## No setter as this slot should not be manipulated by the user.
## -

setMethod("getNCOLS", "mmDATASTRUCT",
                 function(object)
                 {
                     return(object@.NCOLS)
                 }
)
## - 
## No setter as this slot should not be manipulated by the user.
## -

setMethod("getTable", "mmDATASTRUCT",
          function(object)
          {
              return(object@.Table)
          }
)

setReplaceMethod("setTable", signature(object   = "mmDATASTRUCT",
                                       value    = "data.table"),
                 function(object, value)
                 {
                     object@.Table  <- value
                     return(object)
                 }
)

### ----------------------------------------------------------------------- 
### dim
### Returning the dimensions of the underyling 'data.table' object
### @param  x   An object of class 'mmSTRUCT'.
### @see    ?dim()
### -----------------------------------------------------------------------
setMethod("dim", "mmDATASTRUCT",
          function(x) 
          {
              dim(x.Table)
          }
)

### -----------------------------------------------------------------------
### tableSize
### Returns the size of slot @.Table in megabytes.
### @param  object  An object of class 'mmSTRUCT'.
### @see    ?object.size()
### -----------------------------------------------------------------------
setMethod("tableSize", "mmDATASTRUCT",
          function(object)
          {
              mbytes    <- format(object.size(object@.Table)/1e+6, digits = 2)
              cat("Object holds ", as.numeric(mbytes), " MB data.\n", sep = "")
          }
)
