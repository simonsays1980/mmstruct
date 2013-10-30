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

### =========================================================================
### Class 'mmSTRUCT' 
### -------------------------------------------------------------------------
setGeneric("getName", function(object) standardGeneric("getName"))

setGeneric("setName<-", function(object, value) standardGeneric("setName<-"))

setGeneric("getCreated", function(object) standardGeneric("getCreated"))

setGeneric("getModified", function(object) standardGeneric("getModified"))

setGeneric("setModified<-", function(object, value) standardGeneric("setModified<-"))

setGeneric("getFinCenter", function(object) standardGeneric("getFinCenter"))

setGeneric("setFinCenter<-", function(object, value) standardGeneric("setFinCenter<-"))

setGeneric("getNROWS", function(object) standardGeneric("getNROWS"))

setGeneric("getNCOLS", function(object) standardGeneric("getNCOLS"))

setGeneric("getTable", function(object) standardGeneric("getTable"))

setGeneric("setTable<-", function(object, value) standardGeneric("setTable<-"))

setGeneric("tableSize", function(object) standardGeneric("tableSize"))

### ==========================================================================
### Class 'mmTRACE'
### --------------------------------------------------------------------------
setGeneric("getStart", function(object) standardGeneric("getStart"))

setGeneric("getEnd", function(object) standardGeneric("getEnd"))

setGeneric("getNSEC", function(object) standardGeneric("getNSEC"))

setGeneric("getNCOMP", function(object) standardGeneric("getNCOMP"))

setGeneric("getCompanies", function(object) standardGeneric("getCompanies"))

setGeneric("getSecurities", function(object, addComp = TRUE) standardGeneric("getSecurities"))

setGeneric("getVolumeSummary", function(object, ...) standardGeneric("getVolumeSummary"))

setGeneric("isCleaned", function(object, verbose = FALSE) standardGeneric("isCleaned"))

setGeneric("setCleaned<-", function(object, value) standardGeneric("setCleaned<-"))

setGeneric("isEnhanced", function(object, verbose = FALSE) standardGeneric("isEnhanced"))

setGeneric("cleanUp", function(object, ...) standardGeneric("cleanUp"))

setGeneric("cleanUpEnhanced", function(object, ...) standardGeneric("cleanUpEnhanced"))

setGeneric("hasTRD_EXCTN_DT", function(object, verbose = FALSE) standardGeneric("hasTRD_EXCTN_DT"))

setGeneric("hasCOMPANY_SYMBOL", function(object, verbose = FALSE) standardGeneric("hasCOMPANY_SYMBOL"))

setGeneric("hasBOND_SYM_ID", function(object, verbose = FALSE) standardGeneric("hasBOND_SYM_ID"))

setGeneric("hasCUSIP_ID", function(object, verbose = FALSE) standardGeneric("hasCUSIP_ID"))

setGeneric("hasTRD_EXCTN_TM", function(object, verbose = FALSE) standardGeneric("hasTRD_EXCTN_TM"))

setGeneric("hasMSG_SEQ_NB", function(object, verbose = FALSE) standardGeneric("hasMSG_SEQ_NB"))

setGeneric("hasORIG_MSG_SEQ_NB", function(object, verbose = FALSE) standardGeneric("hasORIG_MSG_SEQ_NB"))

setGeneric("hasTRC_ST", function(object, verbose = FALSE) standardGeneric("hasTRC_ST"))

setGeneric("hasASOF_CD", function(object, verbose = FALSE) standardGeneric("hasASOF_CD"))

setGeneric("hasSALE_CNDTN_CD", function(object, verbose = FALSE) standardGeneric("hasSALE_CNDTN_CD"))

setGeneric("hasSALE_CNDTN2_CD", function(object, verbose = FALSE) standardGeneric("hasSALE_CNDTN2_CD"))

setGeneric("hasAGU_QSR_ID", function(object, verbose = FALSE) standardGeneric("hasAGU_QSR_ID"))

setGeneric("hasBUY_CPCTY_CD", function(object, verbose = FALSE) standardGeneric("hasBUY_CPCTY_CD"))

setGeneric("hasCMSN_TRD", function(object, verbose = FALSE) standardGeneric("hasCMSN_TRD"))

setGeneric("hasCNTRA_MP_ID", function(object, verbose = FALSE) standardGeneric("hasCNTRA_MP_ID"))

setGeneric("hasDAYS_TO_STTL_CT", function(object, verbose = FALSE) standardGeneric("hasDAYS_TO_STTL_CT"))

setGeneric("hasDISSEM_FL", function(object, verbose = FALSE) standardGeneric("hasDISSEM_FL"))

setGeneric("hasENTRD_VOL_QT", function(object, verbose = FALSE) standardGeneric("hasENTRD_VOL_QT"))

setGeneric("hasRPTD_PR", function(object, verbose = FALSE) standardGeneric("hasRPTD_PR"))

setGeneric("hasRPTD_SIDE_CD", function(object, verbose = FALSE) standardGeneric("hasRPTD_SIDE_CD"))

setGeneric("hasSCRTY_TYPE_CD", function(object, verbose = FALSE) standardGeneric("hasSCRTY_TYPE_CD"))

setGeneric("hasSELL_CPCTY_CD", function(object, verbose = FALSE) standardGeneric("hasSELL_CPCTY_CD"))

setGeneric("hasSPCL_TRD_FL", function(object, verbose = FALSE) standardGeneric("hasSPCL_TRD_FL"))

setGeneric("hasTRDG_MKT_CD", function(object, verbose = FALSE) standardGeneric("hasTRDG_MKT_CD"))

setGeneric("hasTRD_RPT_DT", function(object, verbose = FALSE) standardGeneric("hasTRD_RPT_DT"))

setGeneric("hasTRD_RPT_TM", function(object, verbose = FALSE) standardGeneric("hasTRD_RPT_TM"))

setGeneric("hasWIS_FL", function(object, verbose = FALSE) standardGeneric("hasWIS_FL"))


