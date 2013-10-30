## Copyright (C) 2013 
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
### Functions to convert and match times
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
### mmstruct.dtformat
### Read dates and times and convert to 'POSIXct'
### ------------------------------------------------------------------------
mmstruct.dtformat <- function(dates, times, FinCenter = "GMT", 
                              format = "YYYYmmdd", type = c("gmt2any",
                                                            "any2gmt")) 
{
    ## convert to seconds since 1970-01-01 00:00:00 UTC
    num <- dtparseV_format_cc(dates, times, format)
    ## convert to base FinCenter
    num.base <- .formatFinCenterNum(num, FinCenter = FinCenter, 
                                    type = type) 
    .POSIXct(num.base, tz = FinCenter)
}

### ------------------------------------------------------------------------
### .formatFinCenterNum
### Converts numerical time format (secs since 1970-01-01 00:00:00 GMT) to 
### other timezones. The time parsing functions have set the GMT (or UTC) 
### time as base. If now empirical times are converted to numerical formats 
### they would always occur as GMT times. To change the base to the actual
### Financial Center's time zone the data comes from a conversion has to be 
### made. This conversion makes use of lists storing the daylight saving 
### times and corresponding offsets (see tparse-DaylightSavingTimes.R). 
### After conversion the '.POSIXct()' function can set the timezone to the 
### appropriate Financial Center's timezone. 
### @param  num         numerical daytime values (seconds since 1970-01-01 
###                     00:00:00 GMT/UTC
### @param  FinCenter   basic timezone (in Abbrevations as e.g. "EST" for 
###                     "Eastern Standard Time" or locations (e.g. 
###                     "America/New_York". 
### @param  type        a character indicating conversion from or to the 
###                     basic timezone defined in argument 'FinCenter'
### @see tparse-DaylightSavingTimes.R, RcppExports.R, ?POSIXct
### @author This function has been taken from the great 'timeDate' package.
###         The authors of this function are Diethelm Wuertz and Yohan Chalabi.
### ---------------------------------------------------------------------
.formatFinCenterNum <- function(num, FinCenter, type = c("gmt2any", "any2gmt"))
{
    if (FinCenter == "GMT" || FinCenter == "UTC") {
        ## No conversion is needed
        return(num)
    } 
    type <- match.arg(type)
    signum <- switch(type, 
                     "gmt2any" = +1,
                     "any2gmt" = -1)
    ## Get the DST list from the database in tparse-DaylightSavingTimes.R
    try <- try(dst.list <- rulesFinCenter(FinCenter), silent = TRUE)
    if (inherits(try, "try-error")) {
        stop(gettextf("'%s' is not a valid FinCenter", FinCenter))
    }
    offsetIdx <- findInterval(num, dst.list$numeric)
    ## consider first DST rule if event occurred before
    offsetIdx[offsetIdx < 1] <- 1

    return(num + signum * dst.list$offSet[offsetIdx])
}
