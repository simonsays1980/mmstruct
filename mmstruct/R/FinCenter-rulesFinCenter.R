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
### Functions to return DST rules for Financial Centers
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
### rulesFinCenter
### Returns the rules for a certain Financial Center. Rules are stored in the 
### tparse-DaylightSavingTimes.R file for all timezones over the world. 
### @param FinCenter    the Financial Center for which the rulees should be 
###                     returned.
### @see FinCenter-DaylightSavingTimes.R 
### @author This function has been taken from the great 'timeDate' package 
###         and the author of this function is Diethelm Wuertz.
### ------------------------------------------------------------------------
rulesFinCenter <- function(FinCenter = "")
{
    if (FinCenter == "") {
        FinCenter <- getMmstructOptions("myFinCenter")
    }

    # Check:
    if (any(FinCenter %in% c("GMT", "UTC", " "))) {
        stop("There are no DST rules for GMT FinCenter!")
    }

    # Match City:
    fccity <- strsplit(FinCenter, "/")[[1]]
    City <- fccity[length(fccity)]
    fun <- match.fun(City)

    # Return Value:
    fun()
}
