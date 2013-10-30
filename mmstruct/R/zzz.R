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
### onLoad settings
### ------------------------------------------------------------------------

### ------------------------------------------------------------------------
### .onLoad
### Sets the global variables for the mmstruct package. 
### @author This function has been taken from the great 'timeDate' package 
###         and the authors of this function are Diethelm Wuertz and Martin 
###         Maechler
### ------------------------------------------------------------------------
.onLoad <- function(libname, pkgname)
{
    # setting mmstruct global variables
    setMmstructOptions(myFinCenter = "GMT", 
                       currentYear = as.POSIXlt(Sys.time())$year + 1900,
                       myUnits     = "days")
    if (!is.numeric(getMmstructOptions("max.print"))) {
        setMmstructOptions(max.print = 100)
    }
}
