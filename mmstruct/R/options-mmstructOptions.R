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
### Global options for the mmstruct package and functions to manipulate them
### ------------------------------------------------------------------------

.mmstructOptions <- new.env(hash = TRUE)

### ------------------------------------------------------------------------
### setMmstructOptions
### Sets options for the mmstruct package by names.
### @param  ... options names with corresponding values
### @see    
### @author This function has been taken from the great 'timeDate' package
###         and the author of this function is Yohan Chalabi
### ------------------------------------------------------------------------
setMmstructOptions <- function(...)
{
    x <- list(...)
    ## check if argument was a single list
    if (length(x) == 1 && is.list(x[[1]])) {
        x <- x[[1]]
    }
    nm <- names(x)
    if (is.null(nm) || "" %in% nm) {
        stop("all arguments must be named")       
    }
    old <- lapply(nm, function(m) unname(getMmstructOptions(m)))
    names(old) <- nm
    sapply(nm, function(nm) assign(nm, x[[nm]],
                                   envir = .mmstructOptions))
    invisible(old)
}

### ------------------------------------------------------------------------
### getMmstructOptions
### Gets options for the mmstruct package by names.
### @param  x       option or list of options to retrieve
### @param  unset   indicator for options that are not set
### @see    
### @author This function has been taken from the great 'timeDate' package
###         and the author of this function is Yohan Chalabi
### ------------------------------------------------------------------------
getMmstructOptions <- function(x = NULL, unset = "")
{
    if (is.null(x)) {
        x <- ls(all.names = TRUE, envir = .mmstructOptions)        
    }
    unlist(mget(x, envir = .mmstructOptions, mode = "any",
                ifnotfound = as.list(unset)))
}

