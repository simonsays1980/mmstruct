\name{getVolumeSummary}
\alias{getVolumeSummary}
\title{
Generate data summary for TRACE trading volumes.
}
\description{
    Generate a data summary for TRACE trading volumes for an \code{mmTRACE} object defined in
    the package \code{mmstruct}. 
}
\usage{
getVolumeSummary(object, from, to, by = c("sec", "comp"), aggregate, 
                 crit = c("SUM", "MEAN", "MEDIAN", "STD", "MAX", "MIN"),
                 quantile, quantile.crit = c("SUM", "MEAN", "MEDIAN", "STD", "MAX", "MIN"))
}

\arguments{
    \item{object}{
        an S4 object of class \code{mmTRACE} defined in the \code{mmstruct} package with TRACE data in 
        slot \code{@.Table}.
}
    \item{from}{
        a \code{POSIXct} object indicating the starting date/time for the date range to consider; defaults
        to slot \code{@.Start} of \code{object}.
}
    \item{to}{
        a \code{POSIXct} object indicating the ending date/time for the date range to consider; defaults
        to slot \code{@.End} of \code{object}; must be greater in time than \code{from}.
}
    \item{by}{
        variable to group by; \code{"sec"} for securities and \code{"comp"} for companies.
}
    \item{aggregate}{
        aggregation function; only evaluated in case \code{by == "comp"}.      
}
    \item{crit}{
        criterion on which the function chosen in argument \code{aggregate} should work on.
}
    \item{quantile}{
        integer value; computes number of quantiles given; if a double value is given \code{as.integer()} 
        is used to round the value.
}
    \item{quantile.crit}{
        criterion on which quantiles should be computed.
}
}
\details{
    This function is an \code{S4} method of the \code{mmTRACE} class in \code{mmstruct}. Slot \code{@.Table}
    of \code{mmTRACE} must be specified. 
    The function returns an \code{data.table} object defined in the package \code{data.table} with 
    a summary about trade volumes (\code{ASCII_RPTD_VOL_TX} in the wrds database) with the following 
    columns: 
    \tabular{rll}{
        [, 1] \tab COMPANY_SYMBOL \tab if \code{by == "comp"} lists the company symbol.\cr
        [, 2] \tab BOND_SYM_ID    \tab if \code{BOND_SYM_ID} is a column name in the TRACE data,\cr
        [, 2] \tab CUSIP_ID       \tab if \code{BOND_SYM_ID} is not a column name, the cusip\cr
        [, 3] \tab SUM            \tab sum of volumes over all days in a bond\cr
        [, 4] \tab MEAN           \tab mean volume over all days in a bond\cr
        [, 5] \tab MEDIAN         \tab median volume among all days in a bond\cr
        [, 6] \tab STD            \tab standard deviation over all days in a bond\cr
        [, 7] \tab MAX            \tab maximum volume among all days of a bond\cr
        [, 8] \tab MIN            \tab minimum volume among all days of a bond
    } 
   Argument \code{aggregate} can be used to aggregate along the bonds for a certain company. Aggregate needs
   a criterion for which the aggregation should be performed. All columns of the regular summary table can 
   be chosen; the default is \code{SUM}. The result will be a \code{data.table} object with the corresponding
   bond symbol in case of rank functions (\code{max,min,median}) and the remaining columns of the summary 
   for a certain company. In case several bonds match all are returned for a company. If a non-ranking 
   function is chosen (e.g. \code{mean, sd, var}) no bond symbol is returned and only the company symbol with
   the aggregated summary statistics are returned. 
   Note, \code{aggregate} is not evaluated in case \code{by == "sec"}.
   Argument \code{quantile} computes quantiles for bonds (\code{by = "sec"}) or companies (\code{by = "comp"})
   for a given criterion in \code{quantile.crit}. \code{quantile.crit} must be one of the general summary 
   columns and defaults to \code{SUM}. 
   }
\value{
    An object of class \code{data.table} defined by the \code{data.table} package.
}

\note{ 
    This function is a class method of the S4 \code{mmTRACE} class defined in the \code{mmstruct} package.
}


\seealso{
\code{\link{data.table}},  \code{\link{as.POSIXct}}, \code{\link{mmTRACE}},  \code{\link{mmstruct}}
}
\examples{
\dontrun{

## Get trading volume summary for TRACE data for all 
## bonds in the sample
getVolumeSummary(trace.object)

## Get a trading volume summary for TRACE data for all
## for all bonds grouped by company
getVolumeSummary(trace.object, by = "comp")

## Get a trading volume summary for companies and aggregate 
## by maximum trading volume
getVolumeSummary(trace.object, by = "comp", quantile = 3, quantile.crit = "SUM")

## Get a trading volume summary for TRACE data for all
## bonds but only within the time range of 2010-01-01 and
## 2010-05-31.
from <- as.POSIXct("2010-01-01", tz = "America/New_York")
to <- as.POSIXct("2010-05-31", tz = "America/New_York")
getVolumeSummary(trace.object, from = from, to = to)
   
}
}
