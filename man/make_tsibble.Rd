% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{make_tsibble}
\alias{make_tsibble}
\title{Make \code{tsibble}}
\usage{
make_tsibble(df, epiyear, epiweek, key = location)
}
\arguments{
\item{df}{A \code{tibble} containing columns \code{epiyear} and \code{epiweek}.}

\item{epiyear}{Unquoted variable name containing the MMWR epiyear.}

\item{epiweek}{Unquoted variable name containing the MMWR epiweek.}

\item{key}{Unquoted variable name containing the name of the column to be the tsibble key. See \link[tsibble:as-tsibble]{tsibble::as_tsibble}.}
}
\value{
A \code{tsibble} containing additional columns \code{monday} indicating the date
for the Monday of that epiweek, and \code{yweek} (a yearweek vctr class object)
that indexes the \code{tsibble} in 1 week increments.
}
\description{
This function converts an input \code{tibble} with columns for \link[lubridate:year]{lubridate::epiyear} and \link[lubridate:week]{lubridate::epiweek}  into a \link[tsibble:tsibble]{tsibble::tsibble} object. The \code{tsibble} has columns specifying indices for the time series as well as a date for the Monday of the epiyear/epiweek combination at each row.
}
\examples{
# Create an example tibble
d <- tibble::tibble(epiyear=c(2020, 2020, 2021, 2021),
                    epiweek=c(52, 53, 1, 2),
                    location="US",
                    somedata=101:104)
# Convert to tsibble (keyed time series tibble)
make_tsibble(d, epiyear = epiyear, epiweek=epiweek, key=location)
}
