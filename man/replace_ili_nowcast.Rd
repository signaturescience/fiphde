% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{replace_ili_nowcast}
\alias{replace_ili_nowcast}
\title{Replace ILInet with nowcast data}
\usage{
replace_ili_nowcast(ilidat, weeks_to_replace = 1)
}
\arguments{
\item{ilidat}{Data from \link{get_cdc_ili}.}

\item{weeks_to_replace}{Number of weeks of \code{ilidat} to replace. Defaults to 2.}
}
\value{
The same as the \code{ilidat} input, but with \code{weeks_to_replace} weeks replaced with nowcasted data.
}
\description{
Replaces \code{weighted_ili} from \link{get_cdc_ili} with nowcast data from \link{get_nowcast_ili} for the number of specified \code{weeks_to_replace}.
}
\examples{
\dontrun{
ilidat <- get_cdc_ili(years=2021)
ilidat <-
  ilidat \%>\%
  dplyr::filter(location=="US" | abbreviation=="VA") \%>\%
  dplyr::group_by(location) \%>\%
  dplyr::slice_max(week_start, n=4) \%>\%
  dplyr::select(location:weighted_ili)
ilidat
iliaug <- replace_ili_nowcast(ilidat, weeks_to_replace=1)
iliaug

# arrange for comparison
ilidat <- ilidat \%>\% dplyr::arrange(location, week_start)
iliaug <- iliaug \%>\% dplyr::arrange(location, week_start)
# Compare US
waldo::compare(ilidat \%>\% dplyr::filter(location=="US"),
               iliaug \%>\% dplyr::filter(location=="US"))
# Compare VA
waldo::compare(ilidat \%>\% dplyr::filter(location=="51"),
               iliaug \%>\% dplyr::filter(location=="51"))
}
}