% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{prep_hdgov_hosp}
\alias{prep_hdgov_hosp}
\title{Prep hospitalization data}
\usage{
prep_hdgov_hosp(
  hdgov_hosp,
  statesonly = TRUE,
  trim = list(epiyear = 2020, epiweek = 43),
  remove_incomplete = TRUE,
  min_per_week = 1,
  augment = FALSE,
  augment_stop = "2020-10-18"
)
}
\arguments{
\item{hdgov_hosp}{Daily hospital utilization data from \link{get_hdgov_hosp}}

\item{statesonly}{Logical as to whether or not to limit to US+DC+States only (i.e., drop territories); default is \code{TRUE}}

\item{trim}{Named list with elements for epiyear and epiweek corresponding to the minimum epidemiological week to retain; defaults to \code{list(epiyear=2020, epiweek=43)}, which is the first date of report in the healthdata.gov hospitalization data; if set to \code{NULL} the data will not be trimmed}

\item{remove_incomplete}{Logical as to whether or not to remove the last week if incomplete; default is \code{TRUE}}

\item{min_per_week}{The minimum number of flu.admits per week needed to retain that state. Default removes states with less than 1 flu admission per week over the last 30 days.}

\item{augment}{Logical as to whether or not the data should be augmented with NHSN hospitalizations imputed backwards in time (see 'Details' for more); default is \code{FALSE}}

\item{augment_stop}{Date at which the time series imputation data should stop; yyyy-mm-dd format; only used if "augment" is \code{TRUE} default is \code{"2020-10-18"}}
}
\value{
A \code{tibble} with hospitalization data summarized to epiyear/epiweek with the following columns:
\itemize{
\item \strong{abbreviation}: Abbreviation for the location
\item \strong{location}: FIPS code for the location
\item \strong{week_start}: Date of beginning (Sunday) of the given epidemiological week
\item \strong{monday}: Date of Monday of the given epidemiological week
\item \strong{week_end}: Date of end (Saturday) of the given epidemiological week
\item \strong{epiyear}: Year of reporting (in epidemiological week calendar)
\item \strong{epiweek}: Week of reporting (in epidemiological week calendar)
\item \strong{flu.admits}: Count of flu cases among admitted patients on previous week
\item \strong{flu.admits.cov}: Coverage (number of hospitals reporting) for incident flu cases
\item \strong{ili_mean}: Estimate of historical ILI activity for the given epidemiological week
\item \strong{ili_rank}: Rank of the given epidemiological week in terms of ILI activity across season (1 being highest average activity)
\item \strong{hosp_mean}: Estimate of historical flu hospitalization rate for the given epidemiological week
\item \strong{hosp_rank}: Rank of the given epidemiological week in terms of flu hospitalizations across season (1 being highest average activity)
}
}
\description{
This function prepares hospitalization data retrieved using \link{get_hdgov_hosp} for downstream forecasting. The function optionally limits to states only, trims to a given date, removes incomplete weeks, and removes locations with little reporting over the last month.
}
\details{
The preparation for the weekly flu hospitalization data includes an option to "augment" the input time series. The augmentation is based on an extended time series that was developed with an imputation approach. The extended time series estimates flu hospitalizations at the state-level in years before NHSN reporting became available. If the user decides to include the imputed data, then the time series is extended backwards in time from the "augment_stop" date (defaults to October 18, 2020). The prepended data augmentation is formatted to match the NSHN reporting format. For more details on the data augmentation approach, refer to the publication: \url{https://www.medrxiv.org/content/10.1101/2024.07.31.24311314}.
}
\examples{
\dontrun{
# Retrieve hospitalization data
hdgov_hosp <- get_hdgov_hosp(limitcols=TRUE)
# Prepare and summarize to weekly resolution
h <- prep_hdgov_hosp(hdgov_hosp)
h
}
}
\references{
\url{https://www.medrxiv.org/content/10.1101/2024.07.31.24311314}
}
