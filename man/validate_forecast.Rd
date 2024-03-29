% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/submit.R
\name{validate_forecast}
\alias{validate_forecast}
\title{Validate forecast submission}
\usage{
validate_forecast(subdat)
}
\arguments{
\item{subdat}{A \code{tibble} with submission ready forecasts prepped by and stored in output of \link{format_for_submission}}
}
\value{
Named list with elements for each test (including logical for whether or not test passed and message if failed) and an overall "valid" logical with \code{TRUE} if all tests passed an \code{FALSE} if at least one failed
}
\description{
This function will take the prepped forecast data from \link{format_for_submission} and run a series of tests to validate the format.
}
\examples{
\dontrun{
# Get raw data from healthdata.gov
h_raw <- get_hdgov_hosp(limitcols=TRUE)

# Prep, and make a tsibble
prepped_hosp <- prep_hdgov_hosp(h_raw, statesonly=TRUE)
prepped_hosp_tsibble <- make_tsibble(prepped_hosp,
                                     epiyear = epiyear,
                                     epiweek=epiweek,
                                     key=location)
# Limit to only Virginia and US
prepped_hosp_tsibble <-
  prepped_hosp_tsibble \%>\%
  dplyr::filter(location \%in\% c("US", "51"))

# Fit a model
hosp_fitfor <- ts_fit_forecast(prepped_hosp_tsibble,
                               horizon=4L,
                               outcome="flu.admits",
                               covariates=TRUE)



# Format for submission
formatted_list <- format_for_submission(hosp_fitfor$tsfor, method = "ts")
# Validate one of the forecasts
# Note that this expects forecast is prepared with forecast date = Monday of the current week
ens_forc <- formatted_list$ensemble
ens_forc$forecast_date <- this_monday()
validate_forecast(ens_forc)
}

}
