#' @title FIPHDE explorer app launcher
#'
#' @description
#'
#' The explorer app allows a user to view plots of forecasts, inspect tabular output of submission files, and download subsets of forecast submission data. The app includes an interface to interactively select locations to include in the plots, table, and download. This function wraps `shiny::runApp` and accepts arguments for the observed data against which the forecasts should be plotted, as well as the directory containing submission files, both of which are temporarily attached to the global environment for use during the app session. Additional arguments passed to `...` will be inherited by [shiny::runApp].
#'
#' The explorer is meant to review *candidate* submission files. As such, the app is written to expect that submission files in the "submission_dir" argument are named with ".candidate.csv" suffix. For examples of submission files and the naming convention see `system.file("extdata", "submission-example", package = "fiphde")`. Note that submission files can be included for multiple models in model-specific sub-directories.
#'
#' @param .data A `tibble` with historical data for trend leading up to forecast
#' @param submission_dir Full path to directory of submission files containing forecast submissions to explore; submission files in the directory must be named with ".candidate.csv" suffix
#' @param app_dir Full path to directory of explorer app; default is `NULL` and app directory will be resolved from `system.file("app", package="fiphde")`
#' @param ... Additional arguments to be passed to [shiny::runApp]
#'
#' @return This function starts a Shiny app. On exit it removes objects (see ".data" and "submission_dir") that are temporarily attached and used by the app session.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' submission_dir <- system.file("extdata", "submission-example", package = "fiphde")
#' prepped_hosp <-
#'   get_hdgov_hosp(limitcols = TRUE) %>%
#'   prep_hdgov_hosp(statesonly=TRUE, min_per_week = 0, remove_incomplete = TRUE) %>%
#'   dplyr::filter(abbreviation != "DC")
#' fiphde_launcher(.data = prepped_hosp,
#'                 submission_dir = submission_dir,
#'                 host = "0.0.0.0",
#'                 launch.browser = TRUE,
#'                 port = 80)
#' }
fiphde_launcher <- function(.data, submission_dir, app_dir=NULL,...) {

  ## error handling for submission file naming convention expected by app
  if(!length(list.files(submission_dir, pattern = ".candidate.csv$", recursive = TRUE)) > 0) {
    stop(sprintf("None of the files in %s are named with the '.candidate.csv' convention. See ?fiphde_launcher for more information.", submission_dir))
  }
  ## attach these values from the args objects to the global env ...
  ## will be visible to running shiny app
  .GlobalEnv$.data <- .data
  .GlobalEnv$.submission_dir <- submission_dir
  ## side effect to clean up
  on.exit(rm(.submission_dir, .data, envir = .GlobalEnv))
  if(!is.null(app_dir)) {
    shiny::runApp(appDir = app_dir, ... )
  } else {
    shiny::runApp(appDir = system.file("app", package="fiphde"), ... )
  }
}
