#' FIPHDE explorer app launcher
#'
#' @description
#'
#' The explorer app allows a user to view plots of forecasts, inspect tabular output of submission files, and download subsets of forecast submission data. The app includes an interface to interactively select locations to include in the plots, table, and download. This function wraps `shiny::runApp` and accepts arguments for the data against which the forecasts should be plotted, as well as the directory containing submission files, both of which are temporarily attached to the global environment for use during the app session. Additional arguments passed to `...` will be inherited by \link[shiny]{runApp}.
#'
#' @param .data Tibble with historical data for trend leading up to forecast
#' @param submission_dir Full path to directory of submission files containing forecast submissions to explore
#' @param app_dir Full path to directory of explorer app; default is `NULL` and app directory will be resolved from `system.file("app", package="fiphde")`
#' @param ... Additional arguments to be passed to \link[shiny]{runApp}
#'
#' @return This function starts a shiny app. On exit it removes objects (see ".data" and "submission_dir") that are temporarily attached and used by the app session.
#'
#' @export
#' @md
#'
fiphde_launcher <- function(.data, submission_dir, app_dir=NULL,...) {
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
