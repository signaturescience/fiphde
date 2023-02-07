#' Compute weighted interval score
#'
#' This function is adapted from the `evalcast::weighted_interval_score`.
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param quantile Vector of forecasted quantiles
#' @param value Vector of forecasted values
#' @param actual_value Actual value
#'
#' @references <https://github.com/cmu-delphi/covidcast/tree/main/R-packages/evalcast>
#'
#' @export
weighted_interval_score <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "weighted_interval_score")
  if (all(is.na(actual_value))) return(NA)
  actual_value <- unique(actual_value)

  value <- value[!is.na(quantile)]
  quantile <- quantile[!is.na(quantile)]

  med <- value[dplyr::near(quantile, 0.5)]

  if (length(med) > 1L) return(NA)

  wis <- 2 * mean(pmax(
    quantile * (actual_value - value),
    (1 - quantile) * (value - actual_value),
    na.rm = TRUE))

  return(wis)
}

#' @title Score function parameter checker
#' @description
#'
#' This function is adapted from the `evalcast::score_func_param_checker`.
#'
#' Unexported utility function to check parameters of a score function. Used in [weighted_interval_score].
#'
#' @param quantiles Vector of forecasted quantiles
#' @param values Vector of forecasted values
#' @param actual_value Actual value
#' @param id Name of the score function, e.g. "weighted_interval_score"
#' @references <https://github.com/cmu-delphi/covidcast/tree/main/R-packages/evalcast>
#' @return Nothing. Called for side effects.
score_func_param_checker <- function(quantiles, values, actual_value, id = "") {
  id_str = paste0(id, ": ")
  if (length(actual_value) > 1) {
    if (length(actual_value) != length(values)) {
      stop(paste0(id_str, "actual_value must be a scalar or the same length as values"))
    }
    actual_value = unique(actual_value)
  }
  if (length(actual_value) != 1) {
    stop(paste0(id_str, "actual_value must have exactly 1 unique value"))
  }
  if (length(quantiles)!=length(values)) {
    stop(paste0(id_str, "quantiles and values must be of the same length"))
  }
  if (any(duplicated(quantiles))) {
    stop(paste0(id_str, "quantiles must be unique."))
  }
}
