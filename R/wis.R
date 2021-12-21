#' Compute weighted interval score
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param quantile vector of forecasted quantiles
#' @param value vector of forecasted values
#' @param actual_value Actual value.
#'
#' @references <https://cmu-delphi.github.io/covidcast/evalcastR/>.
#'
#' @export
weighted_interval_score <- function(quantile, value, actual_value) {
  score_func_param_checker(quantile, value, actual_value, "weighted_interval_score")
  if (all(is.na(actual_value))) return(NA)
  actual_value <- unique(actual_value)

  value <- value[!is.na(quantile)]
  quantile <- quantile[!is.na(quantile)]

  # per Ryan: WIS is equivalent to quantile loss modulo an extra 0.5 AE term
  # for the median forecast (counted twice).
  #
  # update: WIS is now being redefined to match exactly, still some question
  # about the correct denominator but the formula seems to be  1 / (K + 0.5)
  #
  # Finally, the multiplication by 2 is because alpha_k = 2*quantile_k
  #
  med <- value[find_quantile_match(quantile, 0.5)]

  if (length(med) > 1L) return(NA)

  wis <- 2 * mean(pmax(
    quantile * (actual_value - value),
    (1 - quantile) * (value - actual_value),
    na.rm = TRUE))

  return(wis)
}

find_quantile_match <- function(quantiles, val_to_match, tol=1e-8){
  return(abs(quantiles - val_to_match) < tol  & !is.na(quantiles))
}

score_func_param_checker <- function(quantiles, values, actual_value, id = ""){
  id_str = paste0(id, ": ")
  if (length(actual_value) > 1) {
    assertthat::assert_that(length(actual_value) == length(values),
                            msg = paste0(id_str,
                                         "actual_value must be a scalar or the same length",
                                         " as values"))
    actual_value = unique(actual_value)
  }
  assertthat::assert_that(length(actual_value) == 1,
                          msg = paste0(id_str,
                                       "actual_value must have exactly 1 unique value"))
  assertthat::assert_that(length(quantiles) == length(values),
                          msg = paste0(id_str,
                                       "quantiles and values must be of the same length"))
  assertthat::assert_that(!any(duplicated(quantiles)),
                          msg = paste0(id_str,
                                       "quantiles must be unique."))
}
