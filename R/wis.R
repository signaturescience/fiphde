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
  med <- value[dplyr::near(quantile, 0.5)]

  if (length(med) > 1L) return(NA)

  wis <- 2 * mean(pmax(
    quantile * (actual_value - value),
    (1 - quantile) * (value - actual_value),
    na.rm = TRUE))

  return(wis)
}

#' @title Score function parameter checker
#' @description Unexported utility function to check parameters of a score function. See [weighted_interval_score].
#' @param quantiles vector of forecasted quantiles
#' @param values vector of forecasted values
#' @param actual_value Actual value.
#' @param id name of the score function, e.g. "weighted_interval_score".
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


#' Calculate WIS score
#'
#' Helper function to calculate weighted interval score (WIS) for prepped forecasts
#'
#' @param .forecasts Tibble with prepped foreacsts
#' @param .test Tibble with test data including observed value for flu admissions stored in "flu.admits" column
#'
#' @return Tibble with the WIS for each combination of epiweek and epiyear
#' @export
#'
wis_score <- function(.forecasts, .test) {
  .forecasts %>%
    dplyr::left_join(.test) %>%
    dplyr::select(epiweek,epiyear,quantile,value,flu.admits) %>%
    dplyr::group_by(epiweek, epiyear) %>%
    dplyr::summarise(wis = fiphde::weighted_interval_score(quantile = quantile, value = value, actual_value = flu.admits))
}

