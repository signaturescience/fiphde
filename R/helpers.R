## read CSV data from an API with retries for transient failures
read_csv_retry <- function(...) {
  read_csv <- purrr::insistently(
    readr::read_csv,
    ## retries with backoff capped at 5 attempts
    rate = purrr::rate_backoff(
      pause_base = 2,
      pause_cap = 30,
      max_times = 5
    ),
    quiet = FALSE
  )

  read_csv(...)
}
