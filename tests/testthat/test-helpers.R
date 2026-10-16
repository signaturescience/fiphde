## exercise retries without waiting between attempts
local_fast_backoff <- function(.env = parent.frame()) {
  rate_backoff <- purrr::rate_backoff
  local_mocked_bindings(
    rate_backoff = function(pause_base, pause_cap, max_times) {
      rate_backoff(pause_base = 0, pause_cap = 0, pause_min = 0,
                   max_times = max_times, jitter = FALSE)
    },
    .package = "purrr",
    .env = .env
  )
}

test_that("CSV retries preserve the reader arguments and output", {
  fp <- tempfile(fileext = ".csv")
  on.exit(unlink(fp))
  writeLines(c("value,label", "1,first", "2,second"), fp)

  dat <- read_csv_retry(fp, col_types = "ic", show_col_types = FALSE)

  expect_s3_class(dat, "tbl_df")
  expect_equal(dat$value, c(1L, 2L))
  expect_equal(dat$label, c("first", "second"))
})

test_that("CSV retries recover from transient failures", {
  local_fast_backoff()
  attempts <- 0L
  expected <- tibble::tibble(value = 42)
  local_mocked_bindings(
    read_csv = function(...) {
      attempts <<- attempts + 1L
      if(attempts < 3L) stop("connection reset")
      expected
    },
    .package = "readr"
  )

  expect_message(dat <- read_csv_retry("https://example.com/data.csv"), "connection reset")
  expect_equal(attempts, 3L)
  expect_equal(dat, expected)
})

test_that("CSV retries stop after five attempts and reset for the next call", {
  local_fast_backoff()
  attempts <- 0L
  local_mocked_bindings(
    read_csv = function(...) {
      attempts <<- attempts + 1L
      stop("connection reset")
    },
    .package = "readr"
  )

  expect_error(suppressMessages(read_csv_retry("https://example.com/data.csv")),
               "failed after 5 attempts")
  expect_equal(attempts, 5L)
  expect_error(suppressMessages(read_csv_retry("https://example.com/data.csv")),
               "failed after 5 attempts")
  expect_equal(attempts, 10L)
})

test_that("NHSN retries preserve preliminary and final endpoint formatting", {
  local_fast_backoff()
  expected <- tibble::tibble(
    abbreviation = "US",
    week_end = as.Date("2024-11-09"),
    flu.admits = 100,
    flu.admits.cov = 50,
    flu.admits.cov.perc = 95
  )

  for(endpoint in c("https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv",
                    "https://data.cdc.gov/api/views/ua7e-t2fy/rows.csv")) {
    attempts <- 0L
    local_mocked_bindings(
      read_csv = function(file, ...) {
        expect_equal(file, endpoint)
        attempts <<- attempts + 1L
        if(attempts == 1L) stop("connection reset")
        dat <- expected
        names(dat) <- c("Geographic aggregation", "Week Ending Date",
                        "Total Influenza Admissions",
                        "Number Hospitals Reporting Influenza Admissions",
                        "Percent Hospitals Reporting Influenza Admissions")
        ## preliminary reporting includes extra spaces in two column names
        if(grepl("mpgq-jmmr", file)) {
          names(dat)[4:5] <- c("Number Hospitals Reporting  Influenza Admissions",
                              "Percent Hospitals Reporting  Influenza Admissions")
        }
        dat
      },
      .package = "readr"
    )

    expect_message(dat <- get_nhsn_weekly(endpoint), "connection reset")
    expect_equal(dat, expected)
    expect_equal(attempts, 2L)
  }
})
