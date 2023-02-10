# Tests borrowed from https://github.com/cmu-delphi/covidcast/blob/main/R-packages/evalcast/tests/testthat/test-err-measures.R

# dput(evalcast::covidhub_probs())
covidhub_probs <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
                    0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
test_that("WIS is correct", {
  l <- 1:11
  u <- 23:13
  w <- covidhub_probs[1:11]
  wis <- (sum(w * (u - l) + (l - 10) * (10 < l) + (10 - u) * (10 > u)) + 1) / 11.5
  expect_equal(weighted_interval_score(covidhub_probs, 1:23, 10), wis)
})

test_that("wis works, median only", {
  y <- c(1, -15, 22)
  m <- c(1, 2, 3)
  quantile_probs <- 0.5

  actual <- purrr::map2_dbl(y, m, weighted_interval_score, quantile = .5)
  expected <- abs(y - m)


  expect_identical(actual, expected)
})

test_that("Score parameter checking", {
  expect_invisible(score_func_param_checker(.01, 10, 42, "wis"))
  expect_invisible(score_func_param_checker(c(.01, .02), c(10, 20), 1, "wis"))
  expect_error(score_func_param_checker(c(.01, .01), c(10, 20), 1, "wis"))
})

