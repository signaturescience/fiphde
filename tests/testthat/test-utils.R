load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("Utility functions", {
  expect_true(inherits(this_monday(), "Date"))
  expect_true(inherits(is_monday(), "logical"))
  expect_equal(mnz(c(.1, 0, -.2, NA, .3, .4, .0001, -.3, NA, 999)), .0001)
  expect_equal(mnz_replace(c(.1, 0, -.2, NA, .3, 42, .0001, -.3, NA)),
               c(0.1, 1e-04, 1e-04, NA, 0.3, 42, 1e-04, 1e-04, NA))
  expect_equal(mmwr_week_to_date(2020,53), as.Date("2020-12-27"))
  expect_equal(seq(1,100,7) %>% smoothie(n=4, weight=c(1,2,3,4)), 92)
})
