test_that("Clinical nowcasting adds weeks without deprecated summarise behavior", {
  clin <- tibble::tibble(
    abbreviation = rep(c("TX", "VA"), each = 5),
    location = rep(c("48", "51"), each = 5),
    week_start = rep(seq(as.Date("2024-12-01"), by = "week", length.out = 5), 2),
    n_positive = c(999, 10, 20, 30, 40, 999, 2, 4, 6, 8),
    total = rep(c(9999, 100, 200, 300, 400), 2)
  ) %>%
    dplyr::mutate(
      epiyear = lubridate::epiyear(week_start),
      epiweek = lubridate::epiweek(week_start),
      p_positive = n_positive / total * 100)

  for(weeks_to_replace in c(1, 2)) {
    expect_warning(nowcast <- clin_nowcast(clin, weeks_to_replace = weeks_to_replace), NA)

    ## retain observed data and append the last-four-week averages for each location
    observed <- dplyr::filter(nowcast, week_start <= as.Date("2024-12-29"))
    ahead <- nowcast %>%
      dplyr::filter(week_start > as.Date("2024-12-29")) %>%
      dplyr::select(location, abbreviation, week_start, epiyear, epiweek,
                    n_positive, total, p_positive)
    expected <- tibble::tibble(
      location = rep(c("48", "51"), each = weeks_to_replace),
      abbreviation = rep(c("TX", "VA"), each = weeks_to_replace),
      week_start = rep(as.Date(c("2025-01-05", "2025-01-12"))[1:weeks_to_replace], 2),
      epiyear = 2025,
      epiweek = rep(c(2, 3)[1:weeks_to_replace], 2),
      n_positive = rep(c(25, 5), each = weeks_to_replace),
      total = 250,
      p_positive = rep(c(10, 2), each = weeks_to_replace)
    )

    expect_equal(observed, clin)
    expect_equal(ahead, expected)
    expect_equal(names(nowcast), names(clin))
    expect_false(dplyr::is_grouped_df(nowcast))
  }
})
