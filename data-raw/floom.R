library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)
library(fiphde)

hosp1 <-
  get_nhsn_weekly(endpoint = "https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv") %>%
  prep_nhsn_weekly(., augment = FALSE, adjust_partial = FALSE) %>%
  filter(week_end >= as.Date("2020-10-18") & week_end <= as.Date("2024-04-27")) %>%
  arrange(location, week_end) %>%
  select(abbreviation, location, week_start, monday, week_end, epiyear, epiweek, flu.admits, flu.admits.cov)

hosp2 <-
  get_nhsn_weekly(endpoint = "https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv") %>%
  prep_nhsn_weekly(., augment = FALSE, adjust_partial = TRUE) %>%
  filter(week_end >= as.Date("2024-04-28") & week_end <= as.Date("2024-11-02")) %>%
  arrange(location, week_end) %>%
  select(abbreviation, location, week_start, monday, week_end, epiyear, epiweek, flu.admits, flu.admits.cov)

hosp3 <-
  get_nhsn_weekly(endpoint = "https://data.cdc.gov/api/views/mpgq-jmmr/rows.csv") %>%
  prep_nhsn_weekly(., augment = FALSE, adjust_partial = FALSE) %>%
  filter(week_end >= as.Date("2024-11-03")) %>%
  arrange(location, week_end) %>%
  select(abbreviation, location, week_start, monday, week_end, epiyear, epiweek, flu.admits, flu.admits.cov)

hosp1$epoch <- "2020-10-18 to 2024-04-27"
hosp1$source <- "NHSN API (legacy)"
hosp2$epoch <- "2024-04-28 to 2024-11-02"
hosp2$source <- "Non-seasonal imputation (partially adjusted)"
hosp3$epoch <- "2024-11-03 to Present"
hosp3$source <- "NHSN API (contemporary)"

all_bound <- bind_rows(list(hosp1,hosp2,hosp3))

## plot the bound data (all with partial adjustment in non-required period)
## use the plot helper and plotting steps below to see how the data look with partial adjustment
## this will inform which locations may or may not need alternative imputation
plot_floom <- function(dat,loc) {
  tmp <-
    dat %>%
    dplyr::filter(location == loc)

  tmp %>%
    ggplot(aes(week_start, flu.admits)) +
    geom_point(aes(group = epoch, color = epoch)) +
    geom_line(aes(group = epoch, color = epoch)) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    labs(title = unique(tmp$abbreviation), x = NULL, y = "Flu admits")
}

if(interactive()) {
  p <-
    all_bound$location %>%
    unique(.) %>%
    map(., function(x) plot_floom (dat = all_bound, x))

  ## create pdf with two locations plotted on each page
  pdf_fp1 <- file.path(tempdir(), "floom1.pdf")
  message(sprintf("PDF of plots with partially adjusted data for April - November 2024 gap written to %s", pdf_fp1))
  ggsave(
    filename = pdf_fp1,
    plot = gridExtra::marrangeGrob(p, nrow=2, ncol=1),
    width = 8.5, height = 11
  )
}

## based on the plots above ...
## ... identify locations that need alternative imputation ...
## ... which may be due to gaps or clear issues with data in non-required reporting weeks (even after partial adjustment)
to_ns_impute <- c("01","04","16","25","27","32","33","35","37","38","39","44","46","49","54")

## first build a tibble with locations that have partial adjustment
partial_imputes <-
  all_bound %>%
  filter(!location %in% to_ns_impute)

set.seed(54321)
## then impute other locations using the "val" method
ns_imputes <-
  to_ns_impute %>%
  map_df(., function(x) ns_impute(all_bound, location = x, method = "val", begin_date = "2024-04-28", end_date = "2024-11-02"))

## bind together (some will be ns_imputed some wont)
all_bound_nsimputed <-
  bind_rows(partial_imputes, ns_imputes) %>%
  arrange(location) %>%
  mutate(epoch =
           case_when(
             location %in% to_ns_impute & between(week_start, as.Date("2024-04-27"),as.Date("2024-11-02")) ~ "2024-04-28 to 2024-11-02",
             .default = epoch
           )) %>%
  mutate(source =
           case_when(
             location %in% to_ns_impute & between(week_start, as.Date("2024-04-27"),as.Date("2024-11-02")) ~ "Non-seasonal imputation (value)",
             .default = source
           ))

if(interactive()) {
  p2 <-
    all_bound_nsimputed$location %>%
    unique(.) %>%
    map(., function(x) plot_floom(dat = all_bound_nsimputed, x))

  ## create pdf with two locations plotted on each page
  pdf_fp2 <- file.path(tempdir(), "floom2.pdf")
  message(sprintf("PDF of plots with some partially adjusted data and some 'non-seasonal' imputed for April - November 2024 gap written to %s", pdf_fp2))
  ggsave(
    filename = pdf_fp2,
    plot = gridExtra::marrangeGrob(p2, nrow=2, ncol=1),
    width = 8.5, height = 11
  )
}

## NOTE: the nhsn imputation doesnt include national resolution
national_imputed <-
  fiphde:::nhsn_imputed %>%
  group_by(date) %>%
  summarise(mean_flu_admits = sum(mean_flu_admits),
            year = first(year),
            week = first(week)) %>%
  mutate(location = "US")

nhsn_floom <-
  fiphde:::nhsn_imputed %>%
  bind_rows(., national_imputed) %>%
  select(abbreviation = location, week_start = date, epiyear = year, epiweek = week, flu.admits = mean_flu_admits) %>%
  filter(week_start < as.Date("2020-10-18")) %>%
  mutate(epoch = "2009-01-04 to 2020-10-17") %>%
  mutate(source = "Imputation") %>%
  left_join(., fiphde:::locations %>% select(abbreviation, location)) %>%
  bind_rows(., all_bound_nsimputed) %>%
  ## NOTE: for now just US and states
  filter(!location == "11") %>%
  mutate(week_end = ifelse(is.na(week_end), week_start + 6, week_end)) %>%
  mutate(monday = ifelse(is.na(monday), week_start + 1, week_start)) %>%
  left_join(fiphde:::historical_severity) %>%
  ## ensures no "new" NSHN data (i.e., after reporting became mandatory)
  filter(week_start <= "2024-11-03")

nhsn_floom

write_csv(nhsn_floom, here::here("data-raw/floom.csv"))
