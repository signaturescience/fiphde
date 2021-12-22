library(tidyverse)
library(fiphde)


# Get data
# the years argument for cdcfluview::ilinet gets the *season* corresponding to the year.
# so, 2019 = the 2019-2020 flu season. If you only get 2020-2021, you're getting the
# 2020-2021 and 2021-2022 flu season, meaning that you will *NOT* capture early 2020 data.
ilidat <- get_cdc_ili(region="national", years=2010:lubridate::year(lubridate::today()))

# Subset to US only
ilidat_us <- ilidat %>% filter(location=="US")

# convenience function
plot_forecast <- function(forecast) {
  forecast$ili_bound %>%
    mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>%
    filter(date>"2021-07-01") %>%
    ggplot(aes(date, ili)) +
    geom_line(alpha=.5, lwd=.2) +
    geom_point(aes(col=forecasted)) +
    theme_bw() +
    labs(x="Date", y="Unweighted ILI", title="ILI forecast",
         subtitle=paste0("Trim date: ", forecast$ilidat %>% mutate(date=cdcfluview::mmwr_week_to_date(epiyear, epiweek)) %>% pull(date) %>% min()))
}

# Forecast ILI
p1 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01", constrained=TRUE) %>% plot_forecast() + ggtitle("Constrained")
p2 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-01-01", constrained=TRUE) %>% plot_forecast() + ggtitle("Constrained")
p3 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-05-01", constrained=TRUE) %>% plot_forecast() + ggtitle("Constrained")
p4 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2015-05-01", constrained=TRUE) %>% plot_forecast() + ggtitle("Constrained")

p5 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-03-01", constrained=FALSE) %>% plot_forecast() + ggtitle("Unconstrained")
p6 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-01-01", constrained=FALSE) %>% plot_forecast() + ggtitle("Unconstrained")
p7 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2020-05-01", constrained=FALSE) %>% plot_forecast() + ggtitle("Unconstrained")
p8 <- forecast_ili(ilidat_us, horizon=4L, trim_date="2015-05-01", constrained=FALSE) %>% plot_forecast() + ggtitle("Unconstrained")

(p1+p5)/(p2+p6)/(p3+p7)/(p4+p8) + plot_layout(guides = "collect")

ggsave("~/Downloads/sensitivity-demo.png", width=8, height=11.5)
