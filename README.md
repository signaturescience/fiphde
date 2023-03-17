
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fiphde

<!-- badges: start -->

[![R-CMD-check](https://github.com/signaturescience/fiphde/workflows/R-CMD-check/badge.svg)](https://github.com/signaturescience/fiphde/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/fiphde)](https://CRAN.R-project.org/package=fiphde)
<!-- badges: end -->

FIPHDE: **F**orecasting **I**nfluenza in Support of **P**ublic
**H**ealth **DE**cision Making

## Motivation

`fiphde` was originally created to operationally forecast influenza
hospitalizations during the 2021-22 and 2022-23 seasons of the [FluSight
challenge](https://github.com/cdcepi/Flusight-forecast-data). The
package includes functions for data retrieval, modeling, near-term
forecasting, and forecast summarization. Functionality from `fiphde` has
been implemented for other infectious disease forecasting, including the
2022-23 [DoD COVID-like illness forecasting
challenge](https://github.com/cdcepi/DoD-CLI-forecast-data).

## Installation

To install `fiphde` from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("signaturescience/fiphde", build_vignettes = TRUE)
```

## Usage

To get started, see the [package
vignette](https://signaturescience.github.io/fiphde/articles/basic_usage.html):

``` r
vignette("basic_usage", package="fiphde")
```
