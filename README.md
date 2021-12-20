
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fiphde

<!-- badges: start -->

[![R-CMD-check](https://github.com/signaturescience/fiphde/workflows/R-CMD-check/badge.svg)](https://github.com/signaturescience/fiphde/actions)
<!-- badges: end -->

Forecasting Influenza in Support of Public Health Decision Making

## Installation

You can install the development version of fiphde from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("signaturescience/fiphde")
```

You will need a GitHub personal access token to do so. First, create a
new token at <https://github.com/settings/tokens> after logging in. Copy
this token. Next, in your `~/.Renviron` file, add the following line:

    GITHUB_PAT="paste-in-your-github-pat-from-step-1-here"

The `devtools::install_github()` function automatically tries to
retrieve your token through the `devtools::github_pat()` function, which
defaults to looking at the `GITHUB_PAT` environment variable.

Alternatively, you may directly supply your token in the install
command:

``` r
devtools::install_github("signaturescience/fiphde", auth_token="paste-your-token-here")
```

## Usage

*Work in progress…*
