
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ifwtrends

<!-- badges: start -->

[![Automated update of the common category
trend](https://github.com/johannes97s/ifwtrends/actions/workflows/schedule_category_trends_update.yaml/badge.svg?branch=main)](https://github.com/johannes97s/ifwtrends/actions/workflows/schedule_category_trends_update.yaml)
<!-- badges: end -->

ifwtrends is used to evaluate Google Trends data to support economic
forecasts.

## Installation

You can install the current development version of ifwtrends from
[GitHub](https://github.com/johannes97s/ifwtrends) with:

``` r
# install.packages("remotes")

# If you want the package vignettes to be loaded during installation, use:
remotes::install_github("johannes97s/ifwtrends", build_vignettes = TRUE)

# If not, then use instead:
remotes::install_github("johannes97s/ifwtrends", build_vignettes = FALSE)
```

## Example

If you want to do a principal component analysis (PCA) on a Google
Trends (GTrends or GT) time series, you can do:

``` r
library(ifwtrends)
# Search keywords and do a subsequent PC analysis on a
# GTrends time series 
pca(keywords = c("Pluto", "Saturn"),
    categories = 0,
    geo = "DE",
    time = paste("2020-01-01", Sys.Date()))
#> [time]: 'date'
#> # A tibble: 22 x 5
#>    date          PC1    PC2 Pluto Saturn
#>    <date>      <dbl>  <dbl> <int>  <int>
#>  1 2020-01-01  2.66   1.24      9     56
#>  2 2020-02-01 -3.32   0.115    10     50
#>  3 2020-03-01 -3.34   1.12      9     50
#>  4 2020-04-01  1.72  -1.78     12     55
#>  5 2020-05-01  6.70  -0.684    11     60
#>  6 2020-06-01  7.68   0.336    10     61
#>  7 2020-07-01 -0.302 -0.824    11     53
#>  8 2020-08-01 -1.32   0.155    10     52
#>  9 2020-09-01 -1.32   0.155    10     52
#> 10 2020-10-01  1.68   0.215    10     55
#> # ... with 12 more rows

# Search for a GTrends category and do a subsequent PC
# analysis
pca(keywords = NA,
    categories = c(651),
    geo = "DE",
    time = paste("2020-01-01", Sys.Date()))
#> [time]: 'date'
#> # A tibble: 22 x 3
#>    date          PC1 `651`
#>    <date>      <dbl> <int>
#>  1 2020-01-01 -4.55     35
#>  2 2020-02-01 -4.55     35
#>  3 2020-03-01 -2.55     37
#>  4 2020-04-01 -3.55     36
#>  5 2020-05-01 -2.55     37
#>  6 2020-06-01  1.45     41
#>  7 2020-07-01 -0.545    39
#>  8 2020-08-01 -2.55     37
#>  9 2020-09-01 -0.545    39
#> 10 2020-10-01  1.45     41
#> # ... with 12 more rows
```

Further examples and a detailed introduction can be found in the
(German) vignette.
