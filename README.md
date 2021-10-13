
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ifwtrends

<!-- badges: start -->
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
#>  1 2020-01-01  1.70   0.549    12     56
#>  2 2020-02-01 -4.30   0.697    12     50
#>  3 2020-03-01 -6.33  -0.253    11     48
#>  4 2020-04-01  2.74   2.52     14     57
#>  5 2020-05-01  7.74   2.40     14     62
#>  6 2020-06-01  8.74   2.38     14     63
#>  7 2020-07-01  0.672 -0.426    11     55
#>  8 2020-08-01 -0.328 -0.401    11     54
#>  9 2020-09-01  0.672 -0.426    11     55
#> 10 2020-10-01  2.65  -1.47     10     57
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
#>  1 2020-01-01 -5.82     37
#>  2 2020-02-01 -2.82     40
#>  3 2020-03-01 -1.82     41
#>  4 2020-04-01 -3.82     39
#>  5 2020-05-01 -1.82     41
#>  6 2020-06-01  1.18     44
#>  7 2020-07-01 -1.82     41
#>  8 2020-08-01 -2.82     40
#>  9 2020-09-01  0.182    43
#> 10 2020-10-01  2.18     45
#> # ... with 12 more rows
```

Further examples and a detailed introduction can be found in the
(German) vignette.

## Functions and Roadmap

| Function         | Description                                                                      | Status        |
|------------------|----------------------------------------------------------------------------------|---------------|
| `daily_series()` | Uses a complex econometric method to calculate daily data based on monthly data. | 🟢 Implemented |
| `est_trends()`   | Estimates the trend of a Google Trends category.                                 | 🟢 Implemented |
| `factorR2()`     | Calculates and plots the *R*<sup>2</sup> of the time series.                     | 🟢 Implemented |
| `roll()`         | A function to use `pca()` on backtesting.                                        | 🟢 Implemented |
| `pca()`          | A function to do a Principal Component Analysis on GTrends data.                 | 🟢 Implemented |
