
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
# install.packages("devtools")
devtools::install_github("johannes97s/ifwtrends", build_vignettes = TRUE)
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
    start = "2020-01-01",
    end = Sys.Date())
#> [time]: 'date'
#> # A tibble: 22 x 5
#>    date          PC1    PC2 Pluto Saturn
#>    <date>      <dbl>  <dbl> <int>  <int>
#>  1 2020-01-01  1.90   0.544    10     56
#>  2 2020-02-01 -4.10   0.406    10     50
#>  3 2020-03-01 -6.12   1.36      9     48
#>  4 2020-04-01  1.92  -0.456    11     56
#>  5 2020-05-01  6.96  -2.34     13     61
#>  6 2020-06-01  8.94  -1.29     12     63
#>  7 2020-07-01 -1.06  -1.52     12     53
#>  8 2020-08-01  0.897  0.521    10     55
#>  9 2020-09-01 -1.10   0.475    10     53
#> 10 2020-10-01  1.90   0.544    10     56
#> # ... with 12 more rows

# Search for a GTrends category and do a subsequent PC
# analysis
pca(keywords = NA,
    categories = c(651),
    geo = "DE",
    start = "2020-01-01",
    end = Sys.Date())
#> [time]: 'date'
#> # A tibble: 22 x 3
#>    date          PC1 `651`
#>    <date>      <dbl> <int>
#>  1 2020-01-01 -4.18     38
#>  2 2020-02-01 -3.18     39
#>  3 2020-03-01 -1.18     41
#>  4 2020-04-01 -4.18     38
#>  5 2020-05-01 -1.18     41
#>  6 2020-06-01  1.82     44
#>  7 2020-07-01  0.818    43
#>  8 2020-08-01 -1.18     41
#>  9 2020-09-01 -2.18     40
#> 10 2020-10-01  3.82     46
#> # ... with 12 more rows
```

## Functions and Roadmap

| Function         | Description                                                                      | Status        |
|------------------|----------------------------------------------------------------------------------|---------------|
| `daily_series()` | Uses a complex econometric method to calculate daily data based on monthly data. | 🟢 Implemented |
| `est_trends()`   | Estimates the trend of a Google Trends category.                                 | 🟢 Implemented |
| `factorR2()`     | Calculates and plots the *R*<sup>2</sup> of the time series.                     | 🟢 Implemented |
| `roll()`         | A function to use `pca()` on backtesting.                                        | 🟢 Implemented |
| `pca()`          | A function to do a Principal Component Analysis on GTrends data.                 | 🟢 Implemented |
