
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ifwtrends

<!-- badges: start -->
<!-- badges: end -->

ifwtrends is used to evaluate Google Trends data for economic forecasts.

## Installation

You can install the current development version of ifwtrends from
[GitHub](https://github.com/johannes97s/ifwtrends) with:

``` r
# install.packages("devtools")
devtools::install_github("johannes97s/ifwtrends", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ifwtrends)
# Search keywords and do a subsequent PC analysis on a
# GTrends time series 
pca(keywords = c("Pluto", "Saturn"),
    categories = 0,
    geo = "DE",
    start = "2006-01-01",
    end = Sys.Date(),
    components = max(length(keywords), length(categories)))
#> [time]: 'date'
#> # A tibble: 190 × 5
#>    date          PC1    PC2 Pluto Saturn
#>    <date>      <dbl>  <dbl> <int>  <int>
#>  1 2006-01-01  -8.90  2.91     11     44
#>  2 2006-02-01 -23.0   0.493     8     30
#>  3 2006-03-01 -20.0   0.368     8     33
#>  4 2006-04-01 -16.0   1.20      9     37
#>  5 2006-05-01 -21.1  -2.59      5     32
#>  6 2006-06-01 -16.0   0.201     8     37
#>  7 2006-07-01 -18.1  -2.71      5     35
#>  8 2006-08-01 -15.6  10.2      18     37
#>  9 2006-09-01 -13.9   4.11     12     39
#> 10 2006-10-01  -3.91  2.70     11     49
#> # … with 180 more rows

# Search for a GTrends category and do a subsequent PC
# analysis
pca(keywords = NA,
    categories = c(651),
    geo = "DE",
    start = "2006-01-01",
    end = Sys.Date(),
    components = max(length(keywords), length(categories)))
#> [time]: 'date'
#> # A tibble: 190 × 3
#>    date         PC1 `651`
#>    <date>     <dbl> <int>
#>  1 2006-01-01  33.6    79
#>  2 2006-02-01  25.6    71
#>  3 2006-03-01  22.6    68
#>  4 2006-04-01  28.6    74
#>  5 2006-05-01  33.6    79
#>  6 2006-06-01  25.6    71
#>  7 2006-07-01  31.6    77
#>  8 2006-08-01  21.6    67
#>  9 2006-09-01  22.6    68
#> 10 2006-10-01  25.6    71
#> # … with 180 more rows
```

## Roadmap

| Function         | Description                                                                      | Status        |
|------------------|----------------------------------------------------------------------------------|---------------|
| `daily_series()` | Uses a complex econometric method to calculate daily data based on monthly data. | 🟢 Implemented |
| `est_trends()`   | Estimates the trend of a Google Trends category.                                 | 🟢 Implemented |
| `factorR2()`     | Calculates and plots the *R*<sup>2</sup> of the time series.                     | 🟢 Implemented |
| `roll()`         | A function to use `pca()` on backtesting.                                        | 🟢 Implemented |
| `pca()`          | A function to do a Principal Component Analysis on GTrends data.                 | 🟢 Implemented |
