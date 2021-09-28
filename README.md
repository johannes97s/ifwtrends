
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
devtools::install_github("johannes97s/ifwtrends")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(ifwtrends)
## basic example code
```

## Roadmap

| Function         | Description                                                                      | Status        |
|------------------|----------------------------------------------------------------------------------|---------------|
| `pca()`          | A function to do a Principal Component Analysis on GTrends data.                 | 🟢 Implemented |
| `roll()`         | A function to use `pca()` on backtesting.                                        | 🟢 Implemented |
| `daily_series()` | Uses a complex econometric method to calculate daily data based on monthly data. | 🟢 Implemented |
| `factorR2()`     | A function to calculate and plot the *R*<sup>2</sup> of the time series.         | 🟢 Implemented |
