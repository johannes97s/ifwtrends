
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ifwtrends

<!-- badges: start -->

[![Automated update of the common category
trend](https://github.com/johannes97s/ifwtrends/actions/workflows/schedule_category_trends_update.yaml/badge.svg?branch=main)](https://github.com/johannes97s/ifwtrends/actions/workflows/schedule_category_trends_update.yaml)
<!-- badges: end -->

ifwtrends is used to evaluate Google Trends data to support economic
forecasts.

## Installation

You can install the current version of ifwtrends from
[GitHub](https://github.com/johannes97s/ifwtrends) with:

``` r
# install.packages("remotes")

# If you want the package vignettes to be loaded during installation, use:
remotes::install_github("johannes97s/ifwtrends", build_vignettes = TRUE)

# If not, then use instead:
remotes::install_github("johannes97s/ifwtrends", build_vignettes = FALSE)

# If you want to use the development version, use:
remotes::install_github("johannes97s/ifwtrends@dev", build_vignettes = FALSE)
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
#> # A tibble: 23 x 5
#>    date          PC1    PC2 Pluto Saturn
#>    <date>      <dbl>  <dbl> <int>  <int>
#>  1 2020-01-01  3.14   0.459    12     57
#>  2 2020-02-01 -3.90  -1.40     10     50
#>  3 2020-03-01 -5.90  -1.36     10     48
#>  4 2020-04-01  1.14   0.499    12     55
#>  5 2020-05-01  8.16   1.36     13     62
#>  6 2020-06-01  8.16   1.36     13     62
#>  7 2020-07-01  1.12  -0.501    11     55
#>  8 2020-08-01  0.141  0.519    12     54
#>  9 2020-09-01  0.121 -0.481    11     54
#> 10 2020-10-01  3.10  -1.54     10     57
#> # ... with 13 more rows

# Search for a GTrends category and do a subsequent PC
# analysis
pca(keywords = NA,
    categories = c(651),
    geo = "DE",
    time = paste("2020-01-01", Sys.Date()))
#> [time]: 'date'
#> # A tibble: 23 x 3
#>    date          PC1 `651`
#>    <date>      <dbl> <int>
#>  1 2020-01-01 -4.48     34
#>  2 2020-02-01 -4.48     34
#>  3 2020-03-01 -1.48     37
#>  4 2020-04-01 -4.48     34
#>  5 2020-05-01 -2.48     36
#>  6 2020-06-01  0.522    39
#>  7 2020-07-01  0.522    39
#>  8 2020-08-01 -3.48     35
#>  9 2020-09-01 -1.48     37
#> 10 2020-10-01  2.52     41
#> # ... with 13 more rows
```

Further examples and a detailed introduction can be found in the
(German) vignette.
