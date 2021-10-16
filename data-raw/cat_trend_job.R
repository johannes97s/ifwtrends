# This file is used by Github Actions to automatically do a monthly update
# on the common trend of Google Trends category data.

library(tibble)
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(gtrendsR)
library(usethis)

est_trend <- function() {
  end <- Sys.Date()
  dates <- seq.Date(
    from = as.Date("2006-01-01"),
    to = end,
    by = "month"
  )


  series <- tibble::tibble(date = dates)


  # Creates a sample of 250 Google Trends categories and
  # a fixed category (67 is arbitrary chosen).

  cat_samp <- unique(c(
    sample(gtrendsR::categories$id, 250, replace = FALSE),
    "67"
  ))


  for (i in cat_samp) {

    # Error catcher to catch gtrends errors
    tryCatch(
      {
        g <- gtrendsR::gtrends(
          geo = "DE",
          time = stringr::str_c("2006-01-01 ", end),
          category = i
        )$interest_over_time
      },
      error = function(e) {
        message("Caught an error!")
        print(e)
      },
      warning = function(w) {
        message("Caught an warning!")
        print(w)
      }
    )

    # If we get a valid time series, add this to the result tibble
    if (!is.null(g)) {
      series <- dplyr::bind_cols(series, {{ i }} := g$hits)
    }
  }
  # transform the tibble to a sensible format
  series <- series %>%
    tidyr::pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
    dplyr::mutate(value = log(value)) %>%
    dplyr::arrange(id)

  # calculate the common trend of the time series
  fit <- unname(
    lm(
      value ~ id - 1 + poly(as.numeric(date), 5, raw = T),
      data = series
    )$fitted.values
  )

  # return the common trend of time series category 67
  # (67 is arbitrary chosen, but most categories have nevertheless
  # a very similar trend, so that it doesn't make much of a difference.
  # More importantly,
  # category 67 has always values,
  # so we wont take an empty time series.)
  comtrend <- series %>%
    dplyr::mutate(trend = fit) %>%
    dplyr::filter(id == 67) %>%
    dplyr::select(date, trend)

  return(comtrend)
}


## code to prepare `cat_trend_job` dataset goes here
comtrend <- est_trend()


usethis::use_data(comtrend,
  overwrite = TRUE, internal = TRUE
)
