# This file is used by Github Actions to automatically do a monthly update
# on the common trend of Google Trends category data.

library(tibble)
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(gtrendsR)

est_trend <- function() {
  end <- Sys.Date()
  dates <- seq.Date(
    from = as.Date("2006-01-01"),
    to = end,
    by = "month"
  )


  series <- tibble::tibble(date = dates)
  missing <- NULL

  # Creates a sample of 250 Google Trends categories and
  # a fixed category (67 is arbitrary chosen).

  cat_samp <- unique(c(
    sample(gtrendsR::categories$id, 250, replace = FALSE),
    "67"
  ))

  k <- 0

  for (i in cat_samp) {
    Sys.sleep(0.1)

    g <- gtrendsR::gtrends(
      geo = "DE",
      time = stringr::str_c("2006-01-01 ", end),
      category = i
    )$interest_over_time

    if (is.null(g)) {
      missing <- c(missing, i)
    } else {
      series <- dplyr::bind_cols(series, {{ i }} := g$hits)
    }

    k <- k + 1

  }

  series <- series %>%
    tidyr::pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
    dplyr::mutate(value = log(value)) %>%
    dplyr::arrange(id)

  fit <- unname(
    lm(
      value ~ id - 1 + poly(as.numeric(date), 5, raw = T),
      data = series
    )$fitted.values
  )

  comtrend <- series %>%
    dplyr::mutate(trend = fit) %>%
    dplyr::filter(id == 67) %>%
    dplyr::select(date, trend)



  return(comtrend)
}


## code to prepare `cat_trend_job` dataset goes here
comtrend <- est_trend()


usethis::use_data(comtrend,
                  overwrite = TRUE, internal = TRUE)
