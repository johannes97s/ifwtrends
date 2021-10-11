#' Preparation of Google Trends data
#' @description \code{g_index} downloads for
#' various search requests respective trends categories
#' data andapplies a logarithmic transformation aswell as a
#' seasonal adjustment.
#' The function returns first derivatives (lagged if desired).
#'
#' @param keyword A vector (chr) with search requests.
#' @param category A vector (num) with category numbers from Google Trends.
#' @param geo A geographical region to restrict the search queries to.
#' @param time Time period from where the relative values should be taken
#' (for more information, visit the
#' documentation of \code{\link[trendecon]{ts_gtrends}}).
#' @param lags Number of delays in additional columns (max. value: 4).
#' @return Firstly, each row will be log transformed and
#' seasonal adjusted (via RJDemetra's X-13 ARIMA methods).
#' Furthermore, the first derivatives of these adjusted time
#' series will be returned (optionally with additional columns containing
#' lags).
#' @import magrittr tibble zoo trendecon dplyr tsbox
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom stats prcomp
#' @importFrom tidyselect any_of
#' @importFrom lubridate as_date
#' @importFrom stringr str_c
#' @importFrom gtrendsR gtrends
#' @examples \dontrun{
#' g_index(keyword = c("ikea", "saturn"), time = "2018-01-01 2021-01-01")
#' }
#' @export
g_index <- function(keyword = NA,
                    category = 0,
                    geo = "DE",
                    time = str_c("2006-01-01 ", Sys.Date()),
                    lags = 0) {
  start <- str_sub(time, 1, 10)
  end <- str_sub(time, 12, 21)
  dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")

  # data containing a trend calculated on 200 GTrends time series'.
  fit <- readRDS("data/comtrend.rds") %>%
    select(time = date, trend) %>%
    filter(time >= as.Date(start))

  g_dat2 <- ts_gtrends(
    keyword = keyword,
    category = category,
    geo = "DE",
    time = time
  ) %>%
    mutate(value = log(value)) %>%
    mutate(value = replace(value, value == -Inf, NA_real_)) %>%
    mutate(value = na.approx(value, rule = 2))

  if (!("id" %in% names(g_dat2))) {
    # Add category
    g_dat2 <- mutate(
      g_dat2,
      id = as.character(
        as.vector(sapply(category, rep, length(dates)))
        )
      )
  }

  g_dat_adj <- g_dat2 %>%
    left_join(fit, by = "time") %>%
    mutate(time = as.Date(time), adj = value - trend) %>%
    select(id, time, adj) %>%
    seas_adj(freq = "quarter", method = "arima") %>%
    rename(s_adj = value) %>%
    unique()

  if (!("id" %in% names(g_dat_adj))) {
    g_dat_adj <- mutate(
      g_dat_adj,
      id = as.character(rep(category, each = length(dates)))
      )

  }

  index <- g_dat_adj %>%
    group_by(id)

  # Add lagged columns
  if (lags >= 1) index <- mutate(index, lag_1 = lag(s_adj))
  if (lags >= 2) index <- mutate(index, lag_2 = lag(s_adj, 2))
  if (lags >= 3) index <- mutate(index, lag_3 = lag(s_adj, 3))
  if (lags == 4) index <- mutate(index, lag_4 = lag(s_adj, 4))

  index <- index %>%
    ungroup() %>%
    rename(lag_0 = s_adj) %>%
    filter(across(everything(), ~ !is.na(.))) %>%
    pivot_longer(cols = -c(id, time), names_to = "lag", values_to = "value") %>%
    pivot_wider(names_from = c(id, lag), values_from = value)

  return(index)
}
