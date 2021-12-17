#' Seasonal adjustment helper function
#'
#' @description \code{seas_adj} takes a tibble of
#' time series and returns a tibble with seasonal adjusted values.
#'
#' @param series The input tibble in tidy
#' form with columns \code{time},
#' \code{value} and optional column \code{id}.
#' Monthly or quarterly frequency.
#' @param log.trafo Logical,
#' indicates if value should be transformed to log(value).
#' @param method Character, which method for
#' adjustment should be choosen. See Details.
#' @return Returns a tibble with seasonal adjusted values and a
#' date column.
#'
#' For method, there can be choosen \code{"firstdiff"} and \code{"arima"}.
#' If \code{"firstdiff"}, first derivatives with \code{lag = 1} are computed.
#' If \code{"arima"}, the X-13ARIMA-SEATS  procedure is used
#' (performed by [seasonal::seas()] from the seasonal package).
#'
#' @examples
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2020-01-01 2021-06-01")
#' gtseas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")
#' @import dplyr zoo
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom seasonal seas
#' @importFrom seasonal final
#' @importFrom tsbox ts_ts
#' @importFrom tsbox ts_tsibble
#' @importFrom tsibble group_by_key
#' @keywords internal
seas_adj <- function(series, freq = "month", log.trafo = F, method = "arima") {
  series <- helper_adj(series, log.trafo)

  # Seasonal adjustment with X-13 ARIMA
  if (method == "arima") {
    series <- series %>%
      ts_ts() %>%
      seas(transform.function = "none") %>%
      final() %>%
      ts_tsibble()
  } else if (method == "firstdiff") {
    # Seasonal adjustment with first derivates and lag = 4
    # as we use quarterly data. If one does monthly data, set
    # lag = 12.
    if (freq == "month") {
      k <- 12
    } else if (freq == "quarter") {
      k <- 4
    } else {
      stop("seas_adj(): Please enter 'month' respective 'quarter' for freq.")
    }

    # compute the first differences according to month/quarter.
    series <- series %>%
      group_by_key() %>%
      mutate(value = c(rep(0, k), diff(value, k)))
  }

  return(series)
}


#' Seasonal adjustment of a Google Trends time series
#'
#' This function can either seasonal adjust a given time series
#' or create a new time series based on a Google Trends search query
#' and then directly seasonally adjust it.
#'
#' @param timeseries A already created time
#' series to seasonal adjust. Preferably already as tsibble or tibble.
#' If another data format is given, it will be coerced to a tsibble.
#' @param keyword A vector (chr) of keywords to search for.
#' @param category A vector (numeric) of category numbers to search for.
#' @param geo The region to search in.
#' @param timeframe A time frame to search the queries in consisting of
#' a start date and an end date in YYYY-MM-DD form.
#' @param freq Character "month" or "quarter" for the frequency.
#' @param log.trafo Logical,
#' indicates if value should be transformed to log(value).
#' @param method Character, which method for
#' adjustment should be choosen. See Details.
#' @return Returns a tsibble with sesonal adjusted values and a
#' date column. Any key column will be lost. Therefore, you should only
#' to this with single time series with one keyword or category.
#'
#' @section Methods:
#' With the seasonal adjustment method,
#' a choice can be made between \code{"firstdiff"} and \code{"arima"}.
#' If \code{"firstdiff"}, first derivatives with \code{lag = 1} are computed.
#' If \code{"arima"}, the X-13ARIMA-SEATS  procedure is used
#' (performed by [seasonal::seas()] from the seasonal package).
#'
#' @examples
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2020-01-01 2021-06-01")
#' gtseas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")
#'
#' gtseas_adj(category = 179, timeframe = "2015-01-01 2021-01-01", method = "arima")
#' @export
gtseas_adj <- function(timeseries = NULL, keyword = NA, category = NA,
                       geo = "DE", timeframe = paste("2006-01-01", Sys.Date()),
                       method = "arima", freq = "monthly", log.trafo = FALSE) {

  # If no time series is given,
  # we want to create one with keyword or category
  if (is.null(timeseries)) {

    # Query for the category respective keyword
    if (all(!is.na(keyword))) {
      series <- gtsearch(
        keyword = keyword, geo = geo,
        timeframe = timeframe
      )
    } else {
      series <- gtsearch(
        category = category, geo = geo,
        timeframe = timeframe
      )
    }

    # Additionally now, seasonal adjust the new time series
    seas_adj_series <- seas_adj(
      series = series, freq = freq,
      log.trafo = log.trafo, method = method
    )
  } else if (!is.null(timeseries) & ((is.na(keyword) & is.na(category)))) {

    # Seasonal adjust the new time series
    seas_adj_series <- seas_adj(
      series = timeseries, freq = freq,
      log.trafo = log.trafo, method = method
    )
  }

  return(seas_adj_series)
}
