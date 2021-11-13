#' Seasonal adjustment
#'
#' @description \code{seas_adj} takes a tibble of
#' time series and returns a tibble with seasonal adjusted values.
#'
#' @param series The input tibble in tidy
#' form with columns \code{time},
#' \code{value} and optional column \code{id}.
#' Monthly or quarterly frequency.
#' @param freq Character "month" or "quarter" for the frequency.
#' @param log.trafo Logical,
#' indicates if value should be transformed to log(value).
#' @param method Character, which method for
#' adjustment should be choosen. See Details.
#' @return Returns a tibble with trend adjusted values and a
#' date column.
#'
#' For method, there can be choosen \code{"firstdiff"} and \code{"arima"}.
#' If \code{"firstdiff"}, first derivatives with \code{lag = 1} are computed.
#' If \code{"arima"}, the X-13ARIMA-SEATS  procedure is used
#' (performed by the [seasonal::seas()] function from the seasonal package).
#'
#' @examples
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "2020-01-01 2021-06-01")
#' seas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")
#'
#' @import dplyr tsbox zoo
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom seasonal seas
#' @importFrom seasonal final
#' @export
seas_adj <- function(series, freq = "month", log.trafo = F, method = "arima"){

  series <- helper_adj(series, log.trafo)

  # Seasonal adjustment with X-13 ARIMA
  if (method == "arima") {

    series <- series %>%
      ts_ts() %>%
      seas(transform.function = "none") %>%
      final() %>%
      ts_tbl()


  } else if (method == "firstdiff"){
    # Seasonal adjustment with first derivates and lag = 4
    # as we use quarterly data. If one does monthly data, set
    # lag = 12.
    if (freq == "month"){
      k = 12
    } else if (freq == "quarter"){
      k = 4
    } else {
      stop("seas_adj(): Please enter 'month' respective 'quarter' for freq.")
    }

    series <- series %>%
      group_by(id) %>%
      mutate(value = c(rep(0,k), diff(value, k))) %>%
      ungroup() %>%
      ts_tbl()
  }

  return(series)

}

