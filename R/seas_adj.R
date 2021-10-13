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
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "all")
#' seas_adj(series, freq = "month", log.traf = TRUE, method = "firstdiff")
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom seasonal final
#' @importFrom seasonal seas
#' @importFrom tsbox ts_ts
#' @export
seas_adj <- function(series, freq = "month", log.trafo = F, method = "arima"){

  series <- helper_adj(series, log.trafo)

  # Seasonal adjustment with X-13 ARIMA
  if (method == "arima") {
    #Saisonbereinigung mit X-13 ARIMA

    series <- ts_ts(series)

    h <- function(ts){
      m <- x13(ts)
      return(m$final$series[,"sa"])
    }

    if (identical(dim(series), NULL)){
      series <- ts_tbl(h(series))
    }

    if (dim(series)[2] > 1){
      series <- as.list(series)
      series <- lapply(series, h)
      n <- names(series)
      t1 <- series[[1]]
      for (i in 2:length(series)) t1 <- ts_c(t1, series[[i]])
      dimnames(t1)[[2]] <- n
      series <- ts_tbl(t1)
    }

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
