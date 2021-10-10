#' Basic adjustment via helper function
#' This function is a helper function to reduce duplicate code in the
#' exported functions below. For further info, see their docstrings.
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#' @importFrom dplyr mutate
#' @importFrom tsbox ts_tbl
helper_adj <- function(series, log.trafo = F){

  if ("ts" %in% class(series)){
    series <- series %>%
      ts_tbl()
  }

  # Log transformation
  if (log.trafo){
    series <- mutate(series, value = log(value))
  }

  if (!("id" %in% names(series))){
    series <- mutate(series, id = "id")
  }

  return(series)
}


#' Trend adjustment
#' @description \code{trend_adj} takes a tibble
#'  of time series and returns a tibble with trend adjusted values.
#'
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#' @param method Character which method for
#' trend adjustment should be choosen. See Details.
#' @return Returns a tibble with trend adjusted values and a
#' date column.
#'
#' For trend_method there can be choosen
#' \code{"firstdiff"} and \code{"comtrend"}.
#' If you choose \code{"firstdiff"},
#' first differences with \code{lag = 1} is executed.
#' Else with \code{"comtrend"},
#' there is a polynom of degree 5
#' with id-fixed effects estimated,
#' which captures the common trend.
#' The residuals where then used as the adjusted series.
#' For further detail, see Woloszko et al. (2020). Attention:
#' This method is not implemented as of the current development state.
#'
#' @examples
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "all")
#' trend_adj(series, log.trafo = TRUE, method = "firstdiff")
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @importFrom stats poly
#' @export
trend_adj <- function(series, log.trafo = F, method = "firstdiff"){

  series <- helper_adj(series, log.trafo)

  # Trend adjustmend with first differences and lag = 1
  if(method == "firstdiff"){

    series <- series %>%
      group_by(id) %>%
      mutate(value = c(0, diff(value))) %>%
      ungroup()

  } else if(method == "comtrend"){
   # Trend adjustmented as mentionend in Paper (which??)
    stop("trend_adj(): method 'comtrend' is not implemented as of now. Please use 'firstdiff' instead.")
    # As of now, this method fails to be correctly implemented.
    if (("id" %in% names(series))){
      fit <- lm(value ~ id -1 +poly(as.numeric(time), 3, raw = T), data = series)
    } else {
        fit <- lm(value ~ +poly(as.numeric(time), 3, raw = T), data = series)
    }

    series <- mutate(series, value = fit$residuals)
  }

  return(series)
}

#' Seasonal adjustment
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





