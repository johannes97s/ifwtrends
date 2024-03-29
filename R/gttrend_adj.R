#' Basic adjustment via helper function
#'
#' This function is a helper function to reduce
#' duplicate code in\code{\link{trend_adj}()}
#' and \code{\link{seas_adj}()}.
#' For further info, see their docstrings.
#'
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#'
#' @return This function returns a tibble.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom tsibble as_tsibble
#' @keywords internal
helper_adj <- function(series, log.trafo = F) {
  if (!("tbl_ts" %in% class(series))) {
    series <- series %>%
      # Use the first character column as a key
      # (because the column name is not known ex ante)
      as_tsibble(
        key = ifelse("id" %in% colnames(series), "id", colnames(series[lapply(series, typeof) == "character"])[1])
      )
  }

  # Log transformation
  if (log.trafo) {
    series <- mutate(series, value = log(value))
  }

  return(series)
}


#' Trend adjustment
#' @description \code{trend_adj} takes a tibble
#'  of time series and returns a tibble with trend adjusted values.
#'
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' Be careful, that when using the method \code{moving_avg}, you can only
#' use a series with one keyword respective category.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#' @param method As trend adjustment method, one can choose
#' between "moving_avg", "firstdiff", and "comtrend." See Details for
#' more information.
#' @return Returns a tibble with trend adjusted values and a
#' date column.
#'
#' For a trend method, there can be choosen between
#' \code{"firstdiff"}, \code{"moving_avg"} and \code{"comtrend"}.
#' If you choose \code{"firstdiff"},
#' first differences with \code{lag = 1} are computed.
#' If you choose the moving average,
#' the time series will be decomposed into its components
#' and the trend will be subtracted
#' from the whole time series (using loess).
#' With \code{"comtrend"},
#' there is a polynom of degree 5
#' with id-fixed effects estimated,
#' which captures the common trend of a sample of categories
#' in Google Trends.
#' The residuals are then used as the adjusted series.
#' For further detail, see Woloszko et al. (2020) and
#' the function [est_trend()].
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @importFrom stats poly
#' @importFrom tsbox ts_ts
#' @importFrom tsibble group_by_key
#' @importFrom stats stl
#' @keywords internal
trend_adj <- function(series, method = "moving_avg", log.trafo = FALSE) {

  # Trend adjustmend with first differences and lag = 1
  if (method == "firstdiff") {
    series <- helper_adj(series, log.trafo)

    trend_adj_series <- series %>%
      group_by_key() %>%
      mutate(value = c(0, diff(value)))
  } else if (method == "moving_avg") {

    # we need a real time series object for applying stl()
    if (!("ts" %in% class(series))) {
      series <- ts_ts(series)
    }

    # compute a trend via decomposing
    # the time series and subtract it afterwards
    trend <- stl(x = series, s.window = "periodic")$time.series[, 2]
    trend_adj_series <- series - trend
  } else if (method == "comtrend") {
    stopifnot("Your input series needs a column 'date' and a column 'value'" = c("date", "value") %in% colnames(series))

    trend <- comtrend

    trend_adj_series <- left_join(x = series, y = trend, by = "date") %>%
      mutate(trend_adj = value - trend) %>%
      select(date, trend_adj)
  }

  return(trend_adj_series)
}

#' Trend adjustment of a Google Trends time series
#'
#' This function can either trend adjust a given time series
#' or create a new time series based on a Google Trends search query
#' and then directly trend adjust it.
#'
#' @param timeseries A already created time
#' series to trend adjust. Preferably already as tsibble or tibble.
#' If another data format is given, it will be coerced to a tsibble.
#' @param keyword A vector (chr) of keywords to search for.
#' @param category A vector (numeric) of category numbers to search for.
#' @param geo The region to search in.
#' @param timeframe A time frame to search the queries in consisting of
#' a start date and an end date in YYYY-MM-DD form.
#' @param log.trafo Logical,
#' indicates if value should be transformed to log(value).
#' @param method As trend adjustment method, one can choose
#' between "moving_avg", "firstdiff", and "comtrend." See Details for
#' more information.
#' @return Returns a tibble with trend adjusted values and a
#' date column.
#'
#' For a trend method, there can be choosen between
#' \code{"firstdiff"}, \code{"moving_avg"} and \code{"comtrend"}.
#' If you choose \code{"firstdiff"},
#' first differences with \code{lag = 1} are computed.
#' If you choose the moving average,
#' the time series will be decomposed into its components
#' and the trend will be subtracted
#' from the whole time series (using loess).
#' With \code{"comtrend"},
#' there is a polynom of degree 5
#' with id-fixed effects estimated,
#' which captures the common trend of a sample of categories
#' in Google Trends.
#' The residuals are then used as the adjusted series.
#' For further detail, see Woloszko et al. (2020) and
#' the function [est_trend()].
#'
#' @examples
#' # Trend adjusting a already established series.
#' series <- trendecon::ts_gtrends("ikea", time = "all")
#' gttrend_adj(series, log.trafo = TRUE, method = "moving_avg")
#'
#' # Search for a new series and trend adjust it
#' gttrend_adj(
#'   category = 179, timeframe = "2015-01-01 2021-01-01",
#'   method = "moving_avg", log.trafo = FALSE
#' )
#' @export
gttrend_adj <- function(timeseries = NULL, keyword = NA, category = NA,
                        geo = "DE", timeframe = paste("2006-01-01", Sys.Date()),
                        method = "moving_avg", log.trafo = FALSE) {

  # If no time series is given,
  # we want to create one with keyword or category
  if (is.null(timeseries)) {

    # Query for the category respective keyword
    if (!is.na(keyword)) {
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

    # Additionally now, trend adjust the new time series
    trend_adj_series <- trend_adj(
      series = series, method = method,
      log.trafo = log.trafo
    )
  } else if (!is.null(timeseries) & ((is.na(keyword) & is.na(category)))) {

    # Trend adjust the new time series
    trend_adj_series <- trend_adj(
      series = timeseries, method = method,
      log.trafo = log.trafo
    )
  }


  return(trend_adj_series)
}
