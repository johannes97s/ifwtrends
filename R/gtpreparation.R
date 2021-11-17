#' Preparation of Google Trends data
#'
#' @description \code{gtpreparation} downloads for
#' various search requests respective trends categories
#' data and applies a logarithmic transformation aswell as a
#' seasonal adjustment.
#' The function returns seasonal adjusted
#' first derivatives (lagged if desired).
#'
#' @param keyword A vector (chr) with search requests.
#' @param category A category ID from Google Trends. As of now,
#' only one category can be given. If you need to use more categories,
#' the use of a \code{for}-loop is recommended.
#' @param geo A geographical region to restrict the search queries to.
#' @param time Time period from where the relative values should be taken
#' (for more information, visit the
#' documentation of \code{\link[trendecon]{ts_gtrends}}). Attention:
#' As this function will only work on monthly data, you need to enter
#' for time a time frame that is longer than 5 years.
#' Otherwise, the time series from Google Trends will be based on
#' monthly or daily data and cannot be evaluated.
#' @param lags Number of delays in additional columns (max. value: 4).
#'
#' @return Firstly, each row will be log transformed and
#' seasonal adjusted (via \code{seasonal}'s X-13 ARIMA methods).
#' Furthermore, the first derivatives of these adjusted time
#' series will be returned (optionally with additional columns containing
#' lags).
#'
#' @import tibble zoo dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect any_of
#' @importFrom lubridate as_date
#' @importFrom lubridate ymd
#' @importFrom lubridate years
#' @importFrom stringr str_c
#' @importFrom gtrendsR gtrends
#' @importFrom trendecon ts_gtrends
#' @importFrom tsibble as_tsibble
#' @examples
#' gtpreparation(keyword = "ikea", time = "2020-01-01 2021-01-01")
#' @export
gtpreparation <- function(keyword = NA,
                          category = 0,
                          geo = "DE",
                          time = str_c("2006-01-01 ", Sys.Date()),
                          lags = 0) {

  # some date variables
  start <- str_sub(time, 1, 10)
  end <- str_sub(time, 12, 21)
  dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")

  # Only monthly time series can be used. Hence,
  # anything shorter than 5 years cannot be analysed (as this
  # are weekly/daily time series).
  stopifnot("You need to use a time frame longer than 5 years (otherwise we wont have monthly data)!" =  ymd(end) - ymd(start) > years(5))

  # data containing a trend calculated on 250 GTrends time series'.
  # comtrend is saved as internal data in
  # R/sysdata.rda and is automatically
  # loaded into namespace
  fit <- comtrend %>%
    select(time = date, trend) %>%
    filter(time >= as.Date(start) & time <= as.Date(end))

  # make search queries
  google_data <- ts_gtrends(
    keyword = keyword,
    category = category,
    geo = "DE",
    time = time
  ) %>%
    mutate(value = log(value)) %>%
    mutate(value = replace(value, value == -Inf, NA_real_)) %>%
    mutate(value = na.approx(value, rule = 2))


  # add a id column consisting of the category
  if (!("id" %in% names(google_data))) {
    # Reformulate the category id into its name
    google_data <- mutate(
      google_data,
      id = as.character(
        gtrendsR::categories[gtrendsR::categories$id == category, 1]
      ))
  }

  # Trend adjust and seasonal adjust data
  adjusted_data <- google_data %>%
    full_join(fit, by = "time") %>%
    mutate(time = as.Date(time), adj = value - trend) %>%
    select(id, time, adj) %>%
    gtseas_adj(method = "arima") %>%
    rename(s_adj = value) %>%
    mutate(
      id = as.character(
        gtrendsR::categories[gtrendsR::categories$id == category, 1]
      )
    ) %>%
    as_tsibble(key = id)

  # group by category
  grouped_data <- adjusted_data %>%
    group_by_key()

  # Add lagged columns
  if (lags >= 1) grouped_data <- mutate(grouped_data, lag_1 = lag(s_adj))
  if (lags >= 2) grouped_data <- mutate(grouped_data, lag_2 = lag(s_adj, 2))
  if (lags >= 3) grouped_data <- mutate(grouped_data, lag_3 = lag(s_adj, 3))
  if (lags == 4) grouped_data <- mutate(grouped_data, lag_4 = lag(s_adj, 4))

  # Reorder some stuff
  result <- grouped_data %>%
    ungroup() %>%
    rename(lag_0 = s_adj) %>%
    filter(across(everything(), ~ !is.na(.)))%>%
    pivot_longer(cols = -c(id, time), names_to = "lag", values_to = "value") %>%
    pivot_wider(names_from = lag, values_from = value)

  return(result)
}
