#' Helper function to add ID column
#'
#' This function checks if the dataframe from Google
#' contains a column "id". If there is no said column,
#' this function adds one with the fitting ID from Google.
#'
#' @param data A tibble or dataframe containing a Google Trends
#' time series.
#' @param keyword The keyword or keywords that have been searched in
#' the prior function.
#' @param category The category ID from Google Trends.
#' @return A tibble with an added ID column, if there was no beforehand.
#' Otherwise, the original data will be returned.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tidyselect everything
#' @keywords internal
add_id_column <- function(data, keyword, category){

  if (!("id" %in%  colnames(data))) {
    # Case if a single keyword are used
    if (length(keyword == 1) & category == 0) {
      data <- mutate(
        data, id = keyword
      ) %>%
        select(id, everything())
    } else {
      # Reformulate the category id into its name
      data <- mutate(
        data,
        id = as.character(
          gtrendsR::categories[gtrendsR::categories$id == category, 1]
        )) %>%
        select(id, everything())
    }
    # If more than one keyword is used, the ID
    # column will be automatically added by the
    # search function before.
  }
  return(data)
}

#' Preparation of Google Trends data
#'
#' @description \code{gtpreparation} downloads for
#' various search requests respective categories
#' data and applies a logarithmic transformation aswell as a
#' seasonal adjustment on downloaded data.
#' The function returns seasonal adjusted
#' first derivatives (lagged if desired).
#'
#' @param keyword A character vector with search requests.
#' @param category A numerical category ID from Google Trends. As of now,
#' only one category can be given. If you need to use more categories,
#' the use of a \code{for}-loop is recommended.
#' @param geo A geographical region to restrict the search queries to.
#' @param time Time period from where the relative values should be taken
#' (for more information, visit the
#' documentation of \code{\link[trendecon]{ts_gtrends}}). Attention:
#' As this function will only work on monthly data, you need to enter
#' for time a time frame that is longer than 5 years.
#' Otherwise, the time series from Google Trends will be based on
#' weekly or daily data and cannot be evaluated.
#' @param lags Number of delays in additional columns (max. value: 4).
#' Be careful, the first \code{i} months will not be returned
#' with \code{i} being the number of lags.
#'
#' @return Firstly, each row will be log transformed and
#' seasonal adjusted (via [seasonal::seas()]'s X-13 ARIMA methods).
#' Furthermore, the first derivatives of these adjusted time
#' series will be returned
#' (optionally with additional columns containing
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
#' gtpreparation(keyword = "ikea", time = "2015-01-01 2021-01-01")
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
    mutate(value = na.approx(value, rule = 2)) %>%
    add_id_column(keyword, category)


  # Trend adjust and seasonal adjust data
  adjusted_data <- google_data %>%
    full_join(fit, by = "time") %>%
    mutate(time = as.Date(time), adj = value - trend) %>%
    select(id, time, adj) %>%
    seas_adj(method = "arima") %>%
    rename(s_adj = value)


  # group by category
  grouped_data <- adjusted_data %>%
    group_by_key() %>%
    add_id_column(keyword, category)


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

  if (!("id" %in% names(result))) {
    # Reformulate the category id into its name
    result <- mutate(
      result,
      id = as.character(
        gtrendsR::categories[gtrendsR::categories$id == category, 1]
      ))
  }

  return(result)
}
