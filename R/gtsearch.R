#' Helper function for gtsearch
#'
#' This is a helper function for [gtsearch()] to
#' catch some errors and to mutate the Google series
#' to our liking
#'
#' @param series A dataframe from [gtrendsR::gtrends()] with Google data.
#' @param is_keyword A logical argument to determine, if we searched beforehand
#' for a keyword (TRUE) or a category (FALSE).
#' @param timeframe A time frame to search the queries in consisting of
#' a start date and an end date in YYYY-MM-DD form.
#' @param as_tbl_ts Logical value to determine if tibble should already
#' be coerced to a tsibble with keyword or category column as key.
#' @importFrom gtrendsR gtrends
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tsibble as_tsibble
#' @importFrom tibble as_tibble
#' @keywords internal
helper_search <- function(series, is_keyword, timeframe, as_tbl_ts = TRUE) {

  # There are some keywords/categories, that don't exist over the whole
  # time. In that case, the gtrendsR function would return simply NULL.
  # This tries to catch that.
  if (is.null(series)) {
    message("No results for given keyword/category could be found. Returning a tibble with zeroes.")
    # download a dummy series that always exist, e.g. "google"
    dummy <- gtrends(
      keyword = "google", category = 0, geo = "DE", time = timeframe, onlyInterest = TRUE
    )$interest_over_time %>%
      as_tibble()

    dummy$hits <- 0
    dummy[,3] <- "no_data"
    series <- dummy
  }

  # Transform the series in a fitting format for us.
  if (is_keyword) {
    result <- series %>%
      select(date, hits, keyword) %>%
      mutate(date = as.Date(date)) %>%
      as_tibble()
  } else {
    result <- series %>%
      select(date, hits, category) %>%
      mutate(date = as.Date(date)) %>%
      as_tibble()
  }

  # Coerce the tibble to a tsibble if wished.
  if (as_tbl_ts) {
    result <- result %>%
      # The third column is the keyword/category column
      as_tsibble(key = 3)
  }

  return(result)
}

#' Simple Google Trends data search
#'
#' [gtsearch()] returns a simple tibble with the original
#' interest data in a keyword or category from Google Trends.
#'
#' @description \code{gtsearch} is a simple wrapper
#' around the [gtrendsR::gtrends()] function. It only
#' returns a time series in form of a tibble with the actual
#' (in relative terms) search volume of keywords or categories.
#'
#' @param keyword A vector (chr) of keywords to search for.
#' @param category A single numeric value stating the category number.
#' To search for multiple categories at once is as of today not possible.
#' Use an \code{for}-loop instead.
#' @param geo The region to search in (e.g. for Germany "DE").
#' @param timeframe A time frame to search the queries in consisting of
#' a start date and an end date in YYYY-MM-DD form.
#' @param as_tbl_ts Logical value to determine if tibble should already
#' be coerced to a tsibble with keyword or category column as key.
#'
#' @return A tsibble with a time series of Google Trends search volume from
#' given inputs and the keyword respective category as key for it.
#'
#' @examples
#' gtsearch(keyword = c("pluto", "saturn"), timeframe = "2020-01-01 2020-06-01")
#' @import tibble
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom tsibble as_tsibble
#' @importFrom tibble as_tibble
#' @export
gtsearch <- function(keyword = NA,
                     category = 0,
                     geo = "DE",
                     timeframe = paste("2006-01-01", Sys.Date()),
                     as_tbl_ts = TRUE) {
  stopifnot("You can only enter something either in keyword or in category!" = is.na(keyword) | category == 0)

  # check if keyword is still NA,
  # then go by category
  if (anyNA(keyword)) {
    stopifnot("You can only enter one single category number!" = length(category) == 1)
    result <- gtrends(
      category = category, geo = geo,
      time = timeframe
    )$interest_over_time %>%
      helper_search(is_keyword = FALSE, timeframe = timeframe, as_tbl_ts = as_tbl_ts)


  } else if (anyNA(category) | category == 0) {
    result <- gtrends(
      keyword = keyword, geo = geo,
      time = timeframe
    )$interest_over_time %>%
      helper_search(is_keyword = TRUE, timeframe = timeframe, as_tbl_ts = as_tbl_ts)

  }

  return(result)
}


