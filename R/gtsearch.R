#' Simple Google Trends data search
#'
#' \code{gtsearch} returns a simple tibble with the original
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
#' gtsearch(keywords = c("pluto", "saturn"), timeframe = "2020-01-01 2020-06-01")
#' @import tibble
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom tsibble as_tsibble
#' @importFrom tibble as_tibble
#' @export
gtsearch <- function(keyword = NA,
                     category = NA,
                     geo = "DE",
                     timeframe = paste("2006-01-01", Sys.Date()),
                     as_tbl_ts = TRUE) {
  stopifnot("You can only enter something either in keyword or in category!" = is.na(keyword) | category == 0)

  # check if keyword is still NA,
  # then go by category
  if (anyNA(keyword)) {
    stopifnot("You can only enter one single category number!", length(category) == 1)
    result <- gtrends(
      category = category, geo = geo,
      time = timeframe
    )$interest_over_time %>%
      select(date, hits, category) %>%
      mutate(date = as.Date(date)) %>%
      as_tibble()

    # if wished, tibble can be coerced to a tsibble
    if (as_tbl_ts) {
      result <- result %>%
        as_tsibble(key = category)
    }

  } else if (anyNA(category)) {

    result <- gtrends(
      keyword = keyword, geo = geo,
      time = timeframe
    )$interest_over_time %>%
      select(date, hits, keyword) %>%
      mutate(date = as.Date(date)) %>%
      as_tibble()

      if (as_tbl_ts) {
      result <- result %>%
        as_tsibble(key = keyword)
    }
  }

  return(result)
}
