#' Simple Google Trends data search
#'
#' \code{gtsearch} returns a simple tibble with the original
#' interest data in a keyword or category from Google Trends.
#'
#' @description \code{gtsearch} is a simple wrapper
#' around the \code{gtrends} function from package. It only
#' returns a time series in form of a tibble with the actual
#' (in relative terms) search volume of keywords or categories.
#'
#' @param keywords A vector (chr) of keywords to search for.
#' @param categories A vector (numeric) of category numbers to search for.
#' @param geo The region to search in.
#' @param timeframe A time frame to search the queries in consisting of
#' a start date and an end date in YYYY-MM-DD form.
#'
#' @return A tibble with a time series of Google Trends search volume from
#' given inputs.
#'
#' @examples
#' gtsearch(keywords = c("pluto", "saturn"), timeframe = "2020-01-01 2020-06-01")
#' @import tibble
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @export
gtsearch <- function(keyword = NA,
                     category = NA,
                     geo = "DE",
                     timeframe = paste("2006-01-01", Sys.Date())) {
  # stopifnot("You can only enter something either in keyword or in category!" = is.na(keyword) | category == 0)

  result <- gtrends(
    keyword = keyword, category = category, geo = geo,
    time = timeframe
  )$interest_over_time

  result <- result %>%
    select(date, hits, keyword, category)

  if (is.na(keyword)) {
    result <- result %>% select(-keyword)
  } else if (is.na(category)) {
    result <- result %>% select(-category)
  }

  return(result)
}
