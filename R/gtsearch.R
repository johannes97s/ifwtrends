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
#' @param timeframe A time frame to search the queries in.
#'
#' @return A tibble with a time series of Google Trends search volume from
#' given inputs.
#'
#' @examples
#' gtsearch(keywords = c("pluto", "saturn"), timeframe = "2020-01-01 2020-06-01")
#' @import tibble
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom stringr str_c
#' @importFrom tidyselect any_of
#' @export
gtsearch <- function(keywords = NA,
                     categories = 0,
                     geo = "DE",
                     timeframe = paste("2006-01-01", Sys.Date())) {
  stopifnot("You can only enter something either in keywords or in categories!" = is.na(keywords) | categories == 0)


  # Implies that we have 0 or 1 category,
  # so that one can loop through all given
  # (or, in the case that one categories given, not given)
  # keywords.
  if (length(categories) == 1 & length(keywords) > 1) {
    result_list <- vector("list", length = length(keywords))

    for (i in seq_along(keywords)) {

      # Create a temporary result df from the original gtrendsR function.
      temp_result <-
        tibble::as_tibble(gtrends(
          keyword = keywords[i],
          category = categories,
          geo = geo,
          time = timeframe
        )$interest_over_time)

      # Add the temporary df to a list
      # that will be comprehended in the next step
      result_list[[i]] <- temp_result
    }

    # Comprehend the list into a
    # tibble (faster than creating an empty tibble
    # in the beginning)
    result <- bind_rows(result_list) %>%
      select(date, hits, keyword) %>%
      mutate(date = as.Date(date)) %>%
      pivot_wider(names_from = keyword, values_from = hits)


    # Repeat for the vice versa case.
  } else if (length(keywords) == 1 & length(categories) > 1) {
    result_list <- vector("list", length = length(categories))

    for (i in seq_along(categories)) {
      temp_result <-
        tibble::as_tibble(gtrends(
          keyword = keywords[i],
          category = categories,
          geo = geo,
          time = timeframe
        )$interest_over_time)

      result_list[[i]] <- temp_result
    }

    result <- bind_rows(result_list) %>%
      select(date, hits, keyword) %>%
      mutate(date = as.Date(date)) %>%
      pivot_wider(names_from = keyword, values_from = hits)
  }

  return(result)
}
