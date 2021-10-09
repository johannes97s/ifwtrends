#' Simple Google Trends data search
#'
#' \code{gtsearch} returns a simple tibble with the original
#' interest data in a keyword or category from Google Trends.
#'
#' @description \code{gtsearch} is a simple wrapper
#' around the \code{gtrends} function from package. It only
#' returns a time series in form of a tibble with the actual
#' (in relative terms) search volume of keywords or categories
#'
#' @param keywords A vector (chr) of keywords to search for.
#' @param categories A vector (numeric) of category numbers to search for.
#' @param geo The region to search in.
#' @param start Start date of time series.
#' @param end End date of time series.
#'
#' @return A tibble with a time series of Google Trends search volume from
#' given inputs.
#' @examples \dontrun{
#' gtsearch(keywords = c("pluto", "saturn"), end = "2020-01-01")
#' }
#' @import tibble magrittr
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom gtrendsR gtrends
#' @importFrom stringr str_c
#' @importFrom tidyselect any_of
#' @export
gtsearch <- function(keywords = NA,
                     categories = 0,
                     geo = "DE",
                     start = "2006-01-01 CET",
                     end = Sys.Date()) {
  stopifnot("Es darf lediglich entweder was bei keywords oder bei categories eingegeben werden" = is.na(keywords) | categories == 0)

  max_items <- max(length(keywords), length(categories))
  result_list <- vector("list", length = max_items)
  time_frame <- paste(start,end, sep =" ")

  if (length(categories) == 1) {
    for (i in seq_along(keywords)) {
      temp_result <-
        tibble::as_tibble(gtrends(
          keyword = keywords[i],
          category = categories,
          geo = geo,
          time = time_frame
        )$interest_over_time)

      result_list[[i]] <- temp_result
    }
    result <- bind_cols(result_list)
  } else if (length(keywords) == 1) {
    for (i in seq_along(categories)) {
      temp_result <-
        tibble::as_tibble(gtrends(
          keyword = keywords[i],
          category = categories,
          geo = geo,
          time = time_frame
        )$interest_over_time)

      result_list[[i]] <- temp_result
    }
    result <- bind_cols(result_list)
  } else {
    dat <- tibble::tibble()

    for (kw in keywords) {
      for (cat in categories) {
        temp <-
          tibble::as_tibble(gtrends(
            keyword = kw,
            category = cat,
            geo = geo,
            time = time_frame
          )$interest_over_time)

        if (nrow(temp) == 0) {
          stop(str_c("Keine Daten fuer Kategorie ", cat))
        }

        if ("keyword" %in% names(temp)) {
          temp <- select(temp, -category)
        }

        temp <- temp %>%
          mutate(date = as_date(date)) %>%
          select(date, key = any_of(c("keyword", "category")), value = hits) %>%
          filter(date %in% dates)

        result <- bind_rows(dat, temp)
      }
    }
  }
  result$date <- as_date(result$date)
  result <- result[,1:2]
  return(result)
}
