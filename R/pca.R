#' Principal component analysis on Google Trends time series
#' @description \code{pca} computes for several search queries
#' or several categories the principal
#' components of the monthly time series.
#'
#' @param keywords A vector (chr) with search queries (or a single search query).
#' @param categories A vector (num) with Google Trends category numbers.
#' @param geo  A geographical region to restrict the search queries to.
#' @param time A string consisting of a start date
#' and an end date (separated with a space).
#' Check the example for an example.
#'
#' @return Tibble with monthly principal components
#'  next to the actual time series.
#' @examples \dontrun{
#' pca(keywords = c("ikea", "saturn"), time = "2018-01-01 2020-01-01")
#' }
#' @import magrittr tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom gtrendsR gtrends
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom stats prcomp
#' @importFrom tidyselect any_of
#' @importFrom tsbox ts_ts
#' @importFrom lubridate as_date
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @export
pca <- function(keywords = NA,
                categories = 0,
                geo = "DE",
                time = str_c("2006-01-01 ", Sys.Date())) {

  start <- str_sub(time, 1, 10)
  end <- str_sub(time, 12, 21)

  stopifnot("Either choose keywords or categories! Leave the other argument empty" = is.na(keywords) | categories == 0)

  # Check if function is used on the first day of the month
  day <- format(end, format = "%d")
  if (day == "01") {
    # If indeed it is the first day of the month,
    # we need to shorten the dates vector by one month because
    # gtrends data don't include the first day of the month if that's today.
    end <- seq(end, length = 2, by = "-1 months")[2]
  }

  dates <- seq.Date(as.Date(start), as.Date(end), by = "month")
  dat <- tibble::tibble()

  for (kw in keywords) {
    for (cat in categories) {
      temp <-
        tibble::as_tibble(gtrends(
          keyword = kw,
          category = cat,
          geo = geo,
          time = "all"
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

      dat <- bind_rows(dat, temp)
    }
  }

  pc <- bind_cols(date = dates, as_tibble(prcomp(ts_ts(dat))$x))
  dat <- select(pivot_wider(dat, names_from = key, values_from = value), -date)
  result <- bind_cols(pc, dat)

  return(result)
}
