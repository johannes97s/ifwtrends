#' Generate a consistent daily time series
#'
#' @description Since Google only provides data for a short time
#' period of less than nine months, one needs to apply tricks
#' to get a longer time series with daily data.
#' \code{daily_series} estimates
#' via the Chow-Lin method a consistent long daily time series based
#' on monthly data.
#'
#' @param keyword A character vector consisting of the
#' search query. As of now, you can only enter one single
#' keyword. Categories are not possible.
#' @param geo A geographical region to restrict the search query to.
#' @param from Start date of the search query.
#'
#' @return Tibble with daily relative search volumes.
#'
#' @section Warning:
#' This function takes a long time and generates a lot of
#' queries at Google. An IP ban is therefore quite likely.
#'
#' @examples \dontrun{
#' daily_series(keyword = "Ikea", geo = "NL", from = "2021-01-01")
#' }
#' @import trendecon tsbox lubridate zoo tibble tempdisagg
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom gtrendsR gtrends
#' @importFrom stats time
#' @export
daily_series <- function(keyword = c("arbeitslos"),
                         geo = "DE",
                         from = "2006-01-01") {

  # Convert date and compute first time frame
  from <- as.Date(from)
  n1 <- as.numeric((Sys.Date() - from - 180) / 15) + 50
  # set n1 to a positive value if it was negativ before
  ifelse(n1 > 0, n1 <- n1, n1 <- 4)

  # Retrieve and transform the gtrends volume
  # for all time frames.

  # start by download daily series
  d <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "15 days", windowsize = "6 months",
    # n_windows calculated such that it reaches up to current date
    n_windows = n1, wait = 20, retry = 10,
    prevent_window_shrinkage = TRUE
  )
  d2 <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    # by: today - 90 days
    from = seq(Sys.Date(), length.out = 2, by = "-90 days")[2],
    stepsize = "1 day", windowsize = "3 months",
    n_windows = 12, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  dd <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(d), trendecon:::aggregate_windows(d2))

  # download weekly series
  n2 <- as.numeric((Sys.Date() - from - 5 * 365) / (11 * 7)) + 10
  ifelse(n2 > 0, n2 <- n2, n2 <- 4)

  w <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "11 weeks", windowsize = "5 years",
    n_windows = n2, wait = 20, retry = 10,
    prevent_window_shrinkage = TRUE
  )
  w2 <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = seq(Sys.Date(), length.out = 2, by = "-1 year")[2],
    stepsize = "1 week", windowsize = "1 year",
    n_windows = 12, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  ww <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(w), trendecon:::aggregate_windows(w2))

  # download monthly series
  n3 <- as.numeric(Sys.Date() - from - 15 * 365) / (30) + 12
  ifelse(n3 > 0, n3 <- n3, n3 <- 4)

  m <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "1 month", windowsize = "15 years",
    n_windows = n3, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  m2 <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "1 month", windowsize = "20 years", ### Hier evtl aufpassen, geht nur bis 2026!
    n_windows = 1, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  mm <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(m), trendecon:::aggregate_windows(m2))

  dd <- select(dd, -n)
  ww <- select(ww, -n)
  mm <- select(mm, -n)

  ww %>%
    mutate(week = lubridate::week(time), year = lubridate::year(time)) %>%
    filter(week <= 52) %>%
    select(time, value) -> ww

  dd <- ts_regular(ts_dts(dd))
  dd$value <- 0.5 * (na.locf(dd$value, fromLast = TRUE) + na.locf(dd$value))


  ww <- ts_regular(ts_dts(ww))
  ww$value <- 0.5 * (na.locf(ww$value, fromLast = TRUE) + na.locf(ww$value))

  mm <- ts_regular(ts_dts(mm))
  mm$value <- 0.5 * (na.locf(mm$value, fromLast = TRUE) + na.locf(mm$value))

  wd <- tempdisagg::td(ww ~ dd, method = "fast", conversion = "mean")
  wd <- stats::predict(wd)

  mwd <- tempdisagg::td(mm ~ wd, method = "fast", conversion = "mean")
  mwd <- stats::predict(mwd)

  return(as_tibble(mwd))
}
