#' Returns a tibble for backtesting with PCA.
#'
#' @description \code{roll}
#' Returns for the time from start_period to end
#' the respective result from [pca()] back.
#'
#' @param keyword A vector (chr) with search queries (or a single search query).
#' @param category A vector (num) with Google Trends category numbers.
#' @param geo A geographical region to restrict the search queries to.
#' @param start_series Start date of the time series.
#' @param start_period Start date of the returned time frame.
#' @param end End date of time series and to be returned time frame.
#' @param fun Name of a function, that will be applied to a time series.
#' @param ... Additional parameter for
#' the function that is in \code{fun} specified.
#' @return Tibble with monthly data where [pca()] is
#' applied on every column.
#' For each column, a new month is added.
#'
#' @examples
#' roll(keyword = "ikea", start_series = "2018-01-01", start_period = "2019-01-01", end = "2020-01-01")
#' @importFrom stringr str_c
#' @importFrom trendecon ts_gtrends
#' @export
roll <- function(keyword = NA,
                 category = 0,
                 geo = "DE",
                 start_series = "2006-01-01",
                 start_period = "2014-01-01",
                 end = Sys.Date(),
                 fun = trendecon::ts_gtrends,
                 ...) {
  period <- seq.Date(as.Date(start_period), as.Date(end), by = "month")
  dates <- seq.Date(as.Date(start_series), as.Date(end), by = "month")


  f <- function(d) {
    fun(
      keyword = keyword,
      category = category,
      geo = geo,
      time = stringr::str_c(start_series, " ", d),
      ...
    )
  }

  tl <- lapply(period, f)

  return(tl)
}
