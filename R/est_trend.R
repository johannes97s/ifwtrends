#' Estimates the trend of Google search queries
#'
#' @description \code{est_trend} downloads for a sample
#' of 250 random Google Trends categories the relative search volume.
#' Based on this, a common trend is calculated and saved in an
#' internal dataframe the user cannot access but the packages' functions do.
#' However, the user can use this
#' function to calculate the trend for himself.
#' The internal dataframe used in other functions is updated on a
#' monthly basis (on the fifth day of each month) on GitHub.
#'
#' @return Returns a tibble with a
#' time series, which consists of a polynomial trend
#' of degree five of a category.
#'
#' @section Warning:
#' This function takes a few minutes to work and generates
#' a lot of traffic at Google. An IP ban is therefore quite likely.
#' @examples \dontrun{
#' est_trend()
#' }
#' @import tibble gtrendsR rlang
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stats lm
#' @importFrom stats poly
#' @importFrom stringr str_c
#' @export
est_trend <- function() {

  # Set the start and end
  end <- Sys.Date()
  dates <- seq.Date(
    from = as.Date("2006-01-01"),
    to = end,
    by = "month"
  )

  # Creates an empty tibble only with all
  # dates from start to end.
  series <- tibble(date = dates)

  # Creates a sample of 250 Google Trends categories and
  # a fixed category (67 is arbitrary chosen).
  cat_samp <- unique(c(
    sample(gtrendsR::categories$id, 250, replace = FALSE),
    "67"
  ))

  # Generates time series based on the sample categories
  for (i in cat_samp) {

    # Error catcher to catch gtrends errors
    tryCatch(
      {
        g <- gtrends(
          geo = "DE",
          time = str_c("2006-01-01 ", end),
          category = i
        )$interest_over_time
      },
      error = function(e) {
        message("Caught an error!")
        print(e)
      },
      warning = function(w) {
        message("Caught an warning!")
        print(w)
      }
    )

    # If we get a valid time series,
    # add this to the result tibble
    if (!is.null(g)) {
      series <- bind_cols(series, {{ i }} := g$hits)
    }
  }
  # Transform the tibble to a sensible format
  series <- series %>%
    pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
    mutate(value = log(value)) %>%
    arrange(id)

  # Calculate the common trend of the time series
  fit <- unname(
    lm(
      value ~ id - 1 + poly(as.numeric(date), 5, raw = T),
      data = series
    )$fitted.values
  )

  # Return the common trend of time series category 67
  # (67 is arbitrary chosen, but most categories have nevertheless
  # a very similar trend, so that it doesn't make much of a difference.
  # More importantly,
  # category 67 has always values,
  # so we won't get an empty time series.)
  comtrend <- series %>%
    mutate(trend = fit) %>%
    filter(id == 67) %>%
    select(date, trend)

  return(comtrend)
}
