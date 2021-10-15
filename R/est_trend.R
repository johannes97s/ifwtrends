#' Estimates the trend of Google search queries
#'
#' @description \code{est_trend} downloads for a sample
#' of 250 random Google Trends categories the relative search volume.
#' Based on this, a common trend is calculated.
#'
#' @return Returns a time series which consists of a polynomial trend
#' of degree five of a category.
#' @examples \dontrun{
#' est_trend()
#' }
#' @import tibble magrittr gtrendsR
#' @importFrom stringr str_c
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom stats poly
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
est_trend <- function() {
  end <- Sys.Date()
  dates <- seq.Date(
    from = as.Date("2006-01-01"),
    to = end,
    by = "month"
  )

  result <- vector("list", length = 2)

  series <- tibble(date = dates)
  missing <- NULL

  # Creates a sample of 250 Google Trends categories and
  # a fixed category (67 is arbitrary chosen).

  cat_samp <- unique(c(
    sample(gtrendsR::categories$id, 250, replace = FALSE),
    "67"
  ))

  k <- 0

  for (i in cat_samp) {
    Sys.sleep(0.1)

    g <- gtrends(
      geo = "DE",
      time = str_c("2006-01-01 ", end),
      category = i
    )$interest_over_time

    if (is.null(g)) {
      missing <- c(missing, i)
    } else {
      series <- bind_cols(series, !!as.character(eval(i)) := g$hits)
    }

    k <- k + 1

  }

  series <- series %>%
    pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
    mutate(value = log(value)) %>%
    arrange(id)

  result[[1]] <- series

  fit <- unname(
    lm(
      value ~ id - 1 + poly(as.numeric(date), 5, raw = T),
      data = series
    )$fitted.values
  )

  comtrend <- series %>%
    mutate(trend = fit) %>%
    filter(id == 67) %>%
    select(date, trend)

  result[[2]] <- comtrend

  return(result)
}
