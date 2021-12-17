#' R squared of the series' regression on factors
#'
#' @description \code{factorR2} returns the \eqn{R^2}
#' of the series' regression on the factors
#' in form of a tibble.
#'
#' @param series A tibble with multiple time series as columns.
#' @param factors A tibble with factors as columns.
#' @param plot A logical indicator whether the function should
#' additionally return a bar plot of the \eqn{R^2}.
#'
#' @return A tibble of \eqn{R^2} from each time series for each factor will be
#' returned.
#' If \code{plot = TRUE}, a plot containing the \eqn{R^2} is returned
#' and displayed additonally.
#'
#' @examples
#' dat <- pca(
#'   keywords = c("Pluto", "Saturn"),
#'   categories = 0,
#'   geo = "DE",
#'   time = paste("2020-01-01", "2020-06-01")
#' )
#'
#' series <- dplyr::select(dat, date, 4:5)
#' factors <- dplyr::select(dat, date, 2:3)
#'
#' factorR2(series, factors, plot = TRUE)
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_c
#' @importFrom stats lm
#' @export
factorR2 <- function(series, factors, plot = F) {
  stopifnot("series must contain leading time column" = class(series[[1]]) == "Date")
  stopifnot("factors must contain leading time column" = class(factors[[1]]) == "Date")

  # An empty list to later catch all data
  R2 <- vector("list", length = (dim(series)[2] - 1))

  # Helper to get the R2
  f <- function(series) {
    s <- summary(lm(factors[-1][[i]] ~ series))
    s$r.squared
  }

  # Apply the helper function throughout both dfs
  for (i in seq_along(factors[-1])) {
    R2[[i]] <- apply(series[-1], 2, f)
  }

  # Combine the results to a tibble
  res <- bind_cols(
    tibble(factors = str_c("PC", 1:length(factors[-1]))),
    bind_rows(R2)
  )

  if (!plot) {
    # Plot is not displayed
    return(res)
  } else {

    # Conversion to a long tibble for creating a ggplot
    pcomp <- pivot_longer(factors, -date, names_to = "series", values_to = "value")
    series <- pivot_longer(series, -date, names_to = "series", values_to = "value")
    r2 <- pivot_longer(res, -factors, names_to = "series", values_to = "R2")

    # ggplot barplot
    plt <- r2 %>%
      # filter(factors == "PC1") %>%
      ggplot(aes(x = series, y = R2)) +
      geom_bar(stat = "identity") +
      facet_wrap(~factors, ncol = 1) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60, size = 6, vjust = 1, hjust = 1)
      ) +
      labs(title = "R squared of the regression on different principal components") +
      scale_y_continuous(breaks = c(0, 0.5, 1))

    return(list(res = res, plot = plt))
  }
}
