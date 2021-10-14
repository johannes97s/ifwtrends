#' Basic adjustment via helper function
#'
#' This function is a helper function to reduce
#' duplicate code in\code{\link{trend_adj}()}
#' and \code{\link{seas_adj}()}.
#' For further info, see their docstrings.
#'
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#' @importFrom dplyr mutate
#' @importFrom tsbox ts_tbl
#' @keywords internal
helper_adj <- function(series, log.trafo = F){

  if ("ts" %in% class(series)){
    series <- series %>%
      ts_tbl()
  }

  # Log transformation
  if (log.trafo){
    series <- mutate(series, value = log(value))
  }

  if (!("id" %in% names(series))){
    series <- mutate(series, id = "id")
  }

  return(series)
}


#' Trend adjustment
#' @description \code{trend_adj} takes a tibble
#'  of time series and returns a tibble with trend adjusted values.
#'
#' @param series The input tibble in tidy form
#' with columns \code{time}, \code{value} and optional column \code{id}.
#' @param log.trafo Logical, indicates if value
#' should be transformed to log(value).
#' @param method Character which method for
#' trend adjustment should be choosen. See Details.
#' @return Returns a tibble with trend adjusted values and a
#' date column.
#'
#' For trend_method there can be choosen
#' \code{"firstdiff"} and \code{"comtrend"}.
#' If you choose \code{"firstdiff"},
#' first differences with \code{lag = 1} is executed.
#' Else with \code{"comtrend"},
#' there is a polynom of degree 5
#' with id-fixed effects estimated,
#' which captures the common trend.
#' The residuals where then used as the adjusted series.
#' For further detail, see Woloszko et al. (2020). Attention:
#' This method is not implemented as of the current development state.
#'
#' @examples
#' series <- trendecon::ts_gtrends(c("ikea", "saturn"), time = "all")
#' trend_adj(series, log.trafo = TRUE, method = "firstdiff")
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom magrittr %>%
#' @importFrom stats lm
#' @importFrom stats poly
#' @export
trend_adj <- function(series, log.trafo = F, method = "firstdiff"){

  series <- helper_adj(series, log.trafo)

  # Trend adjustmend with first differences and lag = 1
  if(method == "firstdiff"){

    series <- series %>%
      group_by(id) %>%
      mutate(value = c(0, diff(value))) %>%
      ungroup()

  } else if(method == "comtrend"){
    # Trend adjustmented as mentionend in Paper (which??)
    stop("trend_adj(): method 'comtrend' is not implemented as of now. Please use 'firstdiff' instead.")
    # As of now, this method fails to be correctly implemented.
    if (("id" %in% names(series))){
      fit <- lm(value ~ id -1 +poly(as.numeric(time), 3, raw = T), data = series)
    } else {
      fit <- lm(value ~ +poly(as.numeric(time), 3, raw = T), data = series)
    }

    series <- mutate(series, value = fit$residuals)
  }

  return(series)
}

