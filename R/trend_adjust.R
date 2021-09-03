# library(tidyverse)
#
#
# t <- ts_gtrends(c("ikea", "saturn"), time = "all") %>%
#   mutate(value = log(value))
#
# t <- tibble(time = t$time, value = 1:212*0.5+rnorm(212, sd = 6))
#
#
# fd_1 = c(0, diff(t$value))
# fd_12 = c(rep(0,12), diff(fd_1, 12))
#
# t <- bind_cols(t, fd_1 = fd_1, fd_12 = fd_12)
#
#       ggplot(t, aes(x = time, y = value, color = id)) +
#       geom_line() +
#       facet_grid(id ~ ., scales = "free_y") +
#       theme(legend.position = "none")
#
#
# fit2 <- lm(value ~ as.numeric(time), data = t)
#
# t <- bind_cols(t, adj = fit2$residuals)
# fd_adj_1 = c(0, diff(t$adj))
# fd_adj_12 = c(rep(0,12), diff(fd_1, 12))
#
#
#
# t <- bind_cols(t, fd_adj_1 = fd_adj_1, fd_adj_12 = fd_adj_12)
#   pivot_longer(t, cols = -time, names_to = "id", values_to = "hits") %>%
#   ggplot(aes(x = time, y = hits, color = id )) +
#   geom_line() +
#   facet_grid(id ~ ., scales = "free_y") +
#   theme(legend.position = "none")

#'Seasonal and trend adjustment
#'@description \code{ser_adj} Takes a tibble of timeseries and adds columns with seasonal and trend adjustment
#'
#'
#'
#'@param series The input tibble in tidy form with columns \code{id}, \code{time} and \code{value}. Has to be of monthly frequency.
#'@param trend Logical, indicates if trend adjustment should be done.
#'@param seas Logical, indicates if seasonal adjustment should be done.
#'@param trend_method. Character which method for trend adjustment should be choosen. See Details.
#'
#'For trend_method there can be choosen \code{"firstdiff"} and \code{"comtrend"}.
#'If \code{"firstdiff"}, first differences with \code{lag = 1} is executed.
#'If \code{"comtrend"}, there is a polynom of degree 5 with id-Fixed Effects estimated, which captures the common trend. The residuals where then used as the adjusted series. For further Detial see Woloszko et.al. (2020)
#'
#'@example
#'series <- ts_gtrends(c("ikea", "saturn"), time = "all")
#'ser_adj(series, trend = T, trend_method = "firstdiff")
#'@import tidyverse gtrendsR tsbox seasonal zoo
#'@export
ser_adj <- function(series, trend = T, seas = T, trend_method = "firstdiff", seas_method = "firsdiff"){
  series <- mutate(series, value = log(value))
  if(seas && seas_method == "arima"){
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- as.list(ts_ts(series))
    series <- ts_tbl(final(seas(series)))
  }
  if(trend && trend_method == "firstdiff"){
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(0, diff(value))) %>%
      ungroup()
  }
  if(trend && trend_method == "comtrend"){
    if (("id" %in% names(series))) fit <- lm(value ~ id -1 +poly(as.numeric(time), 3, raw = T), data = series)
    else fit <- lm(value ~ +poly(as.numeric(time), 3, raw = T), data = series)
    series <- mutate(series, value = fit$residuals)
  }
  if(seas && seas_method == "firstdiff"){
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(rep(0,4), diff(value, 4)))
  }
  series
}



all <- ts_gtrends(keyword = NA, category = c(651,330), time = "all")

