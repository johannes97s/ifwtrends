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
ser_adj <- function(series, trend = T, seas = T, trend_method = "firstdiff", seas_method = "firstdiff"){
  series <- mutate(series, value = log(value)) #Log-Trafo
  if(seas && seas_method == "arima"){          #Saisonbereinigung mit X-13 ARIMA
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- as.list(ts_ts(series))
    series <- ts_tbl(final(seas(series)))
  }
  if(trend && trend_method == "firstdiff"){   #Trendbereinigung mit ersten Differenzen mit lag = 1
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(0, diff(value))) %>%
      ungroup()
  }
  if(trend && trend_method == "comtrend"){    #Trendbereinigung mit Polynom wie in Paper
                                              #Noch nicht richtig implementier!!
    if (("id" %in% names(series))) fit <- lm(value ~ id -1 +poly(as.numeric(time), 3, raw = T), data = series)
    else fit <- lm(value ~ +poly(as.numeric(time), 3, raw = T), data = series)
    series <- mutate(series, value = fit$residuals)
  }
  if(seas && seas_method == "firstdiff"){     #Saisonbereinigung mit ersten Differenzen mit lag = 4
                                              #da gerade Quartalsdaten. Fuer monatsdaten lag = 12
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(rep(0,4), diff(value, 4))) #Wenn Monatsdaten hier 12 statt 4
  }
  series
}


