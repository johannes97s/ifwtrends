#'Trend adjustment
#'@description \code{trend_adj} Takes a tibble of timeseries and returns tibble with trend adjusted values.
#'
#'
#'
#'@param series The input tibble in tidy form with columns \code{time}, \code{value} and optional column \code{id}.
#'@param log.trafo Logical, indicates if value should be transformed to log(value).
#'@param method. Character which method for trend adjustment should be choosen. See Details.
#'
#'For trend_method there can be choosen \code{"firstdiff"} and \code{"comtrend"}.
#'If \code{"firstdiff"}, first differences with \code{lag = 1} is executed.
#'If \code{"comtrend"}, there is a polynom of degree 5 with id-Fixed Effects estimated, which captures the common trend. The residuals where then used as the adjusted series. For further Detial see Woloszko et.al. (2020)
#'
#'@example
#'series <- ts_gtrends(c("ikea", "saturn"), time = "all")
#'trend_adj(series, log.trafo = T, method = "firstdiff")
#'@import tidyverse gtrendsR tsbox seasonal zoo
#'@export
trend_adj <- function(series, log.trafo = F, method = "firstdiff"){
  if (log.trafo) series <- mutate(series, value = log(value)) #Log-Trafo
  if(method == "firstdiff"){   #Trendbereinigung mit ersten Differenzen mit lag = 1
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(0, diff(value))) %>%
      ungroup()
  }
  if(method == "comtrend"){    #Trendbereinigung mit Polynom wie in Paper
                                              #Noch nicht richtig implementier!!
    if (("id" %in% names(series))) fit <- lm(value ~ id -1 +poly(as.numeric(time), 3, raw = T), data = series)
    else fit <- lm(value ~ +poly(as.numeric(time), 3, raw = T), data = series)
    series <- mutate(series, value = fit$residuals)
  }
  series
}



#'Seasonal adjustment
#'@description \code{seas_adj} Takes a tibble of timeseries and returns tibble with seasonal adjusted values.
#'
#'@param series The input tibble in tidy form with columns \code{time}, \code{value} and optional column \code{id}. Monthly or quarterly frequency.
#'@param freq Character "month" or "quarter" for the frequency.
#'@param log.trafo Logical, indicates if value should be transformed to log(value).
#'@param method. Character which method for adjustment should be choosen. See Details.
#'
#'For method there can be choosen \code{"firstdiff"} and \code{"arima"}.
#'If \code{"firstdiff"}, first differences with \code{lag = 1} is executed.
#'If \code{"arima"}, the X-13ARIMA-SEATS of US  procedure is used.
#'
#'@example
#'series <- ts_gtrends(c("ikea", "saturn"), time = "all")
#'seas_adj(series, freq = "month, log.traf = T, method = "firstdiff")
#'@import tidyverse gtrendsR tsbox seasonal zoo
#'@export
seas_adj <-function(series, freq = "month", log.trafo = F, method = "arima"){
  if (log.trafo) series <- mutate(series, value = log(value)) #Log-Trafo
  if(method == "arima"){                      #Saisonbereinigung mit X-13 ARIMA
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    series <- as.list(ts_ts(series))
    series <- ts_tbl(seasonal::final(seasonal::seas(series,
                                                    transform.function = "none")))
  }
  if(method == "firstdiff"){     #Saisonbereinigung mit ersten Differenzen mit lag = 4
                                 #da gerade Quartalsdaten. Fuer monatsdaten lag = 12
    if (!("id" %in% names(series))) series <- mutate(series, id = "id")
    if (freq == "month") k = 12
    if (freq == "quarter") k = 4
    series <- series %>%
      group_by(id) %>%
      mutate(value = c(rep(0,k), diff(value, k))) %>% #Wenn Monatsdaten hier 12 statt 4
      ungroup()
  }
  series
}





