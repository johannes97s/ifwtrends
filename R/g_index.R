#'Aufbereitung der Google Daten.
#'@description \code{g_index} lädt für mehrere Suchbegriffe/Kategorien Daten herunter, führt log Trafo und Saisonbereinigung durch und gibt (gelagte) erste Differenzen aus
#'
#'@param keyword Eine character-Vektor mit dem Suchbegriffen
#'@param category Ein Numeric Vektor mit den Kategorien
#'@param geo Die Region
#'@param time Der Zeitraum in der Angabe wie in ts_gtrends
#'@param lags Wie viele lags sollen als zusätzliche Spalte ausgegeben werden. Bis zu 4.
#'@return Für jede Reihe wird zunächst eine log-Trafo durchgeführt.
#' Dann wird mit JDemetra eine Saisonbereinigung mit X-13 Arima durchgeführt.
#' Dann werden erste Differenzen zurückgegeben. Standardmäßig mit lag
#'@import magrittr tibble zoo trendecon dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom stats prcomp
#' @importFrom tidyselect any_of
#' @importFrom tsbox ts_ts
#' @importFrom lubridate as_date
#' @importFrom stringr str_c
#' @import tsbox
#' @import gtrendsR
#' @import trendecon
#' @import zoo
#' @examples \dontrun{
#' g_index(keyword = c("ikea","saturn"), time = "2018-01-01 2021-01-01")
#' }
#' @importFrom gtrendsR gtrends
#' @importFrom gtrendsR gtrends
#' @export
g_index <- function(
  keyword = NA,
  category = 0,
  geo = "DE",
  time = str_c("2006-01-01 ", Sys.Date()),
  lags = 0){
    start <- str_sub(time, 1,10)
    end <- str_sub(time, 12,21)
    dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")


    fit <- readRDS("data/comtrend.rds") %>%
      select(time = date, trend) %>%
      filter(time >= as.Date(start))

    g_dat2 <- ts_gtrends(keyword = keyword,
                        category = category,
                        geo = "DE",
                        time = time) %>%
                mutate(value = log(value)) %>%
                mutate(value = replace(value, value == -Inf, NA_real_)) %>%
                mutate(value = na.approx(value, rule = 2))
    if (!("id" %in% names(g_dat2))) g_dat2 <- mutate(g_dat2, id = as.character(as.vector(sapply(category, rep, length(dates)))))


    g_dat_adj <- g_dat2 %>%
      left_join(fit, by = "time") %>%
      mutate(time = as.Date(time), adj = value - trend) %>%
      select(id, time, adj) %>%
      seas_adj(freq = "quarter", method = "arima") %>%
      rename(s_adj = value) %>%
      unique()

    if (!("id" %in% names(g_dat_adj))) g_dat_adj <- mutate(g_dat_adj, id = as.character(rep(category, each = length(dates))))


    index <- g_dat_adj %>%
      group_by(id)
      #mutate(s_adj = c(0, diff(s_adj, 1))) %>%
      if (lags >=1) index <- mutate(index, lag_1 = lag(s_adj))
      if (lags >=2) index <- mutate(index, lag_2 = lag(s_adj,2))
      if (lags >=3) index <- mutate(index, lag_3 = lag(s_adj,3))
      if (lags == 4) index <- mutate(index, lag_3 = lag(s_adj,3))

      index <- index %>%
        ungroup() %>%
        rename(lag_0 = s_adj) %>%
        filter(across(everything(), ~!is.na(.))) %>%
        pivot_longer(cols = -c(id, time), names_to = "lag", values_to = "value") %>%
        pivot_wider(names_from = c(id, lag), values_from = value)

      return(index)
}

