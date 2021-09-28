library(tsbox)
library(gtrendsR)
library(trendecon)
library(tidyverse)
library(zoo)
library(lubridate)

#'Konsistente taegliche Zeitreihe
#'@description \code{daily_series}. Schaetzt mit Chow-Lin eine konsistente lange Zeitreihe mit taeglicher Frequenz zurueck.
#'
#'
#'@param keyword Der Suchbegriff. Bis jetzt nur einer möglich
#'@param geo Region
#'@param from Startdatum
#'
#'Vorsicht, braucht sehr lange und verursacht viele Google-Suchanfragen.
#'
#'@return Tabelle der taeglichen Werten
#'@examples
#'daily_series(keyword = "Ikea", geo = "NL", from = "2008-01-01")
#'
#'@import tidyverse gtrendsR trendecon tsbox lubridate zoo
#'@export
daily_series <- function(keyword = c("arbeitslos"),
                         geo = "DE",
                         from = "2006-01-01"){
  d <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "15 days", windowsize = "6 months",
    n_windows = 400, wait = 20, retry = 10,
    prevent_window_shrinkage = TRUE
  )
  d2 <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = seq(Sys.Date(), length.out = 2, by = "-90 days")[2],
    stepsize = "1 day", windowsize = "3 months",
    n_windows = 12, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  dd <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(d), trendecon:::aggregate_windows(d2))

  # download weekly series
  w <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "11 weeks", windowsize = "5 years",
    n_windows = 88, wait = 20, retry = 10,
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
  m <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "1 month", windowsize = "17 years",
    n_windows = 1, wait = 20, retry = 10,
    prevent_window_shrinkage = FALSE
  )
  m2 <- trendecon:::ts_gtrends_windows(
    keyword = keyword,
    geo = geo,
    from = from,
    stepsize = "1 month", windowsize = "20 years",
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
  dd$value <- 0.5*(na.locf(dd$value,fromLast =TRUE) + na.locf(dd$value))


  ww <-  ts_regular(ts_dts(ww))
  ww$value <- 0.5*(na.locf(ww$value,fromLast =TRUE) + na.locf(ww$value))

  mm <-  ts_regular(ts_dts(mm))
  mm$value <- 0.5*(na.locf(mm$value,fromLast =TRUE) + na.locf(mm$value))

  # mm %>%
  #   mutate(week = week(time), year = year(time)) %>%
  #   group_by(week, year) %>%
  #   mutate(value = mean(value)) %>%
  #   ungroup() %>%
  #   select( - time) %>%
  #   filter(week <= 52) %>%
  #   unique() %>%
  #   bind_cols(time = ww$time) %>%
  #   select(time, value)-> mm



  wd <- tempdisagg::td(ww ~ dd, method = "fast", conversion = "mean")
  wd <- predict(wd)

  mwd <- tempdisagg::td(mm ~ wd, method = "fast", conversion = "mean")
  mwd <- predict(mwd)
  as_tibble(mwd)
}

###############Tests


# keyword = "arbeitslos"
# geo = "DE"
# from = "2006-01-01"
#
# series <- daily_series(keyword = "arbeitslos",
#                        geo = "DE",
#                        from = "2006-01-01")
#
# series %>%
#   mutate(month = floor_date(time, "month")) %>%
#   group_by(month) %>%
#   mutate(monthl_aggr = mean(value)) %>%
#   ungroup() %>%
#   select(-month) %>%
#   left_join(ts_gtrends(keyword = keyword, geo = geo, time = "2006-01-01 2021-08-30", retry = 10), by = "time") -> mwd_mon
#
#
#
# names(mwd_mon) <- c("time", "daily", "monthl_aggr", "orig")
# mwd_mon <- fill(mwd_mon, orig, .direction = "down")
#
# write.xlsx(mwd_mon, "data_Wirtschaftskrise_2010.xlsx")
#
#
#
# ggplot(fill(pivot_longer(mwd_mon, cols = -time, names_to = "id", values_to = "value")), aes(x = time, y = value, color = id)) +
#   geom_line()
#
# corrr::correlate(mwd_mon$monthl_aggr, mwd_mon$orig)
#
# max(abs(mwd_mon$monthl_aggr - mwd_mon$orig))
#
#
#
#
#
#
#
# time <- str_c("2006-01-01 ", as.character(Sys.Date()))
# orig <- select(gtrends(keyword = "arbeitslos",
#                        geo = "DE",
#                        time = time)$interest_over_time, date, value = hits)
# orig %>%
#   mutate(date = floor_date(as.Date(date), "month")) %>%
#   group_by(date) %>%
#   mutate(mnl = mean(value)) %>%
#   ungroup() %>%
#   select(date, mnl) %>%
#   unique() -> orig_avg
#
# as_tibble(series) %>% mutate(date = floor_date(as.Date(time), "month")) %>%
#   group_by(date) %>%
#   mutate(dail_avg = mean(value)) %>%
#   ungroup() %>%
#   select(date, dail_avg) %>%
#   unique() %>%
#   left_join(orig_avg, by = "date") %>%
#   pivot_longer(cols = -date, names_to = "id", values_to = "value") %>%
#   ggplot(aes(x = date, y = value, color = id)) + geom_line()



