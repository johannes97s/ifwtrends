library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)


end = "2019-12-31"
dat <- readxl::read_xlsx("~/Google Trends/Service_Import.xlsx") %>%
  mutate(time = as.yearqtr(as.Date(Name))) %>%
  select(time, value = `BD IMPORTS - SERVICES CONA`) %>%
  filter(time <= as.yearqtr(as.Date(end))) %>%
  mutate(time = as.Date(time))

time = str_c("2006-01-01 ", end)
g_dat <- ts_gtrends(keyword = NA,
                    category = "1003",
                    geo = "DE",
                    time = time) %>%
  mutate(time = as.yearqtr(time)) %>%
  group_by(time) %>%
  mutate(value = mean(value)) %>%
  ungroup() %>%
  unique() %>%
  mutate(time = as.Date(time)) %>%
  filter(time <= max(dat$time))

dat <- ser_adj(dat, trend = F, seas = F, trend_method = "firstdiff")
g_dat <- ser_adj(g_dat, trend = F, seas = T, trend_method = "firstdiff", seas_method = "arima")
plot(dat$time, dat$value, type = "l")
plot(g_dat$time, g_dat$value, type = "l")


x <- g_dat$value[-(1:4)]
y <- dat$value[-(1:4)]


