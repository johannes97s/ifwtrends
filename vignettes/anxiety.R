library(glmnet)
library(tidyverse)
library(lubridate)
library(zoo)
library(trendecon)
library(gtrendsR)
library(tsbox)
library(RJDemetra)

setwd("~/IFW/ifwtrends")
r_list = roll(keyword = c("arbeitslos","angst","crash","hartz 4","krise","grundsicherung","kündigung",
                          "entlassung"),
              start_series = "2006-01-01",
              start_period = "2018-01-01",
              end = Sys.Date(),
              fun = g_index,
              lags = 2)

saveRDS(r_list, "data/anxiety.rds")

r_list <- readRDS("data/anxiety.rds")

vdax<- readxl::read_xlsx("data/vdax.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "month"), value = as.numeric(`VDAX-NEW VOLATILITY INDEX - PRICE INDEX`))


dat <- vdax %>%
  #mutate(value = value/lag(value) - 1) %>%
  filter(time >= min(first(r_list)$time))



forecast_m(r_list, dat, fd = F)$forec %>%
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()



