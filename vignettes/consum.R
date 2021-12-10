library(glmnet)
library(tidyverse)
library(lubridate)
library(zoo)
library(trendecon)
library(gtrendsR)
library(tsbox)
library(RJDemetra)

setwd("~/IFW/ifwtrends")
start = "2006-01-01"
end = "2021-07-01"

consexp <- readxl::read_xlsx("data/consumer_exp_GER.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "quarter"), value = as.numeric(`BD CONSUMER EXPENDITURE CONA`))


dat <- consexp %>%
  mutate(value = value/lag(value) -1 ) %>%
  filter(time >= as_date(start), time <= as_date(end)) %>%
  drop_na()



keyword = c(NA)
category_aco= c(956, 276, 179)
category_ret = c(560, 121,
                 277, 123,
                 988, 68,
                 660, 658, 466, 465, 659, 948,
                 270, 271, 137, 158,
                 646, 249, 256,
                 468, 898, 473, 815, 289,
                 382, 383,
                 355, 41, 439, 3, 1010, 432, 882, 614, 78, 408,
                 74,
                 179, 276,
                 7, 143, 146, 508, 38)
category_test = c(560,277)



res_raw <- gtpreparation(keyword = NA, category = category_test,
                   time = str_c(start, " ", end),
                   lags = 2)



################################################
# start_series = "2006-01-01"
# start_period_0 = "2016-01-01"
# end_0 = "2017-12-31"
#
# r0 <-  roll(keyword = NA,
#             category = category_ret,
#             start_series = start_series,
#             start_period = start_period_0,
#             end = end_0,
#             fun = g_index,
#             lags = 2)
# saveRDS(r0, "data/retail_gindex_roll_1217")

start_series = "2006-01-01"
start_period_1 = "2018-01-01"
end_1 = "2019-12-31"

r1 <-  roll(keyword = NA,
            category = category_ret,
            start_series = start_series,
            start_period = start_period_1,
            end = end_1,
            fun = g_index,
            lags = 2)
saveRDS(r1, "data/retail_gindex_roll_1219")

start_period_2 = "2020-01-01"
end_2 = "2021-07-31"

r2 <-  roll(keyword = NA,
           category = category_ret,
           start_series = start_series,
           start_period = start_period_2,
           end = end_2,
           fun = g_index,
           lags = 2)

saveRDS(r2, "data/retail_gindex_roll_0721")


r1 <- readRDS("data/retail_gindex_roll_1219")
r2 <- readRDS("data/retail_gindex_roll_0721")

r <- c(r1, r2)[-43] #Auf Gleiche L?nge wie dat K?rzen


forecast_q(r, dat) %>%
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()

series <- last(r)


series %>%
  select(time, contains("lag_0")) %>%
  pivot_longer(cols = -time, names_to = "cat", values_to = "value") %>%
  group_by(cat) %>%
  mutate(value = var(value)) %>%
  ggplot(aes(x = time, y = value, color = cat)) +
    geom_line() +
    theme(legend.position = "none")
