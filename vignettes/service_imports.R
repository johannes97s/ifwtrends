library(glmnet)
library(tidyverse)
library(lubridate)
library(zoo)
library(trendecon)
library(gtrendsR)
library(tsbox)
library(RJDemetra)

setwd("~/IFW/ifwtrends")
r_list = roll(category = c(203, 206,179,1003,1004,208,1010,1011),
              start_series = "2006-01-01",
              start_period = "2018-01-01",
              end = Sys.Date(),
              fun = g_index,
              lags = 2)

saveRDS(r_list, "data/travel.rds")

r_list2 <- r_list[1:42]

test <- readxl::read_xlsx("data/service_imports.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "quarter"), value = as.numeric(`BD IMPORTS - SERVICES CONA`))


dat <- test %>%
  mutate(value = c(0, diff(value,1)) ) %>% 
  filter(time < as.Date("2021-08-01"))



forecast_q(r_list2, dat, fd = T)$forec %>% 
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()








