library(glmnet)
library(tidyverse)
library(lubridate)
library(zoo)
library(trendecon)
library(gtrendsR)
library(tsbox)
library(RJDemetra)

setwd("~/IFW/ifwtrends")
r_list = roll(keyword = NA,
              category = c(67,1003),
              start_series = "2006-01-01",
              start_period = "2018-01-01",
              end = Sys.Date(),
              fun = g_index,
              lags = 4)

saveRDS(r_list, "vignettes/travel2.rds")

r_list <- readRDS("data/travel2.rds")
r_list2 <- r_list[1:45]

test <- readxl::read_xlsx("vignettes/service_imports.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "quarter"), value = as.numeric(`BD IMPORTS - SERVICES CONA`))


dat <- test %>%
  mutate(value = c(0, diff(value,1)) ) %>%
  filter(time < as.Date("2021-08-01")) %>%
  filter(time > as.Date("2006-03-01"))



forecast_q(r_list2, dat, fd = T)$forec %>%
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()


mod <- forecast_q(r_list2, dat, fd = T)$last_model

r <- r_list2[[43]]


  r_raw <- mutate(r, time = floor_date(time, "quarter")) %>% #aggregate GT Data to quarter
    group_by(time) %>%                              #
    transmute_at(.vars = vars(-time), .funs =  mean) %>%  #
    ungroup() %>%
    unique()


if (fd) r_raw <- mutate(r_raw, time = time, #first differences if fd=T set
                                    across(.cols = -1, function(y) c(0, diff(y,1))),
                                     .keep = "used")

r_raw <-  select(r_raw, time, everything()) %>%
    filter(time != as.Date("2011-01-01")) %>% #omit structural breaks
    filter(time != as.Date("2016-01-01")) %>%
    mutate(across(everything(), function(y) replace(y, y == -Inf, NA_real_))) %>%
    mutate(across(everything(), function(y) replace(y, y == Inf, NA_real_)))





plot(predict(mod, as.matrix(r_raw[-1])), type = "l")






