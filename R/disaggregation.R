library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)


dat_d <- as_tibble(read.csv("raw/de/arbeitslos_d.csv")) %>%
  mutate(time = as.Date(time))
dat_m <- as_tibble(read.csv("raw/de/arbeitslos_m.csv")) %>%
  mutate(value_m = value, time = as.Date(time)) %>%
  select(time, value_m) %>%
  filter(time <= as.Date("2020-10-01"))
dat_aggr_m <- dat_d %>%
                mutate(time = floor_date(time, "month")) %>%
                group_by(time) %>%
                mutate(aggr_value = mean(value)) %>%
                select(time, aggr_value) %>%
                unique()

pivot_longer(left_join(dat_m, dat_aggr_m), cols = value_m:aggr_value, names_to = 'type', values_to = "value") %>%
  ggplot(aes(x = time, y = value, col = type)) +
  geom_line()
