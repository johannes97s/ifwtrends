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

r_list2 <- r_list[1:44]

vdax <- readxl::read_xlsx("data/service_imports.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "quarter"), value = as.numeric(`BD IMPORTS - SERVICES CONA`))


dat <- vdax %>%
  mutate(value = c(0, diff(log(value),1)) ) %>% 
  drop_na()


forecast_q(r_list2, dat, fd = T) %>% 
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()
