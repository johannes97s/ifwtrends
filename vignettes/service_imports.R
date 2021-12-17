# Dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(ifwtrends)
library(lubridate)
library(zoo)
library(gtrendsR)
library(trendecon)
library(tsibble)
library(glmnet)



# Get Google data ---------------------------------------------------------


# setwd("~/IFW/ifwtrends")
r_list <- roll(
  keyword = NA,
  category = c(67, 1003),
  start_series = "2006-01-01",
  start_period = "2018-01-01",
  end = Sys.Date(),
  fun = gtpreparation,
  lags = 4
)

# Optionally, save it to reduce data requests at Google
# (and prevent an IP ban)
# saveRDS(r_list, "./vignettes/travel2.rds")
#
# r_list <- readRDS("./vignettes/travel2.rds")


# Read service imports data from Datastream -------------------------------


datastream <- readxl::read_xlsx("./vignettes/service_imports.xlsx") %>%
  transmute(
    time = floor_date(as.Date(Name), "quarter"),
    value = as.numeric(`BD IMPORTS - SERVICES CONA`)
  ) %>%
  mutate(value = c(0, diff(value, 1))) %>%
  filter(time < as.Date("2021-08-01") & time > as.Date("2006-02-01"))



# Forecast the datastream data --------------------------------------------

forecast_q(r_list, datastream, fd = T)$forec %>%
  left_join(datastream, by = "time") %>%
  rename(Datastream = value, Estimation = index) %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x = time, y = value, color = id)) +
  geom_line() +
  labs(colour = "Series", x = "Time", y = "Value", title = "Comparison between actual data from Datastream\n and estimation via Google Trends data.")
