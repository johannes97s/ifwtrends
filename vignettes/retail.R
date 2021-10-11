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

retail <- readxl::read_xlsx("data/retail_GER.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "month"), value = `BD RETAIL SALES EXCL CARS (CAL ADJ) X-12-ARIMA VOLA`)


dat <- retail %>%
  mutate(value = log(as.numeric(value))) %>%
  mutate(value = c(0, diff(value, 1))) %>%
  filter(time >= as_date(start), time <= as_date(end))



keyword = c(NA)
category_aco= c(956, 276, 179)
category_ret = c(560, 121,
                 277, 123,
                 988, 68,
                 660, 658, 466, 465, 659, 948,
                 270, 271, 137, 158,
                 646, 249, 256,
                 898, 289, #Hier 3 Kategorien für Autokauf von RWI ausgelassen
                 382, 383,
                 355, 41, 439, 3, 1010, 432, 882, 614, 78, 408,
                 74,
                 179, 276,
                 7, 143, 146, 508, 38)
category_test = c(560,121,277)



res_raw <- g_index(keyword = NA, category = category_test,
                   time = str_c(start, " ", end),
                   lags = 2)

res <- res_raw



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

start_period_2 = "2019-01-01"
end_2 = "2021-07-31"

r2 <-  roll(keyword = NA,
           category = category_ret,
           start_series = start_series,
           start_period = start_period_2,
           end = end_2,
           fun = g_index,
           lags = 2)

saveRDS(r2, "data/retail_gindex_roll_0721")



r <- readRDS("data/retail_gindex_roll_0721")


r <- lapply(r, function(x) mutate(x, across(.cols = -1, function(x) c(0, diff(x, 1))), .keep = "used"))
r <- lapply(r, function(x) relocate(bind_cols(x, dat[1:nrow(x),-1]), time, dat = value))


build_model <- function(series){

  y <- as.matrix(series[2])
  x <- as.matrix(series[-c(1,2)])


  cv <- cv.glmnet(x, y, alpha = 0)
  model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min)
  model
  # model <- lm(dat ~ ., data = series[-1])
  # summary(model)$coefficients[,4]
}


models <- lapply(lag(r)[-1], build_model)



coef <- as_tibble(t(as.data.frame(models))) %>%
  bind_cols(time = seq.Date(as_date(start_period)+months(1), as_date(end), by = "month"))

coef %>%
  pivot_longer(cols = -time, names_to = "coef", values_to = "pValue") %>%
  group_by(coef) %>%
  mutate(mean = mean(pValue)) %>%
  filter(mean <= 0.4) %>%
  ggplot(aes(x = time, y = pValue, color = coef)) +
  geom_line() +
  geom_hline(yintercept = 0.1) +
  facet_grid(coef ~ .)








h <- function(model, series){
  est <- predict(model, as.matrix(series[,-c(1,2)]))
  tibble(time = series[,1], est = est, dat = series[2])
}

forc <- mapply(h, models, r1[-1], SIMPLIFY = F)


m1 <- build_model(r1[[1]])
h(m1, r1[[2]])‚



