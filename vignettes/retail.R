library(glmnet)
library(tidyverse)
library(lubridate)
library(zoo)
library(trendecon)
library(gtrendsR)
library(tsbox)
library(RJDemetra)

setwd("~/ifwtrends")
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
category_test = c(560,121,277)



res_raw <- g_index(keyword = NA, category = category_test,
                   time = str_c(start, " ", end),
                   lags = 2)



################################################
start_series = "2006-01-01"
start_period_0 = "2016-01-01"
end_0 = "2017-12-31"

r0 <-  roll(keyword = NA,
            category = category_ret,
            start_series = start_series,
            start_period = start_period_0,
            end = end_0,
            fun = g_index,
            lags = 2)
saveRDS(r0, "data/retail_gindex_roll_1217")

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

r_raw <- c(r1, r2[-c(1:12,31)])
r_raw <- lapply(r_raw, function(x){
  mutate(x, time = floor_date(time, "quarter")) %>%
    group_by(time) %>%
    transmute_at(.vars = vars(-time), .funs =  mean) %>%
    ungroup() %>%
    unique
})

r_raw <- lapply(r_raw, function(x) select(x,time, contains("lag_0")))
r_raw <- lapply(r_raw, function(x) mutate(x, across(.cols = -1, function(x) c(0, diff(x, 1))), .keep = "used"))
r_raw <- lapply(r_raw, function(x) relocate(bind_cols(x, dat[1:nrow(x),]), time, dat = value))


r_factors <- lapply(r_raw, function(x){
  pc <- as_tibble(prcomp(x[-c(1,2)])$x)
  bind_cols(x[c(1,2)], pc[,1:10])
  })

r <- r_factors


build_model <- function(series){

  y <- as.matrix(series[2])
  x <- as.matrix(series[-c(1,2)])


  cv <- cv.glmnet(x, y, alpha = 0)
  model <- glmnet(x, y, alpha = 0, lambda = 0)#cv$lambda.min)
  model
  # model <- lm(dat ~ ., data = series[-1])
  # model
  #summary(model)$coefficients[,4]
}

covariats <- lapply(r, function(x) as.matrix(x[-c(1,2)]))

models <- lapply(r, function(x) build_model(x))

pred_values <- mapply(predict, lag(models)[-1], covariats[-1])

last_values <- sapply(pred_values, last)

forec <- tibble(time = seq.Date(as.Date(start_period_1), as.Date(end_2)-months(2), by ="month"),
                value = last_values) %>%
  left_join(dat, by = "time") %>%
  rename(index = value.x, consump =value.y) %>%
  drop_na()

sqrt(sum((forec$index - forec$consump)^2))


forec %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()


coef <- as_tibble(t(as.data.frame(models))) %>%
  bind_cols(time = seq.Date(as_date(start_period_0), as_date(end_2), by = "month"))

coef %>%
  pivot_longer(cols = -time, names_to = "coef", values_to = "value") %>%
  group_by(coef) %>%
  mutate(var = var(value)) %>%
  ungroup() %>%
  filter(var <= 0.5*mean(var)) %>%
  ggplot(aes(x = time, y = value, color = coef)) +
  geom_line() +
  theme(legend.position = "none")+
  facet_grid(coef ~ .)








h <- function(model, series){
  est <- predict(model, as.matrix(series[,-c(1,2)]))
  tibble(time = series[,1], est = est, dat = series[2])
}

forc <- mapply(h, models, r1[-1], SIMPLIFY = F)


m1 <- build_model(r1[[1]])
h(m1, r1[[2]])



