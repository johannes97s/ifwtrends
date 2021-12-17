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


forecast_q <- function(r_list, data, fd = T) {

  # get quarterly data
  r_raw <- r_list[1:length(r_list) %% 3 == 0]

  # get the number of lags from the suffix
  number_of_lags <- sub(".*_", "", colnames(r_raw[[1]])[length(colnames(r_raw[[1]]))])

  r_raw <- lapply(r_raw, function(x) {
    y <- as_tibble(x) %>%
      mutate(time = floor_date(time, "quarter")) %>%
      group_by(id, time) %>%
      summarise(across(1, mean))

    if (number_of_lags >= 1) y <- mutate(y, lag_1 = lag(lag_0, 1))
    if (number_of_lags >= 2) y <- mutate(y, lag_2 = lag(lag_0, 2))
    if (number_of_lags >= 3) y <- mutate(y, lag_3 = lag(lag_0, 3))
    if (number_of_lags == 4) y <- mutate(y, lag_4 = lag(lag_0, 4))
    y <- ungroup(y)

    return(y)
  })

  # compute first differences if fd=T
  if (fd) {
    r_raw <- lapply(r_raw, function(x) {
      mutate(
        x,
        across(
          .cols = -c(1, 2), function(y) c(0, diff(y, 1))
        ),
        .keep = "all"
      )
    })
  }


  r_raw <- lapply(r_raw, function(x) {
    left_join(x, data[1:nrow(x), ], by = "time") %>%
      select(time, id, data = value, everything()) %>%
      filter(time != as.Date("2011-01-01")) %>% # omit structural breaks
      filter(time != as.Date("2016-01-01")) %>%
      mutate(across(everything(), function(y) replace(y, y == -Inf, NA_real_))) %>%
      mutate(across(everything(), function(y) replace(y, y == Inf, NA_real_)))
  })


  # use PCA
  # r_factors <- lapply(r_raw, function(x){
  #   pc <- as_tibble(prcomp(x[-c(1,2)])$x)
  #   bind_cols(x[c(1,2)], pc[,1:min(20, length(r_raw[[1]])-2)]) %>% #number of PCs
  #     drop_na()
  # })
  # set r <- r_factors to use PCA-Model

  r <- r_raw

  # Impute NAs with 0
  r <- lapply(r, function(x) {
    x[is.na(x)] <- 0
    return(x)
  })
  return(r)

  # Function to estimate the model via RIDGE regression
  build_model <- function(series) {
    y <- as.matrix(series[1])
    x <- as.matrix(series[-c(1)])

    # Cross validation
    #cv <- cv.glmnet(x, y, alpha = 0)

    # Alternative arguments for glmnet():
    # OLS: lambda= 0
    # LASSO: alpha = 1
    model <- glmnet(x = x, y = y, alpha = 0, lambda = 0) # cv$lambda.min)

    return(model)
  }

  # Trends Data to forecast with previous estimated model
  covariats <- lapply(r, function(x) data.matrix(x[-(1:3)]))

  # estimate model, deselect everything besides variables (columns 1, 2)
  models <- lapply(data.matrix(r), function(x) build_model(x[-(1:2)]))

  pred_values <- mapply(predict, object = models, newx = covariats) # forecast

  last_values <- sapply(pred_values, last) # select last value in each vintage as forecast
  # for relevant quarter

  forec <- tibble(
    time = seq.Date(max(first(r)$time), max(last(r)$time), by = "quarter"),
    value = last_values
  ) %>%
    mutate(time = floor_date(time, "quarter")) %>%
    rename(index = value)

  last_model <- last(models)
  return(list(
    forec = forec, # returns forcasted values and
    last_model = last_model
  )) # model estimated with contemporary data
}

forecast_q(r_list, datastream, fd = T)$forec %>%
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x = time, y = value, color = id)) +
  geom_line()
