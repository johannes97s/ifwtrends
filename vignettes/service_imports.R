library(tidyverse)
library(readxl)
library(ifwtrends)
library(lubridate)
library(zoo)
library(gtrendsR)
library(trendecon)
library(tsibble)
library(glmnet)

gtpreparation <- function(keyword = NA,
                          category = 0,
                          geo = "DE",
                          time = str_c("2006-01-01 ", Sys.Date()),
                          lags = 0) {

  # some date variables
  start <- str_sub(time, 1, 10)
  end <- str_sub(time, 12, 21)
  dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")

  # Only monthly time series can be used. Hence,
  # anything shorter than 5 years cannot be analysed (as this
  # are weekly/daily time series).
  stopifnot("You need to use a time frame longer than 5 years (otherwise we wont have monthly data)!" =  ymd(end) - ymd(start) > years(5))

  # data containing a trend calculated on 250 GTrends time series'.
  # comtrend is saved as internal data in
  # R/sysdata.rda and is automatically
  # loaded into namespace
  load("./R/sysdata.rda")
  fit <-  comtrend %>%
    select(time = date, trend) %>%
    filter(time >= as.Date(start) & time <= as.Date(end))

  # make search queries
  google_data <- ts_gtrends(
    keyword = keyword,
    category = category,
    geo = "DE",
    time = time
  ) %>%
    mutate(value = log(value)) %>%
    mutate(value = replace(value, value == -Inf, NA_real_)) %>%
    mutate(value = na.approx(value, rule = 2))


  # add a id column consisting of the category
  if (!("id" %in% names(google_data))) {
    # Reformulate the category id into its name
    google_data <- mutate(
      google_data,
      id = as.character(
        gtrendsR::categories[gtrendsR::categories$id == category, 1]
      ))
  }

  # Trend adjust and seasonal adjust data
  adjusted_data <- google_data %>%
    full_join(fit, by = "time") %>%
    mutate(time = as.Date(time), adj = value - trend) %>%
    select(id, time, adj)  |>
    ifwtrends:::seas_adj(method = "arima") %>%
    rename(s_adj = value)

  # group by category
  grouped_data <- adjusted_data %>%
    group_by_key()

  # Add lagged columns
  if (lags >= 1) grouped_data <- mutate(grouped_data, lag_1 = lag(s_adj))
  if (lags >= 2) grouped_data <- mutate(grouped_data, lag_2 = lag(s_adj, 2))
  if (lags >= 3) grouped_data <- mutate(grouped_data, lag_3 = lag(s_adj, 3))
  if (lags == 4) grouped_data <- mutate(grouped_data, lag_4 = lag(s_adj, 4))

  # Reorder some stuff
  result <- grouped_data %>%
    ungroup() %>%
    rename(lag_0 = s_adj) %>%
    filter(across(everything(), ~ !is.na(.)))%>%
    pivot_longer(cols = -c(id, time), names_to = "lag", values_to = "value") %>%
    pivot_wider(names_from = lag, values_from = value)

  return(result)
}


#setwd("~/IFW/ifwtrends")
r_list = roll(keyword = NA,
              category = c(67,1003),
              start_series = "2006-01-01",
              start_period = "2018-01-01",
              end = Sys.Date(),
              fun = gtpreparation,
              lags = 4)

saveRDS(r_list, "./vignettes/travel2.rds")
#
# r_list <- readRDS("./vignettes/travel2.rds")
#r_list2 <- r_list[1:45]

test <- readxl::read_xlsx("./vignettes/service_imports.xlsx") %>%
  transmute(time = floor_date(as.Date(Name), "quarter"), value = as.numeric(`BD IMPORTS - SERVICES CONA`))


dat <- test %>%
  mutate(value = c(0, diff(value,1)) ) %>%
  filter(time < as.Date("2021-08-01")) %>%
  filter(time > as.Date("2006-03-01"))



forecast_q <- function(r_list, dat, fd = T){

  r_raw <- r_list[1:length(r_list) %% 3 == 0]
  number_of_lags <- sub(".*_", "", colnames(r_raw[[1]])[length(colnames(r_raw[[1]]))])

  r_raw <- lapply(r_raw, function(x){
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
    r_raw <- lapply(r_raw, function(x){
      mutate(
        x,
        across(
          .cols = -c(1,2), function(y) c(0, diff(y,1))
        ),
        .keep = "all")
    })
  }


  r_raw <- lapply(r_raw, function(x){
    left_join(x, dat[1:nrow(x), ], by = "time") %>%
      select(time, dat = value, everything()) %>%
      filter(time != as.Date("2011-01-01")) %>% #omit structural breaks
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

  r <- r_raw #set r <- r_factors to use PCA-Model
  # Impute NAs with 0
  r <- lapply(r, function(x){
    x[is.na(x)] <- 0
    return(x)})
  return(r)
  build_model <- function(series){ #Function to estimate the model
    y <- as.matrix(series[2])
    x <- as.matrix(series[-c(1,2)])


    cv <- cv.glmnet(x, y, alpha = 0)
    model <- glmnet(x, y, alpha = 0, lambda = 0)#cv$lambda.min) #alpha = 1 LASSO
    return(model)                                                    #lambda= 0 OLS
  }




  #Trends Data to forecast with previous estimated model
  covariats <- lapply(r, function(x) data.matrix(x[-(1:3)]))

  #estimate model, deselect everything besides variables
  models <- lapply(data.matrix(r[-(1:3)]), function(x) build_model(x))

  pred_values <- mapply(predict, models, covariats[-c(1)]) #forecast

  last_values <- sapply(pred_values, last) #select last value in each vintage as forecast
  #for relevant quarter

  forec <- tibble(time = seq.Date(max(first(r)$time)+months(3), max(last(r)$time), by ="quarter"),
                  value = last_values) %>%
    mutate(time = floor_date(time, "quarter")) %>%
    rename(index = value)

  last_model = last(models)
  return(list(forec = forec,             #returns forcasted values and
              last_model = last_model))    #model estimated with contemporary data
}

forecast_q(r_list, dat, fd = T)$forec %>%
  left_join(dat, by = "time") %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x=  time, y = value, color = id)) +
  geom_line()

