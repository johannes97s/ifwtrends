###
#Takes a list of vintages r_list, generated with roll and a quarterly target variable dat
#to evaluate a forecast.
#Google Data should be log-transformed
#currently it uses a RIDGE-Regression but other models such as OLS, LASSO or
#PCA are available (change the relevant lines of code)


#' Quarterly forecast evaluation of vintages
#'
#' @description Todo
#'
#' @param r_list todo
#' @param dat todo
#' @param fd todo
#'
#' @return Todo
#'
#' @examples
#' 1+1
#' 1 + 2
#' @importFrom magrittr %>%
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' @importFrom stats predict
#' @export
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
