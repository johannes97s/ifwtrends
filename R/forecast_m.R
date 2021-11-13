###
# Takes a list of vintages r_list, generated with roll and a quarterly target variable dat
# to evaluate a forecast.
# Google Data should be log-transformed
# currently it uses a RIDGE-Regression but other models such as OLS, LASSO or
# PCA are available (change the relevant lines of code)

#' Monthly forecast evaluation of vintages
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
#' 1 + 1
#' 1 + 2
#' @importFrom magrittr %>%
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' @importFrom stats predict
#' @export
forecast_m <- function(r_list, dat, fd = T) {
  colnames(dat)[1] <- "time"
  print("colnames ok")
  if (fd) {
    r_list <-
      lapply(
        r_list,
        function(x){
          mutate(x,
            time = time,
            across(
              .cols = last_col(),
              # first differences
              function(y) c(0, diff(y, 1))
            )
          )
        }
      )
  }
  print("fd ok")
  r_raw <- lapply(r_list, function(x) {
    left_join(x, dat[1:nrow(x), ], by = "time") %>%
      select(time, dat = value, everything()) %>%
      filter(time != as.Date("2011-01-01")) %>% # omit structural breaks
      filter(time != as.Date("2016-01-01")) %>%
      mutate(across(everything(), function(y) replace(y, y == -Inf, NA_real_))) %>%
      mutate(across(everything(), function(y) replace(y, y == Inf, NA_real_)))
  })

  print("r_raw ok")
  # use PCA
  # r_factors <- lapply(r_raw, function(x){
  #   pc <- as_tibble(prcomp(x[-c(1,2)])$x)
  #   bind_cols(x[c(1,2)], pc[,1:min(20, length(r_raw[[1]])-2)]) %>% #number of PCs
  #     drop_na()
  # })

  r <- r_raw # set r <- r_factors to use PCA-Model


  build_model <- function(series) {
    # Function to estimate the model
    y <- as.matrix(series[2])
    x <- as.matrix(series[-c(1, 2)])



    # cv <- cv.glmnet(x, y, alpha = 1)
    # model <- glmnet(x, y, alpha = 1, lambda =0)# cv$lambda.min) #alpha = 1 LASSO
    # model                                                    #lambda= 0 OLS
    lm(dat ~ ., data = series[-1])
  }

  covariats <- lapply(r, function(x) x[-c(1,2)]) #Trends Data to forecast with
  #previous estimated model
  models <- lapply(lag(r)[-1], function(x) build_model(x))           #estimate model

  print("models ok")
  pred_values <- mapply(predict, models, covariats[-c(1)]) # forecast

  last_values <- sapply(pred_values, last) # select last value in each vintage as forecast
  # for relevant month


  forec <- tibble(
    time = seq.Date(max(first(r)$time) + months(1), max(last(r)$time), by = "month"),
    value = last_values
  ) %>%
    mutate(time = floor_date(time, "month")) %>%
    rename(index = value)


  last_model = last(models)
  s_niv = lapply(models, function(x) summary(x)$coef[,4])
  return(list(forec = forec,             #returns forcasted values and
              last_model = last_model,
              s_niv = s_niv))

}
