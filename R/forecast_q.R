###
#Takes a list of vintages, generated with roll and a quarterly target variable dat
#to evaluate a forecast.
#currently it uses a RIDGE-Regression but other models such as OLS, LASSO or
#PCA are available (change the relevant lines of code)





forecast_q <- function(r, dat){
  r_raw <- r[1:length(r) %% 3 == 0]
  r_raw <- lapply(r_raw, function(x){
    mutate(x, time = floor_date(time, "quarter")) %>% #aggregate GT Data to quarter
      group_by(time) %>%                              #
      transmute_at(.vars = vars(-time), .funs =  mean) %>%  #
      ungroup() %>%
      unique() %>%
      mutate(time = time, across(.cols = -1, function(y) c(0, diff(y,1))), .keep = "used") %>% #first differences
      left_join(dat[1:nrow(x), ], by = "time") %>%
      select(time, dat = value, everything()) %>%
      filter(time != as.Date("2011-01-01")) %>% #omit structural breaks
      filter(time != as.Date("2016-01-01")) %>%
      drop_na()
    
  })
  
  r_factors <- lapply(r_raw, function(x){
    pc <- as_tibble(prcomp(x[-c(1,2)])$x)
    bind_cols(x[c(1,2)], pc[,1:20]) %>% #number of PCs
      drop_na()
  })
  
  r <- r_raw #set r <- r_factors to use PCA-Model
  
  
  build_model <- function(series){ #Function to estimate the model
    
    y <- as.matrix(series[2])
    x <- as.matrix(series[-c(1,2)])
    
    
    cv <- cv.glmnet(x, y, alpha = 0) 
    model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min) #alpha = 1 LASSO
    model                                                    #lambda= 0 OLS
  }
  
  covariats <- lapply(r, function(x) as.matrix(x[-c(1,2)])) #Trends Data to forecast with 
                                                            #previous estimated model
  models <- lapply(r, function(x) build_model(x))           #estimate model
  
  pred_values <- mapply(predict, lag(models, 1)[-c(1)], covariats[-c(1)]) #forecast
  
  last_values <- sapply(pred_values, last) #select last value in each vintage as forecast
                                           #for relevant quarter
  
  
  forec <- tibble(time = seq.Date(max(first(r)$time)+months(3), max(last(r)$time), by ="quarter"),
                  value = last_values) %>%
    mutate(time = floor_date(time, "quarter")) %>%
    rename(index = value) %>%
    # fill(consump, .direction = "up") %>%
    # mutate(time = floor_date(time, "quarter")) %>%
    # group_by(time) %>%
    # mutate(index = mean(index)) %>%
    drop_na()
  
  return(forec)
}