library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)
library(lubridate)
library(caret)
library(glmnet)
library(lattice)

g_index <- function(
  keyword = NA,
  category = 0,
  geo = "DE",
  time = str_c("2006-01-01 ", Sys.Date()),
  dat,
  k = 1){

    start <- str_sub(time, 1,10)
    end <- str_sub(time, 12,21)
    dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")
    dat <- dat %>%
      filter(time >= as.Date(start)) %>%
      mutate(time = as.yearqtr(as.Date(time))) %>%
      filter(time <= as.yearqtr(as.Date(end))) %>%
      mutate(time = as.Date(time)) %>%
      mutate(value = log(value)) %>%
      select(time, dat = value)

    fit <- as_tibble(openxlsx::read.xlsx("~/IFW/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
      select(time = date, fit) %>%
      filter(time >= as.Date(start))

    g_dat2 <- ts_gtrends(keyword = keyword,
                        category = category,
                        geo = "DE",
                        time = time) %>%
                mutate(value = log(value)) %>%
                mutate(value = replace(value, value == -Inf, NA_real_)) %>%
                mutate(value = na.approx(value, rule = 2))
    if (!("id" %in% names(g_dat2))) g_dat2 <- mutate(g_dat2, id = as.character(category))
    #g_dat <- ts_pick(ts_prcomp(g_dat), "PC1")
    g_dat <- g_dat2 %>%
      left_join(fit, by = "time") %>%
      mutate(time = as.yearqtr(time)) %>%
      group_by(time, id) %>%
      mutate(value = mean(value), fit = mean(fit)) %>%
      ungroup() %>%
      unique() %>%
      mutate(time = as.Date(time)) %>%
      filter(time <= max(dat$time)) %>%
      mutate(adj = value - fit)


    g_dat_adj <- g_dat %>%
      select(id, time, adj) %>%
      seas_adj(freq = "quart", method = "arima") %>%
      rename(s_adj = value)
    if (!("id" %in% names(g_dat_adj))) g_dat_adj <- mutate(g_dat_adj, id = as.character(category))




    g_dat <- left_join(g_dat, g_dat_adj, by = c("time", "id")) %>%
      left_join(dat, by = "time") %>%
      unique()


    # g_dat %>%
    #   select(id, time, s_adj, dat) %>%
    #   pivot_longer(cols = -c("id", "time"), names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color = id)) +
    #   facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()

    index <- g_dat %>%
        select(id, time, s_adj, dat) %>%
        group_by(id) %>%
        mutate(s_adj = c(rep(0,k), diff(s_adj, k)), dat = c(rep(0,k), diff(dat, k))) %>%
          ungroup()

    # index %>%
    #     pivot_longer(cols = -c("id", "time"), names_to = "key", values_to = "values") %>%
    #     #filter(id == "Koffer") %>%
    #     ggplot(aes(x = time, y = values, color = id)) +
    #     facet_grid(key ~., scales = "free_y" ) +
    #     geom_line()


    index <- index %>%
      pivot_wider(names_from = id, values_from = s_adj)

    y <- index[[2]]
    x <- as.matrix(index[-c(1:2)])

    cv <- cv.glmnet(x, y, alpha = 0)
    model <- glmnet(x, y, alpha = 0, lambda = cv$lambda.min1)
    coef <- coef(model, s = cv$lambda.min)
    R2 <- model$dev.ratio


    index<- bind_cols(index, as_tibble(predict(model, x, cv$lambda.min)))
    # index %>%
    #   select(time, dat, s1) %>%
    #   pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color =key)) +
    #   #facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()


    # fd_tb <- index %>%
    #   group_by(id)
    #   select(time, dat, s1) %>%
    #   mutate(dat = c(rep(0,k), diff(dat,k)), s1 = c(rep(0,k), diff(s1,k)))
    #   ungroup()

    # fd_tb %>%
    #   pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color =key)) +
    #   #facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()
    fd <- T
    if (fd) return(list(series = index, coef = coef))
    #else return(list(series = index, coef = coef))

}

dat <- readxl::read_xlsx("~/IFW/Service_Import.xlsx")
names(dat) <- c("time","value")
keyword = c("Dienstreise",
            "Reisepass",
            "Spanien",
            "Italien",
            "koffer",
            "Flug",
            "Hotel")
category = c(67)



res <- g_index(keyword = keyword, category = category, time = str_c("2011-01-01 ", "2020-11-01"),dat = dat, k =1)



t <- ts_gtrends(keyword = c("italien","spanien"),
            category = category,
            geo = "DE",
            time = str_c("2011-01-01 ", "2019-11-01")) %>%
  mutate(value = log(value)) %>%
  mutate(value = replace(value, value == -Inf, NA_real_)) %>%
  mutate(value = na.approx(value, rule = 2))
if (!("id" %in% names(t))) t <- mutate(t, id = as.character(category))


fit <- as_tibble(openxlsx::read.xlsx("~/IFW/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
  select(time = date, fit) %>%
  filter(time >= as.Date(start))

g_dat <- t %>%
  left_join(fit, by = "time") %>%
  mutate(time = as.yearqtr(time)) %>%
  group_by(time, id) %>%
  mutate(value = mean(value), fit = mean(fit)) %>%
  ungroup() %>%
  unique() %>%
  mutate(time = as.Date(time)) %>%
  filter(time <= max(dat$time)) %>%
  mutate(adj = value - fit) %>%
  select(id, time, adj)


series <- as.list(ts_ts(g_dat))
s1 <- seasonal::seas(series,
              transform.function = "none")



res$series %>%
  select(time, dat, s1) %>%
  pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
  #filter(id == "Koffer") %>%
  ggplot(aes(x = time, y = values, color =key)) +
  #facet_grid(key ~., scales = "free_y" ) +
  geom_line()


summary(lm(dat~ s1 + lag(s1) + lag(s1,4), data = res$series))
xyplot(dat~ lag(s1)+lag(s1,4), data = res$series, type = c("p","r"), col.line = "red")

h <- function(...) select(g_index(...)$series, time, s1)


############
keyword_12 = c(NA)
category_12 = c(67,1003)
r1 <-  roll(keyword = keyword_12,
            category = category_12,
            start_series = "2011-01-01",
            start_period = "2017-01-01",
            fun = h,
            dat = dat,
            k = 4)
Sys.sleep(180)

r2 <-  roll(keyword = keyword_12,
            category = category_12,
            start_series = "2011-01-01",
            start_period = "2017-01-01",
            fun = h,
            dat = dat,
            k = 1)
Sys.sleep(180)
############
keyword_34 = c("Dienstreise",
            "Reisepass",
            "stau",
            "koffer",
            "Flug",
            "Hotel")
category_34 = c(67)
r3 <- roll(keyword = keyword_34,
           category = category_34,
           start_series = "2011-01-01",
           start_period = "2017-01-01",
           fun = h,
           dat = dat,
           k = 4)
Sys.sleep(180)
r4 <- roll(keyword = keyword_34,
           category = category_34,
           start_series = "2011-01-01",
           start_period = "2017-01-01",
           fun = h,
           dat = dat,
           k = 1)
Sys.sleep(180)

############
keyword_56 = c("Dienstreise",
            "Reisepass",
            "Spanien",
            "Italien",
            "koffer",
            "Flug",
            "Hotel")
category_56 = c(67)
r5 <- roll(keyword = keyword_56,
           category = category_56,
           start_series = "2011-01-01",
           start_period = "2017-01-01",
           fun = h,
           dat = dat,
           k = 4)
Sys.sleep(180)
r6 <- roll(keyword = keyword_56,
           category = category_56,
           start_series = "2011-01-01",
           start_period = "2017-01-01",
           fun = h,
           dat = dat,
           k = 1)


keyword_34 = c("Dienstreise",
               "Reisepass",
               "stau",
               "koffer",
               "Flug",
               "Hotel")
category_34 = c(67)


start_series <- "2011-01-01"
start_period <- "2017-01-01"
end = Sys.Date()

h <- function(...) select(g_index(...)$series, time, s1)

period <-  seq.Date(as.Date(start_period), as.Date(end), by = "month")
dates <- seq.Date(as.Date(start_series), as.Date(end), by = "month")
n <- length(dates)
f <- function(d) h(keyword = keyword,
                     category = category,
                     geo = geo,
                     time = stringr::str_c(start_series," ", d),
                     dat = dat,
                     k = 1)
tl <- lapply(period, f)



