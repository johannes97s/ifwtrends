library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)
library(lubridate)
library(caret)
library(glmnet)
library(lattice)

end = Sys.Date()




g_index <- function(
  keyword = NA,
  category = 0,
  geo = "DE",
  time = str_c("2006-01-01 ", Sys.Date()),
  dat,
  fd = T,
  k = 1){

    start = str_sub(time, start=  1, end = 10)
    end = str_sub(time, start = 12, end = 21)
    dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")
    dat <- dat %>%
      filter(time >= as.Date(start)) %>%
      mutate(time = as.yearqtr(as.Date(time))) %>%
      filter(time <= as.yearqtr(as.Date(end))) %>%
      mutate(time = as.Date(time)) %>%
      mutate(value = log(value)) %>%
      select(time, dat = value)

    fit <- as_tibble(openxlsx::read.xlsx("~/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
      select(time = date, fit) %>%
      filter(time >= as.Date(start))

    g_dat2 <- ts_gtrends(keyword = keyword,
                        category = category,
                        geo = "DE",
                        time = time) %>%
                mutate(value = log(value))
    if (!("id" %in% names(g_dat2))) g_dat2 <- mutate(g_dat2, id = as.character(categories))
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
      mutate(adj = value - fit) %>%
      mutate(value = replace(value, value == -Inf, NA_real_), adj = replace(adj, adj == -Inf, NA_real_))

    print(g_dat)
    g_dat_adj <- g_dat %>%
      select(id, time, adj) %>%
      mutate(adj = na.approx(adj)) %>%
      seas_adj(method = "arima") %>%
      rename(s_adj = value)
    if (!("id" %in% names(g_dat_adj))) g_dat_adj <- mutate(g_dat_adj, id = as.character(categories))




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


    fd_tb <- index %>%
      select(time, dat, s1) #%>%
      #mutate(dat = c(rep(0,k), diff(dat,k)), s1 = c(rep(0,k), diff(s1,k)))

    # fd_tb %>%
    #   pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color =key)) +
    #   #facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()
    if (fd) return(list(series = fd_tb, coef = coef))
    else return(list(series = index, coef = coef))

}

dat <- readxl::read_xlsx("~/Google Trends/Service_Import.xlsx")
names(dat) <- c("time","value")
keyword = c("reisepass",
             "koffer",
             "dienstreise",
             "flug",
             "hotel")



res <- g_index(keyword = keyword, category = 67, time = "2006-01-01 2019-12-31", dat = dat, fd = F, k =1)

res$series %>%
  select(time, dat, s1) %>%
  pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
  #filter(id == "Koffer") %>%
  ggplot(aes(x = time, y = values, color =key)) +
  #facet_grid(key ~., scales = "free_y" ) +
  geom_line() +
  labs(title = "Titel")


summary(lm(dat~ s1 + lag(s1) + lag(s1,4), data = res$series))
xyplot(dat~ lag(s1)+lag(s1,4), data = res$series, type = c("p","r"), col.line = "red")

h = function(keyword, category = 0, geo = "DE", time, dat, fd = F, k = 1){
  g_index(keyword = keyword, category = category, time = time, dat = dat, fd = fd, k = k)$series %>%
  select(time, dat,  s1)
}
t <- roll(keyword = keyword,
          category = 0,
           start_series = "2006-01-01",
           start_period = "2018-01-01",
           end = "2018-03-01",
           fun = h,
           dat = dat,
           fd = F,
           k = 1)




series <- ts_gtrends(keyword = keyword,
                     category = 67,
           geo = "DE",
           time = str_c("2006-01-01 ", "2019-12-31"))


series[2,3] <- NA
ts_tbl(series)



series <- series %>%
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



series <- as.list(ts_ts(series))
series <- ts_tbl(seasonal::final(seasonal::seas(series,
                                                transform.function = "none")))











