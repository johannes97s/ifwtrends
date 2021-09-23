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
  time = str_c("2006-01-01 ", Sys.Date())){

    start <- str_sub(time, 1,10)
    end <- str_sub(time, 12,21)
    dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = "month")


    fit <- as_tibble(openxlsx::read.xlsx("~/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
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
      mutate(time = as.Date(time), adj = value - fit)


    g_dat_adj <- g_dat %>%
      select(id, time, adj) %>%
      seas_adj(freq = "quart", method = "arima") %>%
      rename(s_adj = value)
    #print(g_dat_adj)
    if (!("id" %in% names(g_dat_adj))) g_dat_adj <- mutate(g_dat_adj, id = as.character(category))





    g_dat <- left_join(g_dat, g_dat_adj, by = c("time", "id")) %>%
      unique()


    index <- g_dat %>%
      select(id, time, s_adj) %>%
      group_by(id) %>%
      #mutate(s_adj = c(0, diff(s_adj, 1))) %>%
      group_by(id) %>%
      mutate(lag_1 = lag(s_adj),
             #lag_2 = lag(s_adj,2),
             #lag_3 = lag(s_adj,3),
             #lag_4 = lag(s_adj,4)
             ) %>%
      ungroup() %>%
      rename(lag_0 = s_adj) %>%
      filter(across(everything(), ~!is.na(.))) %>%
      pivot_longer(cols = -c(id, time), names_to = "lag", values_to = "value") %>%
      pivot_wider(names_from = c(id, lag), values_from = value)

      return(index)
}


start = "2011-01-01"
end = "2019-12-30"

dat <- readxl::read_xlsx("~/Google Trends/Einzelhandel.xlsx", sheet = "Abbildung") %>%
  select(c(1,3,12))
dat <- dat[-c(1:182,370:375),]

names(dat) <- c("time", "einzelhandel", "gastgewerbe")

dates = seq.Date(as_date("2006-01-01"), as_date("2021-07-01"), by = "month")

dat <- dat %>%
  select(time, einzelhandel)


names(dat) <- c("time", "value")

dat <- dat %>%
  mutate(value = log(as.numeric(value))) %>%
  mutate(value = c(0, diff(value, 1))) %>%
  mutate(time = dates) %>%
  filter(time >= as_date(start), time <= as_date(end))

dat <- dat[-c(1, nrow(dat)),]

keyword = c(NA)
category_aco= c(956, 276, 179)
category_ret = c(18, 78, 68, 531, 355, 121, 841)
category = category_ret

res_raw <- g_index(keyword = keyword, category = category, time = str_c(start, " ", end))

res <- res_raw


est <- filter(res, time <= max(dat$time))




y <- dat[2]
x <- est[-1]

d <- bind_cols(y,x)


#cv <- cv.glmnet(x, y, alpha = 0)
model <- lm(value ~ . , data = d)#glmnet(x, y, alpha = 0, lambda = 0)




x2 <- res[-1]

dat[nrow(x2)-2,] <- list(max(dat$time) + months(1), NA)
dat[nrow(x2)-1,] <- list(max(dat$time) + months(1), NA)
dat[nrow(x2),] <- list(max(dat$time) + months(1), NA)



index<- bind_cols(dat, rename(as_tibble(predict(model, x2)), index = value))%>%
  left_join(res)


openxlsx::write.xlsx(index, "Gastgewerbe und Index.xlsx")

index %>%
  select(time, value, index) %>%
  pivot_longer(cols = -time, names_to = "id", values_to = "value") %>%
  ggplot(aes(x = time, y = value, color = id))+
  geom_line() +
  xlim(as_date("2006-01-01"), NA) +
  ggtitle("Google Suchanfragen bis 14.09.2021")







############
keyword_12 = c(NA)
category_12 = category

r1 <-  roll(keyword = keyword_12,
            category = category_12,
            start_series = "2006-01-01",
            start_period = "2017-01-01",
            fun = h,
            dat = dat)




forecast <- function(series){

}















t <- ts_gtrends(keyword = c("Hotel"),
            category = 67,
            geo = "DE",
            time = str_c("2011-01-01 ", "2017-03-01")) %>%
  mutate(value = log(value)) %>%
  mutate(value = replace(value, value == -Inf, NA_real_)) %>%
  mutate(value = na.approx(value, rule = 2))
if (!("id" %in% names(t))) t <- mutate(t, id = as.character(category))


fit <- as_tibble(openxlsx::read.xlsx("~/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
  select(time = date, fit) %>%
  filter(time >= as.Date("2011-01-01"))

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
seasonal::final(s1)


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



