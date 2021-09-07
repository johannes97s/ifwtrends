library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)
library(caret)
library(glmnet)
library(lattice)

end = Sys.Date()




g_index <- function(
  keywords = NA,
  categories = 0,
  geo = "DE",
  end = Sys.Date(),
  dat,
  fd = T){


    dates <- seq.Date(from = as.Date( "2006-01-01"), to = as.Date(end), by = "month")
    dat <- dat %>%
      mutate(time = as.yearqtr(as.Date(time))) %>%
      filter(time <= as.yearqtr(as.Date(end))) %>%
      mutate(time = as.Date(time)) %>%
      mutate(value = log(value)) %>%
      select(time, dat = value)

    fit <- as_tibble(openxlsx::read.xlsx("~/ifwtrends/data/trend_67_0921.xlsx", detectDates = T)) %>%
      select(time = date, fit)

    g_dat2 <- ts_gtrends(keyword = keywords,
                        category = categories,
                        geo = "DE",
                        time = str_c("2006-01-01 ", end)) %>%
                mutate(value = log(value))
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
        select(id, time, s_adj, dat) #%>%
        #group_by(id) %>%
        #mutate(s_adj = c(0, diff(s_adj)), dat = c(0, diff(dat)))

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


    index<- bind_cols(index, as_tibble(predict(model, x, cv$lambda.min)))
    # index %>%
    #   select(time, dat, s1) %>%
    #   pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color =key)) +
    #   #facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()


    k = 4
    fd_tb <- index %>%
      select(time, dat, s1) %>%
      mutate(dat = c(rep(0,k), diff(dat,k)), s1 = c(rep(0,k), diff(s1,k)))

    # fd_tb %>%
    #   pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
    #   #filter(id == "Koffer") %>%
    #   ggplot(aes(x = time, y = values, color =key)) +
    #   #facet_grid(key ~., scales = "free_y" ) +
    #   geom_line()
    if (fd) return(fd_tb)
    else return(index)

}

dat <- readxl::read_xlsx("~/Google Trends/Service_Import.xlsx")
names(dat) <- c("time","value")
keywords = c("Koffer",
             "Reisepass",
             "dienstreise",
             "stau",
             "flug",
             "Hotel")
res <- g_index(keywords = keywords, dat = dat)

res %>%
  pivot_longer(cols = -time, names_to = "key", values_to = "values") %>%
  #filter(id == "Koffer") %>%
  ggplot(aes(x = time, y = values, color =key)) +
  #facet_grid(key ~., scales = "free_y" ) +
  geom_line()


summary(lm(dat~lag(s1), data = fd_tb))
xyplot(dat~ lag(s1), data = fd_tb, type = c("p","r"), col.line = "red")




