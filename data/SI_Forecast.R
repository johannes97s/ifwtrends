library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)


end = "2019-12-31"
dates <- seq.Date(from = as.Date( "2006-01-01"), to = as.Date(end), by = "month")


dat <- readxl::read_xlsx("~/Google Trends/Service_Import.xlsx") %>%
  mutate(time = as.yearqtr(as.Date(Name))) %>%
  select(time, value = `BD IMPORTS - SERVICES CONA`) %>%
  filter(time <= as.yearqtr(as.Date(end))) %>%
  mutate(time = as.Date(time)) %>%
  mutate(value = log(value)) %>%
  select(time, dat = value)

fit <- as_tibble(openxlsx::read.xlsx("~/ifwtrends/data/trend_67.xlsx", detectDates = T)) %>%
  select(time = date, fit)
g_dat <- ts_gtrends(keyword = c("Koffer","dienstreise","stau", "reise"),
                    category = 0,
                    geo = "DE",
                    time = str_c("2006-01-01 ", end)) %>%
  mutate(value = log(value))

#g_dat <- ts_pick(ts_prcomp(g_dat), "PC1")

g_dat <- g_dat %>%
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
  select(time, s_adj = value)

# t<-g_dat %>%
#   select(id, time, adj)
#
# t <- as.list(ts_ts(t))
# plot(seasonal::final(seasonal::seas(t, multimode = "R")))





g_dat <- left_join(g_dat, g_dat_adj, by = "time") %>%
  left_join(dat, by = "time") %>%
  #mutate(s_adj = c(0, diff(s_adj)), dat = c(0, diff(dat)))
  unique()


g_dat %>%
  select(id, time, s_adj, dat) %>%
  pivot_longer(cols = -c("id", "time"), names_to = "key", values_to = "values") %>%
  #filter(id == "Koffer") %>%
  ggplot(aes(x = time, y = values, color = id)) +
  facet_grid(key ~., scales = "free_y" ) +
  geom_line()



dat <- dat %>%
  mutate(value = c(rep(0,4),diff(dat, 4)))

g_dat <- g_dat %>%
  mutate(fd = c(rep(0,4), diff(s_adj, 4)))


summary(lm(dat$value~ g_dat$fd))
summary(lm(dat$value~ lag(g_dat$fd)))




