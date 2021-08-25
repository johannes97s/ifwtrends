library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)


# proc_keyword_init("arbeitslos", "DE")
# proc_keyword_init("Hartz 4", "DE")
# proc_index("arbeitslos", "DE", "arbeitslos_ind")
#
#
# s <- ts_gtrends_mwd("amazon", "DE")
#
#


keyword = c("arbeitslos")
geo = "DE"

from <- "2006-01-01"
d <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = from,
  stepsize = "15 days", windowsize = "6 months",
  n_windows = 348, wait = 20, retry = 10,
  prevent_window_shrinkage = TRUE
)
d2 <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = seq(Sys.Date(), length.out = 2, by = "-90 days")[2],
  stepsize = "1 day", windowsize = "3 months",
  n_windows = 12, wait = 20, retry = 10,
  prevent_window_shrinkage = FALSE
)
dd <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(d), trendecon:::aggregate_windows(d2))

# download weakly series
w <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = from,
  stepsize = "11 weeks", windowsize = "5 years",
  n_windows = 68, wait = 20, retry = 10,
  prevent_window_shrinkage = TRUE
)
w2 <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = seq(Sys.Date(), length.out = 2, by = "-1 year")[2],
  stepsize = "1 week", windowsize = "1 year",
  n_windows = 12, wait = 20, retry = 10,
  prevent_window_shrinkage = FALSE
)
ww <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(w), trendecon:::aggregate_windows(w2))

# download monthly series
m <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = from,
  stepsize = "1 month", windowsize = "15 years",
  n_windows = 12, wait = 20, retry = 10,
  prevent_window_shrinkage = FALSE
)
m2 <- trendecon:::ts_gtrends_windows(
  keyword = keyword,
  geo = geo,
  from = from,
  stepsize = "1 month", windowsize = "20 years",
  n_windows = 12, wait = 20, retry = 10,
  prevent_window_shrinkage = FALSE
)
mm <- trendecon:::aggregate_averages(trendecon:::aggregate_windows(m), trendecon:::aggregate_windows(m2))



dd <- select(dd, -n)
ww <- select(ww, -n)
mm <- select(mm, -n)

ww %>%
  mutate(week = week(time), year = year(time)) %>%
  filter(week <= 52) %>%
  select(time, value) -> ww

mm %>%
  mutate(week = week(time), year = year(time)) %>%
  group_by(week, year) %>%
  mutate(value = mean(value)) %>%
  ungroup() %>%
  select( - time) %>%
  filter(week <= 52) %>%
  unique() %>%
  bind_cols(time = ww$time) %>%
  select(time, value)-> mm




wd <- tempdisagg::td(ww ~ dd, method = "fast", conversion = "mean")
wd <- predict(wd)

mwd <- tempdisagg::td(mm ~ wd, method = "fast", conversion = "mean")
mwd <- predict(mwd)

write.xlsx(mwd, "mwd.xlsx")
write.xlsx(wd, "wd.xlsx")
write.xlsx(mm, "mm.xlsx")
write.xlsx(ww, "ww.xlsx")
