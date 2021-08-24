library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)


proc_keyword_init("arbeitslos", "DE")
proc_keyword_init("Hartz 4", "DE")
proc_index("arbeitslos", "DE", "arbeitslos_ind")


s <- ts_gtrends_mwd("amazon", "DE")


h <- function (keyword = "Insolvenz", geo = "CH")
  {
    if (length(keyword) > 1)
      stop("Only a single keyword is allowed.")
    from <- "2016-01-01"
    d <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = from,
                            stepsize = "15 days", windowsize = "6 months",
                            n_windows = 348, wait = 20, retry = 10, prevent_window_shrinkage = TRUE)
    d2 <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = seq(Sys.Date(),
                                                                      length.out = 2, by = "-90 days")[2], stepsize = "1 day",
                             windowsize = "3 months", n_windows = 12, wait = 20,
                             retry = 10, prevent_window_shrinkage = FALSE)
    dd <- aggregate_averages(aggregate_windows(d), aggregate_windows(d2))
    w <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = from,
                            stepsize = "11 weeks", windowsize = "5 years",
                            n_windows = 68, wait = 20, retry = 10, prevent_window_shrinkage = TRUE)
    w2 <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = seq(Sys.Date(),
                                                                      length.out = 2, by = "-1 year")[2], stepsize = "1 week",
                             windowsize = "1 year", n_windows = 12, wait = 20,
                             retry = 10, prevent_window_shrinkage = FALSE)
    ww <- aggregate_averages(aggregate_windows(w), aggregate_windows(w2))
    m <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = from,
                            stepsize = "1 month", windowsize = "15 years",
                            n_windows = 12, wait = 20, retry = 10, prevent_window_shrinkage = FALSE)
    m2 <- trendecon:::ts_gtrends_windows(keyword = keyword, geo = geo, from = from,
                             stepsize = "1 month", windowsize = "20 years",
                             n_windows = 12, wait = 20, retry = 10, prevent_window_shrinkage = FALSE)
    mm <- aggregate_averages(aggregate_windows(m), aggregate_windows(m2))
    dd <- select(dd, -n)
    ww <- select(ww, -n)
    mm <- select(mm, -n)
    print(dd)
    wd <- tempdisagg::td(ww ~ dd, method = "fast", conversion = "mean")
    wd <- predict(wd)
    mwd <- tempdisagg::td(mm ~ wd, method = "fast", conversion = "mean")
    mwd <- predict(mwd)
    mwd
}
h("arbeitslos", "DE")
