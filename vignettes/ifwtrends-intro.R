## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(knitr)
library(dplyr)
library(tibble)
library(ggplot2)
library(stringr)


## ----message=FALSE------------------------------------------------------------
pca(keywords = c("ikea", "saturn"),
    categories = 0,
    geo = "DE",
    time = str_c("2018-01-01 ",Sys.Date()))


## ---- fig.width = 7, fig.asp = 0.8--------------------------------------------

dat <- pca(keywords = c("ikea", "saturn", "amazon", "ebay"),
    categories = 0,
    geo = "DE",
    time = str_c("2018-08-01 ", Sys.Date()))

series <- dat %>% select(date, 6:9)
factors <- dat %>% select(date, 2:5)

factorR2(series, factors, plot = T)


## ----message=FALSE------------------------------------------------------------
roll(keyword = c("ikea", "saturn"),
     geo = "DE",
     start_series = "2011-01-01",
     start_period = "2018-05-01",
     end = "2018-12-01",
     fun = ts_gtrends)


## ----message=FALSE------------------------------------------------------------
daily_series(keyword = c("arbeitslos"),
           geo = "DE",
           from = "2021-06-01")

