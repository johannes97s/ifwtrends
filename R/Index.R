library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)

g_index <- function(keywords = NULL,
                    categories = 0,
                    end,
                    start = as.Date("2004-01-01"),
                    roll = F){
  dates = seq.Date(as.Date("2004-01-01"), as.Date(end), by = "month")
  if (!roll){
    ts_gtrends(
      keyword = kw,
      geo = "DE",
      time = "all") %>%
       filter(time %in% dates) -> dat
  return(prcomp(ts_ts(dat)))
  }
}
