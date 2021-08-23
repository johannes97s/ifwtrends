library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)

############################
#Damit kann unter Angabe von
g_index <- function(keywords = NA,
                    categories = 0,
                    end,
                    start = as.Date("2004-01-01"),
                    roll = F){
  stopifnot("Nur keywords oder categories darf angegeben werden" = is.na(keywords) | categories == 0)
  dates = seq.Date(as.Date("2004-01-01"), as.Date(end), by = "month")
  if (!roll){
    dat = tibble(date = NULL, keyword = NULL, category = NULL, value = NULL)
    for (kw in keywords){
      for (cat in categories){
        as_tibble(gtrends(
          keyword = kw,
          category = cat,
          geo = "DE",
          time = "all")$interest_over_time)  %>%
          mutate(date = as.Date(date)) %>%
          select(date = date, key = any_of(c("keyword", "category")), value = hits) %>%
          filter(date %in% dates) -> temp
        dat <- bind_rows(dat, temp)
      }
    }
  }
  return(pivot_wider(ts_prcomp(dat), names_from = id, values_from = value))
}

