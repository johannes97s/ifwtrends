library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)

############################
#Damit kann unter Angabe von
pca <- function(keywords = NA,
                categories = 0,
                start = as.Date("2004-01-01"),
                end,
                components = max(length(keywords), length(categories))){
  stopifnot("Nur keywords oder categories darf angegeben werden" = is.na(keywords) | categories == 0)
  dates = seq.Date(as.Date("2004-01-01"), as.Date(end), by = "month")
  dat = tibble(date = NULL, key = NULL, value = NULL)
  for (kw in keywords){
    for (cat in categories){
      as_tibble(gtrends(
        keyword = kw,
        category = cat,
        geo = "DE",
        time = "all")$interest_over_time)  -> temp
        if (NROW(temp) == 0) stop(str_c("Keine Daten für Kategorie ", cat))
        temp %>%
          mutate(date = as_date(date)) %>%
          select(date = date, key = any_of(c("keyword", "category")), value = hits) %>%
          filter(date %in% dates) -> temp
      dat <- bind_rows(dat, temp)
    }
  }
  return(pivot_wider(ts_prcomp(dat), names_from = id, values_from = value)[1:components])
}

roll <- function(keywords,
                 categories,
                 start_series = as.Date("2006-01-01"),
                 start_period = as.Date("2014-01-01"),
                 end = as.Date("2021-01-01"),
                 components = max(length(kewyords), length(categories))){
  period <-  seq.Date(as.Date(start_period), as.Date(end), by = "month")
  dates <- seq.Date(as.Date("2006-01-01"), as.Date(end), by = "month")
  pc <- tibble(date = dates)
  n <- length(dates)#L?nge der ganzen Reihe
  for (i in period){
    d <- as.Date(i, origin = "1970-01-01")
    pca(keywords = keywords, categories = categories, start = start_series, end = d) %>%
      select(-time()) -> temp
    names(temp) <- c()

    }
}
