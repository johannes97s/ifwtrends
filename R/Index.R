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
                n = max(length(kewyords), length(categories))){
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
  if (roll){
    }
  return(pivot_wider(ts_prcomp(dat), names_from = id, values_from = value))
}

roll <- function(keywords, categories, start = as.Date("2014-01-01"), end = as.Date("2021-01-01")){
  period <-  seq.Date(as.Date(start), as.Date(end), by = "month")

}
