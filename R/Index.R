library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(corrr)
library(lubridate)

############################


#'Hauptkomponenten aus Suchbegriffen oder Kategorien
#'\code{pca} Berechnet für mehrere Suchbegriffe oder mehrere Kategorien die Hauptkomponenten der monatl Zeitreihen
#'
#'@param keywords Eine character-Vektor mit dem Suchbegriffen
#'@param categories Ein Numeric Vektor mit den Kategorien
#'@param geo Die Region
#'@param start Das Startdatum der Zeitreihen.
#'@param end Das Enddatum der Zeitreihen.
#'@param components Die gewünschte Anzahl an Hauptkomponenten.
#'
#'@retrun Monatliche Tabelle der Hauptkomponenten und der Zeitreihen.
#'@examples pca(keywords = c("ikea", "saturn"), end = "2020-01-01", components = 1
#'
#'@export

pca <- function(keywords = NA,
                categories = 0,
                geo = "DE",
                start = "2006-01-01",
                end = Sys.Date(),
                components = max(length(keywords), length(categories))){
  stopifnot("Nur keywords oder categories darf angegeben werden" = is.na(keywords) | categories == 0)
  dates = seq.Date(as.Date(start), as.Date(end), by = "month")
  dat = tibble()
  for (kw in keywords){
    for (cat in categories){
      as_tibble(gtrends(
        keyword = kw,
        category = cat,
        geo = geo,
        time = "all")$interest_over_time) -> temp
        if (NROW(temp) == 0) stop(str_c("Keine Daten für Kategorie ", cat))
        if ("keyword" %in% names(temp)) temp <- select(temp, -category)
        temp %>%
          mutate(date = as_date(date)) %>%
          select(date, key = any_of(c("keyword", "category")), value = hits) %>%
          filter(date %in% dates) -> temp
      dat <- bind_rows(dat, temp)
    }
  }
  pc <- pivot_wider(ts_prcomp(dat), names_from = id, values_from = value)[1:(components+1)]
  dat <-select(pivot_wider(dat, names_from = key, values_from = value), -date)
  bind_cols(pc, dat)
}

#' Gibt

roll <- function(keywords = NA,
                 categories = 0,
                 start_series = "2006-01-01",
                 start_period = "2014-01-01",
                 end = Sys.Date(),
                 components = max(length(keywords), length(categories))){
  period <-  seq.Date(as.Date(start_period), as.Date(end), by = "month")
  dates <- seq.Date(as.Date("2006-01-01"), as.Date(end), by = "month")
  pc <- tibble(date = dates)
  n <- length(dates)#L?nge der ganzen Reihe
  for (i in period){
    d <- as.Date(i, origin = "1970-01-01")
    pca(keywords = keywords,
        categories = categories,
        start = start_series,
        end = d,
        components = components) %>%
        select(-time) -> temp
    rest <- matrix(NA, n - nrow(temp), components)
    colnames(rest) <- str_c("PC", 1:components)
    rest <- as_tibble(rest)
    temp <- bind_rows(temp, rest)
    names(temp) <- str_c(names(temp), " to ", d)
    pc <- bind_cols(pc, temp)
  }
  pc
}
