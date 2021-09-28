############################


#'Hauptkomponenten aus Suchbegriffen oder Kategorien
#'@description \code{pca} Berechnet fuer mehrere Suchbegriffe oder mehrere Kategorien die Hauptkomponenten der monatlichen Zeitreihen
#'
#'@param keywords Eine character-Vektor mit dem Suchbegriffen
#'@param categories Ein Numeric Vektor mit den Kategorien
#'@param geo Die Region
#'@param start Das Startdatum der Zeitreihen.
#'@param end Das Enddatum der Zeitreihen.
#'@param components Die gewuenschte Anzahl an Hauptkomponenten.
#'
#'@return Monatliche Tabelle der Hauptkomponenten und der Zeitreihen.
#'@examples pca(keywords = c("ikea", "saturn"), end = "2020-01-01", components = 1)
#'
#'@import tidyverse gtrendsR tsbox lubridate
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
        if (NROW(temp) == 0) stop(str_c("Keine Daten fuer Kategorie ", cat))
        if ("keyword" %in% names(temp)) temp <- select(temp, -category)
        temp %>%
          mutate(date = as_date(date)) %>%
          select(date, key = any_of(c("keyword", "category")), value = hits) %>%
          filter(date %in% dates) -> temp
      dat <- bind_rows(dat, temp)
    }
  }
  pc <- bind_cols(date = dates, as_tibble(prcomp(ts_ts(dat))$x))
  dat <-select(pivot_wider(dat, names_from = key, values_from = value), -date)
  bind_cols(pc, dat)
}

#' Gibt pca fuer Backtesting zurueck
#'@description \code{roll} Gibt fuer start_period bis end die jeweils dann aktuelle Berechnung von pca aus.
#'
#'@param keywords Eine character-Vektor mit dem Suchbegriffen
#'@param categories Ein Numeric Vektor mit den Kategorien
#'@param geo Die Region
#'@param start_series Das Startdatum der Zeitreihen.
#'@param start_period Das Startdatum des Ausgabefensters.
#'@param end Das Enddatum der Zeitreihen.
#'@param components Die gewuenschte Anzahl an Hauptkomponenten.
#'@param fun Funktion, die auf die rollende Zeitreihe angewendet werden soll.
#'
#'@return Monatliche Tabelle mit pca in jeder Spalte. Je Spalte wird ein neuer Monat hinzugenommen.
#'
#'@examples roll(keywords = c("ikea", "saturn"), start_period = "2018-01-01", end = "2020-01-01")
#'@import tidyverse
#'@export
roll <- function(keywords = NA,
                 categories = 0,
                 geo = "DE",
                 start_series = "2006-01-01",
                 start_period = "2014-01-01",
                 end = Sys.Date(),
                 fun = trendecon::ts_gtrends,
                 ...){
  period <-  seq.Date(as.Date(start_period), as.Date(end), by = "month")
  dates <- seq.Date(as.Date(start_series), as.Date(end), by = "month")
  n <- length(dates)
  f <- function(d) fun(keyword = keywords,
                       category = categories,
                       geo = geo,
                       time = stringr::str_c(start_series," ", d),
                       ...)
  tl <- lapply(period, f)
  # tl <- lapply(tl, function(x){
  #                       select(x, -time)
  #                       rest <- matrix(NA, n - nrow(x), 1)
  #                       colnames(rest) <- "value"
  #                       rest <- as_tibble(rest)
  #                       bind_rows(x, as_tibble(rest))} )

  tl

}

