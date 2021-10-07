#' Gibt pca fuer Backtesting zurueck
#'@description \code{roll} Gibt fuer start_period bis end die jeweils dann aktuelle Berechnung von pca aus.
#'
#'@param keyword Eine character-Vektor mit dem Suchbegriffen
#'@param category Ein Numeric Vektor mit den Kategorien
#'@param geo Die Region
#'@param start_series Das Startdatum der Zeitreihen.
#'@param start_period Das Startdatum des Ausgabefensters.
#'@param end Das Enddatum der Zeitreihen.
#'@param fun Funktion, die auf die rollende Zeitreihe angewendet werden soll.
#'@param ... Zusaetzliche Parameter, die an die Funktion in fun weitergegeben werden
#'@return Monatliche Tabelle mit pca in jeder Spalte. Je Spalte wird ein neuer Monat hinzugenommen.
#'
#'@examples \dontrun{
#'roll(keyword = c("ikea", "saturn"), start_period = "2018-01-01", end = "2020-01-01")
#'}
#'@importFrom stringr str_c
#'@importFrom trendecon ts_gtrends
#'@export
roll <- function(keyword = NA,
                 category = 0,
                 geo = "DE",
                 start_series = "2006-01-01",
                 start_period = "2014-01-01",
                 end = Sys.Date(),
                 fun = trendecon::ts_gtrends,
                 ...){
  period <-  seq.Date(as.Date(start_period), as.Date(end), by = "month")
  dates <- seq.Date(as.Date(start_series), as.Date(end), by = "month")
  n <- length(dates)
  f <- function(d) fun(keyword = keyword,
                       category = category,
                       geo = geo,
                       time = stringr::str_c(start_series," ", d),
                       ...)
  tl <- lapply(period, f)

  return(tl)

}

