#' Hauptkomponentenanalyse
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
#'@examples \dontrun{
#'pca(keywords = c("ikea", "saturn"), end = "2020-01-01", components = 1)
#'}
#'@import magrittr tibble gtrendsR
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom stats prcomp
#' @importFrom tidyselect any_of
#' @importFrom tsbox ts_ts
#' @importFrom lubridate as_date
#' @importFrom stringr str_c
#' @export
pca <- function(keywords = NA,
                categories = 0,
                geo = "DE",
                start = "2006-01-01",
                end = Sys.Date(),
                components = max(length(keywords), length(categories))){
  stopifnot("Nur keywords oder categories darf angegeben werden" = is.na(keywords) | categories == 0)
  dates = seq.Date(as.Date(start), as.Date(end), by = "month")
  dat = tibble::tibble()
  for (kw in keywords){
    for (cat in categories){
      tibble::as_tibble(gtrends(
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
      dat <- dplyr::bind_rows(dat, temp)
    }
  }
  pc <- bind_cols(date = dates, as_tibble(prcomp(ts_ts(dat))$x))
  dat <-select(pivot_wider(dat, names_from = key, values_from = value), -date)

  return(bind_cols(pc, dat))
}
