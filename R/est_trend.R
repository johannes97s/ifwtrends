#' Schätzt den Trend einer GTrends-Kategorie
#' @description \code{est_trend}. Schätzt den Trend einer GTrends-Kategorie
#'
#' Schätzt den gemeinsamen Trend aller Google Reihen.
#'
#' @return Gibt eine Zeitreihe zurück, die den polynomialen Trend
#' fünften Grades einer Kategorie enthält.
#' @examples \dontrun{
#' est_trend()
#' }
#' @import tibble magrittr
#' @importFrom gtrendsR gtrends
#' @import gtrendsR
#' @importFrom stringr str_c
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom openxlsx write.xlsx
#' @importFrom readxl read_excel
#' @importFrom stats poly
#' @importFrom rlang !!
#' @importFrom rlang :=
#' @export
est_trend <- function(){

  end = Sys.Date()
  dates <- seq.Date(
    from = as.Date( "2006-01-01"),
    to = end,
    by = "month"
    )

  series <- tibble(date = dates)
  missing = NULL
  cat_samp <- unique(c(sample(gtrendsR::categories$id, 20), "67")) #67 is arbitrary chosen

  k = 0
  for (i in cat_samp){

    Sys.sleep(0.1)

    g <- gtrends(
      geo = "DE",
      time = str_c("2006-01-01 ", end),
      category = i
      )$interest_over_time

    if (is.null(g)) {
      missing <- c(missing, i)
    } else {
      series <- bind_cols(series, !!as.character(eval(i)) := g$hits)
    }

    k <- k+1
    print(k)

  }

  series <- series %>%
    pivot_longer( cols = -date, names_to = "id", values_to = "value") %>%
    mutate(value = log(value)) %>%
    arrange(id)

  saveRDS(series, "data/cat_sample.rds")

  fit <- unname(lm(value ~ id -1 +poly(as.numeric(date), 5, raw = T), data = series)$fitted.values)
  comtrend <- series %>%
    mutate(trend = fit) %>%
    filter(id == 67) %>%
    select(date, trend)
  saveRDS(comtrend, "data/comtrend.rds")
  series
}




