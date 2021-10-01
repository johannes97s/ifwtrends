#' \eqn{R^2} der Regression der Serien auf die Faktoren
#'@description \code{factorR2} gibt \eqn{R^2} der Regression der Serien auf die Faktoren als Tabelle aus.
#'
#'@param series tibble mit den Zeitreihen als Spalten.
#'@param factors tibble mit den Faktoren als Spalten.
#'@param plot plot=TRUE gibt zusaetzlich einen Barplot der \eqn{R^2} aus.
#'@return Tabelle der \eqn{R^2} jeder Zeitreihe auf jeden Faktor. Wenn plot=T, dann wird
#'zusaetzlich ein Plot zurueckgegeben.
#' @examples \dontrun{
#' dat <- pca(keywords = c("ikea", "saturn", "amazon", "ebay"),
#' categories = 0, geo = "DE", start = "2006-01-01", end = Sys.Date(),
#' components = max(length(keywords), length(categories)))
#'
#' series <- dat %>% select(date, 6:9)
#' factors <- dat %>% select(date, 2:5)
#'
#' factorR2(series, factors, plot = T)
#' }
#' @import tibble ggplot2 magrittr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_c
#' @importFrom stats lm
#' @export
factorR2 <- function(series, factors, plot = F){

  stopifnot("series muss fuehrende Zeitspalte enthalten" = class(series[[1]]) == "Date")
  stopifnot("factors muss fuehrende Zeitspalte enthalten" = class(factors[[1]]) == "Date")

  R2 <- vector("list", length = (dim(series)[2] - 1))

  # Helper to get the R2
  f <- function(series) {
    s <- summary(lm(factors[-1][[i]] ~ series))
    s$r.squared
  }

  # Apply the helper function throughout both dfs
  for (i in seq_along(factors[-1])){
    R2[[i]] <-  apply(series[-1], 2, f)
  }

  res <- bind_cols(
    tibble(factors = str_c("PC",1:length(factors[-1]))),
    bind_rows(R2)
    )

  if (!plot){
    return(res)

  } else {
    pcomp <- pivot_longer(factors, -date, names_to = "series", values_to = "value")
    series <- pivot_longer(series, -date, names_to = "series", values_to = "value")
    r2 <- pivot_longer(res, -factors, names_to = "series", values_to = "R2")

    plt <- r2 %>%
      #filter(factors == "PC1") %>%
      ggplot(aes(x = series, y = R2))+
      geom_bar(stat = "identity")+
      facet_wrap(~factors, ncol = 1)+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 60, size = 6, vjust = 1, hjust=1))+
      labs(title = "Bestimmtheitsmass der Regression auf verschiedene Hauptkomponenten")+
      scale_y_continuous(breaks=c(0, 0.5, 1))

    return(list(res = res, plot = plt))
    }
}
