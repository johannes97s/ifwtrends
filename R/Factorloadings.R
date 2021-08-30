library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(forecast)


#Hilfsfunktion
f <- function(serie, i, dat){
  summary(lm(dat[[i+1]] ~ serie[[1]]))$r.squared
}

#' R^2 der Regression der Serien auf die Faktoren
#'@description \code{factorR2} gibt R2 der Regression der Serien auf die Faktoren als Tabelle aus.
#'
#'
#'@param series tibble mit den Zeitreihen als Spalten.
#'@param factors tibble mit den Faktoren als Spalten.
#'@param plot plot=TRUE gibt zusätzlich einen Barplot der R2 aus.
#'
#'@return Tabelle der R^2 jeder Zeitreihe auf jeden Faktor. Wenn plot=T zusätzlich Plot.
#'@examples
#'2+2
#'@export
#'
factorR2 <- function(series, factors, plot = F){
  stopifnot("series muss führende Zeitspalte enthalten" = class(series[[1]]) == "Date")
  stopifnot("factors muss führende Zeitspalte enthalten" = class(factors[[1]]) == "Date")
  R2 <- tibble()
  for (i in seq_along(factors[-1])){
    f <- function(serie) {
      s <- summary(lm(factors[-1][[i]] ~ serie))
      s$r.squared
    }
    R2 <- bind_rows(R2, apply(series[-1], 2, f))
  }
  res <- bind_cols(tibble(factors = str_c("PC",1:length(factors[-1]))), R2)
  if (!plot) return(res)
  if (plot){
    pivot_longer(factors, -date, names_to = "series", values_to = "value") -> pcomp
    pivot_longer(series, -date, names_to = "series", values_to = "value") -> series
    pivot_longer(res, -factors, names_to = "series", values_to = "R2")-> r2
    r2 %>%
      #filter(factors == "PC1") %>%
      ggplot(aes(x = series, y = R2))+
      geom_bar(stat = "identity")+
      facet_wrap(~factors, ncol = 1)+
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 60, size = 6, vjust = 1, hjust=1))+
      labs(title = "Bestimmtheitsmaß der Regression auf verschiedene Hauptkomponenten")+
      scale_y_continuous(breaks=c(0, 0.5, 1))
    return(res)
  }
}


