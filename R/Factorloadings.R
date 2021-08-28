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

#'R2 der Regression der Serien auf die Faktoren
#'\code(factorR2)
#'
#'@param series tibble mit den Zeitreihen als Spalten.
#'@param factors tibble mit den Faktoren als Spalten.
#'
#'@return Tabelle der R^2 jeder Zeitreihe auf jeden Faktor
#'@examples
#'2+2
#'@export
factorR2 <- function(series, factors){
  series <- select(series, -1)
  factors <- select(factors, -1)
  R2 <- tibble()
  for (i in seq_along(factors)){
    f <- function(serie) {
      s <- summary(lm(factors[[i]] ~ serie))
      s$r.squared
    }
    R2 <- bind_rows(R2, apply(series, 2, f))
  }
  bind_cols(tibble(factors = str_c("PC",1:length(factors))), R2)
}
#
#
# factors <- dat[2:6]
# series <- dat[7:length(dat)]
#
#
# write.xlsx(factorR2(series, factors), "Faktoren_R2.xlsx")
#
#
#
