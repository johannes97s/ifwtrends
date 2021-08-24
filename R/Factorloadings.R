library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(forecast)

series <- as_tibble(read.xlsx("~/ifwtrends/Kategorien_und_PC.xlsx", detectDates = T))
lm(series[[2]] ~ `Events.&.Listings`, data = series)


factorR2 <- function(series, factors){
  dat <- bind_cols(series, factors)
  R2 <- tibble(factor = names(factors))
  mapply(f, )
}

g <- function(i){
  lm(series[[i+1]], serie, data = series )
}

f <- function(serie){
  fac <- 2:6
  mapply(g, fac)
}

