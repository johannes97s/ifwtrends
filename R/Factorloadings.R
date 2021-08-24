library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)
library(forecast)

dat <- as_tibble(read.xlsx("~/ifwtrends/Kategorien_und_PC.xlsx", detectDates = T))
s = summary(lm(dat[[3]] ~ dat[[7]], data = dat))
s$r.squared


f <- function(serie, i, dat){
  summary(lm(dat[[i+1]] ~ serie[[1]]))$r.squared
}

factorR2 <- function(series, factors){
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
