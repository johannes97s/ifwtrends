# library(tidyverse)
# library(gtrendsR)
# library(trendecon)
# library(openxlsx)
# library(corrr)
#
# source("Trends.R")
#
#
#
#
#
# x1 <- lag(dat_comp$Index)
# y1 <- dat_comp$VDAXm
#
# fit1 <- lm(y1 ~ x1)
# summary(fit1)
#
# q <- quantile(x1, 0.75, na.rm = T)
# y2 <- y1[x1 >= q]
# x2 <- x1[x1 >= q]
#
#
# plot(x2, y2)
# fit2 <- lm(y2 ~ x2)
# summary(fit2)
#
# correlate(x2, y2)
