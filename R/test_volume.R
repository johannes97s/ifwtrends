library(tidyverse)
library(babynames)
library(gtrendsR)
library(trendecon)
babynames

tb <- tibble()

for (i in babynames$name){
  temp <- bind_cols(ts_gtrends(keyword = i,
                  geo = "US",
                  time = "today 3-m")[1,], name = i)
  tb <- bind_rows(tb, temp)
}

write.csv(tb, "Test_queries_volume.csv")


setHandleParameters(proxyhost = "192.111.129.145",
                    proxyport = 16894)
ts_gtrends(keyword = "ikea", geo = "DE", gprop = "web", onlyInterest = T)
