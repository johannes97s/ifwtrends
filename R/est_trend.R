

library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)


est_trends <- function(){
  end = Sys.Date
  dates <- seq.Date(from = as.Date( "2006-01-01"), to = as.Date(end), by = "month")

  series <- tibble(date = dates)
  missing = NULL
  cat_samp <- unique(c(sample(categories$id, 280), "67")) #67 is arbitrary chosen
  k = 0
  for (i in cat_samp){
    Sys.sleep(0.1)
    g <- gtrends(geo = "DE", time = str_c("2006-01-01 ", end), category = i)$interest_over_time
    if (is.null(g)) missing <- c(missing, i)
    else series <- bind_cols(series, !!as.character(eval(i)) := g$hits)
    k <- k+1
    print(k)
  }

  series <- series %>%
    pivot_longer( cols = -date, names_to = "id", values_to = "value") %>%
    mutate(value = log(value))

  openxlsx::write.xlsx(series, "~/ifwtrends/data/cat_sample_Q32021.xlsx")
  series <- readxl::read_excel("cat_sample_Q42019.xlsx")

  series<-arrange(series, id)
  fit <- lm(value ~ id -1 +poly(as.numeric(date), 5, raw = T), data = series)
  series <- mutate(series, date = as.Date(date), fit = fit)
  series
}
###################





save(series %>%
  filter(id == "67") %>%
  select(-value), file = "comtrend.RData")




