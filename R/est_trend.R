library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(zoo)










end = "2019-12-31"
dates <- seq.Date(from = as.Date( "2006-01-01"), to = as.Date(end), by = "month")

series <- tibble(date = dates)
missing = NULL
cat_samp <- unique(c(sample(categories$id, 280), "67", "1003"))
k = 0
for (i in cat_samp){
  Sys.sleep(0.1)
  g <- gtrends(geo = "DE", time = str_c("2006-01-01 ", end), category = i)$interest_over_time
  if (is.null(g)) missing <- c(missing, i)
  else series <- bind_cols(series, !!as.character(eval(i)) := g$hits)
  k = k+1
  print(k)
}

series <- series %>%
  #pivot_longer( cols = -date, names_to = "id", values_to = "value") %>%
  mutate(value = log(value))

#openxlsx::write.xlsx(series, "~/ifwtrends/data/cat_sample_Q42019.xlsx")
#series <- readxl::read_excel("cat_sample_Q42019.xlsx")

series<-arrange(series, id)
fit <- lm(value ~ id -1 +poly(as.numeric(date), 5, raw = T), data = series)
series <- mutate(series, fit = fit$fitted.values)
ggplot(series, aes(x = date, y = fit, color = id)) +
  geom_line() +
  theme(legend.position = "none")

trend_67 <- series %>%
  filter(id == "67") %>%
  select(-value)

trend_1003 <- series %>%
  filter(id == "1003") %>%
  select(-value)

openxlsx::write.xlsx(trend_67, "~/ifwtrends/data/trend_67.xlsx")
openxlsx::write.xlsx(trend_1003, "~/ifwtrends/data/trend_1003.xlsx")
