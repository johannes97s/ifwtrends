library(tidyverse)
library(gtrendsR)
library(trendecon)
library(tsbox)
library(openxlsx)


dates <- seq.Date(from = as.Date( "2004-01-01"), to = as.Date("2021-08-01"), by = "month")


####################Tibble mit Kategorien aus dem Paper erstellen
cat_paper <- as_tibble(read.csv("Kategorien_Woloszko.csv", head = F))
names(cat_paper) = "name"
cat<- as_tibble(categories)

cat_paper %>%
  left_join(cat, by = "name") %>%
  unique() -> cat_paper
##################################################

##################Tibble mit Zeitreihen erstellen
series <- tibble(date = dates)

missing = NULL
for (i in cat_paper$id[21:50]){
  g <- gtrends(geo = "DE", time = "all", category = i)$interest_over_time
  if (is.null(g)) missing <- c(missing, cat_paper$name[21:50][cat_paper$id[21:50] == i])
  else series <- bind_cols(series, i = g$hits)
}

names(series) <- c("date", setdiff(cat_paper$name[21:50], missing))


series %>%
  pivot_longer(cols = -date, names_to = "id", values_to = "hits") -> series2
pc <- ts_pick(ts_prcomp(series2), c("PC1", "PC2", "PC3", "PC4", "PC5"))

#####Exportieren
# series2 <- pivot_wider(series2, names_from = id, values_from = hits)
#pc <- rename(pivot_wider(pc, names_from = id, values_from = value), date = time)
#
# write.xlsx(left_join(pc, series2, by = "date"), file = "Kategorien_und_PC.xlsx")

###################Loadings der Hauptkomponenten

load <- prcomp(series[-1], scale. = T)$rotation[,1:5]


##########################################
########plots
ggplot(series2, aes(x = date, y = hits, color = id)) +
  geom_line()

ggplot(pc, aes(x = time, y = value, color = id)) +
  geom_line()
####################################






