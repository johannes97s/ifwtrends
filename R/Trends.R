library(tidyverse)
library(gtrendsR)
library(trendecon)
library(openxlsx)
library(corrr)
library(lubridate)

kw = c("Wirtschaftskrise", "Kurzarbeit", "arbeitslos", "Insolvenz")
kw <- ind_activ
ts_gtrends(
  keyword = kw,
  geo = "US",
  time = "all"
) %>%
  filter(time >= as.Date("2006-01-01") & time <= as.Date("2021-06-01")) -> dat
dat$time
#################Index aus Paper importieren

index_paper <-as.tibble(read.csv("https://raw.githubusercontent.com/trendecon/data/master/data/de/trendecon_sa.csv"))


index_paper %>%
  mutate(time = floor_date(as.Date(time), "month")) %>%
  group_by(time) %>%
  mutate(Index_paper = mean(value)) %>%
  select(-value) %>%
  unique() %>%
  mutate(time = as.Date(time)) %>%
  filter((time >= as.Date("2006-01-01") & time <= as.Date("2021-06-01")))-> index_paper
#######################

pca <- function(series, start = "2014-01-01", end = max(series$time)){
  period <- seq.Date(as.Date(start), as.Date(end), by = "month")
  dates <- seq.Date(as.Date("2006-01-01"), as.Date(end), by = "month")
  pc <- tibble(id = "PC1", time = dates)
  n <- length(dates)#L?nge der ganzen Reihe

  for(i in period){ #L?nge der Periode oft
    d <- as.Date(i, origin = "1970-01-01")
    series %>%
      filter(time <= d) -> temp
    pc_temp <- ts_pick(ts_prcomp(temp), "PC1")
    pc <- bind_cols(pc, tibble(n = c(pc_temp$value, rep(NA, (n-length(pc_temp$value))))))
  }
  return(pc)
}
tb <- pca(dat, start = "2014-01-01", end = "2021-06-01")


dates <- seq.Date(as.Date("2014-01-01"), as.Date("2021-06-01"), by = "month")
names = c("id", "time",str_c("to ", dates ))
names(tb) <- names


tb %>%
  select(-id) %>%
  left_join(index_paper, by = "time") %>%
  left_join(pivot_wider(dat, names_from = "id", values_from = "value"), by = "time")  -> tb

write.xlsx(tb, file = "Google_Index.xlsx")



# pc <- pca(dat, start = "2014-01-01")$plt_data
#
# pc1 <- ts_pick(ts_prcomp(dat), "PC1")
#
# pc <- bind_rows(pc1, pc)
#
#
# plt <- ggplot(pc, aes(x = time, y = value )) +
#   geom_line(aes(color = id))
#
# plt

###################Daten Einlesen und aufbereiten

dat_ger <- readxl::read_xlsx("Deutschland-Daten.xlsx", sheet = "Monatsdaten")
dat_ger %>%
  mutate(time = as.Date(Monatsdaten)) %>%
  select(time, Ifo_new, Erwartungm_new, VDAXm, policym, Retailsalesm, GFK) %>%
  filter((time >= as.Date("2006-01-01") & time <= as.Date("2021-07-01"))) %>%
  mutate(time = floor_date(time, "month"),
         Ifo_new = as.numeric(Ifo_new),
         Retailsalesm = as.numeric(Retailsalesm)) %>%
  mutate(Retailsalesm_growth = (Retailsalesm - lag(Retailsalesm))/Retailsalesm) -> dat_ger


##################Unseren Index erstellen und zusammenf?gen
ts_pick(ts_prcomp(dat), "PC1") %>%
  select(time, value) %>%
  right_join(dat_ger, by = "time") %>%
  right_join(index_paper, by = "time") %>%
  rename(Index = value) %>%
  #mutate(Index = -(Index-mean(Index))/sd(Index)) %>%
  relocate(Index_paper, .after = Index) -> dat_comp


####################Plot und Correlation
dat_comp %>%
  pivot_longer(cols = c(Index, Index_paper), names_to = "id", values_to = "value") %>%
  ggplot(aes(x = time, y = value, color = id)) +
  geom_line()

dat_comp %>%
  select(-c(time)) %>%
  correlate(use = "complete")
########################################

#####################Crosscorrelation

tibble(lag = -3:3,
       Ifo = as.numeric(ccf(dat_comp$Index, dat_comp$Ifo_new, lag = 3, pl = F)$acf),
       Erwartungm_new = as.numeric(ccf(dat_comp$Index, dat_comp$Erwartungm_new, lag = 3, pl = F)$acf),
       VDAXm = as.numeric(ccf(dat_comp$Index, dat_comp$VDAXm, lag = 3, pl = F)$acf)) #-> cross_tb
######################################



####################Ohne Krisen
dat_comp %>%
  filter(time >= as.Date("2010-01-01") & time <= as.Date("2020-01-01")) %>%
  select(-c(time)) %>%
  correlate(use = "complete")
#######################


################GDP einlesen
GDP <- readxl::read_xlsx("Deutschland-Daten.xlsx", sheet = "Quartalsdaten")
GDP %>%
  mutate(time = as.Date(Quartalsdaten)) %>%
  select(time, BIP) %>%
  filter(time >= as.Date("2006-01-01")) %>%
  mutate(time = quarter(time, with_year = T))-> GDP

################Neue Spalte mit Wachstum
a<- c(0, GDP$BIP)[-length(c(0, GDP$BIP))]
b <- GDP$BIP

GDP_growth <- (b-a)/a

bind_cols(GDP, GDP_growth = GDP_growth) -> GDP

###############Indizes auf Quartal aggregieren
ts_pick(ts_prcomp(dat), "PC1") %>%
  left_join(index_paper) %>%
  filter(time <= as.Date("2021-07-01")) %>%
  select(time, Index = value, Index_paper) %>%
  mutate(time = quarter(time, with_year = T)) %>%
  group_by(time) %>%
  mutate(Index = mean(Index)) %>%
  unique() -> Index_quart


################Zusammenfuehren mit GDP
Index_quart %>%
  left_join(GDP) %>%
  filter(time >= 2006.2) %>%
  select(-BIP) %>%
  ungroup() -> dat_comp2
###############

##############Korrelationsmatrix berechnen
dat_comp2 %>%
  select(-time) %>%
  correlate(use = "complete")

#####################Crosscorrelation mit GDP

tibble(lag = -3:3,
       GDP_growth = as.numeric(ccf(dat_comp2$Index, dat_comp2$GDP_growth, lag = 3, pl = F)$acf)) #-> cross_tb
######################################


