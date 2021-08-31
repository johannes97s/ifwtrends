library(tidyverse)


t <- ts_gtrends("ikea", time = "all") %>%
  mutate(value = log(value))



fd_1 = c(0, diff(t$value))
fd_12 = c(rep(0,12), diff(fd_1, 12))

bind_cols(t, fd_1 = fd_1, fd_12 = fd_12) %>%
  pivot_longer(cols = -time, names_to = "id", values_to= "value") %>%
  ggplot(aes(x = time, y = value, color = id)) +
  geom_line() +
  facet_grid(id ~ ., scales = "free_y") +
  theme(legend.position = "none")


