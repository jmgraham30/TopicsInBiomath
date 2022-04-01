library(tidyverse)
fd <- read_csv("fake_data.csv")

glimpse(fd)

repeat_across_year <- fd %>%
  filter(capture == 0) %>%
  group_by(id) %>% filter(n() > 1)

glimpse(repeat_across_year)

