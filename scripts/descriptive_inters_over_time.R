library(tidyverse)
library(targets)
library(here)
tar_load(feeding_edges)
tar_load(flight_edges)
tar_load(situcolors)
edges <- bind_rows(feeding_edges %>% mutate(type = "feeding"), flight_edges %>% mutate(type = "flight"))
source(here::here("R/functions.R"))

interactions_over_time <- edges %>%
  mutate(date = lubridate::date(minTimestamp)) %>%
  group_by(type, date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n/1000, col = type))+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  labs(y = "Interactions (thousands)", x = "Date")+
  theme(text = element_text(size = 14))
interactions_over_time
ggsave(interactions_over_time, file = here("fig/interactions_over_time.png"), width = 6, height = 4)
