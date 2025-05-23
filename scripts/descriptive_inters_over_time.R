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

# Now I want to bring in the record of carcasses so we can look at --------
min <- lubridate::date(min(edges$minTimestamp)) # 2023-05-15
max <- lubridate::date(max(edges$maxTimestamp)) # 2023-09-14

carcs_full <- readxl::read_excel(here("data/raw/FeedingData from 2018_2024_Translated.xlsx"))

carcs <- carcs_full %>%
  select(ID, date = `Date Event`, lat = `WGS84 - LAT`, long = `WGS84 - LONG`, weight = `The weight of the food (kg)`) %>%
  filter(date >= min & date <= max) %>%
  mutate(date = lubridate::date(date))
carcs_sf <- sf::st_as_sf(carcs, coords = c("long", "lat"), crs = "WGS84")
mapview(carcs_sf)

interactions_over_time_withcarcs <- edges %>%
  mutate(date = lubridate::date(minTimestamp)) %>%
  group_by(type, date) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n/1000, col = type))+
  geom_vline(data = carcs, aes(xintercept = date, alpha = weight), linewidth = 0.5)+
  geom_line(linewidth = 1)+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  labs(y = "Interactions (thousands)", x = "Date")+
  theme(text = element_text(size = 14))
interactions_over_time_withcarcs

ggsave(interactions_over_time_withcarcs, file = here("fig/interactions_over_time_withcarcs.png"), width = 6, height = 4)
