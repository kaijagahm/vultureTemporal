# rough quantification of carcass frequency--for reference in discussion
library(targets)
library(sf)
library(mapview)
library(tidyverse)
library(here)

tar_load(carcasses_audited, store = "~/Desktop/projects/arrivalAtCarcasses/_targets/")
tar_load(bbox_south_big, store = "~/Desktop/projects/arrivalAtCarcasses/_targets/")
summer2023 <- carcasses_audited %>% filter(date >= "2023-05-14", date <= "2023-09-15")
summer2023 <- st_crop(summer2023, bbox_south_big)
summer2023 %>%
  ggplot(aes(x = date, y = lat, col = stationName, size = carcassWeight))+
  geom_point()+
  theme_minimal()+
  guides(color = "none")

levels <- summer2023 %>% group_by(stationName) %>% summarize(mn = mean(interval, na.rm = T)) %>% arrange(mn) %>% pull(stationName)
summer2023 %>%
  mutate(stationName = factor(stationName, levels = rev(levels))) %>%
  ggplot(aes(x = stationName, y = interval))+
  geom_hline(aes(yintercept = 2), col = "red", linetype = 2, alpha = 0.5)+
  geom_hline(aes(yintercept = 20), col = "red", linetype = 2, alpha = 0.5)+
  geom_boxplot(fill = NA, col = "lightgray")+
  geom_jitter(width = 0.1, alpha = 0.5, pch = 1, size = 1)+
  scale_y_continuous(breaks = seq(from = 0, to = 90, by = 5))+
  labs(y = "Carcass interval (days)",
       x = "Feeding station")+
  coord_flip()+
  geom_point(data = summer2023 %>% group_by(stationName) %>% summarize(mn = mean(interval, na.rm = T)), aes(x = stationName, y = mn), col = "dodgerblue", size = 2)

summer2023 %>%
  ggplot(aes(x = date, y = lat))+
  geom_point() # does not appear to be a decline in carcasses over time in general
