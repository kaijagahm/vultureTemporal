# rough quantification of carcass frequency
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

summer2023 <- summer2023 %>%
  arrange(date) %>%
  group_by(stationName) %>%
  mutate(interval = date-lag(date))

summer2023 %>%
  group_by(stationName) %>%
  summarize(mn = mean(interval, na.rm = T)) %>%
  ggplot(aes(x = mn))+geom_histogram()

summer2023 %>%
  group_by(stationName) %>%
  summarize(ncarcasses = n(),
            mn = mean(interval, na.rm = T)) %>%
  ggplot(aes(x = ncarcasses, y = mn))+
  geom_point()
