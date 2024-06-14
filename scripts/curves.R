library(targets)
library(tidyverse)
source("R/functions.R")
tar_load(curves)
tar_load(curves_hours)
tar_load(curves_seasons)
theme_set(theme_minimal())

## Seasons
curves_seasons %>%
  ggplot(aes(x = step, y = ent, col = situ))+
  geom_point(size = 2, alpha = 0.7)+
  geom_line(linewidth = 1, alpha = 0.7)+
  scale_color_manual(name = "Situation", values = situcolors)+
  ylab("Relative entropy") + xlab("Amount of aggregation")+
  scale_x_continuous(breaks = 1:9)+
  theme(panel.grid.minor = element_blank(),
        text = element_text( size = 14))+
  ggtitle("Seasons")

## Days
curves %>% 
  mutate(timewindow = factor(timewindow, levels = c(1, 5, 10, 25))) %>%
  ggplot(aes(x = timestep, y = ent, col = situ))+
  geom_point()+
  geom_line()+
  facet_wrap(~timewindow, scales = "free")+
  theme_classic()+
  xlab("Amount of aggregation")+
  ylab("Relative entropy")+
  scale_color_manual(name = "Situation", values = c("firebrick3", situcolors))+
  theme(text = element_text(size = 14))
