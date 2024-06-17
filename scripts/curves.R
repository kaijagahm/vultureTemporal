library(targets)
library(tidyverse)
source("R/functions.R")
tar_load(curves)
curves <- curves %>%
  group_by(situ, timewindow) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
#tar_load(curves_hours)
# curves_hours <- curves_hours %>%
#   group_by(situ, timewindow) %>%
#   mutate(ent_norm = ent/max(ent))
tar_load(curves_seasons)
curves_seasons <- curves_seasons %>%
  group_by(situ) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
tar_load(curves_hours)
curves_hours <- curves_hours %>%
  group_by(situ) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
theme_set(theme_minimal())

## Seasons
curves_seasons %>%
  ggplot(aes(x = step, y = ent_norm, col = situ))+
  geom_point(size = 2, alpha = 0.7)+
  geom_line(linewidth = 1, alpha = 0.7)+
  scale_color_manual(name = "Situation", values = c("firebrick3", situcolors))+
  ylab("Relative entropy (normalized)") + xlab("Amount of aggregation")+
  scale_x_continuous(breaks = 1:9)+
  theme(panel.grid.minor = element_blank(),
        text = element_text( size = 14))+
  ggtitle("Seasons")

## Days
curves %>% 
  mutate(timewindow = factor(timewindow, levels = c(1, 5, 10, 25))) %>%
  ggplot(aes(x = timestep, y = ent_norm, col = situ))+
  geom_point(alpha = 0.7)+
  geom_line(alpha = 0.7)+
  facet_wrap(~timewindow, scales = "free")+
  theme_classic()+
  xlab("Amount of aggregation")+
  ylab("Relative entropy (normalized)")+
  scale_color_manual(name = "Situation", values = c("firebrick3", situcolors))+
  theme(text = element_text(size = 14))

## Hours
curves_hours %>% 
  filter(!is.na(ent_norm)) %>%
  ggplot(aes(x = step, y = ent_norm, col = situ))+
  geom_point(alpha = 0.7)+
  geom_line(alpha = 0.7)+
  theme_classic()+
  xlab("Amount of aggregation")+
  ylab("Relative entropy (normalized)")+
  scale_color_manual(name = "Situation", values = c(cc$feedingColor, cc$flightColor))+
  theme(text = element_text(size = 14))

# Heat maps ---------------------------------------------------------------
## Let's start with seasons so it's more manageable
tar_load(red_flight_cat_seasons)
tar_load(red_feeding_cat_seasons)
tar_load(red_roosting_cat_seasons)
tar_load(red_aggregate_cat_seasons)
tar_load(season_names)
season_names_brief <- c("[1] F20", "[2] B21", "[3] S21", "[4] F21", "[5] B22", "[6] S22", "[7] F22", "[8] B23", "[9] S23")
mat_fl <- provideDimnames(as.matrix(red_flight_cat_seasons$JSD), sep = "",
                       list(season_names_brief, season_names_brief))

mat_fe <- provideDimnames(as.matrix(red_feeding_cat_seasons$JSD), sep = "",
                          list(season_names_brief, season_names_brief))

mat_ro <- provideDimnames(as.matrix(red_roosting_cat_seasons$JSD), sep = "",
                          list(season_names_brief, season_names_brief))

mat_ag <- provideDimnames(as.matrix(red_aggregate_cat_seasons$JSD), sep = "",
                          list(season_names_brief, season_names_brief))

gplots::heatmap.2(mat_fl, trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight") 

gplots::heatmap.2(mat_fe, trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding") 

gplots::heatmap.2(mat_ro, trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting") 

gplots::heatmap.2(mat_ag, trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate") 

# So it is not the case that we see a seasonal signature--if that was true, then we'd expect the patterns to be more similar between seasons of the same type than between adjacent seasons. What we see is that one season basically follows from the last.

## Okay, now the same for some of the daily ones
tar_load(red_flight_cat)
tar_load(red_feeding_cat)
tar_load(red_roosting_cat)
tar_load(red_aggregate_cat)
mats_fl <- map(red_flight_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
mats_fe <- map(red_feeding_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
mats_ro <- map(red_roosting_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
mats_ag <- map(red_aggregate_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
## Let's see first about 25-day windows and then go backwards

### 25-day windows
gplots::heatmap.2(mats_fl[[4]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight") # interesting! We do not recover timescale from these.

gplots::heatmap.2(mats_fe[[4]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding")  # but we do here...

gplots::heatmap.2(mats_ro[[4]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting") # but not here!

gplots::heatmap.2(mats_ag[[4]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate") # sortakinda, a mix

### 10-day windows
gplots::heatmap.2(mats_fl[[3]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight") # ish
gplots::heatmap.2(mats_fe[[3]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding")
gplots::heatmap.2(mats_ro[[3]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting") # generally higher similarity b/c denser, but still not necessarily in order...
gplots::heatmap.2(mats_ag[[3]], trace = "none", dendrogram = "row", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate")
