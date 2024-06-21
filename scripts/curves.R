library(targets)
library(tidyverse)
library(dendextend)
source("R/functions.R")
tar_load(curves)
tar_load(curves_seasons)
tar_load(curves_hours)
tar_load(red_flight_cat)
tar_load(red_feeding_cat)
tar_load(red_roosting_cat)
tar_load(red_aggregate_cat)
tar_load(red_flight_cat_seasons)
tar_load(red_feeding_cat_seasons)
tar_load(red_roosting_cat_seasons)
tar_load(red_aggregate_cat_seasons)
tar_load(red_flight_hours_cat)
tar_load(red_feeding_hours_cat)
tar_load(season_names)
curves <- curves %>%
  group_by(situ, timewindow) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
curves_seasons <- curves_seasons %>%
  group_by(situ) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
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
  ggtitle("Seasons")+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))

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
  theme(text = element_text(size = 14))+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))

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
  theme(text = element_text(size = 14))+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))

# Heat maps ---------------------------------------------------------------
## Let's start with seasons so it's more manageable
mat_fl <- provideDimnames(as.matrix(red_flight_cat_seasons$JSD), sep = "",
                       list(season_names, season_names))

mat_fe <- provideDimnames(as.matrix(red_feeding_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))

mat_ro <- provideDimnames(as.matrix(red_roosting_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))

mat_ag <- provideDimnames(as.matrix(red_aggregate_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))

# gplots::heatmap.2(mat_fl, trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight") 

gplots::heatmap.2(mat_fe, trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) 

gplots::heatmap.2(mat_fe, trace = "none", Rowv = TRUE, Colv = TRUE, dendrogram = "row", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) 

# gplots::heatmap.2(mat_ro, trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting") 

# gplots::heatmap.2(mat_ag, trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate") 

# So it is not the case that we see a seasonal signature--if that was true, then we'd expect the patterns to be more similar between seasons of the same type than between adjacent seasons. What we see is that one season basically follows from the last.

## Okay, now the same for some of the daily ones
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
gplots::heatmap.2(mats_fl[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(25-day windows)") # interesting! We do not recover timescale from these.

gplots::heatmap.2(mats_fe[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(25-day windows)")  # but we do here...

gplots::heatmap.2(mats_ro[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting\n(25-day windows)") # but not here!

gplots::heatmap.2(mats_ag[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate\n(25-day windows)") # sortakinda, a mix

### 10-day windows
gplots::heatmap.2(mats_fl[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(10-day windows)") # ish
gplots::heatmap.2(mats_fe[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(10-day windows)")
gplots::heatmap.2(mats_ro[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting\n(10-day windows)") # generally higher similarity b/c denser, but still not necessarily in order...
gplots::heatmap.2(mats_ag[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate\n(10-day windows)")

### 5-day windows
gplots::heatmap.2(mats_fl[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(5-day windows)") # ish
gplots::heatmap.2(mats_fe[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(5-day windows)")
gplots::heatmap.2(mats_ro[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting\n(5-day windows)") # generally higher similarity b/c denser, but still not necessarily in order...
gplots::heatmap.2(mats_ag[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate\n(5-day windows)")

### 1-day windows
gplots::heatmap.2(mats_fl[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(1-day windows)") # ish
gplots::heatmap.2(mats_fe[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(1-day windows)")
gplots::heatmap.2(mats_ro[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-roosting\n(1-day windows)") # generally higher similarity b/c denser, but still not necessarily in order...
gplots::heatmap.2(mats_ag[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Aggregate\n(1-day windows)")

## Hourly
# It's too computationally intensive to make the hourly plots, I think... and given what the 1-day ones look like, I don't really know that it would do us much good. Most of it would just be blank or red anyway.

