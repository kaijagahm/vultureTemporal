library(targets)
library(tidyverse)
library(dendextend)
source("R/functions.R")
tar_load(curves)
tar_load(curves_seasons)
tar_load(red_flight_cat)
tar_load(red_feeding_cat)
tar_load(red_flight_cat_seasons)
tar_load(red_feeding_cat_seasons)
tar_load(season_names)
curves <- curves %>%
  group_by(situ, timewindow) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
curves_seasons <- curves_seasons %>%
  group_by(situ) %>%
  mutate(ent_norm = ent/max(ent, na.rm = T))
theme_set(theme_minimal())

## Seasons
plt_curves_seasons <- curves_seasons %>%
  ggplot(aes(x = step, y = ent_norm, col = situ))+
  geom_point(size = 2, alpha = 0.7)+
  geom_line(linewidth = 1, alpha = 0.7)+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  ylab("Relative entropy (normalized)") + xlab("Amount of aggregation")+
  scale_x_continuous(breaks = 1:9)+
  theme(panel.grid.minor = element_blank(),
        text = element_text( size = 14))+
  ggtitle("Seasons")+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))
ggsave(plt_curves_seasons, file = here("fig/plt_curves_seasons.png"), width = 6, height = 4)

## Days
plt_curves_days <- curves %>% 
  mutate(timewindow = factor(timewindow, levels = c(1, 5, 10, 25))) %>%
  ggplot(aes(x = timestep, y = ent_norm, col = situ))+
  geom_point(alpha = 0.7)+
  geom_line(alpha = 0.7)+
  facet_wrap(~timewindow, scales = "free")+
  theme_classic()+
  xlab("Amount of aggregation")+
  ylab("Relative entropy (normalized)")+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  theme(text = element_text(size = 14))+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))
ggsave(plt_curves_days, file = here("fig/plt_curves_days.png"), width = 6, height = 4)

# Heat maps ---------------------------------------------------------------
## Let's start with seasons so it's more manageable (feeding only)
mat_fe <- provideDimnames(as.matrix(red_feeding_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))

## automatic
gplots::heatmap.2(mat_fe, trace = "none", Rowv = TRUE, Colv = TRUE, dendrogram = "row", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) # ok, so at first glance, this looks unordered, but we can actually order it without changing the information it contains, I *think*??

## automatic ordered
gplots::heatmap.2(mat_fe, trace = "none", Rowv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), Colv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), dendrogram = "row", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) 

## forcibly ordered (without dendrogram)
gplots::heatmap.2(mat_fe, trace = "none", Rowv = season_names, Colv = season_names, dendrogram = "none", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) # hmm but now this is going the other way!


## ordered
gplots::heatmap.2(mat_fe, trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) 

gplots::heatmap.2(mat_fe, trace = "none", Rowv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), Colv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), dendrogram = "row", density.info = "density", key = FALSE, main = "", margins = c(10, 10)) 

# So it is not the case that we see a seasonal signature--if that was true, then we'd expect the patterns to be more similar between seasons of the same type than between adjacent seasons. What we see is that one season basically follows from the last.

## Okay, now the same for some of the daily ones
mats_fl <- map(red_flight_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
mats_fe <- map(red_feeding_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)), 
                                                   as.character(1:length(.x$JSD)))))
## Let's see first about 25-day windows and then go backwards

### 25-day windows
gplots::heatmap.2(mats_fl[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(25-day windows)") # interesting! We do not recover timescale from these.

gplots::heatmap.2(mats_fe[[4]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(25-day windows)")  # but we do here...

### 10-day windows
gplots::heatmap.2(mats_fl[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(10-day windows)") # ish
gplots::heatmap.2(mats_fe[[3]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(10-day windows)")

### 5-day windows
gplots::heatmap.2(mats_fl[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(5-day windows)") # ish
gplots::heatmap.2(mats_fe[[2]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(5-day windows)")

### 1-day windows
gplots::heatmap.2(mats_fl[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-flight\n(1-day windows)") # ish
gplots::heatmap.2(mats_fe[[1]], trace = "none", Rowv = FALSE, Colv = FALSE, dendrogram = "none", density.info = "density", key.title = "", keysize = 2, key.xlab = "", key.ylab = "", main = "Co-feeding\n(1-day windows)")
