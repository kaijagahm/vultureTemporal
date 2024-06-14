library(targets)
library(tidyverse)
source("R/functions.R")
tar_load(curves)
curves <- curves %>%
  group_by(situ, timewindow) %>%
  mutate(ent_norm = ent/max(ent))
#tar_load(curves_hours)
# curves_hours <- curves_hours %>%
#   group_by(situ, timewindow) %>%
#   mutate(ent_norm = ent/max(ent))
tar_load(curves_seasons)
curves_seasons <- curves_seasons %>%
  group_by(situ) %>%
  mutate(ent_norm = ent/max(ent))
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
