library(targets)
library(tidyverse)
library(dendextend)
library(here)
library(gplots)
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
  annotate(geom = "segment", x = 1, xend = 9, y = 1, yend = 0, alpha = 0.7, col = "black", linetype = 3)+
  geom_point(size = 2, alpha = 0.7)+
  geom_line(linewidth = 1, alpha = 0.7, aes(linetype = situ))+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  scale_linetype_manual(name = "Situation", values = c(1, 2))+
  ylab("Relative entropy (normalized)") + xlab("Layers aggregated")+
  scale_x_continuous(breaks = 1:9)+
  theme_classic()+
  theme(panel.grid.minor = element_blank(),
        text = element_text( size = 14))+
  ggtitle("Seasons")+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))
plt_curves_seasons
ggsave(plt_curves_seasons, file = here("fig/plt_curves_seasons.png"), width = 6, height = 3)

## Days
lines <- curves %>% group_by(timewindow) %>% summarize(xend = max(step)) %>%
  mutate(x = 1, y = 1, yend = 0)
plt_curves_days <- curves %>% 
  mutate(timewindow = factor(timewindow, levels = c(1, 5, 10, 25))) %>%
  ggplot(aes(x = step, y = ent_norm, col = situ))+
  geom_segment(data = lines, aes(x = x, xend = xend, y = y, yend = yend), alpha = 0.7, col = "black", linetype = 3)+
  geom_point(alpha = 0.7)+
  geom_line(alpha = 0.7)+
  facet_wrap(~timewindow, scales = "free")+
  theme_classic()+
  xlab("Layers aggregated")+
  ylab("Relative entropy (normalized)")+
  scale_color_manual(name = "Situation", values = situcolors[1:2])+
  theme(text = element_text(size = 14))+
  guides(color = guide_legend(override.aes = 
                                list(linewidth = 1, size = 3)))
plt_curves_days
ggsave(plt_curves_days, file = here("fig/plt_curves_days.png"), width = 6, height = 4)

# Heat maps ---------------------------------------------------------------
## Let's start with seasons so it's more manageable (feeding only)
mat_fe <- provideDimnames(as.matrix(red_feeding_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))
mat_fl <- provideDimnames(as.matrix(red_flight_cat_seasons$JSD), sep = "",
                          list(season_names, season_names))

## automatic (feeding)
#gplots::heatmap.2(mat_fe, trace = "none", dendrogram = "row", key = FALSE, margins = c(10, 10)) # at first glance, this looks unordered, but I think we can actually rotate the nodes and keep the dendrogram

## automatic (flight)
gplots::heatmap.2(mat_fl, trace = "none", dendrogram = "row", key = FALSE, margins = c(10, 10))

## automatic ordered (feeding) (i.e. enforcing the order as much as possible while rotating the existing dendrogram)
# gplots::heatmap.2(mat_fe, trace = "none", Rowv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), Colv = rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names), dendrogram = "column", key = FALSE, margins = c(10, 10)) 

## automatic ordered (flight)

season_names_abbreviated <- str_remove(season_names, "_")
season_names_abbreviated <- str_replace(season_names_abbreviated, "breeding", "B")
season_names_abbreviated <- str_replace(season_names_abbreviated, "fall", "F")
season_names_abbreviated <- str_replace(season_names_abbreviated, "summer", "S")
season_names_abbreviated <- str_remove(season_names_abbreviated, "20")
colnames(mat_fl) <- season_names_abbreviated
rownames(mat_fl) <- season_names_abbreviated
colnames(mat_fe) <- season_names_abbreviated
rownames(mat_fe) <- season_names_abbreviated

# --- Heatmap 1: Original with dendrogram (as per your reprex) ---
gplots::heatmap.2(mat_fl, trace = "none", dendrogram = "row", key = FALSE,
                  margins = c(10, 10))

# --- Heatmap 2: Dendrogram rotated as close as possible to temporal order (as per your reprex) ---
gplots::heatmap.2(mat_fl, trace = "none",
                  Rowv = dendextend::rotate(as.dendrogram(hclust(as.dist(mat_fl))), order = season_names_abbreviated),
                  Colv = dendextend::rotate(as.dendrogram(hclust(as.dist(mat_fl))), order = season_names_abbreviated),
                  dendrogram = "column", 
                  key = TRUE, margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

gplots::heatmap.2(mat_fe, trace = "none",
                  Rowv = dendextend::rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names_abbreviated),
                  Colv = dendextend::rotate(as.dendrogram(hclust(as.dist(mat_fe))), order = season_names_abbreviated),
                  dendrogram = "column", 
                  key = TRUE, margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

# --- Heatmap 3: No dendrogram, forced temporal order, matching reflection ---
gplots::heatmap.2(mat_fl[rev(season_names_abbreviated), season_names_abbreviated], 
                  trace = "none",
                  dendrogram = "none", # No dendrogram
                  key = FALSE,
                  Rowv = NA,           # Prevent further reordering by Rowv
                  Colv = NA,           # Prevent further reordering by Colv
                  margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

gplots::heatmap.2(mat_fe[rev(season_names_abbreviated), season_names_abbreviated], 
                  trace = "none",
                  dendrogram = "none", # No dendrogram
                  key = FALSE,
                  Rowv = NA,           # Prevent further reordering by Rowv
                  Colv = NA,           # Prevent further reordering by Colv
                  margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

season_order <- c("20F", "21F", "22F", "21B", "22B", "23B", "21S", "22S", "23S")
gplots::heatmap.2(mat_fl[rev(season_order), season_order], 
                  trace = "none",
                  dendrogram = "none", # No dendrogram
                  key = FALSE,
                  Rowv = NA,           # Prevent further reordering by Rowv
                  Colv = NA,           # Prevent further reordering by Colv
                  margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

gplots::heatmap.2(mat_fe[rev(season_order), season_order], 
                  trace = "none",
                  dendrogram = "none", # No dendrogram
                  key = FALSE,
                  Rowv = NA,           # Prevent further reordering by Rowv
                  Colv = NA,           # Prevent further reordering by Colv
                  margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)

# So it is not the case that we see a seasonal signature--if that was true, then we'd expect the patterns to be more similar between seasons of the same type than between adjacent seasons. What we see is that one season basically follows from the last.

## How many individuals would we be left with if we restricted the seasonal analysis to only individuals that were present in all 9 seasons?
tar_load(data_seasons)
indivs_seasons <- map(data_seasons, ~.x %>% ungroup() %>% pull(Nili_id) %>% unique() %>% as_tibble()) %>% data.table::rbindlist(idcol = "season") %>% mutate(present = T) %>%
  as_tibble() %>% pivot_wider(names_from = "season", values_from = "present", id_cols = "value")

in_all <- na.omit(indivs_seasons)
nrow(in_all) # only 20 individuals left!


# 5-day window heatmap ----------------------------------------------------
mats_fl <- map(red_flight_cat, ~as.matrix(.x$JSD) %>%
                 provideDimnames(., sep = "", list(as.character(1:length(.x$JSD)),
                                                   as.character(1:length(.x$JSD)))))
library(viridis)
colors <- viridis(nrow(mats_fl[[2]]), begin = 0, end = 1)
df <- data.frame(ind = 1:length(colors), hex = colors)
ggplot(df)+ # test plot
  geom_rect(aes(xmin = ind, xmax = ind+1, ymin = 0, ymax = 1, fill = factor(ind)))+
  scale_fill_manual(values = df$hex)+
  theme_void()+
  coord_flip()

# --- Heatmap 2: Dendrogram rotated as close as possible to temporal order (as per your reprex) ---
slices_fl <- 1:nrow(mats_fl[[2]])
slices_fe <- 1:nrow(mats_fe[[2]])
h2_fl <- gplots::heatmap.2(mats_fl[[2]], trace = "none",
                  Rowv = dendextend::rotate(as.dendrogram(hclust(as.dist(mats_fl[[2]]))), 
                                            order = slices_fl),
                  Colv = dendextend::rotate(as.dendrogram(hclust(as.dist(mats_fl[[2]]))), 
                                            order = slices_fl),
                  dendrogram = "column", 
                  key = TRUE, margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)
ord_h2fl <- h2_fl$rowInd
df_h2_fl <- df[ord_h2fl,] %>% mutate(xmin = 1:nrow(.), xmax = (1:nrow(.))+1)
ggplot(df_h2_fl)+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = factor(ind, levels = ord_h2fl)))+
  scale_fill_manual(values = df_h2_fl$hex)+
  theme_void()+
  coord_flip()

h2_fe <- gplots::heatmap.2(mats_fe[[2]], trace = "none",
                           Rowv = dendextend::rotate(as.dendrogram(hclust(as.dist(mats_fe[[2]]))), 
                                                     order = slices_fe),
                           Colv = dendextend::rotate(as.dendrogram(hclust(as.dist(mats_fe[[2]]))), 
                                                     order = slices_fe),
                           dendrogram = "column", 
                           key = TRUE, margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)
ord_h2fe <- h2_fe$rowInd
df_h2_fe <- df[ord_h2fe,] %>% mutate(xmin = 1:nrow(.), xmax = (1:nrow(.))+1)
ggplot(df_h2_fe)+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = factor(ind, levels = ord_h2fe)))+
  scale_fill_manual(values = df_h2_fe$hex)+
  theme_void()+
  coord_flip()

# --- Heatmap 3: No dendrogram, forced temporal order, matching reflection ---
h3_fl <- gplots::heatmap.2(mats_fl[[2]][rev(slices_fl), slices_fl], 
                  trace = "none",
                  dendrogram = "none", # No dendrogram
                  key = FALSE,
                  Rowv = NA,           # Prevent further reordering by Rowv
                  Colv = NA,           # Prevent further reordering by Colv
                  margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)
ord_h3fl <- h3_fl$rowInd
df_h3_fl <- df[ord_h3fl,] %>% mutate(xmin = 1:nrow(.), xmax = (1:nrow(.))+1)
ggplot(df_h3_fl)+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = factor(ind, levels = ord_h3fl)))+
  scale_fill_manual(values = df_h3_fl$hex)+
  theme_void()+
  coord_flip()

h3_fe <- gplots::heatmap.2(mats_fe[[2]][rev(slices_fe), slices_fe], 
                           trace = "none",
                           dendrogram = "none", # No dendrogram
                           key = FALSE,
                           Rowv = NA,           # Prevent further reordering by Rowv
                           Colv = NA,           # Prevent further reordering by Colv
                           margins = c(10, 10), density.info = "none", key.title = "", key.xlab= "", key.ylab = "", keysize = 2)
ord_h3fe <- h3_fe$rowInd
df_h3_fe <- df[ord_h3fe,] %>% mutate(xmin = 1:nrow(.), xmax = (1:nrow(.))+1)
ggplot(df_h3_fe)+
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = factor(ind, levels = ord_h3fe)))+
  scale_fill_manual(values = df_h3_fe$hex)+
  theme_void()+
  coord_flip()
