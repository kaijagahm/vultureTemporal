# Demographics and descriptive measures of networks
library(tidyverse)
library(sf)
library(targets)
library(igraph)
library(gt)
source("R/functions.R")
theme_set(theme_minimal())

# Season-scale analysis ---------------------------------------------------
tar_load(season_names) # vector of season names
tar_load(data_seasons) # data split into seasons
tar_load(graphs_flight_seasons) # flight graphs by season
tar_load(graphs_feeding_seasons) # feeding graphs by season
fl_nonzero_seasons <- purrr::map(graphs_flight_seasons, nonzero)
fe_nonzero_seasons <- purrr::map(graphs_feeding_seasons, nonzero)
tar_load(indivs_in_all_seasons) # vector of individuals that are present in all seasons

n_vultures_seasons <- map_dbl(data_seasons, ~length(unique(.x$Nili_id)))
min_dates_seasons <- map(data_seasons, ~min(.x$dateOnly))
max_dates_seasons <- map(data_seasons, ~max(.x$dateOnly))
season_lengths <- map2_dbl(max_dates_seasons, min_dates_seasons, ~as.numeric(difftime(.x, .y, units = "days")))
min_dates_seasons <- map_chr(min_dates_seasons, as.character)
max_dates_seasons <- map_chr(max_dates_seasons, as.character)

fl_dens_seasons <- map_dbl(fl_nonzero_seasons, edge_density)
fe_dens_seasons <- map_dbl(fe_nonzero_seasons, edge_density)

seasons_df <- data.frame(season = season_names,
                         n_vultures = n_vultures_seasons,
                         start_date = min_dates_seasons,
                         end_date = max_dates_seasons,
                         season_length = season_lengths,
                         network_dens_flight = round(fl_dens_seasons, 3),
                         network_dens_feeding = round(fe_dens_seasons, 3))
seasons_df
#length of season
#start date
#end date
#number of individuals
#network density


fl_degrees_seasons <- map(fl_nonzero_seasons, ~as.data.frame(degree(.x)[degree(.x) > 0])) %>% setNames(., season_names) %>% bind_rows(.id = "season") %>% mutate(type = "flight")
fe_degrees_seasons <- map(fe_nonzero_seasons, ~as.data.frame(degree(.x)[degree(.x) > 0])) %>% setNames(., season_names) %>% bind_rows(.id = "season") %>% mutate(type = "feeding")
degrees_seasons <- bind_rows(fl_degrees_seasons, fe_degrees_seasons) %>% left_join(seasons_df, by = "season") %>%
  mutate(degree_relative = `degree(.x)[degree(.x) > 0]`/n_vultures)

degrees_seasons_plot <- degrees_seasons %>%
  ggplot(aes(x = season, y = `degree(.x)[degree(.x) > 0]`, fill = type))+
  geom_boxplot(outlier.size = 0.5)+
  scale_fill_manual(name = "Situation", values = situcolors[1:2])+
  labs(y = "Degree", x = "Season")+
  coord_flip()
ggsave(degrees_seasons_plot, filename = here("fig/degrees_seasons_plot.png"), width = 6, height = 4)

degrees_seasons_plot_relative <- degrees_seasons %>%
  ggplot(aes(x = season, y = degree_relative, fill = type))+
  geom_boxplot(outlier.size = 0.5)+
  scale_fill_manual(name = "Situation", values = situcolors[1:2])+
  labs(y = "Degree (relative)", x = "Season")+
  coord_flip()

# Sub-seasonal scale analysis ---------------------------------------------
tar_load(summer2023data) # data for summer 2023, for sub-seasonal analysis
tar_load(timewindows) # timewindows for reducibility analysis
tar_load(timewindows_heuristic) # time windows for heuristic analysis
tar_load(data_cut) # data cut into subsets for reducibility analysis
tar_load(data_cut_heuristic) # data cut into subsets for heuristic analysis
tar_load(graphs_flight) # flight graphs (reducibility)
tar_load(graphs_feeding) # feeding graphs (reducibility)
tar_load(graphs_flight_heuristic) # flight graphs (heuristic)
tar_load(graphs_feeding_heuristic) # feeding graphs (heuristic)