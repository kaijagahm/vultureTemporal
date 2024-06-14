# Making network plots
library(tidyverse)
library(targets)
library(tidygraph)
library(ggraph)

cc <- list("breedingColor" = "#2FF8CA", "summerColor" = "#CA2FF8", "fallColor" = "#F8CA2F", flightColor = "dodgerblue", roostingColor = "olivedrab4", "feedingColor" = "gold")
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)

tar_load(graphs_flight_seasons)
tar_load(graphs_feeding_seasons)
tar_load(graphs_roosting_seasons)
tar_load(season_names)
tar_load(allvertices_seasons)

gfls <- map(graphs_flight_seasons, ~as_tbl_graph(.x) %>%
              activate(edges) %>%
              filter(weight > 0))
gfes <- map(graphs_feeding_seasons, ~as_tbl_graph(.x) %>%
              activate(edges) %>% 
              filter(weight > 0))
gros <- map(graphs_roosting_seasons, ~as_tbl_graph(.x) %>%
              activate(edges) %>% 
              filter(weight > 0))

# Generate a random, consistent layout with all vertices included.
layout <- data.frame(x = runif(length(allvertices_seasons), 0, 1),
                     y = runif(length(allvertices_seasons), 0, 1))

makeplot <- function(graph, title, mycol, layout){
  plt <- ggraph(graph, layout = layout)+
    geom_edge_link(alpha = 0.3, edge_colour = mycol)+
    geom_node_point(size = 3, alpha = 0.8)+
    theme_graph()+
    theme(text = element_text(family = "Times"))+
    ggtitle(title)
  return(plt)
}

gfls_plots <- map2(gfls, season_names,
                   ~makeplot(.x, .y, cc$flightColor, layout))
gfes_plots <- map2(gfes, season_names,
                   ~makeplot(.x, .y, cc$feedingColor, layout))
gros_plots <- map2(gros, season_names, 
                   ~makeplot(.x, .y, cc$roostingColor, layout))

