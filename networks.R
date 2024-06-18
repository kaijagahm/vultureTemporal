# Making network plots
library(tidyverse)
library(targets)
library(tidygraph)
library(ggraph)
library(magick)
library(gifski)
library(sf)

cc <- list("breedingColor" = "#2FF8CA", "summerColor" = "#CA2FF8", "fallColor" = "#F8CA2F", flightColor = "dodgerblue", roostingColor = "olivedrab4", "feedingColor" = "gold")
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)

tar_load(data_seasons)
tar_load(data_cut)
indivs_seasons <- map(data_seasons, ~unique(.x$Nili_id))
indivs_days <- map(data_cut[[1]], ~unique(.x$Nili_id))
tar_load(graphs_flight_seasons)
tar_load(graphs_feeding_seasons)
tar_load(graphs_roosting_seasons)
tar_load(graphs_flight)
tar_load(graphs_feeding)
tar_load(graphs_roosting)
tar_load(graphs_aggregate)
tar_load(season_names)
tar_load(allvertices_seasons)

# Generate a random, consistent layout with all vertices included.
layout <- data.frame(x = runif(length(allvertices_seasons), 0, 1),
                     y = runif(length(allvertices_seasons), 0, 1),
                     name = allvertices_seasons)

removezeroes <- function(graph, indivs){
  nz <- igraph::delete_edges(graph, which(igraph::E(graph)$weight == 0))
  nz2 <- as_tbl_graph(nz) %>%
    activate(nodes) %>%
    filter(name %in% indivs) %>%
    left_join(layout, by = "name")
  return(nz2)
}

gfls <- map2(graphs_flight_seasons, indivs_seasons, ~removezeroes(.x, .y))
gfes <- map2(graphs_feeding_seasons, indivs_seasons, ~removezeroes(.x, .y))
gros <- map2(graphs_roosting_seasons, indivs_seasons, ~removezeroes(.x, .y))

gfls_days <- map2(graphs_flight[[1]], indivs_days, ~removezeroes(.x, .y))
gfes_days <- map2(graphs_feeding[[1]], indivs_days, ~removezeroes(.x, .y))
gros_days <- map2(graphs_roosting[[1]], indivs_days[-1], ~removezeroes(.x, .y))
gags_days <- map2(graphs_aggregate[[1]], indivs_days, ~removezeroes(.x, .y))

makeplot <- function(graph, title, mycol, layout){
  plt <- ggraph(graph, x = x, y = y)+
    geom_edge_link(alpha = 0.2, edge_colour = mycol)+
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

mindate <- min(data_cut[[1]][[1]]$dateOnly)
maxdate <- min(data_cut[[1]][[length(data_cut[[1]])]]$dateOnly)
dates <- seq.Date(from = lubridate::ymd(mindate), to = lubridate::ymd(maxdate), by = "day")
datesshort <- dates[1:(length(dates)-1)]
gfls_plots_days <- map2(gfls_days[1:length(datesshort)], datesshort,
                   ~makeplot(.x, .y, cc$flightColor, layout))
gfes_plots <- map2(gfes_days[1:length(datesshort)], datesshort,
                   ~makeplot(.x, .y, cc$feedingColor, layout))
gros_plots <- map2(gros_days[1:length(datesshort)], datesshort, 
                   ~makeplot(.x, .y, cc$roostingColor, layout))
gags_plots <- map2(gags_days[1:length(datesshort)], datesshort, 
                   ~makeplot(.x, .y, "firebrick3", layout))

walk2(gfls_plots, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/flight/", .y, ".png")),
         width = 6, height = 8, units = "in")
})
walk2(gfes_plots, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/feeding/", .y, ".png")),
         width = 6, height = 8, units = "in")
})
walk2(gros_plots, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/roosting/", .y, ".png")),
         width = 6, height = 8, units = "in")
})

walk2(gfls_plots_days, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/flight/", .y, ".png")),
         width = 6, height = 8, units = "in")
})
walk2(gfes_plots, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/feeding/", .y, ".png")),
         width = 6, height = 8, units = "in")
})
walk2(gros_plots, season_names, ~{
  ggsave(.x, filename = here::here(paste0("fig/abs2024graphs/seasons/roosting/", .y, ".png")),
         width = 6, height = 8, units = "in")
})

savegif <- function(dir, name, spf, width = 600, height = 800){
  png_files <- list.files(dir, pattern = ".*png$", full.names = TRUE)
  gifski(png_files, gif_file = name, width = width, height = height, delay = spf)
}

dirs <- paste0("fig/abs2024graphs/seasons/", c("flight", "feeding", "roosting"), "/")
names <- paste0("fig/abs2024graphs/gifs/", c("flight", "feeding", "roosting"), "_seasons.gif")

map2(dirs, names, ~savegif(dir = here::here(.x), name = .y, spf = 0.2))


