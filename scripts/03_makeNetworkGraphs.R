# Produce network graphs
library(tidyverse)
library(igraph)
library(ggraph)
library(here)
library(tidygraph)

load(here("data/graphs_flight_tbl.Rda"))
load(here("data/graphs_feeding_tbl.Rda"))
load(here("data/graphs_roosting_tbl.Rda"))
load(here("data/timewindows.Rda"))
load(here("data/fromMvmtSoc/season_names.Rda"))
load(here("data/per_interval_bboxes.Rda"))
load(here("data/overall_bbox.Rda"))
load(here("data/roost_locs_bboxes.Rda"))
source(here("scripts/00_temporal_funs.R"))

# Make all the graphs (and filenames) -------------------------------------
# Seasonal centroids ------------------------------------------------------
flight_overall <- map(1:length(timewindows), ~makegraphs(situation = "flight", timewindow = .x, centroids = "overall", bboxes = overall_bbox, quiet = T), .progress = T)
feeding_overall <- map(1:length(timewindows), ~makegraphs(situation = "feeding", timewindow = .x, centroids = "overall", bboxes = overall_bbox, quiet = T), .progress = T)
roosting_overall <- map(1:length(timewindows), ~makegraphs(situation = "roosting", timewindow = .x, centroids = "overall", bboxes = overall_bbox, quiet = T), .progress = T)

# Interval centroids ------------------------------------------------------
flight_interval <- map(1:length(timewindows), ~makegraphs(situation = "flight", timewindow = .x, centroids = "interval", bboxes = per_interval_bboxes, quiet = T), .progress = T)
feeding_interval <- map(1:length(timewindows), ~makegraphs(situation = "feeding", timewindow = .x, centroids = "interval", bboxes = per_interval_bboxes, quiet = T), .progress = T)
roosting_interval <- map(1:length(timewindows), ~makegraphs(situation = "roosting", timewindow = .x, centroids = "interval", bboxes = per_interval_bboxes, quiet = T), .progress = T)

# Roost graphs by roost ---------------------------------------------------
roosting_roost <- map(1:length(timewindows), ~makegraphs(situation = "roosting", timewindow = .x, centroids = "roost", bboxes = roost_locs_bboxes, quiet = T), .progress = T)

# Combine into a list -----------------------------------------------------
graph_types <- list(flight_overall, feeding_overall, roosting_overall, flight_interval, feeding_interval, roosting_interval, roosting_roost)
## xxx need to load brks

future::plan(future::multisession, workers = 10)
for(i in 1:length(graph_types)){
  grph <- graph_types[[i]]
  cat("Working on graph type", i, "of", length(graph_types), "\n")
  furrr::future_walk(grph, .f = function(x){
    walk2(.x = x$filenames, .y = x$plots, ~{
      ggsave(filename = .x, plot = .y, width = 4, height = 4, create.dir = T)
    }, .progress = T)
  })
}

# # Flattened lists of plots and filenames, in case we want to do it this way.
# filenames_list <- map(graph_types, ~{
#   unlist(map(.x, "filenames"))
# }) %>% unlist()
# 
# plots_list <- map(graph_types, ~{
#   do.call(c, map(.x, "plots"))
# }) %>% do.call(c, .)
# 
# for(i in 1:length(filenames_list)){
#   ggsave(filename = filenames_list[i], plot = plots_list[[i]], height = 6, width = 6, create.dir = T)
#   cat(".")
# }

