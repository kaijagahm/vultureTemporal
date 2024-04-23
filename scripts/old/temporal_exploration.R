# Exploring temporal patterns
library(tidyverse)
library(vultureUtils)
library(igraph)
library(sf)
library(tidygraph)
library(ggraph)
library(zoo)
library(here)

load("data/graphs_flight_tbl.Rda")
load("data/graphs_feeding_tbl.Rda")
load("data/graphs_roosting_tbl.Rda")
load("data/dataPrep/season_names.Rda")

graph_test <- graphs_flight_tbl[[1]][[10]]
layout_seasonal <- graph_test %>% activate(nodes) %>% data.frame() %>% select("x" = X, "y" = Y)
layout_interval <- graph_test %>% activate(nodes) %>% data.frame() %>% select("x" = X_int, "y" = Y_int)
graph_fn(graph_test, layout = layout_interval, title = "test_int", type = "int")

# Graphs with fixed spatial positions (seasonal centroids)
future::plan(future::multisession, workers = 15)
for(i in 1:length(ndays)){ # for(i in 1:length(ndays))
  days <- ndays[[i]]
  fl <- graphs_flight_tbl[[i]]
  fe <- graphs_feeding_tbl[[i]]
  ro <- graphs_roosting_tbl[[i]]
  cat("working on interval", days)

  furrr::future_walk2(fl, brks[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-flight, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X, "y" = Y)
    g <- graph_fn(.x, layout, title, type = "seasonal")
    filename <- paste0("fig/networkGraphs/interval_2020/days_",
                       str_pad(days, width = 2, side = "left", pad = "0"),
                       "/flight_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
  
  furrr::future_walk2(fe, brks[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-feeding, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X, "y" = Y)
    g <- graph_fn(.x, layout, title, type = "seasonal")
    filename <- paste0("fig/networkGraphs/interval_2020/days_", 
                       str_pad(days, width = 2, side = "left", pad = "0"), 
                       "/feeding_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
  
  furrr::future_walk2(ro, brks_roosts[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-roosting, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X, "y" = Y)
    g <- graph_fn(.x, layout, title, type = "seasonal")
    filename <- paste0("fig/networkGraphs/interval_2020/days_",
                       str_pad(days, width = 2, side = "left", pad = "0"),
                       "/roosting_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
}

# Graphs with daily (or interval-specific) spatial locations
for(i in 1){
  days <- ndays[[i]]
  fl <- graphs_flight_tbl[[i]]
  fe <- graphs_feeding_tbl[[i]]
  ro <- graphs_roosting_tbl[[i]]
  cat("working on interval", days)
  
  furrr::future_walk2(fl, brks[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-flight, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X_int, "y" = Y_int)
    g <- graph_fn(.x, layout, title, type = "int")
    filename <- paste0("fig/networkGraphs/interval_2020_spatInterval/days_",
                       str_pad(days, width = 2, side = "left", pad = "0"),
                       "/flight_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
  
  furrr::future_walk2(fe, brks[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-feeding, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X_int, "y" = Y_int)
    g <- graph_fn(.x, layout, title, type = "int")
    filename <- paste0("fig/networkGraphs/interval_2020_spatInterval/days_", 
                       str_pad(days, width = 2, side = "left", pad = "0"), 
                       "/feeding_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
  
  furrr::future_walk2(ro, brks_roosts[[i]], ~{
    end <- lubridate::ymd(.y) + days
    title <- paste0("Co-roosting, ", days, "-day interval: [", .y, ", ", end, ")")
    layout <- .x %>% activate(nodes) %>% data.frame() %>% select("x" = X_int, "y" = Y_int)
    g <- graph_fn(.x, layout, title, type = "int")
    filename <- paste0("fig/networkGraphs/interval_2020_spatInterval/days_",
                       str_pad(days, width = 2, side = "left", pad = "0"),
                       "/roosting_", .y, "_", lubridate::ymd(.y)+2, ".png")
    ggsave(g, filename = filename, width = 6, height = 5)
  }, .progress = T)
}