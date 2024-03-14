# Produce network graphs
library(tidyverse)
library(igraph)
library(ggraph)

load(here("data/graphs_flight_tbl.Rda"))
load(here("data/graphs_feeding_tbl.Rda"))
load(here("data/graphs_roosting_tbl.Rda"))
load(here("data/timewindows.Rda"))
load(here("data/fromMvmtSoc/season_names.Rda"))
load(here("data/per_interval_bboxes.Rda"))
load(here("data/overall_bbox.Rda"))
load(here("data/roost_locs_bboxes.Rda"))

graph_test <- graphs_flight_tbl[[1]][[10]]
plot(graph_test)
layout_seasonal <- graph_test %>% activate(nodes) %>% data.frame() %>% 
  select("x" = X_overall, "y" = Y_overall)
layout_interval <- graph_test %>% activate(nodes) %>% data.frame() %>% 
  select("x" = X_int, "y" = Y_int)
graph_fn(graph_test, layout = layout_interval, title = "test_int", bbox = overall_bbox)

# Seasonal centroids ------------------------------------------------------

# make graphs
# which situation?
# which time window?
# centroid positions

makegraphs <- function(situation, timewindow, centroids){
  # Make the graph titles
  days <- timewindows[timewindow] # how many days?
  breaks <- brks[[timewindow]]
  titles <-  map(breaks, ~{
    paste0("Co-", situation, ", ", days, "-day interval: [",
           .x, ", ", lubridate::ymd(.x) + days, ")")
  })
  
  # Get the graph data list
  if(situation == "flight"){
    graphs <- graphs_flight_tbl[[timewindow]]
  }else if(situation == "feeding"){
    graphs <- graphs_feeding_tbl[[timewindow]]
  }else if(situation == "roosting"){
    graphs <- graphs_roosting_tbl[[timewindow]]
  }else{
    stop("situation must be either 'feeding', 'flight', or 'roosting'.")
  }
  
  # Check that the graphs and titles match up.
  if(length(graphs) != length(titles)){
    stop("graphs and titles don't match!")
  }
  
  # Make the layouts
  if(centroids == "overall"){
    col_suffix <- "_overall"
  }else if(centroids == "interval"){
    col_suffix <- "_int"
  }else if(centroids == "roost"){
    col_suffix <- "_roost"
  }else{
    stop("`centroids` must be 'overall', 'interval', or 'roost'.")
  }
  
  layouts <- map(graphs, ~.x %>%
                   activate(nodes) %>% data.frame() %>%
                   select("x" = paste0("X", col_suffix),
                          "y" = paste0("Y", col_suffix)))
  # XXX START HERE!!!
  plots <- purrr::pmap(.l = list(graphs, layouts, titles),
                       graph_fn(graph = x,
                                layout = y,
                                title = z, 
                                bbox = overall_bbox))

}



future::plan(future::multisession, workers = 15)
for(i in 1:length(timewindows)){
  days <- timewindows[[i]]
  fl <- graphs_flight_tbl[[i]]
  fe <- graphs_feeding_tbl[[i]]
  ro <- graphs_roosting_tbl[[i]]
  cat("working on interval", days)
  
  furrr::future_walk2(fl, brks[[i]], ~{
    title <- paste0("Co-flight, ", days, "-day interval: [", 
                    .y, ", ", lubridate::ymd(.y) + days, ")")
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


# Interval centroids ------------------------------------------------------


# Roost graphs by roost ---------------------------------------------------


