# Exploring temporal patterns
library(tidyverse)
library(vultureUtils)
library(igraph)
library(sf)
library(tidygraph)
library(ggraph)
library(zoo)
library(here)

load("data/mixedModels/linked.Rda")
source(here("scripts/temporal_funs.R"))
future::plan(future::multisession(), workers = 10)

# Prepare for cutting -----------------------------------------------------

# It has become evident that I can only use a single season here  --------
# otherwise just way too much memory
testseason <- sfdata[[9]]
ndays <- c(1, 2, 5, 10, 20, 40)
min <- min(testseason$dateOnly)
max <- max(testseason$dateOnly)
breaks <- map(ndays, ~seq(from = as.POSIXct(min), to = as.POSIXct(max), by = paste(.x, "day", sep = " ")))
annotated <- map(breaks, ~testseason %>%
                   mutate(int = cut(dateOnly, breaks = as.Date(.x), include.lowest = T))) #4.8 GB

# XXX I'll have to find a way to include the last few dates in these (i.e. add one more element to "breaks" that's [last one + 1 interval unit]), but for now it's fine to leave them as NA.

split_sfData <- map(annotated, ~{
  cat("grouping\n")
  lst <- .x %>%
    group_by(int) %>%
    group_split()
  cat("converting to sf\n")
  lst <- map(lst, ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))
  return(lst)
  cat("cleaning up\n")
  gc()
}, .progress = T)
rm(annotated) # we have to be very careful with space here.
gc()
#
length(split_sfData) == length(ndays) # should be TRUE
map_dbl(split_sfData, length)
save(split_sfData, file = "data/split_sfData.Rda")
load("data/split_sfData.Rda")
brks <- map(split_sfData, ~map_chr(.x, ~{as.character(.x$int[1])})) # extract the date breaks
gc()

testseason_roosts <- roosts[[9]]
#roostdata_lumped <- purrr::list_rbind(roosts)
annotated_roosts <- map(breaks, ~testseason_roosts %>%
                          mutate(int = cut(roost_date, breaks = as.Date(.x), include.lowest = T)))
split_sfData_roosts <- purrr::map(annotated_roosts, ~{
  lst <- .x %>%
    group_by(int) %>%
    group_split()
  lst <- map(lst, ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))
  return(lst)
}, .progress = T)
rm(annotated_roosts)
save(split_sfData_roosts, file = "data/split_sfData_roosts.Rda")
load("data/split_sfData_roosts.Rda")
brks_roosts <- map(split_sfData_roosts, ~map_chr(.x, ~{as.character(.x$int[1])})) # extract the date breaks for roosting data
identical(brks, brks_roosts) # TRUE yay!

length(split_sfData_roosts) == length(ndays) # TRUE
map_dbl(split_sfData_roosts, length) # should be very similar to the same for non-roost data, if not identical.

# Okay, now for each of these let's get feeding, flight, and roosting edges
# XXX AAA
outs_flight <- vector(mode = "list", length = length(ndays))
outs_feeding <- vector(mode = "list", length = length(ndays))
outs_roosting <- vector(mode = "list", length = length(ndays))
future::plan(future::multisession, workers = 15)
for(split in 1:length(ndays)){
  cat("Working on data split into", ndays[split], "day intervals\n")
  datalist <- split_sfData[[split]]
  roostlist <- split_sfData_roosts[[split]]
  cat("working on flight\n")
  fl <- suppressWarnings(furrr::future_map(datalist, ~{
    library(sf)
    vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons,
                                 distThreshold = 1000, idCol = "Nili_id",
                                 return = "sri")
  }, .progress = T))

  cat("working on feeding\n")
  fe <- suppressWarnings(furrr::future_map(datalist, ~{
    vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons,
                                  distThreshold = 50, idCol = "Nili_id",
                                  return = "sri")
  }, .progress = T))

  cat("working on roosting\n")
  ro <- suppressWarnings(furrr::future_map(roostlist, ~{
    vultureUtils::getRoostEdges(.x, mode = "polygon",
                                roostPolygons = roostPolygons,
                                return = "sri",
                                latCol = "location_lat",
                                longCol = "location_long",
                                idCol = "Nili_id",
                                dateCol = "roost_date")
  }, .progress = T))
  outs_flight[[split]] <- fl
  outs_feeding[[split]] <- fe
  outs_roosting[[split]] <- ro
  rm(datalist)
  rm(roostlist)
  rm(fl)
  rm(fe)
  rm(ro)
  gc()
}

save(outs_flight, file = "data/calcSocial/outs_flight.Rda")
save(outs_feeding, file = "data/calcSocial/outs_feeding.Rda")
save(outs_roosting, file = "data/calcSocial/outs_roosting.Rda")
#
# Need to fix the ones that have length 0
fix <- function(data){
  unique_indivs <- unique(data$Nili_id)
  sri <- as.data.frame(expand.grid(unique_indivs, unique_indivs)) %>%
    setNames(c("ID1", "ID2")) %>%
    mutate(sri = 0) %>%
    filter(as.character(ID1) < as.character(ID2))
  return(sri)
}

flout <- vector(mode = "list", length = length(ndays))
feout <- vector(mode = "list", length = length(ndays))
roout <- vector(mode = "list", length = length(ndays))

for(i in 1:length(ndays)){
  sri_flight <- outs_flight[[i]]
  sri_feeding <- outs_feeding[[i]]
  sri_roosting <- outs_roosting[[i]]
  data_flight <- split_sfData[[i]]
  data_feeding <- split_sfData[[i]]
  data_roosting <- split_sfData_roosts[[i]]

  data_flight_fixed <- map2(.x = sri_flight,
                            .y = data_flight, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })

  data_feeding_fixed <- map2(.x = sri_feeding,
                            .y = data_feeding, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })
  data_roosting_fixed <- map2(.x = sri_roosting,
                            .y = data_roosting, ~{
                              if(nrow(.x) > 0){
                                out <- .x
                              }else{
                                out <- fix(data = .y)
                              }
                              return(out)
                            })
  flout[[i]] <- data_flight_fixed
  feout[[i]] <- data_feeding_fixed
  roout[[i]] <- data_roosting_fixed
  rm(sri_flight)
  rm(sri_feeding)
  rm(sri_roosting)
  rm(data_flight)
  rm(data_feeding)
  rm(data_roosting)
}

# Now we can make the graphs
graphs_flight <- vector(mode = "list", length = length(ndays))
graphs_feeding <- vector(mode = "list", length = length(ndays))
graphs_roosting <- vector(mode = "list", length = length(ndays))
future::plan(future::multisession, workers = 5)
for(i in 1:length(ndays)){
  flight <- flout[[i]]
  feeding <- feout[[i]]
  roosting <- roout[[i]]
  flightgraphs <- furrr::future_map(flight, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T),
                                    .progress = T)
  feedinggraphs <- furrr::future_map(feeding, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T),
                                     .progress = T)
  roostinggraphs <- furrr::future_map(roosting, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T), .progress = T)
  graphs_flight[[i]] <- flightgraphs
  graphs_feeding[[i]] <- feedinggraphs
  graphs_roosting[[i]] <- roostinggraphs
  rm(flightgraphs)
  rm(feedinggraphs)
  rm(roostinggraphs)
}

save(graphs_flight, file = "data/graphs_flight.Rda")
save(graphs_feeding, file = "data/graphs_feeding.Rda")
save(graphs_roosting, file = "data/graphs_roosting.Rda")

load("data/graphs_flight.Rda")
load("data/graphs_feeding.Rda")
load("data/graphs_roosting.Rda")

# Individual-level metrics ------------------------------------------------
metrics_flight_indiv <- vector(mode = "list", length = length(ndays))
metrics_feeding_indiv <- vector(mode = "list", length = length(ndays))
metrics_roosting_indiv <- vector(mode = "list", length = length(ndays))

for(i in 1:length(ndays)){
  fl <- graphs_flight[[i]]
  fe <- graphs_feeding[[i]]
  ro <- graphs_roosting[[i]]

  metrics_flight_indiv[[i]] <- map2(fl, brks[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "flight",
                        ndays = ndays[i]) %>%
        mutate(normDegree = degree/n,
               normStrength = strength/sum(strength))
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y,
                        n = 0, type = "flight", ndays = ndays[i])
    }
    return(out)
  })
  metrics_feeding_indiv[[i]] <- map2(fe, brks[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "feeding",
                        ndays = ndays[i]) %>%
        mutate(normDegree = degree/n,
               normStrength = strength/sum(strength))
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y,
                        n = 0, type = "feeding", ndays = ndays[i])
    }
    return(out)
  })
  metrics_roosting_indiv[[i]] <- map2(ro, brks_roosts[[i]], ~{
    if(length(.x) > 0){
      out <- data.frame(degree = igraph::degree(.x),
                        strength = igraph::strength(.x),
                        Nili_id = names(igraph::degree(.x)),
                        int = .y,
                        n = length(V(.x)),
                        type = "roosting",
                        ndays = ndays[i]) %>%
        mutate(normDegree = degree/n,
               normStrength = strength/sum(strength))
    }else{
      out <- data.frame(degree = NA, strength = NA, Nili_id = NA, int = .y,
                        n = 0, type = "roosting", ndays = ndays[i])
    }
    return(out)
  })
  rm(fl)
  rm(fe)
  rm(ro)
}
metrics_flight_indiv_df <- map(metrics_flight_indiv, ~purrr::list_rbind(.x))
metrics_feeding_indiv_df <- map(metrics_feeding_indiv, ~purrr::list_rbind(.x))
metrics_roosting_indiv_df <- map(metrics_roosting_indiv, ~purrr::list_rbind(.x))

metrics_indiv <- bind_rows(purrr::list_rbind(metrics_flight_indiv_df),
                     purrr::list_rbind(metrics_feeding_indiv_df),
                     purrr::list_rbind(metrics_roosting_indiv_df))
save(metrics_indiv, file = "data/metrics_indiv.Rda")

# Create tbl graphs and join metrics
graphs_flight_tbl <- vector(mode = "list", length = length(ndays))
graphs_feeding_tbl <- vector(mode = "list", length = length(ndays))
graphs_roosting_tbl <- vector(mode = "list", length = length(ndays))
for(i in 1:length(ndays)){
  fl <- graphs_flight[[i]]
  fe <- graphs_feeding[[i]]
  ro <- graphs_roosting[[i]]

  fl_tbl <- map(fl, ~as_tbl_graph(.x))
  fe_tbl <- map(fe, ~as_tbl_graph(.x))
  ro_tbl <- map(ro, ~as_tbl_graph(.x))

  graphs_flight_tbl[[i]] <- map2(fl_tbl, metrics_flight_indiv[[i]], ~{
    .x %>% activate(nodes) %>%
      left_join(.y, by = c("name" = "Nili_id"))
  })
  graphs_feeding_tbl[[i]] <- map2(fe_tbl, metrics_feeding_indiv[[i]], ~{
    .x %>% activate(nodes) %>%
      left_join(.y, by = c("name" = "Nili_id"))
  })
  graphs_roosting_tbl[[i]] <- map2(ro_tbl, metrics_roosting_indiv[[i]], ~{
    .x %>% activate(nodes) %>%
      left_join(.y, by = c("name" = "Nili_id"))
  })

  rm(fl)
  rm(fe)
  rm(ro)
}

save(graphs_flight_tbl, file = "data/graphs_flight_tbl.Rda")
save(graphs_feeding_tbl, file = "data/graphs_feeding_tbl.Rda")
save(graphs_roosting_tbl, file = "data/graphs_roosting_tbl.Rda")
# XXX AAA

load("data/graphs_flight_tbl.Rda")
load("data/graphs_feeding_tbl.Rda")
load("data/graphs_roosting_tbl.Rda")
load("data/dataPrep/season_names.Rda")

# Centroids by interval ---------------------------------------------------------
interval_centroids <- vector(mode = "list", length = length(ndays))
for(i in 1:length(interval_centroids)){
  interval_centroids[[i]] <- furrr::future_map(split_sfData[[i]], ~{
    library(sf)
    .x %>%
      dplyr::select(Nili_id, int) %>%
      dplyr::group_by(Nili_id, int) %>%
      dplyr::summarize(sf::st_union(geometry), .groups = "drop") %>%
      sf::st_centroid() %>%
      bind_cols(as.data.frame(st_coordinates(.))) %>%
      st_drop_geometry() %>%
      rename("X_int" = X,
             "Y_int" = Y)
  }, .progress = T)
}

all_interval_centroids <- purrr::list_rbind(map(interval_centroids, purrr::list_rbind))
minx_int <- min(all_interval_centroids$X_int)
miny_int <- min(all_interval_centroids$Y_int)
maxx_int <- max(all_interval_centroids$X_int)
maxy_int <- max(all_interval_centroids$Y_int)

# Seasonal centroids ------------------------------------------------------
load("data/akde/sfs_est_centroids.Rda")
row.names(sfs_est_centroids) <- NULL
centrs <- sfs_est_centroids %>%
  filter(seasonUnique == "2020_fall") %>%
  select(seasonUnique, Nili_id, dist_szn_centr) %>%
  bind_cols(as.data.frame(st_coordinates(.))) %>%
  st_drop_geometry()

minx <- min(centrs$X)-(sd(centrs$X)/10)
miny <- min(centrs$Y)-(sd(centrs$Y)/10)
maxx <- max(centrs$X)+(sd(centrs$X)/10)
maxy <- max(centrs$Y)+(sd(centrs$Y)/10)

# Bind the centroids on. NOTE: some of the seasonal centroids are missing, and I'm not sure where to put those individuals. They will be missing from the graph.
for(i in 1:length(ndays)){
  fl <- graphs_flight_tbl[[i]]
  fe <- graphs_feeding_tbl[[i]]
  ro <- graphs_roosting_tbl[[i]]
  interval_centrs <- interval_centroids[[i]]
  
  fl_out <- map2(fl, interval_centrs, ~.x %>% 
                  activate(nodes) %>% 
                  left_join(centrs, by = c("name" = "Nili_id")) %>%
                  left_join(.y, by = c("name" = "Nili_id", "int")))
  
  fe_out <- map2(fe, interval_centrs, ~.x %>% 
                  activate(nodes) %>%
                  left_join(centrs, by = c("name" = "Nili_id")) %>%
                  left_join(.y, by = c("name" = "Nili_id", "int")))
  
  ro_out <- map2(ro, interval_centrs, ~.x %>% 
                  activate(nodes) %>%
                  left_join(centrs, by = c("name" = "Nili_id")) %>%
                  left_join(.y, by = c("name" = "Nili_id", "int")))
  
  graphs_flight_tbl[[i]] <- fl_out
  graphs_feeding_tbl[[i]] <- fe_out
  graphs_roosting_tbl[[i]] <- ro_out
}

graph_test <- graphs_flight_tbl[[1]][[10]]
layout_seasonal <- graph_test %>% activate(nodes) %>% data.frame() %>% select("x" = X, "y" = Y)
layout_interval <- graph_test %>% activate(nodes) %>% data.frame() %>% select("x" = X_int, "y" = Y_int)
graph_fn(graph_test, layout = layout_interval, title = "test_int", type = "int")

graph_fn <- function(graph, layout, title, type){
  if(type == "int"){
    g <- ggraph(graph, layout) +
      geom_edge_link(alpha = 0.3)+
      geom_node_point(aes(col = normDegree, size = normDegree))+
      geom_node_text(aes(label = name), 
                     col = "white", 
                     size = 2)+
      scale_color_viridis_c(limits = c(0, 1))+
      scale_size_continuous(limits = c(0, 1))+
      theme_void()+
      xlim(minx_int, maxx_int)+
      ylim(miny_int, maxy_int)+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "gray80", color = "gray80"))+
      ggtitle(title)
    return(g)
  }else if(type == "seasonal"){
    g <- ggraph(graph, layout) +
      geom_edge_link(alpha = 0.3)+
      geom_node_point(aes(col = normDegree, size = normDegree))+
      geom_node_text(aes(label = name), 
                     col = "white", 
                     size = 2)+
      scale_color_viridis_c(limits = c(0, 1))+
      scale_size_continuous(limits = c(0, 1))+
      theme_void()+
      xlim(minx, maxx)+
      ylim(miny, maxy)+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "gray80", color = "gray80"))+
      ggtitle(title)
    return(g)
  }else{
    stop("Type must be either 'int' or 'seasonal'.")
  }
}

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



# Graphs of metrics -------------------------------------------------------
load("data/metrics_indiv.Rda")
glimpse(metrics_indiv)
metrics_indiv <- metrics_indiv %>%
  group_by(ndays, int) %>%
  mutate(normDegree = degree/(n-1),
         normStrength = strength/sum(strength, na.rm = T)) # XXX WHAT IS WRONG HERE?

metrics_indiv %>%
  ggplot(aes(x = ndays, y = normDegree, col = type))+
  geom_jitter(alpha = 0.1, width = 0.6)+
  theme_classic()+
  facet_wrap(~type) # There are a few individuals that don't roost with the others and therefore have a low degree, even at the high time windows. I thought those were Carmel birds, but I've now removed the northern indivs and we still see this pattern.

metrics_indiv %>%
  ggplot(aes(x = ndays, y = normStrength, col = type))+
  geom_jitter(alpha = 0.1, width = 0.6)+
  theme_classic()+
  facet_wrap(~type) # I do not understand why this goes DOWN. Am I normalizing it incorrectly?

# What about degree/strength per time period?
ids <- unique(metrics_indiv$Nili_id)
rand <- sample(ids, size = 5, replace = FALSE)
metrics_indiv <- metrics_indiv %>%
  mutate(highlight = ifelse(Nili_id %in% rand, T, F))

future::plan(future::multisession(), workers = 5)
furrr::future_walk(ndays, ~{
  days <- .x
  
  plt_deg <- metrics_indiv %>%
    filter(ndays == days, highlight) %>%
    ggplot(aes(x = lubridate::ymd(int), y = normDegree, group = interaction(Nili_id, type)))+
    geom_line(aes(col = Nili_id), alpha = 0.7)+
    facet_wrap(~type, scales = "free", ncol = 1)+
    theme_classic()+
    theme(legend.position = "none")+
    ylab("Degree (normalized)")+
    xlab("Date")+
    ggtitle(paste0("Degree, ", days, "-day intervals,\n5 random vultures"))
  
  plt_str <- metrics_indiv %>%
    filter(ndays == days, highlight) %>%
    ggplot(aes(x = lubridate::ymd(int), y = normStrength, group = interaction(Nili_id, type)))+
    geom_line(aes(col = Nili_id), alpha = 0.7)+
    facet_wrap(~type, scales = "free", ncol = 1)+
    theme_classic()+
    theme(legend.position = "none")+
    ylab("Strength (normalized)")+
    xlab("Date")+
    ggtitle(paste0("Strength, ", days, "-day intervals,\n5 random vultures"))
  
  ggsave(plt_deg, file = paste0("fig/timePlots/degree_", 
                                str_pad(days, width = 2, side = "left", pad = "0"), ".png"),
         width = 9, height = 5)
  ggsave(plt_str, file = paste0("fig/timePlots/strength_", 
                                str_pad(days, width = 2, side = "left", pad = "0"), ".png"),
         width = 9, height = 5)
})

# How do network-level measures change over the time points?
metrics_net <- metrics_indiv %>%
  group_by(ndays, int, type, n) %>%
  summarize(mndeg = mean(degree),
            mnstr = mean(strength),
            mnnormdeg = mean(normDegree),
            mnnormstr = mean(normStrength)) %>%
  mutate(int = lubridate::ymd(int))

metrics_net %>%
  ggplot(aes(x = int, y = mnnormdeg, col = factor(ndays)))+
  geom_point()+
  geom_line()+
  facet_wrap(~type, ncol = 1, scales = "free")+
  geom_hline(aes(yintercept = 0.5), col = "black", linetype = 2) # okay, so let's say we wanted to use this as a criterion and find the "timescale of half-saturation" (need to look at whether there is literature precedent for this being the characteristic timescale.)

metrics_net %>%
  ggplot(aes(x = mnnormdeg, col = factor(ndays)))+
  geom_density()+
  facet_grid(rows = vars(type), cols = vars(ndays), scales = "free")+
  geom_vline(aes(xintercept = 0.5)) # yeah, this is getting a lot closer to what I wanted. The problem is, we would need more seasons of data to really zero in on a timescale here.

# THIS ONE
m <- metrics_net %>%
  group_by(ndays, type) %>%
  summarize(mn_mnnormdeg = mean(mnnormdeg)) %>%
  ggplot(aes(x = ndays, y = mn_mnnormdeg, col = type))+
  geom_point(size = 5)+
  geom_line(linewidth = 2)+
  ylab("Mean degree (normalized)")+
  xlab("Time window (days)")+
  scale_color_manual(name = "Situation",
                     values = c(cc$feedingColor, cc$flightColor, cc$roostingColor))+
  geom_hline(aes(yintercept = 0.5), linetype = 2)+
  theme(text = element_text(size = 20))
ggsave(m, file = "fig/timescales.png", width = 9, height = 7)
# I think this is the graph I've been envisioning the whole time!
  
metrics_net %>%
  ggplot(aes(x = int, y = mnnormstr, col = factor(ndays)))+
  geom_point()+
  geom_line()+
  facet_wrap(~type, ncol = 1, scales = "free")+
  #geom_hline(aes(yintercept = 0.5), col = "black", linetype = 2)+
  NULL


# Daily behaviors ---------------------------------------------------------
metr <- metrics_indiv %>%
  filter(type %in% c("flight", "feeding"), ndays == 1) %>% 
  filter(lubridate::ymd(int) %in% seq(from = lubridate::ymd("2023-05-15"), 
                                      to = lubridate::ymd("2023-06-15"), by = 1)) %>%
  group_by(type, int) %>%
  mutate(normDegree_prop = normDegree/max(normDegree),
         normDegree_scl = (normDegree - mean(normDegree))/sd(normDegree), # looks like this is the one we want. Distributions are still pretty right-skewed, but that's okay.
         normDegree_scl_log = log(normDegree_scl),
         normDegree_log = log(normDegree),
         normStrength_prop = normStrength/max(normStrength),
         normStrength_scl = (normStrength - mean(normStrength))/sd(normStrength),
         normStrength_scl_log = log(normStrength_scl),
         normStrength_log = log(normStrength))
  
metr %>%
  ggplot(aes(x = as.factor(lubridate::ymd(int)), y = normDegree_scl, col = type, fill = type))+
  geom_boxplot(outlier.size = 1,
               position = position_dodge(width = 0), # so we get them on top of each other
               alpha = 0.7)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        NULL)+
  stat_summary(
    fun = ~quantile(.x, 0.75),
    geom = 'line',
    linewidth = 1,
    aes(group = type, colour = type),
    position = position_dodge(width = 0) # this has to match the position_dodge for the boxplot
  )+
  ylab("Normalized degree (scaled)")+
  xlab("Date")+
  scale_color_manual(name = "Situation",
                     values = c(cc$feedingColor, cc$flightColor))+
  scale_fill_manual(name = "Situation",
                     values = c(cc$feedingColor, cc$flightColor))

# Is there a negative relationship between the degree distributions in the two situations?
medians <- metr %>%
  group_by(ndays, type, int) %>%
  summarize(normDegree_scl_med = median(normDegree_scl)) %>%
  pivot_wider(id_cols = c(ndays, int), names_from = type, values_from = normDegree_scl_med) %>%
  mutate(metric = "normDegree_scl") %>%
  bind_rows(metr %>%
              group_by(ndays, type, int) %>%
              summarize(normStrength_scl_med = median(normStrength_scl)) %>%
              pivot_wider(id_cols = c(ndays, int), names_from = type, values_from = normStrength_scl_med) %>% mutate(metric = "normStrength_scl"))

medians %>%
  ggplot(aes(x = feeding, y = flight))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~metric, ncol = 1) # hmm, there seems to be no relationship between the two...
