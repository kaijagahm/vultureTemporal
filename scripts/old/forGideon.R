# Prepping Gideon's intervals for plotting
# Interval 1: March 15-April 15 2023
# Interval 2: November 11-December 15 2023

#| include: false
library(tidyverse) # for data wrangling
library(sf) # for spatial manipulation (necessary as a precursor to getting edgelists)
library(vultureUtils) # for getting edgelists
library(igraph) # for working with networks
library(tidygraph) # for working with networks
library(future) # for parallel processing
library(furrr) # for parallel processing
library(here) # for tidy file paths
library(readxl)
source(here("scripts/00_temporal_funs.R"))
roostPolygons <- sf::st_read(here("data/raw/roosts50_kde95_cutOffRegion.kml"))
ww <- read_excel(here("data/raw/whoswho_vultures_20230920_new.xlsx"))
periods_to_remove <- readxl::read_excel(here("data/raw/whoswho_vultures_20230920_new.xlsx"), sheet = "periods_to_remove")
capture_sites <- read.csv(here("data/raw/capture_sites.csv"))
carmel <- read.csv(here("data/raw/all_captures_carmel_2010-2021.csv"))
jamPolygons <- sf::st_read(here("data/raw/GPS_jamming_3.kml"))

# Authenticate
base::load(here::here("movebankCredentials/pw.Rda"))
MB.LoginObject <- move::movebankLogin(username = "kaijagahm", 
                                      password = pw)
rm(pw)

# Period 1: 15 March 2023 - 15 April 2023
minDate_1 <- "2023-03-15 00:00"
maxDate_1 <- "2023-04-16 00:00"

# Period 2: 11 November 2023 - 15 December 2023
minDate_2 <- "2023-11-11 00:00"
maxDate_2 <- "2023-12-16 00:00"

# Download
data_movebank_1 <- downloadVultures(loginObject = MB.LoginObject,
                                    removeDup = T, dfConvert = T, quiet = T, 
                                    dateTimeStartUTC = minDate_1,
                                    dateTimeEndUTC = maxDate_1)

data_movebank_2 <- downloadVultures(loginObject = MB.LoginObject,
                                    removeDup = T, dfConvert = T, quiet = T, 
                                    dateTimeStartUTC = minDate_2,
                                    dateTimeEndUTC = maxDate_2)

data_movebank <- bind_rows(data_movebank_1, data_movebank_2)

ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id) %>% dplyr::distinct() # pull out just the names columns, nothing else, and remove any duplicates

# join by movebank ID
data_fixednames <- dplyr::left_join(data_movebank, ww_tojoin, 
                                    by = c("local_identifier" = "Movebank_id"))

data_removedperiods <- removeInvalidPeriods(dataset = data_fixednames, periodsToRemove = periods_to_remove)

data_removedgpsjamming <- gpsJamFilter(dataset = data_removedperiods, mask = jamPolygons)

data_cleaned <- cleanData(dataset = data_removedgpsjamming,
                          precise = F,
                          longCol = "location_long",
                          latCol = "location_lat",
                          idCol = "Nili_id",
                          report = F)

data_removedcaptures <- removeCaptures(data = data_cleaned, captureSites = capture_sites, AllCarmelDates = carmel, distance = 500, idCol = "Nili_id")
dat <- data_removedcaptures

# Get roost data
roosts <- get_roosts_df(df = data_removedcaptures, id = "Nili_id")
days <- unique(dat$dateOnly)
days_roosts <- unique(roosts$roost_date)
dat <- dat %>%
  filter(dateOnly %in% days_roosts) # remove days that we don't have roost data for
days <- unique(dat$dateOnly)
all(days %in% days_roosts) # TRUE
all(days_roosts %in% days)

# Cut into days -----------------------------------------------------------
dat_days <- dat %>%
  group_by(dateOnly) %>%
  group_split() %>%
  map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))

roosts_days <- roosts %>%
  group_by(roost_date) %>%
  group_split() %>%
  map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84"))

# Make edges --------------------------------------------------------------
fl <- map(dat_days, ~{
  vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons,
                               distThreshold = 1000, return = "sri")}
  , .progress = T)

fe <- map(dat_days, ~{
  vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons,
                               distThreshold = 50, return = "sri")}
  , .progress = T)

ro <- map(roosts_days, ~{
  vultureUtils::getRoostEdges(.x, mode = "polygon", roostPolygons = roostPolygons, return = "sri")
}, .progress = T)

# Do any of them have length 0?
map_dbl(fl, nrow)
map_dbl(fe, nrow)
map_dbl(ro, nrow)
# Yes, some of them do. So we have to fix them.

# Now we have a problem where some of them have length 0 because there were no interactions of that type during the time interval in question. 
# I want to fill in those with a blank data frame with the same format as the other SRI data frames.
# Function to create empty SRI data frames:
fix <- function(data){
  unique_indivs <- unique(data$Nili_id)
  sri <- as.data.frame(expand.grid(unique_indivs, unique_indivs)) %>%
    setNames(c("ID1", "ID2")) %>%
    mutate(sri = 0) %>%
    filter(as.character(ID1) < as.character(ID2))
  return(sri)
}

fl_fixed <- map2(fl, dat_days, ~{
  if(nrow(.x) > 0){out <- .x}
  else{out <- fix(.y)}
  return(out)
})

fe_fixed <- map2(fe, dat_days, ~{
  if(nrow(.x) > 0){out <- .x}
  else{out <- fix(.y)}
  return(out)
})

ro_fixed <- map2(ro, roosts_days, ~{
  if(nrow(.x) > 0){out <- .x}
  else{out <- fix(.y)}
  return(out)
})

# Make graphs -------------------------------------------------------------
graphs_fl_gv <- map(fl_fixed, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
graphs_fe_gv <- map(fe_fixed, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))
graphs_ro_gv <- map(ro_fixed, ~vultureUtils::makeGraph(mode = "sri", data = .x, weighted = T))

# Get metrics -------------------------------------------------------------
metrics_fl_gv <- map2(graphs_fl_gv, days, ~{
  getmetrics(graph = .x, interval = .y, type = "flight", days = 1)
})

metrics_fe_gv <- map2(graphs_fe_gv, days, ~{
  getmetrics(graph = .x, interval = .y, type = "feeding", days = 1)
})

metrics_ro_gv <- map2(graphs_ro_gv, days_roosts, ~{ # should be the same as `days`, but keeping this consistent just in case.
  getmetrics(graph = .x, interval = .y, type = "roosting", days = 1)
})

# Attach metrics ----------------------------------------------------------
graphs_fl_gv_tbl <- map(graphs_fl_gv, as_tbl_graph)
graphs_fe_gv_tbl <- map(graphs_fe_gv, as_tbl_graph)
graphs_ro_gv_tbl <- map(graphs_ro_gv, as_tbl_graph)

graphs_fl_gv_tbl <- map2(graphs_fl_gv_tbl, metrics_fl_gv, ~{
  .x %>% activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

graphs_fe_gv_tbl <- map2(graphs_fe_gv_tbl, metrics_fe_gv, ~{
  .x %>% activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

graphs_ro_gv_tbl <- map2(graphs_ro_gv_tbl, metrics_ro_gv, ~{
  .x %>% activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

# Daily centroids ---------------------------------------------------------
daily_centroids <- map(dat_days, ~{
  .x %>% select(Nili_id, dateOnly) %>% group_by(Nili_id, dateOnly) %>%
    summarize(st_union(geometry), .groups = "drop") %>%
    st_centroid() %>% bind_cols(as.data.frame(st_coordinates(.))) %>%
    st_drop_geometry() %>%
    rename("X_day" = X,
           "Y_day" = Y)
}, .progress = T)

graphs_fl_gv_tbl <- map2(graphs_fl_gv_tbl, daily_centroids, ~{
  .x %>%
    activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

graphs_fe_gv_tbl <- map2(graphs_fe_gv_tbl, daily_centroids, ~{
  .x %>%
    activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

graphs_ro_gv_tbl <- map2(graphs_ro_gv_tbl, daily_centroids, ~{
  .x %>%
    activate(nodes) %>%
    left_join(.y, by = c("name" = "Nili_id"))
})

# Bounding box ------------------------------------------------------------
centrs <- purrr::list_rbind(daily_centroids) 
bbox <- data.frame(xmin = 34.5, xmax = 35.5, ymin = 30.5, ymax = 31.5)

# Make ggplot graphs ------------------------------------------------------
graph_fn_mod <- function(graph, layout, title, bbox){
  g <- ggraph(graph, layout) +
    geom_edge_link(alpha = 0.3, color = "black")+
    geom_node_point(aes(col = normDegree, size = normDegree), alpha = 0.9)+
    geom_node_text(aes(label = name, color = ), 
                   col = "white", 
                   repel = T,
                   size = 1.5,
                   max.overlaps = 20,
                   force = 5)+
    scale_color_viridis_c(limits = c(0, 1))+
    scale_size_continuous(limits = c(0, 1))+
    xlim(bbox$xmin[1], bbox$xmax[1])+
    ylim(bbox$ymin[1], bbox$ymax[1])+
    theme(legend.position = "none",
          panel.background = element_rect(fill = "gray80", color = "gray80"),
          NULL)+
    ggtitle(title)
  return(g)
}

flight_gg <- map(graphs_fl_gv_tbl, ~{
  layout <- .x %>% activate(nodes) %>% data.frame() %>%
    select("x" = "X_day",
           "y" = "Y_day")
  date <- .x %>% activate(nodes) %>% slice(1) %>% pull(int)
  title = paste0(date, " (flight)")
  g <- graph_fn_mod(.x, layout = layout, title = title, bbox = bbox)
  return(g)
})

feeding_gg <- map(graphs_fe_gv_tbl, ~{
  layout <- .x %>% activate(nodes) %>% data.frame() %>%
    select("x" = "X_day",
           "y" = "Y_day")
  date <- .x %>% activate(nodes) %>% slice(1) %>% pull(int)
  title = paste0(date, " (feeding)")
  g <- graph_fn_mod(.x, layout = layout, title = title, bbox = bbox)
  return(g)
})

roosting_gg <- map(graphs_ro_gv_tbl, ~{
  layout <- .x %>% activate(nodes) %>% data.frame() %>%
    select("x" = "X_day",
           "y" = "Y_day")
  date <- .x %>% activate(nodes) %>% slice(1) %>% pull(int)
  title = paste0(date, " (roosting)")
  g <- graph_fn_mod(.x, layout = layout, title = title, bbox = bbox)
  return(g)
})

walk2(flight_gg, days, ~{
  ggsave(plot = .x, filename = paste0("fig/networkGraphs_forGideon/flight_", .y, ".png"), height = 4, width = 2.5)
})

walk2(feeding_gg, days, ~{
  ggsave(plot = .x, filename = paste0("fig/networkGraphs_forGideon/feeding_", .y, ".png"), height = 4, width = 2.5)
})

walk2(roosting_gg, days, ~{
  ggsave(plot = .x, filename = paste0("fig/networkGraphs_forGideon/roosting_", .y, ".png"), height = 4, width = 2.5)
})

