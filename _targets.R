# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  packages = c("tidyverse", "tibble", "purrr", "sf", "igraph", "muxViz", "Matrix", "dplyr", "vultureUtils")#, # Packages that your targets need for their tasks.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Reducibility analysis
  ## General data ingest and prep
  tar_target(cols_to_remove, c("tag_id", "sensor_type_id", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "battery_charge_percent", "battery_charging_current", "external_temperature", "gps_hdop", "gps_satellite_count", "gps_time_to_fix", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol", "tag_voltage", "update_ts", "visible", "deployment_id", "event_id", "sensor_type", "tag_local_identifier", "location_long.1", "location_lat.1", "optional", "sensor", "earliest_date_born", "exact_date_of_birth", "group_id", "individual_id", "latest_date_born", "local_identifier", "marker_id", "mates", "mortality_date", "mortality_latitude", "mortality_type", "nick_name", "offspring", "parents", "ring_id", "siblings", "taxon_canonical_name", "taxon_detail", "number_of_events", "number_of_deployments")),
  tar_target(alldata_file, "data/fromMvmtSoc/downsampled_10min_forSocial.Rda", format = "file"),
  tar_target(alldata, read_from_path(alldata_file)),
  tar_target(season_names, map_chr(alldata, ~as.character(.x$seasonUnique[1]))),
  tar_target(alldata_prepped, map(alldata, ~prepare_data(.x, cols_to_remove = cols_to_remove))),
  tar_target(roosts_file, "data/roosts.Rda", format = "file"),
  tar_target(roosts, read_from_path(roosts_file)),
  tar_target(roostPolygons_file, "data/raw/roosts50_kde95_cutOffRegion.kml", format = "file"),
  tar_target(roostPolygons, st_read(roostPolygons_file)),
  
  # Time window analysis
  # SEASONS -----------------------------------------------------------------
  tar_target(data_seasons, alldata_prepped), # just renaming these; unnecessary step
  tar_target(roosts_seasons, roosts),
  ### prepare edges
  tar_target(flight_sris_seasons, get_flight_sris(data_seasons, roostPolygons)),
  tar_target(feeding_sris_seasons, get_feeding_sris(data_seasons, roostPolygons)),
  tar_target(allvertices_seasons, unique(list_rbind(data_seasons)$Nili_id)),
  tar_target(nnodes_seasons, length(allvertices_seasons)),

  ### make graphs
  tar_target(graphs_flight_seasons, get_graphs(flight_sris_seasons, allvertices_seasons)),
  tar_target(graphs_feeding_seasons, get_graphs(feeding_sris_seasons, allvertices_seasons)),
  ### Make tensors
  tar_target(tensor_flight_seasons, get_node_tensor(graphs_flight_seasons)),
  tar_target(tensor_feeding_seasons, get_node_tensor(graphs_feeding_seasons)),
  ### get nlayers
  tar_target(nlayers_flight_seasons, length(tensor_flight_seasons)),
  tar_target(nlayers_feeding_seasons, length(tensor_feeding_seasons)),
  ### get reducibility curves
  tar_target(red_flight_cat_seasons, get_reducibility(graphs_flight_seasons,
                                                      nlayers_flight_seasons,
                                                      nnodes_seasons,
                                                      type = "Categorical")),
  tar_target(red_feeding_cat_seasons, get_reducibility(graphs_feeding_seasons,
                                                       nlayers_feeding_seasons,
                                                       nnodes_seasons,
                                                       type = "Categorical")),
  tar_target(curves_seasons, purrr::list_rbind(list(
    get_reduc_curves_df_seasons(red_flight_cat_seasons, "categorical", "flight"),
    get_reduc_curves_df_seasons(red_feeding_cat_seasons, "categorical", "feeding")))),
  
  # DAYS ---------------------------------------------------------------------
  ### select one season to work with (Summer 2023). Including only flight and feeding.
  tar_target(summer2023data, alldata_prepped[[which(season_names == "2023_summer")]]),

  ### Cut data
  tar_target(timewindows, c(1, 5, 10, 25)),
  tar_target(timewindows_heuristic, seq(from = 1, to = 50, by = 5)),
  tar_target(data_cut, cut_data(summer2023data, timewindows)),
  tar_target(data_cut_heuristic, cut_data(summer2023data, timewindows_heuristic)),
  ### Prepare edges
  tar_target(flight_sris, map(data_cut, ~get_flight_sris(.x, roostPolygons))),
  tar_target(feeding_sris, map(data_cut, ~get_feeding_sris(.x, roostPolygons))),
  tar_target(flight_sris_heuristic, map(data_cut_heuristic, ~get_flight_sris(.x, roostPolygons))),
  tar_target(feeding_sris_heuristic, map(data_cut_heuristic, ~get_feeding_sris(.x, roostPolygons))),
  tar_target(allvertices, map(data_cut, ~unique(list_rbind(.x)$Nili_id))),
  tar_target(allvertices_heuristic, map(data_cut_heuristic, ~unique(list_rbind(.x)$Nili_id))),
  
  ## Get edges for the 1-day time window
  tar_target(feeding_edges, getFeedingEdges(dataset = summer2023data, roostPolygons = roostPolygons, return = "edges", consecThreshold = 2)),
  tar_target(flight_edges, getFlightEdges(dataset = summer2023data, roostPolygons = roostPolygons, return = "edges", consecThreshold = 2)),
  tar_target(nnodes, map_dbl(allvertices, length)),
  ### Make graphs
  tar_target(graphs_flight, map2(flight_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(graphs_feeding, map2(feeding_sris, allvertices, ~get_graphs(.x, .y))),

  tar_target(graphs_flight_heuristic, 
             map2(flight_sris_heuristic, allvertices_heuristic, ~get_graphs(.x, .y))),
  tar_target(graphs_feeding_heuristic, 
             map2(feeding_sris_heuristic, allvertices_heuristic, ~get_graphs(.x, .y))),

  ### Make tensors
  tar_target(tensors_flight, map(graphs_flight, get_node_tensor)),
  tar_target(tensors_feeding, map(graphs_feeding, get_node_tensor)),
  ### get nlayers
  tar_target(nlayers_flight, map_dbl(tensors_flight, length)),
  tar_target(nlayers_feeding, map_dbl(tensors_feeding, length)),
  ### get reducibility curves
  tar_target(red_flight_cat, pmap(list(graphs_flight, nlayers_flight, nnodes),
                                  ~get_reducibility(..1, ..2, ..3,
                                                    type = "Categorical"))),
  tar_target(red_feeding_cat, pmap(list(graphs_feeding, nlayers_feeding, nnodes),
                                   ~get_reducibility(..1, ..2, ..3,
                                                     type = "Categorical"))),
  tar_target(curves, purrr::list_rbind(list(
    get_reduc_curves_df(red_flight_cat, timewindows, "categorical", "flight"),
    get_reduc_curves_df(red_feeding_cat, timewindows, "categorical", "feeding")
  ))),
  
  ## Dyads
  tar_target(fe_5days_fordyads, prep(feeding_sris[[2]], "feeding")),
  tar_target(fl_5days_fordyads, prep(flight_sris[[2]], "flight")),
  tar_target(all_5days_fordyads, 
             list_rbind(list(fe_5days_fordyads, fl_5days_fordyads)) %>% 
               mutate(dyad = paste(ID1, ID2, sep = ", ")) %>%
               group_by(dyad, situ) %>%
               mutate(n_dyad_situ = n(),
                      n_dyad_situ_nonzero = sum(weight > 0)) %>%
               ungroup() %>%
               group_by(dyad) %>%
               mutate(n_dyad = n(),
                      n_dyad_nonzero = sum(weight > 0))),
  tar_target(sri_dist_fl, pull(fl_5days_fordyads, weight)),
  tar_target(sri_dist_fe, pull(fe_5days_fordyads, weight)),
  tar_target(static_fl, fl_5days_fordyads %>% 
               select(ID1, ID2, situ, period) %>% 
               mutate(weight = sample(sri_dist_fl))),
  tar_target(static_fe, fe_5days_fordyads %>% 
               select(ID1, ID2, situ, period) %>% 
               mutate(weight = sample(sri_dist_fe))),
  tar_target(static, bind_rows(static_fl, static_fe) %>%
               mutate(dyad = paste(ID1, ID2, sep = ", "))),
  tar_target(gs, get_networks_dyads(all_5days_fordyads)),
  tar_target(gs_static, get_networks_dyads(static)),
  tar_target(shuffled_reps_df, get_shuffled_reps_df(graphs = gs, reps = 100)),
  tar_target(shuffled_reps_static_df, get_shuffled_reps_df(graphs = gs_static, reps = 100)),
  tar_target(replicates, correct_dyad_id_order(shuffled_reps_df)),
  tar_target(replicates_static, correct_dyad_id_order(shuffled_reps_static_df)),
  tar_target(lms_obs_summ, get_lms(all_5days_fordyads)),
  tar_target(lms_obs_summ_static, get_lms(static)),
  tar_target(lms_perm_summ, get_lms_permuted(replicates, workers = 20)),
  tar_target(lms_perm_summ_static, get_lms_permuted(replicates_static, workers = 20)),
  tar_target(combined_obs, get_combined(all_5days_fordyads, lms_obs_summ, lms_perm_summ)),
  tar_target(combined_static, get_combined(static, lms_obs_summ_static, lms_perm_summ_static))
)