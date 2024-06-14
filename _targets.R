# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "purrr", "sf", "igraph", "muxViz", "Matrix", "dplyr") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
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
  
  ## Time window analysis
  # YEARS -------------------------------------------------------------------
  tar_target(data_years, function(alldata_prepped){
    yr_20_21 <- purrr::list_rbind(alldata_prepped[1:3])
    yr_21_22 <- purrr::list_rbind(alldata_prepped[4:6])
    yr_22_23 <- purrr::list_rbind(alldata_prepped[7:9])
    yrs <- list("yr_20_21" = yr_20_21,
                "yr_21_22" = yr_21_22,
                "yr_22_23" = yr_22_23)
    return(yrs)
  }),
  tar_target(roosts_years, function(roosts){
    r_20_21 <- purrr::list_rbind(roosts[1:3])
    r_21_22 <- purrr::list_rbind(roosts[4:6])
    r_22_23 <- purrr::list_rbind(roosts[7:9])
    rs <- list("r_20_21" = r_20_21,
               "r_21_22" = r_21_22,
               "r_22_23" = r_22_23)
    return(rs)
  }), # will come back to this; can't really do reducibility analysis on just three slices.
  
  # SEASONS -----------------------------------------------------------------
  tar_target(data_seasons, alldata_prepped), # just renaming these; unnecessary step
  tar_target(roosts_seasons, roosts),
  ### prepare edges
  tar_target(flight_sris_seasons, get_flight_sris(data_seasons, roostPolygons)),
  tar_target(feeding_sris_seasons, get_feeding_sris(data_seasons, roostPolygons)),
  tar_target(roost_sris_seasons, get_roost_sris(roosts_seasons)),
  tar_target(allvertices_seasons, unique(list_rbind(data_seasons)$Nili_id)),
  tar_target(nnodes_seasons, length(allvertices_seasons)),
  tar_target(aggregate_sris_seasons, NULL),
  ### make graphs
  tar_target(graphs_flight_seasons, get_graphs(flight_sris_seasons, allvertices_seasons)),
  tar_target(graphs_feeding_seasons, get_graphs(feeding_sris_seasons, allvertices_seasons)),
  tar_target(graphs_roosting_seasons, get_graphs(roost_sris_seasons, allvertices_seasons)),
  ### Make tensors
  tar_target(tensor_flight_seasons, get_node_tensor(graphs_flight_seasons)),
  tar_target(tensor_feeding_seasons, get_node_tensor(graphs_feeding_seasons)),
  tar_target(tensor_roosting_seasons, get_node_tensor(graphs_roosting_seasons)),
  ### get nlayers
  tar_target(nlayers_flight_seasons, length(tensor_flight_seasons)),
  tar_target(nlayers_feeding_seasons, length(tensor_feeding_seasons)),
  tar_target(nlayers_roosting_seasons, length(tensor_roosting_seasons)),
  ### get reducibility curves
  tar_target(red_flight_cat_seasons, get_reducibility(graphs_flight_seasons,
                                                      nlayers_flight_seasons,
                                                      nnodes_seasons,
                                                      type = "Categorical")),
  tar_target(red_feeding_cat_seasons, get_reducibility(graphs_feeding_seasons,
                                                      nlayers_feeding_seasons,
                                                      nnodes_seasons,
                                                      type = "Categorical")),
  tar_target(red_roosting_cat_seasons, get_reducibility(graphs_roosting_seasons,
                                                      nlayers_roosting_seasons,
                                                      nnodes_seasons,
                                                      type = "Categorical")),
  tar_target(curves_seasons, purrr::list_rbind(list(
    get_reduc_curves_df_seasons(red_flight_cat_seasons, "categorical", "flight"),
    get_reduc_curves_df_seasons(red_feeding_cat_seasons, "categorical", "feeding"),
    get_reduc_curves_df_seasons(red_roosting_cat_seasons, "categorical", "roosting")))),

  # DAYS ---------------------------------------------------------------------
  ### select one season to work with (Summer 2023). Including all three social situations.
  tar_target(summer2023data, alldata_prepped[[which(season_names == "2023_summer")]]),
  tar_target(summer2023roosts, roosts[[which(season_names == "2023_summer")]]),

  ### Cut data
  tar_target(timewindows, c(1, 5, 10, 25)),
  tar_target(data_cut, cut_data(summer2023data, timewindows)),
  tar_target(roosts_cut, cut_roosts(summer2023roosts, timewindows)),
  ### Prepare edges
  tar_target(flight_sris, map(data_cut, ~get_flight_sris(.x, roostPolygons))),
  tar_target(feeding_sris, map(data_cut, ~get_feeding_sris(.x, roostPolygons))),
  tar_target(roost_sris, map(roosts_cut, ~get_roost_sris(.x))),
  tar_target(allvertices, map(data_cut, ~unique(list_rbind(.x)$Nili_id))),
  tar_target(nnodes, map_dbl(allvertices, length)),
  tar_target(aggregate_sris, get_aggregate_sris(flight_sris, feeding_sris, roost_sris)),
  ### Make graphs
  tar_target(graphs_flight, map2(flight_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(graphs_feeding, map2(feeding_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(graphs_roosting, map2(roost_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(graphs_aggregate, map2(aggregate_sris, allvertices, ~get_graphs(.x, .y))),
  ### Make tensors
  tar_target(tensors_flight, map(graphs_flight, get_node_tensor)),
  tar_target(tensors_feeding, map(graphs_feeding, get_node_tensor)),
  tar_target(tensors_roosting, map(graphs_roosting, get_node_tensor)),
  tar_target(tensors_aggregate, map(graphs_aggregate, get_node_tensor)),
  ### get nlayers
  tar_target(nlayers_flight, map_dbl(tensors_flight, length)),
  tar_target(nlayers_feeding, map_dbl(tensors_feeding, length)),
  tar_target(nlayers_roosting, map_dbl(tensors_roosting, length)),
  tar_target(nlayers_aggregate, map_dbl(tensors_aggregate, length)),
  ### get reducibility curves
  tar_target(red_flight_cat, pmap(list(graphs_flight, nlayers_flight, nnodes),
                                   ~get_reducibility(..1, ..2, ..3,
                                                         type = "Categorical"))),
  tar_target(red_feeding_cat, pmap(list(graphs_feeding, nlayers_feeding, nnodes),
                                   ~get_reducibility(..1, ..2, ..3,
                                                          type = "Categorical"))),
  tar_target(red_roosting_cat, pmap(list(graphs_roosting, nlayers_roosting, nnodes),
                                    ~get_reducibility(..1, ..2, ..3,
                                                           type = "Categorical"))),
  tar_target(red_aggregate_cat, pmap(list(graphs_aggregate, nlayers_aggregate, nnodes),
                                     ~get_reducibility(..1, ..2, ..3,
                                                            type = "Categorical"))),
  tar_target(curves, purrr::list_rbind(list(
    get_reduc_curves_df(red_flight_cat, timewindows, "categorical", "flight"),
    get_reduc_curves_df(red_feeding_cat, timewindows, "categorical", "feeding"),
    get_reduc_curves_df(red_roosting_cat, timewindows, "categorical", "roosting"),
    get_reduc_curves_df(red_aggregate_cat, timewindows, "categorical", "aggregate")
  ))),

  # HOURS -------------------------------------------------------------------
  ### select a 10-day period, let's say July 1-July 10 2023 (not doing roosts for this because it's for hourly data.)
  tar_target(data_10day, summer2023data %>%
               filter(dateOnly >= lubridate::ymd("2023-07-01"),
                      dateOnly <= lubridate::ymd("2023-07-10"))),
  tar_target(data_cut_hours, cuttimes(data_10day, mins = "60")),
  tar_target(allvertices_hours, unique(list_rbind(data_cut_hours)$Nili_id)),
  tar_target(nnodes_hours, length(allvertices_hours)),
  tar_target(flight_sris_hours, get_flight_sris(data_cut_hours, roostPolygons)),
  tar_target(feeding_sris_hours, get_feeding_sris(data_cut_hours, roostPolygons)),
  #### make graphs
  tar_target(graphs_flight_hours, get_graphs(flight_sris_hours, allvertices)),
  tar_target(graphs_feeding_hours, get_graphs(feeding_sris_hours, allvertices)),
  #### make tensors
  tar_target(tensor_flight_hours, get_node_tensor(graphs_flight_hours)),
  tar_target(tensor_feeding_hours, get_node_tensor(graphs_feeding_hours)),
  #### get nlayers
  tar_target(nlayers_feeding_hours, length(tensor_feeding_hours)),
  tar_target(nlayers_flight_hours, length(tensor_flight_hours)),
  ### get reducibility curves
  tar_target(red_flight_hours_cat,
             get_reducibility(graphs_flight_hours, nlayers_flight_hours,
                             nnodes_hours, type = "Categorical")),
  tar_target(red_feeding_hours_cat,
             get_reducibility(graphs_feeding_hours, nlayers_feeding_hours,
                              nnodes_hours, type = "Categorical")),
  tar_target(curves_hours,
             pmap(.l = list(data = list(red_flight_hours_cat,
                                        red_feeding_hours_cat),
                            type = "categorical",
                            situ = c("flight", "feeding")
             ), ~data.frame(ent = ..1$gQualityFunction,
                            timewindow = "1 hour",
                            type = ..2, situ = ..3) %>%
               dplyr::mutate(step = 1:nrow(.))) %>% purrr::list_rbind())
)