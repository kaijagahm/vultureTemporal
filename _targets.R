# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "purrr", "sf", "igraph", "muxViz", "Matrix") # Packages that your targets need for their tasks.
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
  ## data ingest
  tar_target(data_cut_file, "data/data_cut.Rda", format = "file"),
  tar_target(data_cut, read_from_path(data_cut_file)),
  tar_target(roostPolygons_file, "data/raw/roosts50_kde95_cutOffRegion.kml", format = "file"),
  tar_target(roostPolygons, st_read(roostPolygons_file)),
  ## Prepare edges
  tar_target(flight_sris, map(data_cut, ~get_flight_sris(.x, roostPolygons))),
  tar_target(feeding_sris, map(data_cut, ~get_feeding_sris(.x, roostPolygons))),
  tar_target(allvertices, map(data_cut, ~unique(list_rbind(.x)$Nili_id))),
  tar_target(nnodes, map_dbl(allvertices, length)),
  ## Make graphs
  tar_target(graphs_flight, map2(flight_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(graphs_feeding, map2(feeding_sris, allvertices, ~get_graphs(.x, .y))),
  tar_target(tensors_flight, map(graphs_flight, get_node_tensor)),
  tar_target(tensors_feeding, map(graphs_feeding, get_node_tensor)),
  tar_target(nlayers, map_dbl(tensors_flight, length)),
  tar_target(layer_tensor, map(nlayers, get_layer_tensor)),
  tar_target(multis_flight, pmap(.l = list(tensors_flight, layer_tensor, nlayers, nnodes),
                                 .f = get_multiplex)),
  tar_target(multis_feeding, pmap(.l = list(tensors_feeding, layer_tensor, nlayers, nnodes),
                                  .f = get_multiplex)),
  tar_target(ag_flight, pmap(.l = list(multis_flight, nlayers, nnodes),
                             .f = get_aggregate)),
  tar_target(ag_feeding, pmap(.l = list(multis_feeding, nlayers, nnodes),
                              .f = get_aggregate)),
  tar_target(red_flight_cat, pmap(.l = list(graphs_flight, nlayers, nnodes),
                                  .f = ~get_reducibility(..1, ..2, ..3, 
                                                         type = "Categorical"))),
  tar_target(red_flight_ord, pmap(.l = list(graphs_flight, nlayers, nnodes),
                                  .f = ~get_reducibility(..1, ..2, ..3, 
                                                         type = "Ordinal"))),
  tar_target(red_feeding_cat, pmap(.l = list(graphs_feeding, nlayers, nnodes),
                                   .f = ~get_reducibility(..1, ..2, ..3, 
                                                          type = "Categorical"))) # this errors out, presumably because we have empty graphs. Need to make them not empty by attaching the vertices.
)

