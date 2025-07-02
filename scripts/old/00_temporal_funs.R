
#' Make repeatability plots
#' @param df
#' @param var
#' @param situation
#' @param ylab
plotRepeat <- function(df, var, situation = "flight", ylab){
  legend_title <- paste0("Mean\n", var, "\nvalue")
  plt <- df %>%
    filter(type == situation) %>%
    group_by(Nili_id) %>%
    mutate(avg = mean(.data[[var]], na.rm = T)) %>%
    ggplot(aes(x = seasonUnique, y = get(var), group = Nili_id, color = avg))+
    geom_point()+
    geom_line(alpha = 0.7)+
    theme_classic()+
    scale_color_viridis_c(name = legend_title)+
    xlab("Season")+
    ylab(ylab)+
    theme(axis.title = element_text(size = 16))
  return(plt)
}

cols_to_remove <- c("tag_id", "sensor_type_id", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "battery_charge_percent", "battery_charging_current", "external_temperature", "gps_hdop", "gps_satellite_count", "gps_time_to_fix", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "magnetic_field_raw_z", "ornitela_transmission_protocol", "tag_voltage", "update_ts", "visible", "deployment_id", "event_id", "sensor_type", "tag_local_identifier", "location_long.1", "location_lat.1", "optional", "sensor", "earliest_date_born", "exact_date_of_birth", "group_id", "individual_id", "latest_date_born", "local_identifier", "marker_id", "mates", "mortality_date", "mortality_latitude", "mortality_type", "nick_name", "offspring", "parents", "ring_id", "siblings", "taxon_canonical_name", "taxon_detail", "number_of_events", "number_of_deployments")

# Function to get degree and strength
getmetrics <- function(graph, interval, type, days){
  if(length(graph) > 0){
    metrics <- data.frame(degree = igraph::degree(graph),
                          strength = igraph::strength(graph),
                          Nili_id = names(igraph::degree(graph)),
                          int = interval,
                          n = length(V(graph)),
                          type = type,
                          ndays = days) %>%
      mutate(normDegree = degree/(n-1),
             normStrength = strength/sum(strength))
  }else{
    metrics <- data.frame(degree = NA, strength = NA, 
                          Nili_id = NA, int = interval, 
                          n = 0, type = type, ndays = days)
  }
  
  return(metrics)
}

graph_fn <- function(graph, layout, title, bbox){
    g <- ggraph(graph, layout) +
      geom_edge_link(alpha = 0.3, color = "black")+
      geom_node_point(aes(col = normDegree, size = normDegree), alpha = 0.9)+
      geom_node_text(aes(label = name, color = ), 
                     col = "white", 
                     repel = T,
                     size = 1,
                     max.overlaps = 20,
                     force = 1)+
      scale_color_viridis_c(limits = c(0, 1))+
      scale_size_continuous(limits = c(0, 1))+
      theme_void()+
      xlim(bbox$xmin[1], bbox$xmax[1])+
      ylim(bbox$ymin[1], bbox$ymax[1])+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "gray80", color = "gray80"),
            NULL)+
      ggtitle(title)
    return(g)
}

makegraphs <- function(situation, timewindow, centroids, bboxes, quiet){
  # Make the graph titles
  days <- timewindows[timewindow] # how many days?
  breaks <- brks[[timewindow]]
  titles <-  map(breaks, ~{
    paste0("Co-", situation, ", ", days, "-day interval: [",
           .x, ", ", lubridate::ymd(.x) + days, ")")
  })
  
  filenames <- map(breaks, ~{
    paste0("fig/networkGraphs/interval/days_",
           str_pad(days, width = 2, side = "left", pad = "0"),
           "/", "centroid_", centroids, "/",
           situation, "_", .x, ".png")
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
    havegraphs <- map_chr(graphs, ~.x %>% activate(nodes) %>% pull(int) %>% head(1))
    whch_havegraphs <- which(breaks %in% havegraphs)
    breaks <- breaks[whch_havegraphs]
    titles <- titles[whch_havegraphs]
    filenames <- filenames[whch_havegraphs]
    if(length(graphs) != length(titles)){
      stop("graphs and titles don't match!")
    }
  }
  if(!quiet){message("making layouts")}
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
  if(!quiet){message("making plots")}
  plots <- vector(mode = "list", length = length(graphs))
  for(i in 1:length(graphs)){
    if(centroids == "overall"){
      bbox <- bboxes
    }else{
      bbox <- bboxes[[timewindow]]
    }
    plots[[i]] <- graph_fn(graph = graphs[[i]],
                           layout = layouts[[i]],
                           title = titles[[i]],
                           bbox = bbox)
  }
  return(list("plots" = plots, "filenames" = filenames))
  if(!quiet){message("completed")}
  rm(days)
  rm(breaks)
  rm(titles)
  rm(filenames)
  rm(graphs)
  rm(layouts)
  rm(plots)
}

# Version of GetMultilayerReducibility (from muxViz) with more feedback to the console
GetMultilayerReducibility <- function (SupraAdjacencyMatrix, Layers, Nodes, Method, Type) 
{
  NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, 
                                             Layers, Nodes)
  if (Layers > 1) {
    SingleLayerEntropy <- rep(NA, Layers)
    for (i in 1:Layers) {
      SingleLayerEntropy[i] <- GetRenyiEntropyFromAdjacencyMatrix(NodesTensor[[i]], 
                                                                  1)
      if (SingleLayerEntropy[i] < 1e-12) 
        SingleLayerEntropy[i] <- 0
      print(paste("DEBUG:", i, SingleLayerEntropy[i]))
    }
    message("getting JSD")
    JSD <- Matrix::Matrix(0, Layers, Layers)
    if (Type == "Ordinal") {
      JSD <- Matrix::Matrix(1, Layers, Layers)
      for (i in 1:(Layers - 1)) {
        cat(".")
        j <- i + 1
        JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                NodesTensor[[j]], SingleLayerEntropy[i], SingleLayerEntropy[j])
      }
    }
    if (Type == "Categorical") {
      for (i in 1:(Layers - 1)) {
        cat(".")
        for (j in (i + 1):Layers) {
          JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                  NodesTensor[[j]], SingleLayerEntropy[i], 
                                                  SingleLayerEntropy[j])
          JSD[j, i] <- JSD[i, j]
        }
      }
    }
    message("got JSD")
    JSD <- sqrt(JSD)
    hc <- stats::hclust(stats::as.dist(JSD), method = Method)
    MergeMatrix <- hc$merge
    AggregateMatrix <- NodesTensor[[1]]
    for (i in 2:Layers) {
      AggregateMatrix <- AggregateMatrix + NodesTensor[[i]]
    }
    AggregateEntropy <- GetRenyiEntropyFromAdjacencyMatrix(AggregateMatrix, 
                                                           1)
    print(paste("DEBUG: Aggregate entropy", AggregateEntropy))
    print(MergeMatrix)
    gQualityFunction <- rep(0, Layers)
    relgQualityFunction <- rep(0, Layers)
    cntCurrentLayers <- sum(SingleLayerEntropy > 0)
    gQualityFunction[1] <- cntCurrentLayers * AggregateEntropy - 
      sum(SingleLayerEntropy[SingleLayerEntropy > 0])
    relgQualityFunction[1] <- gQualityFunction[1]/(cntCurrentLayers * 
                                                     AggregateEntropy)
    MergedTensor <- list()
    MergedEntropy <- list()
    message("Dealing with merged tensor")
    for (m in 1:(Layers - 1)) {
      if (MergeMatrix[m, 1] < 0) {
        A <- NodesTensor[[-MergeMatrix[m, 1]]]
        entropyA <- SingleLayerEntropy[-MergeMatrix[m, 
                                                    1]]
        SingleLayerEntropy[-MergeMatrix[m, 1]] <- 0
      }
      else {
        A <- MergedTensor[[MergeMatrix[m, 1]]]
        entropyA <- MergedEntropy[[MergeMatrix[m, 1]]]
        MergedEntropy[[MergeMatrix[m, 1]]] <- 0
      }
      if (MergeMatrix[m, 2] < 0) {
        B <- NodesTensor[[-MergeMatrix[m, 2]]]
        entropyB <- SingleLayerEntropy[-MergeMatrix[m, 
                                                    2]]
        SingleLayerEntropy[-MergeMatrix[m, 2]] <- 0
      }
      else {
        B <- MergedTensor[[MergeMatrix[m, 2]]]
        entropyB <- MergedEntropy[[MergeMatrix[m, 2]]]
        MergedEntropy[[MergeMatrix[m, 2]]] <- 0
      }
      MergedTensor[[m]] <- A + B
      tmpLayerEntropy <- GetRenyiEntropyFromAdjacencyMatrix(MergedTensor[[m]], 
                                                            1)
      if (abs(tmpLayerEntropy) < 1e-12) 
        tmpLayerEntropy <- 0
      MergedEntropy[[m]] <- tmpLayerEntropy
      diffEntropy <- 2 * tmpLayerEntropy - (entropyA + 
                                              entropyB)
      reldiffEntropy <- diffEntropy/(2 * tmpLayerEntropy)
      cntCurrentLayers <- Layers - m
      gQualityFunction[m + 1] <- sum(SingleLayerEntropy[SingleLayerEntropy > 
                                                          0])
      for (i in 1:m) {
        if (MergedEntropy[[i]] > 0) {
          gQualityFunction[m + 1] <- gQualityFunction[m + 
                                                        1] + MergedEntropy[[i]]
        }
      }
      gQualityFunction[m + 1] <- cntCurrentLayers * AggregateEntropy - 
        gQualityFunction[m + 1]
      relgQualityFunction[m + 1] <- gQualityFunction[m + 
                                                       1]/(cntCurrentLayers * AggregateEntropy)
    }
  }
  return(list(JSD = JSD, gQualityFunction = gQualityFunction, 
              relgQualityFunction = relgQualityFunction))
}
