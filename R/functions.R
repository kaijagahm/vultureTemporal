# Functions for targets pipeline
library(dplyr)

cc <- list("breedingColor" = "#2FF8CA", "summerColor" = "#CA2FF8", "fallColor" = "#F8CA2F", flightColor = "dodgerblue", roostingColor = "olivedrab4", "feedingColor" = "gold")
situcolors <- c(cc$feedingColor, cc$flightColor, cc$roostingColor)

read_from_path <- function(path){
  envir <- environment()
  data_name <- load(path, envir = envir)
  get(data_name)
}

prepare_data <- function(dataset, cols_to_remove){
  # Convert to SF
  sfdata <- sf::st_as_sf(dataset, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)
  
  # Put all the seasons together and remove the columns we don't need
  sfdata <- sfdata %>%
    dplyr::select(-any_of(cols_to_remove)) 
  
  return(sfdata)
}

cutdates <- function(vec, days){
  # get min and max dates in the vector
  min <- min(vec)
  max <- max(vec)
  
  # determine how many cutpoints we'll need
  ncutpoints <- ceiling(as.numeric(max-min)/days) + 1
  
  # create the vector of cutpoints
  cutpoints <- seq(from = min, by = days, length.out = ncutpoints)
  
  # cut the dates according to the cutpoints. Intervals will have the format [low, high).
  out <- cut(vec, breaks = cutpoints, include.lowest = T, right = F)
  return(out)
}

cuttimes <- function(data, mins){
  interval <- paste0(mins, " min")
  out <- data %>% dplyr::mutate(int = cut.POSIXt(timestamp, interval)) %>%
    group_by(int) %>% group_split() %>% purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
  return(out)
}

cut_data <- function(data, timewindows){
  data_cut <- purrr::map(timewindows, ~{
    out <- data
    out$int <- cutdates(out$dateOnly, .x)
    return(out)
  })
  
  # Okay, now we have the data classified into intervals, time to split each one into a list.
  # There is an annoying thing here: when you have an sf object and you run group_by() %>% group_split() on it, the resulting sub-objects do not keep their sf status. They turn into regular data frames. Grrrrr! So I had to add a step to turn each of them back into an sf object.
  data_cut <- purrr::map(data_cut, ~.x %>% 
                           dplyr::group_by(int) %>% 
                           dplyr::group_split() %>%
                           purrr::map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)))
  return(data_cut)
}

cut_roosts <- function(roosts, timewindows){
  
  roosts_cut <- map(timewindows, ~{
    roosts %>% mutate(int = cutdates(roost_date, .x))
  })
  
  roosts_cut <- map(roosts_cut, ~.x %>% 
                      group_by(int) %>% 
                      group_split() %>%
                      map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F)))
  return(roosts_cut)
}

get_flight_sris <- function(datalist, roostPolygons){
  flight_sri <- purrr::map(datalist, ~{
    library(sf)
    vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri")}, .progress = T)
  flight_sri_2 <- purrr::map(flight_sri, ~{
    if(!("sri" %in% names(.x))){
      .x$sri <- numeric(0)
    }
    .x %>% dplyr::filter(!is.na(sri)) %>%
      dplyr::rename("weight" = "sri") %>%
      dplyr::select(ID1, ID2, weight)
  })
  return(flight_sri_2)
}

get_feeding_sris <- function(datalist, roostPolygons){
  feeding_sri <- purrr::map(datalist, ~{
    library(sf)
    vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50,
                                  idCol = "Nili_id", return = "sri")}, .progress = T)
  feeding_sri_2 <- purrr::map(feeding_sri, ~{
    if(!("sri" %in% names(.x))){
      .x$sri <- numeric(0)
    }
    .x %>% dplyr::filter(!is.na(sri)) %>%
      dplyr::rename("weight" = "sri") %>%
      dplyr::select(ID1, ID2, weight)
  })
  return(feeding_sri_2)
}

get_roost_sris <- function(roostlist){
  roost_sri <- furrr::future_map(roostlist, ~{
    library(sf)
    vultureUtils::getRoostEdges(.x, mode = "distance", distThreshold = 100, dateCol = "roost_date", idCol = "Nili_id", return = "sri")
  })
  roost_sri_2 <- purrr::map(roost_sri, ~{
    .x %>% dplyr::filter(!is.na(sri)) %>%
      dplyr::rename("weight" = "sri")
  })
  return(roost_sri_2)
}

binarize <- function(edgelist){
  binary <- edgelist %>%
    mutate(weight = ifelse(weight > 0, 1, 0))
  return(binary)
}

collapse <- function(nestedlist){
  collapsed <- map(nestedlist, ~as.data.frame(data.table::rbindlist(.x, idcol = "period"))) %>%
    data.table::rbindlist(idcol = "timewindow") %>% 
    as.data.frame()
  return(collapsed)
}

get_aggregate_sris <- function(flight_sris, feeding_sris, roost_sris, list = F){
  if(list){
    fl_bin <- map(flight_sris, ~map(.x, binarize)) %>% collapse() %>% mutate(situ = "flight")
    fe_bin <- map(feeding_sris, ~map(.x, binarize)) %>% collapse() %>% mutate(situ = "flight")
    ro_bin <- map(roost_sris, ~map(.x, binarize)) %>% collapse() %>% mutate(situ = "roosting")
  }else{
    fl_bin <- map(flight_sris, binarize) %>% data.table::rbindlist(idcol = "period") %>% as.data.frame() %>% mutate(situ = "flight")
    fe_bin <- map(feeding_sris, binarize) %>% data.table::rbindlist(idcol = "period") %>% as.data.frame() %>% mutate(situ = "feeding")
    ro_bin <- map(roost_sris, binarize) %>% data.table::rbindlist(idcol = "period") %>% as.data.frame() %>% mutate(situ = "roosting")
  }
  all_bin <- bind_rows(fl_bin, fe_bin, ro_bin)
  if(list){
    aggregated <- all_bin %>%
      group_by(timewindow, period, ID1, ID2) %>%
      summarize(sumweight = sum(weight),
                weight = ifelse(sumweight > 0, 1, 0)) %>%
      select(ID1, ID2, weight)
    aggregated_split <- aggregated %>%
      ungroup() %>%
      group_by(timewindow) %>%
      group_split(.keep = F)
    aggregated_split_split <- map(aggregated_split, ~.x %>%
                                    group_by(period) %>%
                                    group_split(.keep = F))
    return(aggregated_split_split)
  }else{
    aggregated <- all_bin %>%
      group_by(period, ID1, ID2) %>%
      summarize(sumweight = sum(weight),
                weight = ifelse(sumweight > 0, 1, 0)) %>%
      select(ID1, ID2, weight)
    aggregated_split <- aggregated %>%
      group_by(period) %>%
      group_split(.keep = F)
    return(aggregated_split)
  }
}

get_graphs <- function(sris, verts){
  graphs <- purrr::map(sris, ~{igraph::graph_from_data_frame(.x, directed = F, 
                                                             vertices = verts)})
  return(graphs)
}

get_node_tensor <- function(graphs){
  nt <- purrr::map(graphs, ~as_adjacency_matrix(.x, attr = "weight"))
  return(nt)
}

get_layer_tensor <- function(nlayers){
  lt <- muxViz::diagR(1, nlayers, 1)
  return(lt)
}

get_multiplex <- function(nt, lt, nlayers, nnodes){
  m <- muxViz::BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nt, lt, nlayers, nnodes)
  return(m)
}

get_aggregate <- function(multi, nlayers, nnodes){
  ag <- muxViz::GetAggregateNetworkFromSupraAdjacencyMatrix(multi, nlayers, nnodes)
  return(ag)
}

get_reducibility <- function(graphs, nlayers, nnodes, type, method = "ward.D2"){
  without_zeroes <- purrr::map(graphs, ~igraph::delete_edges(.x, which(igraph::E(.x)$weight == 0))) # XXX REMOVE ZERO EDGES bc igraph doesn't know that they're zeroes
  nodetensor <- purrr::map(without_zeroes, igraph::as_adjacency_matrix)
  if(type == "Categorical"){
    layertensor <- Matrix::Matrix(1, nlayers, nlayers) - muxViz:::speye(nlayers)
  }else if(type == "Ordinal"){
    layertensor <- BuildLayersTensor(nlayers, 1, "ordinal")
  }
  
  m <- muxViz::BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodetensor, layertensor, nlayers, nnodes)
  red <- GetMultilayerReducibility_KG(m, nlayers, nnodes, method, Type = type) # note: NOT the muxviz version of this function. Need to use the one I made.
  return(red)
}

GetMultilayerReducibility_KG <- function(SupraAdjacencyMatrix, Layers, Nodes, Method, Type){
  NodesTensor <- SupraAdjacencyToNodesTensor(SupraAdjacencyMatrix, 
                                             Layers, Nodes)
  if (Layers > 1) {
    SingleLayerEntropy <- rep(NA, Layers)
    for (i in 1:Layers) {
      if(sum(NodesTensor[[i]]) == 0){ # ADDED BY KG TO DEAL WITH EMPTY NETWORKS
        SingleLayerEntropy[i] <- NA # XXX check that NA is appropriate
      }else{
        SingleLayerEntropy[i] <- GetRenyiEntropyFromAdjacencyMatrix(NodesTensor[[i]], 
                                                                    1)
      }
      if (SingleLayerEntropy[i] < 1e-12 & !is.na(SingleLayerEntropy[i])){
        SingleLayerEntropy[i] <- 0
      } 
      print(paste("DEBUG:", i, SingleLayerEntropy[i]))
    }
    cat("Calculating JSD\n")
    JSD <- Matrix::Matrix(0, Layers, Layers)
    if (Type == "Ordinal") {
      JSD <- Matrix::Matrix(1, Layers, Layers)
      for (i in 1:(Layers - 1)) {
        cat(".") # ADDED BY KG
        j <- i + 1
        if(sum(NodesTensor[[i]]) == 0 | sum(NodesTensor[[j]]) == 0){
          JSD[i,j] <- 0 # ADDED BY KG. "A higher value of JS divergence indicates greater dissimilarity between distributions, while a value closer to zero indicates greater similarity." So hmm maybe 1 is more appropriate here?
        }else{
          JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                  NodesTensor[[j]], SingleLayerEntropy[i], 
                                                  SingleLayerEntropy[j])
        }
      }
    }
    if (Type == "Categorical") {
      for (i in 1:(Layers - 1)) {
        cat(".") # ADDED BY KG
        for (j in (i + 1):Layers) {
          if(sum(NodesTensor[[i]]) == 0 | sum(NodesTensor[[j]]) == 0){
            JSD[i,j] <- 0 # ADDED BY KG. "A higher value of JS divergence indicates greater dissimilarity between distributions, while a value closer to zero indicates greater similarity." So hmm maybe 1 is more appropriate here?
          }else{
            JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                    NodesTensor[[j]], SingleLayerEntropy[i], 
                                                    SingleLayerEntropy[j])
          }
          JSD[j, i] <- JSD[i, j]
        }
      }
    }
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
    for (m in 1:(Layers - 1)) {
      if (MergeMatrix[m, 1] < 0) {
        A <- NodesTensor[[-MergeMatrix[m, 1]]]
        entropyA <- SingleLayerEntropy[-MergeMatrix[m, 
                                                    1]]
        SingleLayerEntropy[-MergeMatrix[m, 1]] <- 0
      }else {
        A <- MergedTensor[[MergeMatrix[m, 1]]]
        entropyA <- MergedEntropy[[MergeMatrix[m, 1]]]
        MergedEntropy[[MergeMatrix[m, 1]]] <- 0
      }
      if (MergeMatrix[m, 2] < 0) {
        B <- NodesTensor[[-MergeMatrix[m, 2]]]
        entropyB <- SingleLayerEntropy[-MergeMatrix[m, 
                                                    2]]
        SingleLayerEntropy[-MergeMatrix[m, 2]] <- 0
      }else {
        B <- MergedTensor[[MergeMatrix[m, 2]]]
        entropyB <- MergedEntropy[[MergeMatrix[m, 2]]]
        MergedEntropy[[MergeMatrix[m, 2]]] <- 0
      }
      MergedTensor[[m]] <- A + B
      #XXX KG addition for debugging the case where they are all zero
      if(all(MergedTensor[[m]]@p == 0)){
        tmpLayerEntropy <- 0
      }else{
        tmpLayerEntropy <- GetRenyiEntropyFromAdjacencyMatrix(MergedTensor[[m]], 
                                                              1)
      }
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
} # a version of the function that prints progress with dots . at the slow JSD calculation part.

get_heatmap <- function(red, nlayers){
  layernames <- as.character(1:nlayers)
  mat_toplot <- provideDimnames(as.matrix(red$JSD), sep="", 
                                list(layernames, layernames))
  heat <- gplots::heatmap.2(mat_toplot, trace = "none", dendrogram = "row")
  return(heat)
}

get_reduc_curves_df <- function(red, timewindows, type, situ){
  outdf <- map2(red, timewindows, ~{
    df <- setNames(as.data.frame(.x$gQualityFunction), "ent") %>%
      dplyr::mutate(timewindow = .y, step = 1:nrow(.), type = type, situ = situ)
  }) %>% purrr::list_rbind() %>%
    dplyr::mutate(timestep = 1+(step-1)*timewindow) %>%
    dplyr::mutate(timewindow = factor(as.character(timewindow)))
  return(outdf)
}

get_reduc_curves_df_seasons <- function(red, type, situ){
  df <- setNames(as.data.frame(red$gQualityFunction), "ent") %>%
    dplyr::mutate(step = 1:nrow(.), type = type, situ = situ)
  return(df)
}


# Dyads -------------------------------------------------------------------

prep <- function(graphs, situation){
  g <- graphs %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = situation)
  return(g)
}

get_networks_dyads <- function(data){ # ugh this is really sloppy but i'm in a rush so i'm kicking the can down the road. Sorry future self :(
  tonetworks <- data %>%
    group_by(situ, period) %>%
    group_split() %>%
    purrr::map(., as.data.frame)
  gs <- purrr::map(tonetworks, ~igraph::graph_from_data_frame(.x, directed = FALSE))
  return(gs)
}

get_shuffled_reps_df <- function(graphs, reps){
  shuffled_reps <- vector(mode = "list", length = reps)
  for(i in 1:reps){
    shuffled_graphs <- map(graphs, ~{
      V(.x)$name <- sample(V(.x)$name)
      return(.x)
    })
    shuffled <- map(shuffled_graphs, 
                    ~igraph::as_data_frame(.x) %>%
                      mutate(rep = i)) %>% purrr::list_rbind()
    shuffled_reps[[i]] <- shuffled
    cat(".")
  }
  shuffled_reps_df <- purrr::list_rbind(shuffled_reps)
  return(shuffled_reps_df)
}

correct_dyad_id_order <- function(data){
  forward <- data %>%
    mutate(ID1 = from, ID2 = to)
  backward <- data %>%
    mutate(ID1 = to, ID2 = from)
  ordered <- bind_rows(forward, backward) %>%
    filter(ID1 < ID2) %>%
    ungroup() %>%
    select(ID1, ID2, weight, situ, period, rep) %>%
    distinct() %>%
    mutate(dyad = paste(ID1, ID2, sep = ", "))
  return(ordered)
}

get_lms <- function(data){
  for_lms <- data %>%
    ungroup() %>%
    group_split(dyad, situ)
  labels <- map(for_lms, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())
  
  lms_summ <- map(for_lms, ~{
    mod <- lm(weight ~ period, data = .x)
    summ <- broom::tidy(mod)
  }, .progress = T)
  
  labeled <- map2(lms_summ, labels, ~bind_cols(.y, .x)) %>% purrr::list_rbind()
  return(labeled)
}

get_lms_permuted <- function(data, workers){
  future::plan(future::multisession, workers = workers)
  for_lms <- data %>%
    ungroup() %>%
    group_split(dyad, situ, rep)
  lms_summ <- furrr::future_map(for_lms, ~{
    mod <- lm(weight ~ period, data = .x)
    summ <- broom::tidy(mod)
  }, .progress = T)
  
  lms_summ <- lms_summ %>% purrr::list_rbind(names_to = "n")
  
  labels <- data %>% select(ID1, ID2, dyad, situ, rep) %>%
    distinct() %>% mutate(n = 1:nrow(.))
  
  labeled <- left_join(labels, lms_summ, by = "n") %>% select(-n)
  return(labeled)
}

get_combined <- function(data, lm_obs_summ, lm_perm_summ){
  all_obs <- data %>%
    group_by(dyad, situ) %>%
    mutate(nperiods = length(unique(period))) %>%
    ungroup() %>%
    left_join(lms_obs_summ, 
              by = c("ID1", "ID2", "dyad", "situ"), 
              relationship = "many-to-many") %>%
    arrange(dyad, situ, period, term)
  
  combined <-  lms_perm_summ %>%
    left_join(all_obs, by = c("ID1", "ID2", "dyad", "situ", "term"), 
              suffix = c("", "_obs"), relationship = "many-to-many") %>%
    ungroup()
  return(combined)
}
