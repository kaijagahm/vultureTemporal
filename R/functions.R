# Functions for targets pipeline

read_from_path <- function(path){
  envir <- environment()
  data_name <- load(path, envir = envir)
  get(data_name)
}

get_flight_sris <- function(datalist, roostPolygons){
  flight_sri <- furrr::future_map(datalist, ~{
    library(sf)
    vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri")}, .progress = T)
  flight_sri_2 <- purrr::map(flight_sri, ~{
    .x %>% dplyr::filter(sri > 0 & !is.na(sri)) %>%
      dplyr::rename("weight" = "sri")
  })
  return(flight_sri_2)
}

get_feeding_sris <- function(datalist, roostPolygons){
  feeding_sri <- furrr::future_map(datalist, ~{
    library(sf)
    vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50,
                                  idCol = "Nili_id", return = "sri")}, .progress = T)
  feeding_sri_2 <- purrr::map(feeding_sri, ~{
    .x %>% dplyr::filter(sri > 0 & !is.na(sri)) %>%
      dplyr::rename("weight" = "sri")
  })
  return(feeding_sri_2)
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
  nodetensor <- purrr::map(graphs, igraph::as_adjacency_matrix)
  if(type == "Categorical"){
    layertensor <- Matrix::Matrix(1, nlayers, nlayers) - muxViz:::speye(nlayers)
  }else if(type == "Ordinal"){
    layertensor <- BuildLayersTensor(nlayers, 1, "ordinal")
  }
  
  m <- muxViz::BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(nodetensor, layertensor, nlayers, nnodes)
  red <- GetMultilayerReducibility(m, nlayers, nnodes, method, Type = type)
  return(red)
}

GetMultilayerReducibility <- function(SupraAdjacencyMatrix, Layers, Nodes, Method, Type) 
{
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
      if (SingleLayerEntropy[i] < 1e-12 & !is.na(SingleLayerEntropy[i])) 
        SingleLayerEntropy[i] <- 0
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
} # a version of the function that prints progress with dots . at the slow JSD calculation part.

get_heatmap <- function(red, nlayers){
  layernames <- as.character(1:nlayers)
  mat_toplot <- provideDimnames(as.matrix(red$JSD), sep="", 
                                           list(layernames, layernames))
  heat <- gplots::heatmap.2(mat_toplot, trace = "none", dendrogram = "row")
  return(heat)
}
