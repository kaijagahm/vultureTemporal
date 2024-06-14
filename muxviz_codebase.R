GetEigenvaluesOfDensityMatrix <- function (DensityMatrix) 
{
  Eigenvalues <- cbind(eigen(DensityMatrix)$values)
  if (abs(sum(Eigenvalues) - 1) > 1e-08) {
    stop("ERROR! Eigenvalues dont sum to 1! Aborting process.")
  }
  return(Eigenvalues)
}

GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix <- function (AdjacencyMatrix) 
{
  DensityMatrix <- BuildDensityMatrixBGS(AdjacencyMatrix)
  return(GetEigenvaluesOfDensityMatrix(DensityMatrix))
}

GetRenyiEntropyFromAdjacencyMatrix <- function (AdjacencyMatrix, Q = 1) 
{
  Eigenvalues <- GetEigenvaluesOfDensityMatrixFromAdjacencyMatrix(AdjacencyMatrix)
  if (Q == 1) {
    RenyiEntropy <- -sum(Eigenvalues[Eigenvalues > 0] * log(Eigenvalues[Eigenvalues > 
                                                                          0]))
  }
  else {
    RenyiEntropy <- (1 - sum(Eigenvalues[Eigenvalues > 0]^Q))/(Q - 
                                                                 1)
  }
  return(RenyiEntropy)
}

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
    JSD <- Matrix::Matrix(0, Layers, Layers)
    if (Type == "Ordinal") {
      JSD <- Matrix::Matrix(1, Layers, Layers)
      for (i in 1:(Layers - 1)) {
        j <- i + 1
        JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                NodesTensor[[j]], SingleLayerEntropy[i], SingleLayerEntropy[j])
      }
    }
    if (Type == "Categorical") {
      for (i in 1:(Layers - 1)) {
        for (j in (i + 1):Layers) {
          JSD[i, j] <- GetJensenShannonDivergence(NodesTensor[[i]], 
                                                  NodesTensor[[j]], SingleLayerEntropy[i], 
                                                  SingleLayerEntropy[j])
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
}
<bytecode: 0x000002366cc9a180>
  <environment: namespace:muxViz>