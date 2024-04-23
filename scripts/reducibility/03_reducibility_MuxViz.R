#### EXAMPLE SCRIPT TO USE MUXVIZ  ####
# Based on the script sent by Sandra

## Building an X-layer temporal multiplex (where x is the number of slices)
## Getting some metrics
## Running a reducibility analysis
library(tidyverse)
library(igraph)
library(muxViz)
library(ggraph)
library(ggplot2)
library(RColorBrewer)
library(rgl)
library(here)

# Data prep ---------------------------------------------------------------
load("data/data_cut.Rda")
roostPolygons <- sf::st_read(here::here("data/raw/roosts50_kde95_cutOffRegion.kml"))
data_1day <- data_cut[[1]]
rm(data_cut)
future::plan(future::multisession, workers = 10)

flight_sri <- furrr::future_map(data_1day, ~{
  library(sf)
  vultureUtils::getFlightEdges(.x, roostPolygons = roostPolygons, distThreshold = 1000, idCol = "Nili_id", return = "sri")}, .progress = T)
flight_sri_2 <- map(flight_sri, ~{
  .x %>% filter(sri > 0 & !is.na(sri)) %>%
    rename("weight" = "sri")
})

feeding_sri <- furrr::future_map(data_1day, ~{
  library(sf)
  vultureUtils::getFeedingEdges(.x, roostPolygons = roostPolygons, distThreshold = 50,
                                idCol = "Nili_id", return = "sri")}, .progress = T)
feeding_sri_2 <- map(feeding_sri, ~{
  .x %>% filter(sri > 0 & !is.na(sri)) %>%
    rename("weight" = "sri")
})

allvertices <- unique(purrr::list_rbind(data_1day)$Nili_id)
nnodes_flight <- length(allvertices)
nnodes_feeding <- length(allvertices)

graphs_flight <- map(flight_sri_2, ~{
  graph_from_data_frame(.x, directed = F, vertices = allvertices)
})
graphs_feeding <- map(feeding_sri_2, ~{
  graph_from_data_frame(.x, directed = F, vertices = allvertices)
})

# reading files
load("data/timewindows.Rda") # different numbers of days that we're testing
length(timewindows)

# Prepare for tensors -----------------------------------------------------
nlayers <- 10
g_list_flight <- graphs_flight[1:nlayers] # for starters, let's just take the first 100 days.
g_list_feeding <- graphs_feeding[1:nlayers]
node_tensor_flight <- purrr::map(g_list_flight, ~as_adjacency_matrix(.x, attr = "weight"))
node_tensor_feeding <- purrr::map(g_list_feeding, ~as_adjacency_matrix(.x, attr = "weight"))

## The next lines define interlayer links, in this case, for a temporal multiplex
## which is a type of ordinal network where only adjacent layers are connected and
## links run in only one direction. For behavioral multiplex, interlayer
## links need to be categorical, not ordinal.

# layer_tensor_ord <- diagR(c(1,1), nlayers, 1) + diagR(c(1,1), nlayers, -1) 
# layer_tensor_ord # example matrix of interlayer links for ordinal multiplex
layer_tensor <- diagR(c(1,1), nlayers, 1)
layer_tensor # matrix of interlayer links for temporal multiplex # only adjacent layer to the next layer, not to the previous layer (i.e. directed)

## M_OR will become the multiplex
M_OR_flight <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(node_tensor_flight, layer_tensor, nlayers, nnodes_flight) # so, we are using layer_tensor here, which should have only one-way edges. But it seems to still be aggregating layers that are non-adjacent.
M_OR_feeding <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(node_tensor_feeding, layer_tensor, nlayers, nnodes_feeding) # so, we are using layer_tensor here, which should have only one-way edges. But it seems to still be aggregating layers that are non-adjacent.

## Measuring multiplex centralities

# MultiDegree
deg_flight <- GetMultiDegree(M_OR_flight, nlayers, nnodes_flight, F)
deg_feeding <- GetMultiDegree(M_OR_feeding, nlayers, nnodes_feeding, F)
# PageRank versatility
# pr <- GetMultiPageRankCentrality(M_OR, nlayers, nnodes) # this is computationally intractable even with 100 layers
# Strength versatility
str_flight <- GetMultiStrength(M_OR_flight, nlayers, nnodes_flight, isDirected = F)
str_feeding <- GetMultiStrength(M_OR_feeding, nlayers, nnodes_feeding, isDirected = F)
# Eigenvector versatility
# eig <- GetMultiEigenvectorCentrality(M_OR, nlayers, nnodes) # Also takes too long with 100 layers.
# Closeness versatility
clo_flight <- GetMultiClosenessCentrality(M_OR_flight, nlayers, nnodes_flight)$closeness
clo_feeding <- GetMultiClosenessCentrality(M_OR_feeding, nlayers, nnodes_feeding)$closeness

## Generate the aggregate network
AGG_OR_flight <- GetAggregateNetworkFromSupraAdjacencyMatrix(M_OR_flight, nlayers, nnodes_flight)
AGG_OR_feeding <- GetAggregateNetworkFromSupraAdjacencyMatrix(M_OR_feeding, nlayers, nnodes_feeding)

# Reducibility ------------------------------------------------------------
#### REDUCIBILITY ####

# Use Ward linkage for hierarchical clustering and Categorical network of layers
Method <- "ward.D2"

NodeTensor_flight <- list()
NodeTensor_feeding <- list()

# Define the layers by their adjacency matrices
for (l in 1:nlayers) {
  NodeTensor_flight[[l]] <- as_adjacency_matrix(g_list_flight[[l]])
  NodeTensor_feeding[[l]] <- as_adjacency_matrix(g_list_feeding[[l]])
}

# Define how layers are interconnected
LayerTensor <- Matrix::Matrix(1, nlayers, nlayers) - muxViz:::speye(nlayers) #in this case categorical
# Ah, but here we have a layer tensor that has edges in both directions.
# What if we try the one from before, which only has edges in one direction?
layer_tensor

# LayerTensor <- BuildLayersTensor(Layers, 1, "ordinal")

# Categorical multiplex
M_flight <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor_flight, LayerTensor, nlayers, nnodes_flight)
M_feeding <- BuildSupraAdjacencyMatrixFromEdgeColoredMatrices(NodeTensor_feeding, LayerTensor, nlayers, nnodes_feeding)

# Reducibility considering a categorical multiplex
struct.red.cat_flight <- GetMultilayerReducibility(M_flight, nlayers, nnodes_flight, Method, Type="Categorical")
struct.red.cat_feeding <- GetMultilayerReducibility(M_feeding, nlayers, nnodes_feeding, Method, Type="Categorical")
# Reducibility considering an ordinal multiplex
struct.red.ord_flight <- GetMultilayerReducibility(M_flight, nlayers, nnodes_flight, Method, Type="Ordinal")
struct.red.ord_feeding <- GetMultilayerReducibility(M_feeding, nlayers, nnodes_feeding, Method, Type="Ordinal")
# XXX missing values in the co-feeding matrix--need to check this out. In the meantime, just do flight.

# Reducibility heatmap (categorical)
layernames <- as.character(1:nlayers)
mat_toplot_cat_flight <- provideDimnames(as.matrix(struct.red.cat_flight$JSD), sep="", 
                              list(layernames, layernames))
# mat_toplot_cat_feeding <- provideDimnames(as.matrix(struct.red.cat_feeding$JSD), sep="", 
#                                          list(layernames, layernames))
gplots::heatmap.2(mat_toplot_cat_flight, trace = "none", dendrogram = "row") 
# gplots::heatmap.2(mat_toplot_cat_feeding, trace = "none", dendrogram = "row")
# interestingly, we see that the most similar layers are not necessarily the ones that are closest together in time.

# Reducibility heatmap (ordinal)
mat_toplot_ord_flight <- provideDimnames(as.matrix(struct.red.ord_flight$JSD), sep="", 
                                  list(layernames, layernames))
# mat_toplot_ord_feeding <- provideDimnames(as.matrix(struct.red.ord_feeding$JSD), sep="", 
#                                          list(layernames, layernames))
gplots::heatmap.2(mat_toplot_ord_flight, trace = "none") 
# gplots::heatmap.2(mat_toplot_ord_feeding, trace = "none") # theoretically, this one should only group adjacent layers together. So I don't understand the output of the dendrogram...

## Plotting the reducibility curves
df.quality.cat_flight <- data.frame(step = 0:(length(struct.red.cat_flight$relgQualityFunction) - 1),
             q = struct.red.cat_flight$relgQualityFunction)
# df.quality.cat_feeding <- data.frame(step = 0:(length(struct.red.cat_feeding$relgQualityFunction) - 1),
#                                     q = struct.red.cat_feeding$relgQualityFunction)

df.quality.ord_flight <- data.frame(step = 0:(length(struct.red.ord_flight$relgQualityFunction) - 1),
                             q = struct.red.ord_flight$relgQualityFunction)
# df.quality.ord_feeding <- data.frame(step = 0:(length(struct.red.ord_feeding$relgQualityFunction) - 1),
#                              q = struct.red.ord_feeding$relgQualityFunction)

h.cat_flight <- ggplot(df.quality.cat_flight, aes(step, q)) + theme_bw() +
  geom_line(color = "steelblue") + geom_point(color = "steelblue") +
  xlab("Merging Step, m") + ylab("Quality function, q(m)")

h.ord_flight <- ggplot(df.quality.ord_flight, aes(step, q)) + theme_bw() +
  geom_line(color = "steelblue") + geom_point(color = "steelblue") +
  xlab("Merging Step, m") + ylab("Quality function, q(m)")

print(h.cat_flight)
print(h.ord_flight) # wait whaaaat?? At 100 time slices, something different is going on and we actually do see a peak.

# Oh, maybe what's going on is that the heatmap/dendrogram isn't actually demonstrating which layers were aggregated, just which ones are most similar? But... you would expect the aggregation to follow the similarity... so I'm not sure what's going on.
# At this point I could try to dig deeper into the code, but I'm not going to.
# Something to note is that I am including all the individuals here, even the ones that were not observed in a given day's network. They just show up as not being connected. So, this doesn't do a good job of representing uncertainty. Interesting.

# Examining which layers are most similar ---------------------------------
# As in, if we allow the algorithm to group any of them together, which ones come out as similar? 
matlong <- as.data.frame(mat_toplot_cat_flight) %>% mutate(layer1 = 1:nrow(.)) %>% pivot_longer(cols = -layer1, names_to = "layer2", values_to = "sim") %>% mutate(type = "cat") %>%
  mutate(across(contains("layer"), as.numeric)) %>%
  filter(layer1 < layer2) %>%
  mutate(diff = layer2-layer1)

matlong %>% ggplot(aes(x = diff, y = sim))+
  geom_point()+
  theme_classic()+
  geom_smooth()

