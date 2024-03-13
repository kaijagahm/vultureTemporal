# Break the temporal down into smaller pieces
library(tidyverse)
library(vultureUtils)
library(igraph)
library(sf)
library(tidygraph)
library(ggraph)
library(here)

#load("data/fromMvmtSoc/downsampled_10min_forSocial.Rda") # this is the last dataset we produced in the dataPrep script before moving on to the further cleaning for movement, so this is the one we're going to use for the social interactions.
# sfdata <- purrr::map(downsampled_10min_forSocial, ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F))
# sfdata <- map(sfdata, ~.x %>% select(-any_of(cols_to_remove)))
# save(sfdata, file = "data/sfdata.Rda")
whichseason <- 9
# testseason <- sfdata[[whichseason]]
# save(testseason, file = "data/testseason.Rda")
# testseason_roosts <- roosts[[whichseason]]
# save(testseason_roosts, file = "data/testseason_roosts.Rda")
# rm(downsampled_10min_forSocial)
gc()

load("data/testseason.Rda")
load("data/testseason_roosts.Rda")
roostPolygons <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
load("data/fromMvmtSoc/season_names.Rda")
load("data/fromMvmtSoc/roosts.Rda") # XXX load
load("data/fromMvmtSoc/cc.Rda")

# Create the intervals and breaks for cutting
ndays <- c(1, 2, 5, 10, 20, 40)
min <- min(testseason$dateOnly)
max <- max(testseason$dateOnly)
breaks <- map(ndays, ~seq(from = as.POSIXct(min), to = as.POSIXct(max), by = paste(.x, "day", sep = " ")))

# Set up variables for cutting --------------------------------------------
# XXX start here 3/13/24

annotated <- map(breaks, ~testseason %>%
                   mutate(int = cut(dateOnly, breaks = as.Date(.x), include.lowest = T))) #4.8 GB
