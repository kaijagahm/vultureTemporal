library(tidyverse)
library(vultureUtils)
library(igraph)
library(sf)
library(tidygraph)
library(ggraph)
library(zoo)
library(here)

# Repeatability across seasons --------------------------------------------
load("data/mixedModels/linked.Rda")
source(here("scripts/temporal_funs.R"))
future::plan(future::multisession(), workers = 10)

# Repeatability plots -----------------------------------------------------
# PREDICTORS
## note that for these, the situation doesn't matter (defaults to flight) because the predictors are just repeated over and over for each season
plotRepeat(linked, "movement", ylab = "Movement")
plotRepeat(linked, "space_use", ylab = "Space use (log-transformed)")
plotRepeat(linked, "roost_div", ylab = "Roost diversification")

# SOCIAL METRICS
plotRepeat(linked, "normDegree", situation = "flight", ylab = "Normalized degree (flight)")
plotRepeat(linked, "normDegree", situation = "feeding", ylab = "Normalized degree (feeding)")
plotRepeat(linked, "normDegree", situation = "roosting", ylab = "Normalized degree (roosting)")
plotRepeat(linked, "normStrength", situation = "flight", ylab = "Normalized strength (flight)")
plotRepeat(linked, "normStrength", situation = "feeding", ylab = "Normalized strength (feeding)")
plotRepeat(linked, "normStrength", situation = "roosting", ylab = "Normalized strength (roosting)")

# Just by looking at these colors, I would say that the measures (both predictors and responses) are somewhat repeatable, but not strongly.