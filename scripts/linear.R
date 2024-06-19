library(tidyverse)
library(targets)
library(igraph)

tar_load(feeding_sris)
tar_load(flight_sris)
tar_load(roost_sris)
tar_load(timewindows)


fifteen <- combined %>%
  filter(nperiods >= 15)
fifteen_static <- combined_static %>%
  filter(nperiods >= 15)

#### 1.
sigslopes_nonrandom <- fifteen %>% # at least 15 periods
  filter(p.value_obs < 0.05, term == "period") %>% # significant slopes
  group_by(dyad, situ) %>%
  mutate(prop_obs_greater = sum(estimate_obs > estimate)/n(),
         prop_obs_less = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025) # slope significantly different from random

sigslopes_nonrandom_static <- fifteen_static %>%
  filter(p.value_obs <= 0.05, term == "period") %>%
  group_by(dyad, situ) %>%
  mutate(prop_obs_greater = sum(estimate_obs > estimate)/n(),
         prop_obs_less = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025)

#### 2.
friendsenemies <- fifteen %>% # at least 15 periods
  filter(p.value_obs >= 0.05, term == "(Intercept)") %>% # non-significant slopes
  group_by(dyad, situ) %>%
  mutate(prop_obs_greater = sum(estimate_obs > estimate)/n(),
         prop_obs_less = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025) # intercept significantly different from random

friends <- friendsenemies %>%
  filter(prop_obs_greater > prop_obs_less)

enemies <- friendsenemies %>%
  filter(prop_obs_less > prop_obs_greater)
  
friendsenemies_static <- fifteen_static %>% # at least 15 periods
  filter(p.value_obs >= 0.05, term == "(Intercept)") %>% # non-significant slopes
  group_by(dyad, situ) %>%
  mutate(prop_obs_greater = sum(estimate_obs > estimate)/n(),
         prop_obs_less = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025) # intercept significantly different from random

friends_static <- friendsenemies_static %>%
  filter(prop_obs_greater > prop_obs_less)

enemies_static <- friendsenemies_static %>%
  filter(prop_obs_less > prop_obs_greater)

length(unique(friends$dyad))
length(unique(friends_static$dyad))
length(unique(enemies$dyad))
length(unique(enemies_static$dyad))
# these are really weird numbers! What gives??
