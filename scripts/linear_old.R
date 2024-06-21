library(tidyverse)
library(targets)
library(igraph)
library(here)

tar_load(feeding_sris)
tar_load(flight_sris)
tar_load(roost_sris)
tar_load(timewindows)

# # Data prep ------------------------------------------------------------------
# # Prepare the data for 5-day windows.
# prep <- function(graphs, situation){
#   g <- graphs %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = situation)
#   return(g)
# }
# fe <- prep(feeding_sris[[2]], "feeding")
# fl <- prep(flight_sris[[2]], "flight")
# ro <- prep(roost_sris[[2]], "roosting")
# 
# all <- list_rbind(list(fe, fl, ro)) %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
# all(all$ID1 < all$ID2) # should be no repeat dyads or self dyads
# length(unique(all$dyad)) # 
# dyads <- unique(all$dyad) #3655 dyads still
# 
# minimal <- all %>%
#   ungroup() %>%
#   select(ID1, ID2, weight, situ, period)
# 
# # Create stationary networks ----------------------------------------------
# ## Will use this to determine how many significant slopes we would expect to get just by chance
# ### First, get the distributions we'll use to generate the networks
# sri_dist_fl <- minimal %>% filter(situ == "flight") %>% pull(weight)
# sri_dist_fe <- minimal %>% filter(situ == "feeding") %>% pull(weight)
# sri_dist_ro <- minimal %>% filter(situ == "roosting") %>% pull(weight)
# static_fl <- minimal %>% filter(situ == "flight") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_fl))
# static_fe <- minimal %>% filter(situ == "feeding") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_fe))
# static_ro <- minimal %>% filter(situ == "roosting") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_ro))
# 
# static <- bind_rows(static_fl, static_fe, static_ro)
# 
# # Permutations ------------------------------------------------------------
# # In order to figure out the trend, need to permute the node identities separately on each of the networks, and then re-calculate the trends, pulling out the slope and p-value and various other information, and then compare that to the observed slopes.
# 
# tonetworks <- minimal %>%
#   group_by(situ, period) %>%
#   group_split() %>% map(., as.data.frame)
# gs <- map(tonetworks, ~igraph::graph_from_data_frame(.x, directed = FALSE))
# 
# tonetworks_static <- static %>%
#   group_by(situ, period) %>%
#   group_split() %>% map(., as.data.frame)
# gs_static <- map(tonetworks_static, ~igraph::graph_from_data_frame(.x, directed = FALSE))
# 
# reps <- 100
# shuffled_reps <- vector(mode = "list", length = reps)
# shuffled_reps_static <- vector(mode = "list", length = reps)
# for(i in 1:reps){
#   shuffled_graphs <- map(gs, ~{
#     V(.x)$name <- sample(V(.x)$name)
#     return(.x)
#   })
#   shuffled_graphs_static <- map(gs_static, ~{
#     V(.x)$name <- sample(V(.x)$name)
#     return(.x)
#   })
#   shuffled <- map(shuffled_graphs, 
#                   ~igraph::as_data_frame(.x) %>%
#                     mutate(rep = i)) %>% purrr::list_rbind()
#   shuffled_static <- map(shuffled_graphs_static,
#                          ~igraph::as_data_frame(.x) %>%
#                            mutate(rep = i)) %>% purrr::list_rbind()
#   shuffled_reps[[i]] <- shuffled
#   shuffled_reps_static[[i]] <- shuffled_static
#   cat(".")
# }
# 
# shuffled_reps_df <- purrr::list_rbind(shuffled_reps)
# shuffled_reps_static_df <- purrr::list_rbind(shuffled_reps_static)
# # Because of the shuffling, the dyads may now be in the wrong order. Let's get ID1 and ID2 to be correct (ID1 < ID2).
# forward <- shuffled_reps_df %>%
#   mutate(ID1 = from, ID2 = to)
# backward <- shuffled_reps_df %>%
#   mutate(ID1 = to, ID2 = from)
# ordered <- bind_rows(forward, backward) %>%
#   filter(ID1 < ID2)
# rm(forward, backward)
# 
# forward <- shuffled_reps_static_df %>%
#   mutate(ID1 = from, ID2 = to)
# backward <- shuffled_reps_static_df %>%
#   mutate(ID1 = to, ID2 = from)
# ordered_static <- bind_rows(forward, backward) %>%
#   filter(ID1 < ID2)
# rm(forward, backward)
# 
# # Filter out any duplicates or self edges
# replicates <- ordered %>%
#   ungroup() %>%
#   select(ID1, ID2, weight, situ, period, rep) %>%
#   distinct() %>%
#   mutate(dyad = paste(ID1, ID2, sep = ", "))
# replicates_static <- ordered_static %>%
#   ungroup() %>%
#   select(ID1, ID2, weight, situ, period, rep) %>%
#   distinct() %>%
#   mutate(dyad = paste(ID1, ID2, sep = ", "))
# minimal <- minimal %>%
#   mutate(dyad = paste(ID1, ID2, sep = ", "))
# static <- static %>%
#   mutate(dyad = paste(ID1, ID2, sep = ", "))
#
# save(replicates, file = here("data/replicates.Rda"))
# save(replicates_static, file = here("data/replicates_static.Rda"))
# save(minimal, file = here("data/minimal.Rda"))
# save(static, file = here("data/static.Rda"))

load(here("data/replicates.Rda"))
load(here("data/replicates_static.Rda"))
load(here("data/minimal.Rda"))
load(here("data/static.Rda"))

# all(minimal$dyad %in% replicates$dyad)
# all(static$dyad %in% replicates_static$dyad)
# all(replicates$dyad %in% minimal$dyad)
# all(replicates_static$dyad %in% static$dyad)
# length(unique(replicates$dyad))
# length(unique(replicates_static$dyad))
# length(unique(minimal$dyad))
# length(unique(static$dyad))
# 
# ## Now we're set up to run regressions
# # Calculate linear models for observed data
# obs_for_lms <- minimal %>%
#   group_split(dyad, situ)
# obs_for_lms_static <- static %>%
#   group_split(dyad, situ)
# 
# lms_obs_summ <- map(obs_for_lms, ~{
#   mod <- lm(weight ~ period, data = .x)
#   summ <- broom::tidy(mod)
# }, .progress = T)
# 
# lms_obs_summ_static <- map(obs_for_lms_static, ~{
#   mod <- lm(weight ~ period, data = .x)
#   summ <- broom::tidy(mod)
# }, .progress = T)
# 
# obs_labels <- map(obs_for_lms, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())
# obs_labels_static <- map(obs_for_lms_static, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())
# 
# lms_obs_summ_labeled <- map2(lms_obs_summ, obs_labels, ~bind_cols(.y, .x)) %>% purrr::list_rbind()
# lms_obs_summ_labeled_static <- map2(lms_obs_summ_static, obs_labels_static, ~bind_cols(.y, .x)) %>% purrr::list_rbind()

# save(lms_obs_summ_labeled, file = here("data/lms_obs_summ_labeled.Rda"))
# save(lms_obs_summ_labeled_static, file = here("data/lms_obs_summ_labeled_static.Rda"))

load(here("data/lms_obs_summ_labeled.Rda"))
load(here("data/lms_obs_summ_labeled_static.Rda"))

# # Calculate linear models for permuted data
# future::plan(future::multisession, workers = 6)
# perm_for_lms <- replicates %>%
#   group_split(dyad, situ, rep)
# perm_for_lms_static <- replicates_static %>%
#   group_split(dyad, situ, rep)
# lms_perm_summ <- furrr::future_map(perm_for_lms, ~{
#   mod <- lm(weight ~ period, data = .x)
#   summ <- broom::tidy(mod)
# }, .progress = T)
# lms_perm_summ_static <- furrr::future_map(perm_for_lms_static, ~{
#   mod <- lm(weight ~ period, data = .x)
#   summ <- broom::tidy(mod)
# }, .progress = T)
# 
# perm_labels <- replicates %>% select(ID1, ID2, dyad, situ, rep) %>%
#   distinct() %>% mutate(n = 1:nrow(.))
# perm_labels_static <- replicates_static %>% select(ID1, ID2, dyad, situ, rep) %>%
#   distinct() %>% mutate(n = 1:nrow(.))
# 
# lms_perm_summ <- lms_perm_summ %>% purrr::list_rbind(names_to = "n")
# lms_perm_summ_static <- lms_perm_summ_static %>% purrr::list_rbind(names_to = "n")
# 
# lms_perm_summ_labeled <- left_join(perm_labels, lms_perm_summ, by = "n") %>% select(-n)
# lms_perm_summ_labeled_static <- left_join(perm_labels_static, lms_perm_summ_static, by = "n") %>% select(-n)

# save(lms_perm_summ_labeled, file = here("data/lms_perm_summ_labeled.Rda"))
# save(lms_perm_summ_labeled_static, file = here("data/lms_perm_summ_labeled_static.Rda"))

load(here("data/lms_perm_summ_labeled.Rda"))
load(here("data/lms_perm_summ_labeled_static.Rda"))

## Okay, now we can compare the regression results!
# Let's pick a random dyad: "erasmus, scout"
test_obs <- lms_obs_summ_labeled %>% filter(dyad == "erasmus, scout")
test_perm <- lms_perm_summ_labeled %>% filter(dyad == "erasmus, scout")

test_perm %>%
  filter(term == "period") %>%
  ggplot(aes(x = estimate, col = situ))+
  geom_density()+
  theme_minimal()+
  geom_vline(data = test_obs %>% filter(term == "period"), 
             aes(xintercept = estimate, col = situ)) # okay, so for feeding and roosting, the observed is a bit less than what would be expected by chance, but not for flight. And it's not significant for any of them.

# What about for one where we know the observed looks like it has a significant slope? Estavan and Nevada for roosting, for instance.
test_obs <- lms_obs_summ_labeled %>% filter(dyad == "estavan, nevada")
test_perm <- lms_perm_summ_labeled %>% filter(dyad == "estavan, nevada")

test_perm %>%
  filter(term == "period") %>%
  ggplot(aes(x = estimate, col = situ))+
  geom_density()+
  theme_minimal()+
  geom_vline(data = test_obs %>% filter(term == "period"), 
             aes(xintercept = estimate, col = situ)) # nice! so the estimate is significantly different than expected by chance; these individuals actually have a positive trend in their co-roosting frequency over the course of the season, and that's a non-random positive trend.

## Add number of periods
all_minimal <- minimal %>%
  group_by(dyad, situ) %>%
  mutate(nperiods = length(unique(period))) %>%
  ungroup() %>%
  left_join(lms_obs_summ_labeled, by = c("ID1", "ID2", "dyad", "situ"), relationship = "many-to-many") %>%
  arrange(dyad, situ, period, term)

all_static <- static %>%
  group_by(dyad, situ) %>%
  mutate(nperiods = length(unique(period))) %>%
  ungroup() %>%
  left_join(lms_obs_summ_labeled_static, by = c("ID1", "ID2", "dyad", "situ"), relationship = "many-to-many") %>%
  arrange(dyad, situ, period, term)

### oops, gotta calculate the differences from random for the slope estimate
combined <- lms_perm_summ_labeled %>%
  left_join(all_minimal, by = c("ID1", "ID2", "dyad", "situ", "term"), suffix = c("", "_obs"), relationship = "many-to-many") %>%
  ungroup()

combined_static <- lms_perm_summ_labeled_static %>%
  left_join(all_static, by = c("ID1", "ID2", "dyad", "situ", "term"), suffix = c("", "_obs"), relationship = "many-to-many") %>%
  ungroup()

## Now let's look at two different cases:
### 1. significant slopes whose slopes are significantly different from random (at least 15 periods of data)
### 2. non-significant slopes (i.e. flat lines) whose intercepts are significantly different from random (at least 15 periods of data)

fifteen <- combined %>%
  filter(nperiods >= 15) %>%
  group_by(dyad, situ, term) %>%
  mutate(pog = sum(estimate_obs > estimate)/n(),
         pol = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  mutate(nonrandom = case_when(pog <= 0.025 | pol <= 0.025 ~ T, .default = F))
fifteen_static <- combined_static %>%
  filter(nperiods >= 15) %>%
  group_by(dyad, situ, term) %>%
  mutate(pog = sum(estimate_obs > estimate)/n(),
         pol = sum(estimate_obs < estimate)/n()) %>%
  ungroup() %>%
  mutate(nonrandom = case_when(pog <= 0.025 | pol <= 0.025 ~ T, .default = F))

#### 1.
sigslopes_nonrandom <- fifteen %>% 
  filter(term == "period", nonrandom, p.value_obs < 0.05)

sigslopes_nonrandom_static <- fifteen_static %>% 
  filter(term == "period", nonrandom, p.value_obs < 0.05)

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

# Let's quantify some example trends ------------------------------------
minimal <- minimal %>%
  group_by(situ) %>%
  mutate(weight_rel = weight/max(weight))
## choose a random dyad/situ from sigslopes_nonrandom
set.seed(123)
info <- sigslopes_nonrandom %>%
  sample_n(1) %>%
  select(ID1, ID2, situ)

## any other significant slopes for this one?
allforpair_info <- info %>%
  select(ID1, ID2) %>%
  left_join(sigslopes_nonrandom) %>%
  select(ID1, ID2, situ) %>%
  distinct()
dim(test)
head(test)

toplot <- allforpair_info %>%
  select(ID1, ID2) %>%
  left_join(minimal) %>%
  mutate(sig = case_when(situ %in% allforpair_info$situ ~ T, 
                         .default = F)) 
toplot %>%
  ggplot(aes(x = period, y = weight_rel, col = situ, linetype = sig))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_linetype_manual(values = c(5, 1))+
  theme_classic()
