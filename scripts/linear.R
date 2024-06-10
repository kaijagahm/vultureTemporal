library(tidyverse)
library(targets)
library(igraph)

tar_load(feeding_sris)
tar_load(flight_sris)
tar_load(roost_sris)
tar_load(timewindows)

# Some thoughts:
# Will need to pick a time window when there are fewer gaps. Daily will result in lots of fluctuations, necessarily.
# How do we determine which pairs to look at? Don't want to exclude pairs that e.g. had a strong relationship at the beginning and then petered out. Maybe need a certain number of days/periods over the whole season? 

# Data prep ------------------------------------------------------------------
# Prepare the data for 5-day windows.
prep <- function(graphs, situation){
  g <- graphs %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = situation)
  return(g)
}
fe <- prep(feeding_sris[[2]], "feeding")
fl <- prep(flight_sris[[2]], "flight")
ro <- prep(roost_sris[[2]], "roosting")

all <- list_rbind(list(fe, fl, ro)) %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
all(all$ID1 < all$ID2) # should be no repeat dyads or self dyads
length(unique(all$dyad)) # 
dyads <- unique(all$dyad) #3655 dyads still

all <- all %>%
  group_by(dyad, situ) %>%
  mutate(n_dyad_situ = n(),
         n_dyad_situ_nonzero = sum(weight > 0)) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(n_dyad = n(),
         n_dyad_nonzero = sum(weight > 0))

# Exploration -------------------------------------------------------------
# If we look at 5-day windows, how many will we have?
max(all$period) # 25 periods. Okay, let's see a histogram of how many dyads have information for how many periods
all %>%
  ggplot(aes(x = n_dyad_situ))+
  geom_histogram(breaks = 1:25)+
  facet_wrap(~situ, scales = "free")+
  theme_minimal() # okay, so for flight and roosting we don't have very many periods with no data for one or both individuals, mostly good coverage. Feeding happens much less frequently, so naturally we have more dyads that don't interact in a co-feeding context during every 5-day period.

# Let's select dyads that have valid interactions for at least 10 out of 25 5-day periods? (Arbitrary choice)

frequent <- all %>%
  filter(n_dyad_situ >= 10)
length(unique(frequent$dyad)) # that still leaves the vast majority of dyads represented with at least one social situation, which is to be expected, since e.g. almost all of them will have values for roosting and flight.

random_dyads <- sample(unique(frequent$dyad), 10) # visualize 10 random dyads
frequent %>%
  filter(dyad %in% random_dyads) %>%
  ggplot(aes(x = period, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad, scales = "free") # okay, again, this emphasizes that we can't tell much from the ones that almost never have a nonzero sri. We need individuals that actually interact for at least 10 periods.

frequent_interactions <- all %>%
  filter(n_dyad_situ_nonzero >= 10)
length(unique(frequent_interactions$dyad)) # still leaves 648 dyads, a decent sample size!

random_dyads_freq <- sample(unique(frequent_interactions$dyad), 10)
frequent_interactions %>%
  filter(dyad %in% random_dyads_freq) %>%
  ggplot(aes(x = period, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad, scales = "free") # now this is exciting!! We actually see some decent trends, especially in roosting, once you aggregate over enough nights to be able to tell. Arguably we could do this over 10 nights as well, but then there are only 10 periods...

# Now what do we do with this information?
# Figure out how many of them have significant slopes, and which ones
# Follow Hobson et al for suggestions of what to do next
# Ask what individual characteristics predict the trends over time
# Do the Hobson stability analyses, to see if relationships stabilize over the course of the season (there's no reason to believe that they should...)

# Create stationary networks ----------------------------------------------
## Will use this to determine how many significant slopes we would expect to get just by chance
### First, get the distributions we'll use to generate the networks
sri_dist_fl <- minimal %>% filter(situ == "flight") %>% pull(weight)
sri_dist_fe <- minimal %>% filter(situ == "feeding") %>% pull(weight)
sri_dist_ro <- minimal %>% filter(situ == "roosting") %>% pull(weight)
static_fl <- minimal %>% filter(situ == "flight") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_fl))
static_fe <- minimal %>% filter(situ == "feeding") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_fe))
static_ro <- minimal %>% filter(situ == "roosting") %>% select(ID1, ID2, situ, period) %>% mutate(weight = sample(sri_dist_ro))

static <- bind_rows(static_fl, static_fe, static_ro)

# Permutations ------------------------------------------------------------
# In order to figure out the trend, need to permute the node identities separately on each of the networks, and then re-calculate the trends, pulling out the slope and p-value and various other information, and then compare that to the observed slopes.

minimal <- all %>%
  ungroup() %>%
  select(ID1, ID2, weight, situ, period)

tonetworks <- minimal %>%
  group_by(situ, period) %>%
  group_split() %>% map(., as.data.frame)
gs <- map(tonetworks, ~igraph::graph_from_data_frame(.x, directed = FALSE))

tonetworks_static <- static %>%
  group_by(situ, period) %>%
  group_split() %>% map(., as.data.frame)
gs_static <- map(tonetworks_static, ~igraph::graph_from_data_frame(.x, directed = FALSE))

reps <- 100
shuffled_reps <- vector(mode = "list", length = reps)
shuffled_reps_static <- vector(mode = "list", length = reps)
for(i in 1:reps){
  shuffled_graphs <- map(gs, ~{
    V(.x)$name <- sample(V(.x)$name)
    return(.x)
  })
  shuffled_graphs_static <- map(gs_static, ~{
    V(.x)$name <- sample(V(.x)$name)
    return(.x)
  })
  shuffled <- map(shuffled_graphs, 
                              ~igraph::as_data_frame(.x) %>%
                                mutate(rep = i)) %>% purrr::list_rbind()
  shuffled_static <- map(shuffled_graphs_static,
                         ~igraph::as_data_frame(.x) %>%
                           mutate(rep = i)) %>% purrr::list_rbind()
  shuffled_reps[[i]] <- shuffled
  shuffled_reps_static[[i]] <- shuffled_static
  cat(".")
}

shuffled_reps_df <- purrr::list_rbind(shuffled_reps)
shuffled_reps_static_df <- purrr::list_rbind(shuffled_reps_static)
# Because of the shuffling, the dyads may now be in the wrong order. Let's get ID1 and ID2 to be correct (ID1 < ID2).
forward <- shuffled_reps_df %>%
  mutate(ID1 = from, ID2 = to)
backward <- shuffled_reps_df %>%
  mutate(ID1 = to, ID2 = from)
ordered <- bind_rows(forward, backward) %>%
  filter(ID1 < ID2)
rm(forward, backward)

forward <- shuffled_reps_static_df %>%
  mutate(ID1 = from, ID2 = to)
backward <- shuffled_reps_static_df %>%
  mutate(ID1 = to, ID2 = from)
ordered_static <- bind_rows(forward, backward) %>%
  filter(ID1 < ID2)
rm(forward, backward)

# Filter out any duplicates or self edges
replicates <- ordered %>%
  ungroup() %>%
  select(ID1, ID2, weight, situ, period, rep) %>%
  distinct() %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))
replicates_static <- ordered_static %>%
  ungroup() %>%
  select(ID1, ID2, weight, situ, period, rep) %>%
  distinct() %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))
minimal <- minimal %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))
static <- static %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))

all(minimal$dyad %in% replicates$dyad)
all(static$dyad %in% replicates_static$dyad)
all(replicates$dyad %in% minimal$dyad)
all(replicates_static$dyad %in% static$dyad)
length(unique(replicates$dyad))
length(unique(replicates_static$dyad))
length(unique(minimal$dyad))
length(unique(static$dyad))

## Now we're set up to run regressions
# Calculate linear models for observed data
obs_for_lms <- minimal %>%
  group_split(dyad, situ)
obs_for_lms_static <- static %>%
  group_split(dyad, situ)

lms_obs_summ <- map(obs_for_lms, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)

lms_obs_summ_static <- map(obs_for_lms_static, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)

obs_labels <- map(obs_for_lms, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())
obs_labels_static <- map(obs_for_lms_static, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())

lms_obs_summ_labeled <- map2(lms_obs_summ, obs_labels, ~bind_cols(.y, .x)) %>% purrr::list_rbind()
lms_obs_summ_labeled_static <- map2(lms_obs_summ_static, obs_labels_static, ~bind_cols(.y, .x)) %>% purrr::list_rbind()

# Calculate linear models for permuted data
future::plan(future::multisession, workers = 20)
perm_for_lms <- replicates %>%
  group_split(dyad, situ, rep)
perm_for_lms_static <- replicates_static %>%
  group_split(dyad, situ, rep)
lms_perm_summ <- furrr::future_map(perm_for_lms, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)
lms_perm_summ_static <- furrr::future_map(perm_for_lms_static, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)

perm_labels <- replicates %>% select(ID1, ID2, dyad, situ, rep) %>%
                     distinct() %>% mutate(n = 1:nrow(.))
perm_labels_static <- replicates_static %>% select(ID1, ID2, dyad, situ, rep) %>%
  distinct() %>% mutate(n = 1:nrow(.))

lms_perm_summ <- lms_perm_summ %>% purrr::list_rbind(names_to = "n")
lms_perm_summ_static <- lms_perm_summ_static %>% purrr::list_rbind(names_to = "n")

lms_perm_summ_labeled <- left_join(perm_labels, lms_perm_summ, by = "n") %>% select(-n)
lms_perm_summ_labeled_static <- left_join(perm_labels_static, lms_perm_summ_static, by = "n") %>% select(-n)

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

# okay but that's the estimates; what if we do the p-values instead?
test_perm %>%
  filter(term == "period") %>%
  ggplot(aes(x = p.value, col = situ))+
  geom_density()+
  theme_minimal()+
  geom_vline(data = test_obs %>% filter(term == "period"), 
             aes(xintercept = p.value, col = situ)) # hmm, I don't think this is very informative...

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
