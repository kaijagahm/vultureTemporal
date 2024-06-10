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

# Permutations ------------------------------------------------------------
# In order to figure out the trend, need to permute the node identities separately on each of the networks, and then re-calculate the trends, pulling out the slope and p-value and various other information, and then compare that to the observed slopes.

minimal <- all %>%
  ungroup() %>%
  select(ID1, ID2, weight, situ, period)

tonetworks <- minimal %>%
  group_by(situ, period) %>%
  group_split() %>% map(., as.data.frame)
gs <- map(tonetworks, ~igraph::graph_from_data_frame(.x, directed = FALSE))

reps <- 100
shuffled_reps <- vector(mode = "list", length = reps)
for(i in 1:reps){
  shuffled_graphs <- map(gs, ~{
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
# Because of the shuffling, the dyads may now be in the wrong order. Let's get ID1 and ID2 to be correct (ID1 < ID2).
ordered <- shuffled_reps_df %>%
  rowwise() %>%
  mutate(ID1 = min(from, to),
         ID2 = max(from, to))

# Filter out any duplicates or self edges
replicates <- ordered %>%
  ungroup() %>%
  select(ID1, ID2, weight, situ, period, rep) %>%
  distinct() %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))
minimal <- minimal %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))

all(minimal$dyad %in% replicates$dyad)
all(replicates$dyad %in% minimal$dyad)
length(unique(replicates$dyad))
length(unique(minimal$dyad))

## Now we're set up to run regressions
test <- minimal %>% filter(dyad == "adam, Jill", situ == "feeding")
lm.test <- lm(weight ~ period, data = test)

# Calculate linear models for observed data
obs_for_lms <- minimal %>%
  group_split(dyad, situ)

lms_obs_summ <- map(obs_for_lms, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)

obs_labels <- map(obs_for_lms, ~.x %>% select(ID1, ID2, dyad, situ) %>% distinct())

lms_obs_summ_labeled <- map2(lms_obs_summ, obs_labels, ~bind_cols(.y, .x)) %>% purrr::list_rbind()
  
# Calculate linear models for permuted data
future::plan(future::multisession, workers = 20)
perm_for_lms <- replicates %>%
  group_split(dyad, situ, rep)
lms_perm_summ <- furrr::future_map(perm_for_lms, ~{
  mod <- lm(weight ~ period, data = .x)
  summ <- broom::tidy(mod)
}, .progress = T)

perm_labels <- replicates %>% select(ID1, ID2, dyad, situ, rep) %>%
                     distinct() %>% mutate(n = 1:nrow(.))

lms_perm_summ <- lms_perm_summ %>% purrr::list_rbind(names_to = "n")
  
lms_perm_summ_labeled <- left_join(perm_labels, lms_perm_summ, by = "n") %>% select(-n)

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

# Time to ask some real questions about this!
# Out of the dyads that had information for at least 15 (out of 25) periods, and had a significant upward or downward trend (p < 0.05), which of those are significantly different from random?

dyads_15periods <- minimal %>%
  group_by(dyad, situ) %>% filter(length(unique(period)) >= 15) %>%
  select(dyad, situ) %>%
  distinct()

n15 <- length(unique(dyads_15periods$dyad))

dyads_15periods_sigtrend <- dyads_15periods %>%
  left_join(lms_obs_summ_labeled, by = c("dyad", "situ")) %>%
  filter(term == "period", p.value < 0.05)

### oops, gotta calculate the differences from random for the slope estimate
combined <- lms_perm_summ_labeled %>%
  left_join(lms_obs_summ_labeled, by = c("ID1", "ID2", "dyad", "situ", "term"), suffix = c("", "_obs"))

combined_summ <- combined %>%
  filter(term == "period") %>%
  select(dyad, situ, rep, estimate, estimate_obs) %>%
  group_by(dyad, situ) %>%
  summarize(n_obs_greater = sum(estimate_obs > estimate),
            n_obs_less = sum(estimate_obs < estimate),
            prop_obs_greater = n_obs_greater/n(),
            prop_obs_less = n_obs_less/n()) %>%
  ungroup()

dyads_15periods_nonrandom <- dyads_15periods %>%
  ungroup() %>%
  left_join(combined_summ) %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025)

length(unique(dyads_15periods_nonrandom$dyad))/n15

dyads_15periods_sigtrend_nonrandom <- dyads_15periods_sigtrend %>%
  ungroup() %>%
  left_join(combined_summ) %>%
  filter(prop_obs_greater <= 0.025 | prop_obs_less <= 0.025) %>%
  arrange(ID1, ID2, situ)

## Okay cool, now let's visualize!
### pick 8 at random:
whichtoshow <- dyads_15periods_sigtrend_nonrandom %>%
  slice_sample(n = 9) %>% select(dyad, situ)
replicates_toshow <- left_join(whichtoshow, replicates)
obs_toshow <- left_join(whichtoshow, minimal)

replicates_toshow %>%
  ggplot(aes(x = period, y = weight, col = situ))+
  theme_minimal()+
  geom_jitter(alpha = 0.1, width = 0.05, height = 0.05, size = 0.2)+
  facet_wrap(~dyad)+
  geom_smooth(method = "lm", se = F, linewidth = 0.2, aes(group = rep))+
  geom_smooth(data = obs_toshow, method = "lm", se = T, linewidth = 1, col = "black")+
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())
