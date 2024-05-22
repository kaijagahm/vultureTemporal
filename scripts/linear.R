library(tidyverse)
library(targets)

tar_load(feeding_sris)
tar_load(flight_sris)
tar_load(roost_sris)
tar_load(timewindows)

# Dealing first with the daily networks
fe <- feeding_sris[[1]] %>% purrr::imap(~.x %>% mutate(day = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(day = as.numeric(day), weight = as.numeric(weight)) %>% mutate(situ = "feeding")
fl <- flight_sris[[1]] %>% purrr::imap(~.x %>% mutate(day = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(day = as.numeric(day), weight = as.numeric(weight)) %>% mutate(situ = "flight")
ro <- roost_sris[[1]] %>% purrr::imap(~.x %>% mutate(day = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(day = as.numeric(day), weight = as.numeric(weight)) %>% mutate(situ = "roosting")

all <- list_rbind(list(fe, fl, ro)) %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
all(all$ID1 < all$ID2) # should be no repeat dyads or self dyads
length(unique(all$dyad)) # 3626 dyads
dyads <- unique(all$dyad)

all <- all %>%
  group_by(dyad, situ) %>%
  mutate(n_dyad_situ = n()) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(n_dyad = n())

frequent_dyads <- all %>%
  filter(n_dyad_situ > 30) %>% # limiting it to dyads that interacted 30 or more times in any situation (not necessarily in all situations)
  pull(dyad)

# Let's just pick a dyad at random and see how it changes over the days.
set.seed(3)
dy <- sample(frequent_dyads, 1)

all %>%
  filter(dyad == dy) %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line()+
  theme_minimal() # oh right, roost should always have an SRI of 1, 0, or missing. Which makes me curious why it has an SRI of 0.5. hmm. Let's investigate that later.

# Okay, so in feeding and flight, this dyad fluctuates a lot. Let's visualize a regression.
all %>%
  filter(dyad == dy, situ != "roosting") %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm") # fitting a regression here makes it look like the co-feeding sri's don't change much over time for this pair, but co-flight might have a significant decrease.
