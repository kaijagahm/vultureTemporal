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

# Just in case something non-linear is obviously a better fit (I doubt it will be)
all %>%
  filter(dyad == dy, situ != "roosting") %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth() # yeah no this tells us nothing.

# Some thoughts:
# Will need to pick a time window when there are fewer gaps. Daily will result in lots of fluctuations, necessarily.
# How do we determine which pairs to look at? Don't want to exclude pairs that e.g. had a strong relationship at the beginning and then petered out. Maybe need a certain number of days/periods over the whole season? 

# Over the daily scale, how long is the season?
(max(all$day) - min(all$day))+1 #122 days
122/3 # is 40 days.

# How many dyads remain if we restrict it just to the number of dyads that have interactions on at least 40 days?
# Just feeding and flight for now...
ff <- all %>%
  filter(situ != "roosting")
frequent_ff <- ff %>%
  filter(n_dyad_situ >= 40)
length(unique(frequent_ff$dyad)) # only 6 dyads are that persistent. Let's go ahead and visualize them.

frequent_ff %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad) # still a huge amount of variability, but there are one or two that show clear negative trends over the course of the season. That is interesting!

# What if we relax our requirements a little more, to 25 days?
frequent_ff <- ff %>%
  filter(n_dyad_situ >= 25)
length(unique(frequent_ff$dyad)) # now 13 dyads remain

frequent_ff %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad) # especially interesting that for one of the pairs, we have an opposite-direction relationship over the course of the season in two different situations!

# Let's relax even more, to 20 days
frequent_ff <- ff %>%
  filter(n_dyad_situ >= 20)
length(unique(frequent_ff$dyad)) # now 17 dyads remain

frequent_ff %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad) # similar results, with a few more non-significant trends thrown in.

# 5-days ------------------------------------------------------------------
# Okay, so that was interesting, but I think we need to try this with larger windows! 1 day just has too many gaps.
# Prepare the data for 5-day windows.
fe <- feeding_sris[[2]] %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = "feeding")
fl <- flight_sris[[2]] %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = "flight")
ro <- roost_sris[[2]] %>% purrr::imap(~.x %>% mutate(period = .y) %>% mutate(across(everything(), as.character))) %>% purrr::list_rbind() %>% mutate(period = as.numeric(period), weight = as.numeric(weight)) %>% mutate(situ = "roosting")

all <- list_rbind(list(fe, fl, ro)) %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
all(all$ID1 < all$ID2) # should be no repeat dyads or self dyads
length(unique(all$dyad)) # 
dyads <- unique(all$dyad) #3626 dyads still

all <- all %>%
  group_by(dyad, situ) %>%
  mutate(n_dyad_situ = n()) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(n_dyad = n())

# If we look at 5-day windows, how many will we have?
max(all$period) # 25 periods. Okay, let's see a histogram of how many dyads persist for how many periods
all %>%
  ggplot(aes(x = n_dyad_situ))+
  geom_histogram(breaks = 1:25)+
  facet_wrap(~situ)+
  theme_minimal() # right, okay, now roosting is back in the game. 
# UH OH, stop. Why do we not seem to have any zeroes in the data? That's not right. There's a distinction between zeroes and NA's.
