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
length(unique(all$dyad)) # 3655 dyads
dyads <- unique(all$dyad)

all <- all %>%
  group_by(dyad, situ) %>%
  mutate(n_dyad_situ = n(),
         n_dyad_situ_nonzero = sum(weight > 0)) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(n_dyad = n(),
         n_dyad_nonzero = sum(weight > 0))

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
  theme_minimal() # oh right, roost should always have an SRI of 1, 0, or missing. 
# This makes me want to separate the different situations on different axes.

all %>%
  filter(dyad == dy) %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  facet_wrap(~situ)

# Okay, so this dyad fluctuates a lot. Let's visualize a regression.
all %>%
  filter(dyad == dy, situ != "roosting") %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+ # fitting a regression here makes it look like the co-feeding sri's don't change much over time for this pair, but co-flight might have a significant decrease.
  facet_wrap(~situ, scales = "free")

# Just in case something non-linear is obviously a better fit (I doubt it will be)
all %>%
  filter(dyad == dy, situ != "roosting") %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth()+ # yeah no this tells us nothing.
  facet_wrap(~situ, scales = "free") # seems to confirm the instinct that flight shows a decrease, at least.

# Some thoughts:
# Will need to pick a time window when there are fewer gaps. Daily will result in lots of fluctuations, necessarily.
# How do we determine which pairs to look at? Don't want to exclude pairs that e.g. had a strong relationship at the beginning and then petered out. Maybe need a certain number of days/periods over the whole season? 

# Over the daily scale, how long is the season?
(max(all$day) - min(all$day))+1 #122 days
122/3 # is 40 days.

# How many dyads remain if we restrict it just to the number of dyads that have any data (including zeroes) on at least 100 days?
# Just feeding and flight for now...
ff <- all %>%
  filter(situ != "roosting")
frequent_ff <- ff %>%
  filter(n_dyad_situ >= 100)
length(unique(frequent_ff$dyad)) #736 dyads remain

random_dyads <- sample(unique(frequent_ff$dyad), 10) # visualize 10 random dyads
frequent_ff %>%
  filter(dyad %in% random_dyads) %>%
  ggplot(aes(x = day, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad, scales = "free") # hmm, this makes it pretty clear that we ought to care about non-zero sri's in addition to just having information at all.

# Let's re-define frequent_ff as having at least 40 days with a non-zero sri.
frequent_ff <- ff %>%
  filter(n_dyad_situ_nonzero >= 40)
length(unique(frequent_ff$dyad)) #only 6 dyads remain

# What if we relax our requirements a little more, to 25 days?
frequent_ff <- ff %>%
  filter(n_dyad_situ_nonzero >= 25)
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
  filter(n_dyad_situ_nonzero >= 20)
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
dyads <- unique(all$dyad) #3655 dyads still

all <- all %>%
  group_by(dyad, situ) %>%
  mutate(n_dyad_situ = n(),
         n_dyad_situ_nonzero = sum(weight > 0)) %>%
  ungroup() %>%
  group_by(dyad) %>%
  mutate(n_dyad = n(),
         n_dyad_nonzero = sum(weight > 0))

# If we look at 5-day windows, how many will we have?
max(all$period) # 25 periods. Okay, let's see a histogram of how many dyads have information for how many periods
all %>%
  ggplot(aes(x = n_dyad_situ))+
  geom_histogram(breaks = 1:25)+
  facet_wrap(~situ, scales = "free")+
  theme_minimal() # okay, so for flight and roosting we don't have very many periods with no data for one or both individuals, mostly good coverage. Feeding happens much less frequently, so naturally we have more dyads that don't interact in a co-feeding context during every 5-day period.

# Let's select dyads that have valid interactions for at least 10 out of 25 5-day periods? (This is a pretty arbitrary pick.)

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

# Let's see the same plot with a non-linear loess fit:
frequent_interactions %>%
  filter(dyad %in% random_dyads_freq) %>%
  ggplot(aes(x = period, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  geom_smooth()+
  facet_wrap(~dyad, scales = "free") # tells a pretty similar story, if  messy.

# Now what do we do with this information?
# Figure out how many of them have significant slopes, and which ones
# Follow Hobson et al for suggestions of what to do next
# Ask what individual characteristics predict the trends over time
# Do the Hobson stability analyses, to see if relationships stabilize over the course of the season (there's no reason to believe that they should...)

# Another question: what if we look at a single individual and look at all of its salient relationships. The dyad estavan, nevada seemed to be an interesting one for roosting, so maybe let's first subset the data to estavan, and then retain any dyads that estavan is a part of that have at least 10 periods with non-zero interactions?
estavan <- all %>%
  filter(ID1 == "estavan" | ID2 == "estavan")
estavan_salient <- estavan %>%
  filter(n_dyad_situ_nonzero >= 8)
length(unique(estavan_salient$dyad))

estavan_salient %>%
  ggplot(aes(x = period, y = weight, col = situ))+
  geom_point()+
  geom_line(alpha = 0.2)+
  geom_smooth(method = "lm")+
  facet_wrap(~dyad)+
  theme_minimal()

# I wonder: is the strength of the relationship related to the number of days with data? to the number of days with a non-zero sri?
# I wonder: what does the cumulative number of relationships look like when you look at number of non-zero days vs. number of relationships, for a given individual?
## We can actually answer that question with a cumulative sum graph for each individual. Just have to be careful when aggregating by individual, since the information right now is dyad-level. In order to make the data individual-level, I think we need to double it and flip it around...
reversed <- all[,c(2, 1, 3:ncol(all))]
names(reversed)[1:2] <- c("ID1", "ID2")
# note that I am *not* reassigning the "dyad" column because I want duplicate rows to show up as duplicate... I think... or I might just end up ignoring the dyad column... we'll see.
allall <- bind_rows(all, reversed)

thresholds <- 0:max(all$period)
friends_cumsum <- map(thresholds, ~allall %>%
                        group_by(ID1, situ) %>%
                        filter(n_dyad_situ_nonzero >= .x) %>%
                        summarize(nfriends = length(unique(ID2)), .groups = "drop") %>%
                        mutate(thresh = .x)) %>% purrr::list_rbind()

# now let's plot some curves
# picking Jill randomly
Jill <- friends_cumsum %>% filter(ID1 == "Jill")
Jill %>%
  ggplot(aes(x = thresh, y = nfriends, col = situ))+
  geom_point()+
  geom_line()+
  theme_minimal()

# What about with boxplots?
friends_cumsum %>%
  mutate(thresh = factor(thresh, levels = min(thresh):max(thresh))) %>%
  ggplot(aes(x = thresh, y = nfriends))+
  geom_boxplot(fill = "gray30")+
  facet_wrap(~situ)+
  theme_minimal()

# Or with a bunch of lines instead?
friends_cumsum %>%
  ggplot(aes(x = thresh, y = nfriends, group = ID1))+
  geom_line(alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~situ)
# I have a feeling there's a better way to look at this, involving rank order of friends, but I'm not thinking of it right now. Time to move on, and come back to this when I'm fresher.