# LAR/LRA
library(asnipe)
library(targets)
library(tidyverse)
# asnipe is designed to work with group-by-individual matrices, but I'm going to see if I can make it work with my data.

data("group_by_individual")
nrow(gbi) # 347 interactions/groups
ncol(gbi) # 151 individuals
# okay so this is a group-by-individual matrix, which is fundamentally now how my data is stored. 

data("times")
length(times) #347--the timestamp for each group.

data("individuals")
dim(inds) #151 inds, with info about each individual.

# One definition of LARs (note: not LRAs) is "the probability that two individuals re-associate a given number of days from their first observed instance of association" (Sunga et al. 2024)
# Another definition: "Standardized lagged association rates (SLAR) were determined for the entire study period to assess the stability of these relationships. The SLAR g'(t) of a dyad consisting of sharks a and b is an estimate of the average probability that, after a given time lag (t), a randomly selected associate of a will be b, with false absences accounted for according to Whitehead (2008a) and standard errors estimated using jackknifing (Whitehead, 2009)." (Armansin et al. 2016)

# How can we define this at all so that it makes sense for GPS data, or for SRI's that we've already calculated?
## Start from a time period t where (a, b) exists. Pick one to focus on.
## Go to the next time period t+1. sri(a, b)/strength(a) = "probability that, after a given time lag (t), a randomly selected associate of a will be b".
## Then we have to somehow average that over all times when they are interacting... hmm. I don't love that the initial determination of the dyad (a, b) in time period t is binary, and then we incorporate the SRI's only for time t+1.
## Actually, I guess the initial t has to be the first time that a and b ever interact. But how do we know we've captured the first time?

# playing around with the vulture data
tar_load(flight_sris)
## Let's take the 1-day windows first, since they're most detailed.
oneday <- flight_sris[[1]]
days <- purrr::imap(oneday, ~.x %>% mutate(day = .y)) %>% purrr::list_rbind() %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))

firstinteractions <- days %>%
  group_by(dyad) %>%
  filter(weight > 0) %>%
  arrange(day, .by_group = T) %>%
  slice(1) %>%
  ungroup()

# Ok but, first interactions are arbitrary. And when we ask "how long does an association last," we also care about differentiating the associations from random. It can't be considered an association if it's due to random noise. However, when we're talking about time, we really can't use the same wrap-around method, because that messes up time. So maybe we're really concerned with a social network permutation? We want to ask whether a given dyad differs significantly in strength from random dyads? I think that brings us back to identity swapping. 

# What if we binarize all of this? So make it just a sequence of 0's and 1's, which I think Elvira already did?
days_bin <- days %>%
  mutate(yn = ifelse(weight > 0, 1, 0)) %>%
  select(ID1, ID2, dyad, day, yn)

leads <- 1:50

df <- map(leads, ~days_bin %>%
             group_by(dyad) %>%
             mutate(lead_value = lead(yn, .x)) %>%
             group_by(ID1, yn) %>%
             summarize(prop1 = sum(lead_value == 1, na.rm = T)/n(),
                       n = sum(!is.na(lead_value)),
                       .groups = "drop")) %>%
  purrr::list_rbind(names_to = "lead") %>%
  relocate(lead, .after = "yn")

# Visualize as a boxplot  
df %>%
  filter(yn == 1) %>%
  ggplot(aes(x = factor(lead), y = prop1))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  geom_smooth()

# Visualize as numeric
df %>%
  filter(yn == 1) %>%
  ggplot(aes(x = lead, y = prop1))+
  geom_point(alpha = 0.1, pch = 1)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  geom_smooth() # okay, so we're noticing that it goes down and seems to level out a bit, but notably, the probability of reassocation was already really really low! Even 1 day later, the average probability is only 0.05 (that's conditional probablity: p(yes at t + 1)|(yes at t)). Of course we should keep in  mind that p is already very very low on any given day for flight. Roosting might be more informative here, or different windows. 

# what about putting the y on a log scale?
df %>%
  filter(yn == 1) %>%
  ggplot(aes(x = lead, y = log(prop1)))+
  geom_point(alpha = 0.1, pch = 1)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  geom_smooth() # now that's a tad more interesting! a clear pattern is emerging.

# What if we look at the same but for 0's?
df %>%
  filter(yn == 0) %>%
  ggplot(aes(x = lead, y = log(prop1)))+
  geom_point(alpha = 0.1, pch = 1)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank())+
  geom_smooth() # hmm, okay, so there's just a general downward trend over the course of the season. How do we account for that? Need to do some kind of permutation but I'm not sure what kind.

# Next to do: do this on different time scales (hourly should show day/night patterns at the very least, 5-day increments, etc.)
# Also evaluate whether this measure makes sense
# Is there a way to make a weighted version of this instead of just y/n?
# Maybe this still isn't really LAR. This is more "given they interact, what is the probability that they interact the next day?" instead of "given they interact, what is the probability that B is one of A's partners the next day?"--which should account for overall lower association rates the following day. Let's try that, both weighted and unweighted, next.

