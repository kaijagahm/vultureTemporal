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
## Let's take the 5-day windows
fiveday <- flight_sris[[2]]
periods <- purrr::imap(fiveday, ~.x %>% mutate(period = .y)) %>% purrr::list_rbind() %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))

alldyads <- unique(periods$dyad)
allperiods <- 1:max(periods$period)
alldyadperiods <- expand_grid("dyad" = alldyads, "period" = allperiods)
periods <- alldyadperiods %>% left_join(periods) %>% # this adds in all the NA's for the dyads that didn't interact during certain periods
  mutate(ID1 = str_extract(dyad, ".*(?=\\,)"),
         ID2 = str_extract(dyad, "(?<=\\,\\s).*"))

firstinteractions <- periods %>%
  group_by(period) %>%
  filter(weight > 0 & !is.na(weight)) %>%
  arrange(period, .by_group = T) %>%
  slice(1) %>%
  ungroup()

# Ok but, first interactions are arbitrary. And when we ask "how long does an association last," we also care about differentiating the associations from random. It can't be considered an association if it's due to random noise. However, when we're talking about time, we really can't use the same wrap-around method, because that messes up time. So maybe we're really concerned with a social network permutation? We want to ask whether a given dyad differs significantly in strength from random dyads? I think that brings us back to identity swapping. 

# What if we binarize all of this? So make it just a sequence of 0's and 1's, which I think Elvira already did?
periods_bin <- periods %>%
  mutate(yn = case_when(is.na(weight) ~ NA,
                        weight > 0 & !is.na(weight) ~ 1,
                        weight == 0 & !is.na(weight) ~ 0))%>%
  select(ID1, ID2, dyad, period, yn)

periods_bin_rev <- periods_bin
names(periods_bin_rev)[1:2] <- c("ID2", "ID1")
periods_bin_rev <- periods_bin_rev %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))

periods_bin_all <- bind_rows(periods_bin, periods_bin_rev) # add on the opposite dyads
periods_bin_all <- periods_bin_all %>%
  rowwise() %>%
  mutate(dyad_absolute = paste(min(ID1, ID2), max(ID1, ID2), sep = ", ")) %>%
  ungroup()

## weighted, non-binary:
periods_rev <- periods %>% select(ID1, ID2, dyad, period, weight)
names(periods_rev)[1:2] <- c("ID2", "ID1")
periods_rev <- periods_rev %>%
  mutate(dyad = paste(ID1, ID2, sep = ", "))
periods_all <- bind_rows(periods, periods_rev) # add on the opposite dyads
periods_all <- periods_all %>%
  rowwise() %>%
  mutate(dyad_absolute = paste(min(ID1, ID2), max(ID1, ID2), sep = ", ")) %>%
  ungroup()

# Also evaluate whether this measure makes sense
# Is there a way to make a weighted version of this instead of just y/n?
# Maybe this still isn't really LAR. This is more "given they interact, what is the probability that they interact the next day?" instead of "given they interact, what is the probability that B is one of A's partners the next day?"--which should account for overall lower association rates the following day. Let's try that, both weighted and unweighted, next.

# Let's try reinterpreting: given that A and B interact on a given day, what is the probability of B being one of A's associates after a time lag t?
# XXX need to do this with the data doubled since we're looking on an individual basis--LAR's won't be symmetrical for a given dyad
period_strengths <- periods_bin_all %>%
  group_by(ID1, period) %>%
  summarize(str_period = sum(yn == 1, na.rm = T),
            n_period = sum(!is.na(yn)))

period_strengths_w <- periods_all %>%
  group_by(ID1, period) %>%
  summarize(str_period = sum(weight, na.rm = T),
            n_period = sum(!is.na(weight)))

prob_assoc <- periods_bin_all %>%
  arrange(dyad, period) %>%
  group_by(dyad) %>%
  mutate(lead_value = lead(yn, 1),
         lead_period = lead(period, 1)) %>%
  ungroup() %>%
  left_join(period_strengths, by = c("ID1", "lead_period" = "period")) %>%
  rename("str_lead_period_id1" = "str_period",
         "n_lead_period_id1" = "n_period") %>%
  mutate(prob_assoc = lead_value/str_lead_period_id1) %>%
  filter(yn == 1)

prob_assoc_w <- periods_all %>%
  arrange(dyad, period) %>%
  group_by(dyad) %>%
  mutate(lead_value = lead(weight, 1),
         lead_period = lead(period, 1)) %>%
  ungroup() %>%
  left_join(period_strengths_w, by = c("ID1", "lead_period" = "period")) %>%
  rename("str_lead_period_id1" = "str_period",
         "n_lead_period_id1" = "n_period") %>%
  mutate(prob_assoc = lead_value/str_lead_period_id1) %>%
  filter(weight > 0 & !is.na(weight)) # only looking at re-associations for individuals that *did* associate.

levels <- prob_assoc %>%
  filter(!is.na(prob_assoc)) %>%
  group_by(dyad) %>%
  filter(n() >= 7) %>%
  summarize(mn = mean(prob_assoc)) %>% 
  arrange(desc(mn)) %>%
  pull(dyad)

levels_w <- prob_assoc_w %>%
  filter(!is.na(prob_assoc)) %>%
  group_by(dyad) %>%
  filter(n() >= 7) %>%
  summarize(mn = mean(prob_assoc)) %>% 
  arrange(desc(mn)) %>%
  pull(dyad)

prob_assoc %>%
  filter(!is.na(prob_assoc)) %>%
  group_by(dyad) %>%
  filter(n() >=7) %>%
  mutate(dyad = factor(dyad, levels = levels)) %>%
  ggplot(aes(x = dyad, y = prob_assoc, fill = factor(dyad_absolute)))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  ylab("Prob randomly-selected assocate \nof A is B after 5 days (binary)")+
  xlab("Dyad")+
  labs(caption = "(includes dyads with >= 6 valid lags)")
# Note that the dyads are not always next to each other, because these relationships are not symmetrical.

prob_assoc_w %>%
  filter(!is.na(prob_assoc)) %>%
  group_by(dyad) %>%
  filter(n() >=7) %>%
  mutate(dyad = factor(dyad, levels = levels)) %>%
  ggplot(aes(x = dyad, y = prob_assoc, fill = factor(dyad_absolute)))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  ylab("Prob randomly-selected assocate \nof A is B after 5 days (weighted)")+
  xlab("Dyad")+
  labs(caption = "(includes dyads with >= 6 valid lags)")

# In order to look at this over the entire population of dyads, we're going to need a mean and sd...
mns <- prob_assoc_w %>%
  group_by(dyad) %>%
  summarize(mn = mean(prob_assoc, na.rm = T))

# Histogram of the means
mns %>%
  ggplot(aes(x = mn))+
  geom_histogram()

mns %>%
  ggplot(aes(x = 1, y = mn))+
  geom_boxplot()
# This is extremely right-skewed, which is going to be a problem because if it's not normally distributed, I don't know how we can represent it with a mean.
mns %>%
  ggplot(aes(x = log(mn)))+
  geom_histogram() # log-transforming helps, but what does that even mean?

# How does jackknifing fit in here?
# Is this even the right measure?

# Next step is to calculate this over multiple time lags. -----------------
# have to pick weighted/uw; gonna do weighted.
# inputs are period_strengths_w and periods_all

lags <- 1:25
association_probs <- map(lags, ~{
  periods_all %>%
    arrange(dyad, period) %>% group_by(dyad) %>%
    mutate(lead_value = lead(weight, .x),
           lead_period = lead(period, .x)) %>%
    ungroup() %>%
    left_join(period_strengths_w, by = c("ID1", "lead_period" = "period")) %>%
    rename("str_lead_period_id1" = "str_period",
           "n_lead_period_id1" = "n_period") %>%
    mutate(prob_assoc = lead_value/str_lead_period_id1) %>%
    filter(weight > 0 & !is.na(weight))
}) %>% purrr::list_rbind(names_to = "lag")

toplot <- association_probs %>%
  filter(!is.na(lead_value), !is.nan(prob_assoc)) %>%
  group_by(lag, dyad) %>%
  summarize(mn = mean(prob_assoc, na.rm = T)) %>%
  ungroup()

toplot %>%
  ggplot(aes(x = log(mn), col = factor(lag)))+
  geom_density()+
  theme_classic()

toplot %>%
  ggplot(aes(x = factor(lag), y = log(mn)))+
  geom_boxplot() # trends slightly upward b/c logs are reversed... hmm.

# I don't know how to make this not be such a tiny number...

