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

alldyads <- unique(days$dyad)
alldays <- 1:max(days$day)
alldyaddays <- expand_grid("dyad" = alldyads, "day" = alldays)
days <- alldyaddays %>% left_join(days) %>% # this adds in all the NA's for the dyads that didn't interact on certain days.
  mutate(ID1 = str_extract(dyad, ".*(?=\\,)"),
         ID2 = str_extract(dyad, "(?<=\\,\\s).*"))

firstinteractions <- days %>%
  group_by(dyad) %>%
  filter(weight > 0 & !is.na(weight)) %>%
  arrange(day, .by_group = T) %>%
  slice(1) %>%
  ungroup()

# Ok but, first interactions are arbitrary. And when we ask "how long does an association last," we also care about differentiating the associations from random. It can't be considered an association if it's due to random noise. However, when we're talking about time, we really can't use the same wrap-around method, because that messes up time. So maybe we're really concerned with a social network permutation? We want to ask whether a given dyad differs significantly in strength from random dyads? I think that brings us back to identity swapping. 

# What if we binarize all of this? So make it just a sequence of 0's and 1's, which I think Elvira already did?
days_bin <- days %>%
  mutate(yn = case_when(is.na(weight) ~ NA,
                        weight > 0 & !is.na(weight) ~ 1,
                        weight == 0 & !is.na(weight) ~ 0))%>%
  select(ID1, ID2, dyad, day, yn)

leads <- 1:50

# Next to do: do this on different time scales (hourly should show day/night patterns at the very least, 5-day increments, etc.)
# Also evaluate whether this measure makes sense
# Is there a way to make a weighted version of this instead of just y/n?
# Maybe this still isn't really LAR. This is more "given they interact, what is the probability that they interact the next day?" instead of "given they interact, what is the probability that B is one of A's partners the next day?"--which should account for overall lower association rates the following day. Let's try that, both weighted and unweighted, next.

# Let's try reinterpreting: given that A and B interact on a given day, what is the probability of B being one of A's associates after a time lag t?
# XXX need to do this with the data doubled since we're looking on an individual basis--LAR's won't be symmetrical for a given dyad.
daily_strengths <- days_bin %>%
  group_by(ID1, day) %>%
  summarize(str_day = sum(yn == 1, na.rm = T),
            n_day = sum(!is.na(yn)))

prob_assoc <- days_bin %>%
  arrange(dyad, day) %>%
  group_by(dyad) %>%
  mutate(lead_value = lead(yn, 1),
         lead_day = lead(day, 1)) %>%
  ungroup() %>%
  left_join(daily_strengths, by = c("ID1", "lead_day" = "day")) %>%
  rename("str_lead_day_id1" = "str_day",
         "n_lead_day_id1" = "n_day") %>%
  mutate(prob_assoc = lead_value/str_lead_day_id1) %>%
  filter(yn == 1)

prob_assoc %>%
  filter(!is.na(prob_assoc)) %>%
  group_by(dyad) %>%
  summarize(n = n(),
            mnprob = mean(prob_assoc),
            sdprob = sd(prob_assoc)) %>%
  filter(n > 4) %>%
  arrange(desc(mnprob)) %>%
  mutate(dyad = factor(dyad, levels = as.character(dyad))) %>%
  ggplot(aes(x = dyad, y = mnprob))+
  geom_point()+
  geom_errorbar(aes(ymax = mnprob + sdprob,
                    ymin = mnprob - sdprob),
                width = 0)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank())+
  ylab("Prob randomly-selected assocate \nof A is B after 1 day")+
  xlab("Dyad")+
  labs(caption = "(includes dyads with >= 5 valid lags)")+
  geom_hline(aes(yintercept = 0), lty = 3, col = "red")
# There is a lot of missing data here--many dyads do not have re-association data
# Obviously going below 0 doesn't make sense, oops, should do this as a boxplot instead.
# How does jackknifing fit in here?
# Is this even the right measure?
# Can we make it weighted?



