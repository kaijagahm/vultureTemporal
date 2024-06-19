# LAR/LRA
# Inventing a new lagged association rate measure for data that isn't group-by-individual
# (asnipe is designed to work with group-by-individual matrices)
library(targets)
library(tidyverse)

# One definition of LARs (note: not LRAs) is "the probability that two individuals re-associate a given number of days from their first observed instance of association" (Sunga et al. 2024)
# Another definition: "Standardized lagged association rates (SLAR) were determined for the entire study period to assess the stability of these relationships. The SLAR g'(t) of a dyad consisting of sharks a and b is an estimate of the average probability that, after a given time lag (t), a randomly selected associate of a will be b, with false absences accounted for according to Whitehead (2008a) and standard errors estimated using jackknifing (Whitehead, 2009)." (Armansin et al. 2016)

# How can we define this at all so that it makes sense for GPS data, or for SRI's that we've already calculated?
## Start from a time period t where (a, b) exists. Pick one to focus on.
## Go to the next time period t+1. sri(a, b)/strength(a) = "probability that, after a given time lag (t), a randomly selected associate of a will be b".
## Then we have to somehow average that over all times when they are interacting... hmm. I don't love that the initial determination of the dyad (a, b) in time period t is binary, and then we incorporate the SRI's only for time t+1.
## Actually, I guess the initial t has to be the first time that a and b ever interact. But how do we know we've captured the first time?

# playing around with the vulture data
tar_load(flight_sris)
tar_load(feeding_sris)
tar_load(roost_sris)

## Let's take the 5-day windows
fl_5 <- flight_sris[[2]]
fe_5 <- feeding_sris[[2]]
ro_5 <- roost_sris[[2]]

periods_fl <- purrr::list_rbind(fl_5, names_to = "period") %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
periods_fe <- purrr::list_rbind(fe_5, names_to = "period") %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
periods_ro <- purrr::list_rbind(ro_5, names_to = "period") %>% mutate(dyad = paste(ID1, ID2, sep = ", "))

addNAs <- function(periods){
  alldyads <- unique(periods$dyad)
  allperiods <- 1:max(periods$period)
  alldyadperiods <- expand_grid("dyad" = alldyads, "period" = allperiods)
  periods <- alldyadperiods %>% left_join(periods) %>%
    mutate(ID1 = str_extract(dyad, ".*(?=\\,)"),
           ID2 = str_extract(dyad, "(?<=\\,\\s).*"))
  return(periods)
}

periods_fl <- addNAs(periods_fl)
periods_fe <- addNAs(periods_fe)
periods_ro <- addNAs(periods_ro)

addreverse <- function(data){
  rev <- data
  names(rev)[3:4] <- c("ID2", "ID1")
  rev <- rev %>%
    mutate(dyad = paste(ID1, ID2, sep = ", "))
  all <- bind_rows(data, rev)
  all <- all %>%
    rowwise() %>%
    mutate(dyad_absolute = paste(min(ID1, ID2), max(ID1, ID2), sep = ", ")) %>%
    ungroup()
  return(all)
}

periods_all_fl <- addreverse(periods_fl) %>% mutate(situ = "flight")
periods_all_fe <- addreverse(periods_fe) %>% mutate(situ = "feeding")
periods_all_ro <- addreverse(periods_ro) %>% mutate(situ = "roosting")
periods_all <- bind_rows(periods_all_fl, periods_all_fe, periods_all_ro)

# Let's try reinterpreting: given that A and B interact during a given period, what is the probability of B being one of A's associates after a time lag t?
# need to do this with the data doubled since we're looking on an individual basis--LAR's won't be symmetrical for a given dyad
# get strengths for each period
getstrengths <- function(all){
  all <- all %>%
    group_by(ID1, period, situ) %>%
    summarize(str_period = sum(weight, na.rm = T),
              n_period = sum(!is.na(weight))) %>%
    ungroup()
  return(all)
}

strengths <- getstrengths(periods_all)

get_prob_assoc <- function(strengths, all, lead_amt = 1){
  assocs <- all %>%
    arrange(situ, dyad, period) %>%
    group_by(dyad, situ) %>%
    mutate(lead_value = lead(weight, lead_amt),
           lead_period = lead(period, lead_amt)) %>%
    ungroup() %>%
    left_join(strengths, by = c("ID1", "lead_period" = "period", "situ")) %>%
    rename("str_lead_period_id1" = "str_period",
           "n_lead_period_id1" = "n_period") %>%
    mutate(prob_assoc = lead_value/str_lead_period_id1) %>%
    filter(weight > 0 & !is.na(weight)) # only looking at re-associations for individuals that did associate.
  return(assocs)
}

probs <- get_prob_assoc(strengths, periods_all, lead_amt = 1)

# Let's look at some re-association probabilities, restricting it to dyads that interacted in at least 10 different periods.

# get factor levels for ordering
getlevels <- function(probs, situation, thresh = 10){
  levels <- probs %>%
    filter(!is.na(prob_assoc), situ == situation) %>%
    group_by(dyad) %>%
    filter(n() >= thresh) %>%
    summarize(mn = mean(prob_assoc)) %>%
    arrange(desc(mn)) %>%
    pull(dyad)
  return(levels)
}

lv_fl <- getlevels(probs, "flight", thresh = 14)
plt_fl <- probs %>%
  filter(!is.na(prob_assoc), situ == "flight") %>%
  group_by(dyad) %>%
  filter(n() >= 14) %>%
  mutate(dyad = factor(dyad, levels = lv_fl)) %>%
  ggplot(aes(x = dyad, y = prob_assoc, fill = factor(dyad_absolute)))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  ylab("P(partner of A is B)")+
  xlab("Dyad")+
  labs(caption = "(includes dyads with >= 14 valid lags)")+
  ggtitle("1-day reassociations (co-flight)")+
  theme(text =element_text(size = 20))
plt_fl

# note: because there are some really extreme outliers here, going to remove anything above 0.5 for visualization purposes. This only removes 4 points.
lv_fe <- getlevels(probs, "feeding", thresh = 10)
plt_fe <- probs %>%
  filter(!is.na(prob_assoc), situ == "feeding", prob_assoc < 0.5) %>%
  group_by(dyad) %>%
  filter(n() >= 10) %>%
  mutate(dyad = factor(dyad, levels = lv_fe)) %>%
  ggplot(aes(x = dyad, y = prob_assoc, fill = factor(dyad_absolute)))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  ylab("P(partner of A is B)")+
  xlab("Dyad")+
  labs(caption = "(includes dyads with >= 10 valid lags)")+
  ggtitle("1-day reassociations (co-feeding)")+
  theme(text = element_text(size = 20))
plt_fe

lv_ro <- getlevels(probs, "roosting", thresh = 21)
plt_ro <- probs %>%
  filter(!is.na(prob_assoc), situ == "roosting") %>%
  group_by(dyad) %>%
  filter(n() >= 21) %>%
  mutate(dyad = factor(dyad, levels = lv_ro)) %>%
  ggplot(aes(x = dyad, y = prob_assoc, fill = factor(dyad_absolute)))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")+
  ylab("Prob randomly-selected assocate \nof A is B after 5 days")+
  xlab("Dyad (co-roosting)")+
  labs(caption = "(includes dyads with >= 21 valid lags)")
plt_ro

# In order to look at this over the entire population of dyads, we're going to need a mean and sd...
mns <- probs %>%
  group_by(dyad, situ) %>%
  summarize(mn = mean(prob_assoc, na.rm = T))

# Histogram of the means
mns %>%
  ggplot(aes(x = mn))+
  geom_histogram() # extremely right-skewed, which means it's going to be hard to represent with a mean.

mns %>%
  ggplot(aes(x = log(mn)))+
  geom_histogram() # log-transforming helps, but what does that even mean?

# How does jackknifing fit in here?
# Is this even the right measure?

# Next step is to calculate this over multiple time lags. -----------------
lags <- 1:10 # numbers of 5-day increments
probs <- get_prob_assoc(strengths, periods_all)
lar_probs <- map(lags, ~{
  probs <- get_prob_assoc(strengths, periods_all, lead_amt = .x)
  return(probs)
}, .progress = T) %>% purrr::list_rbind(names_to = "lag")

lars <- lar_probs %>%
  filter(!is.na(lead_value), !is.nan(prob_assoc), !is.na(prob_assoc)) %>%
  group_by(lag, dyad, situ) %>%
  summarize(mn = mean(prob_assoc, na.rm = T),
            sd = sd(prob_assoc, na.rm = T),
            n = sum(!is.na(prob_assoc))) %>%
  ungroup() %>%
  arrange(situ, dyad, lag)

# Let's understand what sample sizes these are based off of.
lars %>%
  ggplot(aes(x = n))+
  geom_histogram()+
  facet_grid(rows = vars(lag), cols = vars(situ)) # hmm, this doesn't tell me all that much... but I guess we should probably be basing things on an n of at least 3, in order for the means to mean anything.

head(lars)
lars_n10 <- lars %>%
  filter(n >= 10)

lars_n10 %>%
  ggplot(aes(x = log(mn), col = factor(lag)))+
  geom_density()+
  theme_classic()+
  facet_wrap(~situ)

lars_n10_nonzero <- lars_n10 %>%
  filter(mn > 0)

# attempting to plot the LARs
lars_n10 %>%
  ggplot(aes(x = factor(lag), y = mn))+
  geom_boxplot(outlier.size = 0.1)+ 
  facet_wrap(~situ, scales = "free")+
  theme_classic()

lars_n10 %>%
  ggplot(aes(x = factor(lag), y = log(mn)))+
  geom_boxplot(outlier.size = 0.1)+ 
  facet_wrap(~situ, scales = "free")+
  theme_classic()

# I can't tell if these results are real (i.e. that there just aren't that many differences in reassociation probability over this timescale at all) or if there's something wrong with how I'm calculating this. Really would need to test the method on a simulation to confirm that it's working as intended, but I don't think I have time to do that.

