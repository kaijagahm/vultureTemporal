# Trying to follow the methods in the shark paper
library(targets)
library(tidyverse)
library(abind)
tar_load("flight_sris")
daydata <- flight_sris[[1]] %>% map(., ~.x %>% mutate(dyad = paste(ID1, ID2, sep = ", ")))
alldyads <- purrr::list_rbind(daydata) %>% pull(dyad) %>% unique()
## gotta make the day data complete so we can make association matrices.
allids <- unique(c(purrr::list_rbind(daydata)$ID1, purrr::list_rbind(daydata)$ID2))

complete_daydata <- map(daydata, ~{
  toadd <- alldyads[which(!(alldyads %in% .x$dyad))]
  id1s <- str_extract(toadd, ".*(?=\\,)")
  id2s <- str_extract(toadd, "(?<=\\,\\s).*")
  df_toadd <- data.frame(ID1 = id1s, ID2 = id2s, weight = NA, 
                         dyad = toadd)
  selfs <- data.frame(ID1 = allids, ID2 = allids, weight = 1) %>% mutate(dyad = paste(ID1, ID2, sep = ", "))
  out <- bind_rows(.x, df_toadd)
  rev <- out
  colnames(rev)[1:2] <- c("ID2", "ID1")
  outout <- bind_rows(out, rev)
  outoutout <- bind_rows(outout, selfs)
  return(distinct(outoutout))
})

# "The indices for each dyad were compiled into symmetric and weighted association matrices for the overall study period and for each month to determine how the dynamics of the associations varied over time."
## (We have SRI's for days, not months, and they are in edge lists rather than association matrices, but same deal.)
### First make the daily data complete
daily_ams <- map(complete_daydata, ~.x %>%
                   arrange(ID1, ID2) %>%
                   pivot_wider(id_cols = "ID1", names_from = "ID2", values_from = "weight")) # okay so now we have symmetric and weighted AM's. 
daily_ams_matrices <- map(daily_ams, ~as.matrix(.x[,-1]))

# "We calculated mean and maximum association strengths and the mean number of associations per dyad and individual"
## Ok I can't calculate number of associations, but I can calculate mean and maximum association strengths.

ar <- abind(daily_ams_matrices, along = 3)
mn <- apply(ar, c(1,2), FUN = function(x){ 
  mean(x, na.rm = T)
})
mx <- apply(ar, c(1, 2), FUN = function(x){
  max(x, na.rm = T)
})

# Let's get a sense of the SRI values in general
alldays <- purrr::list_rbind(daydata, names_to = "day")
alldays %>%
  filter(weight > 0, !is.na(weight)) %>%
  ggplot(aes(x = weight))+
  geom_histogram()

# "in general, the margin parameter of apply should be a vector of dimensions you want to preserve, rather than collapse" (https://stackoverflow.com/questions/3197330/using-apply-on-a-multidimensional-array-in-r)