## Stratified analysis of individual peace

# We want to stratify individual peace status by alter punish status

rm(list = ls())

# Individual peace defined as not being a punisher + no one in the local network is a punisher.

# Import functions and packages
mean1 = function(x) {mean(x, na.rm = TRUE)}

library(tidyverse)
library(lme4)
library(magrittr)
library(igraph)

load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

# Regenerate the individual peace status
data1 = data1 %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 & 
                                     behavior_punish_lag == 0, 1, 0))


# Create the list of alters by superid
ndata_alters_list = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list)[4:20] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14", "alter15", "alter16",
                                   "alter17")

ndata_alters_lag = ndata_alters_list
names(ndata_alters_lag)[4:20] = paste0("lag_",names(ndata_alters_lag)[4:20])
ndata_alters_lag$round = ndata_alters_lag$round+1

ndata_alters = merge(ndata_alters_list, ndata_alters_lag, 
                     by = c("superid", "round", "game")) %>%
  arrange(superid, round)

behaviors = data1 %>% 
  filter(round != 0) %>%
  select(superid, game, round, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

pi_status = data1 %>% 
  select(superid, game, round, peace_individual, behavior_punish_lag)

punish_lag_status = data1 %>%
  select(superid, game, round, behavior_punish)

tdata_merged = ndata_long_alters %>%
  left_join(pi_status, by = c("superid", "round", "game")) %>%
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, 
            by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>%
  rename(alter_punish_lag = behavior_punish)

tdata_merged

xtabs(~peace_individual + behavior_punish_lag + alter_punish_lag, tdata_merged)

tdata1 = tdata_merged %>%
  group_by(superid, round, game) %>%
  summarize(alter_punish_count = sum(alter_punish_lag)) %>%
  mutate(alter_punish_flag = ifelse(alter_punish_count == 0, 0, 1)) %>%
  left_join(pi_status, by = c("superid", "game", "round"))

xtabs(~peace_individual + behavior_punish_lag + alter_punish_flag, tdata1)

round(addmargins(prop.table(xtabs(~peace_individual + behavior_punish_lag + alter_punish_flag, tdata1))), 3)
