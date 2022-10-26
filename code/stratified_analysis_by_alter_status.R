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

xtabs(~behavior_punish_lag + peace_individual, data1) # no punishers in prev round are in peace

round(addmargins(prop.table(xtabs(~behavior_punish_lag + peace_individual + alter_punish_flag, tdata1))), 3)

xtabs(~behavior_punish_lag + peace_individual + alter_punish_flag, tdata1)
round(addmargins(prop.table(xtabs(~behavior_punish_lag + peace_individual + alter_punish_flag, tdata1))), 3)

round(addmargins(prop.table(xtabs(~behavior_punish_lag + peace_individual + alter_punish_flag, tdata1), c(3, 1))), 3)

## Based on above, we see that (280 decisions) 4.2% of those who did not punish 
## previously and had no punishing alters did not end up in peace in the 
## following round. 
## Conversely, 5.6% of people who did not punish previously but had punishing
## neighbors did end up in peace.  
## 
## This suggests possible transport of punishing from new members of the social
## network, or spontaneous start of punishment by either new or old members.
## The 104 instances of alter punishment + ego no punishment suggests people
## may excise punishers from their network to establish peace (albeit at a very)
## low rate.
## 

## To further evaluate this, first we check if any individuals repeated this 
## behavior

tdata_p2w = tdata1 %>% 
  filter(alter_punish_flag == 0, 
         behavior_punish_lag == 0, 
         peace_individual == 0) %>%
  mutate(peace_to_war = 1)

tdata_w2p = tdata1 %>%
  filter(alter_punish_flag == 1,
         behavior_punish_lag == 0,
         peace_individual == 1) %>%
  mutate(war_to_peace = 1)

tdata_p2w_to_merge = tdata_p2w %>%
  select(superid, round, game, peace_to_war)
tdata_w2p_to_merge = tdata_w2p %>%
  select(superid, round, game, war_to_peace)
  
data2 = data1 %>% 
  as_tibble %>%
  left_join(tdata_p2w_to_merge, by = c("superid", "round", "game")) %>%
  left_join(tdata_w2p_to_merge, by = c("superid", "round", "game")) %>%
  mutate(peace_to_war = case_when(peace_to_war == 1 ~ 1,
                                  is.na(peace_to_war) == TRUE ~ 0),
         war_to_peace = case_when(war_to_peace == 1 ~ 1,
                                  is.na(war_to_peace) == TRUE ~ 0)) 

# crosstabs/boxplots
names(data2)

# age
ggplot(data2) +
  geom_boxplot(aes(x=factor(peace_to_war), y=age))

ggplot(data2) +
  geom_boxplot(aes(x=factor(war_to_peace), y=age))

# gender
prop.table(xtabs(~gender, data2)) #58% M vs. 31% F
round(prop.table(xtabs(~gender + peace_to_war, data2)), 3) #the ratio is close
round(prop.table(xtabs(~\gender + war_to_peace, data2)), 3) #same for war to peace

# country
round(prop.table(xtabs(~country_3cat + peace_to_war, data2)), 3) #1.6% for US, 0.9% for india
round(prop.table(xtabs(~country_3cat + war_to_peace, data2)), 3) #0.3% for US, 0.6% for india
# yes - makes sense, Indian players are more punishing compared to US players so 
# there should be more peaceful converts among indian players 

# wealth visibility
round(prop.table(xtabs(~showScore + peace_to_war, data2)), 3)
round(prop.table(xtabs(~showScore + war_to_peace, data2)), 3) #no major difference for wealth visibility

# Initial score
round(prop.table(xtabs(~factor(initial_score) + peace_to_war, data2)), 3)
round(prop.table(xtabs(~factor(initial_score) + war_to_peace, data2)), 3)

# Cumulative payoff - different ranges, but mean/median are close - probably due to sample size
ggplot(data2) +
  geom_boxplot(aes(x=factor(peace_to_war), y=cumulativePayoff))

ggplot(data2) +
  geom_boxplot(aes(x=factor(war_to_peace), y=cumulativePayoff))

# degree - slightly high in war-to-peace transition (but not by much)
ggplot(data2) +
  geom_boxplot(aes(x=degree, y=factor(peace_to_war)))

ggplot(data2) +
  geom_boxplot(aes(x=degree, y=factor(war_to_peace)))


## Can we understand what context leads to this behavior?

tdata_merged %>%
  
