# Classifying punishment by types

# 3 tasks
# 1. Classify harming into 4 (3?) categories
# Instrumental: harming to reduce inequality
# Retaliation: Harming in response
# 3rd party (how many?): harming to promote cooperation but not retaliation
# i.e. retaliation == 0 and instrumental == 1
# Random
# 
# For now, consider all punishment in round 1 as random since people have no
# priors
# 
# Which type is most reduced by time pressure?
# 
# 2. Aggression and culture? - skip
# 
# 3. Aggression and gender

# 4 main datasets: 
# harmdata - cleaned raw individual-level data 
# ldata4 - tie component of network-level data
# ndata1 - node component of network-level data
# data1 - main aggregated dataset including lag (rounds t-1, t-2, etc.) data

rm(list = ls())

# harmdata
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata

# ldata4
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata")

# ndata1
load("~/Documents/Projects/harming_esn/Data/raw/ndata_individual.Rdata")

# data1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

# data_1cc - complete case dataset
load("~/Documents/Projects/harming_esn/data/data1_cc.Rdata")

# TASK 1 - Classifying punishment by type
# for the main harming dataset

data1 = data1 %>%
  as_tibble()

data1 %>%
  filter(behavior_punish == 1) #562 punishments

# Instrumental punishment: punishing to reduce inequality
# defined as punishment that occurs when ego's wealth is lower than the mean wealth of alters

# first, get the list of alters
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

current_wealth = data1 %>% 
  filter(round != 0) %>%
  select(superid, game, round, cumulativePayoff, cPayoffS)

ndata_long_alters = ndata_alters_list %>%
  pivot_longer(cols = starts_with("alter"), values_to = ("alter_id")) %>%
  filter(is.na(alter_id) == 0)

ndata_long_alters_wealth = ndata_long_alters %>%
  left_join(current_wealth, by = c("round", "game", "alter_id" = "superid")) %>%
  group_by(superid, round, game) %>%
  summarize(mean_alter_wealth = mean1(cPayoffS))

data1_pc = data1 %>%
  left_join(ndata_long_alters_wealth, by = c("round", "game", "superid")) %>%
  mutate(wealth_inequal = ifelse(cPayoffS < mean_alter_wealth, 1, 0)) 
  
data1_pc %>%
  filter(behavior_punish == 1, round > 0) %>%
  group_by(wealth_inequal) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

data1_pc %>%
  filter(is.na(wealth_inequal) == 1 & behavior_punish == 1) %>%
  select(superid, round, game, cPayoffS, mean_alter_wealth, wealth_inequal) # these had no alters in this round

## 49.6% wealth inequality, 48% wealth equal

# Reactive punishment: punishment in response to punishment
# Defined as: punishment after an alter punished in last round

data1_pc = 
  data1_pc %>% 
  mutate(reactive_p = ifelse(behavior_punish == 1 & local_rate_punish_lag !=0, 1, 0))

data1_pc %>%
  filter(behavior_punish == 1) %>%
  mutate(reactive_p = ifelse(local_rate_punish_lag !=0, 1, 0)) %>%
  group_by(reactive_p) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# 26.5% reactive, 63.9% not (54 NAs)

# Random punishments
# not reactive or calculated

data1_pc_punishes = data1_pc %>%
  filter(behavior_punish == 1)

data1_pc_punishes %>%
  filter(round <= 1) #49

data1_pc_punishes %>%
  filter(round > 1 & wealth_inequal == 0 & reactive_p == 0) #171

data1_pc_punishes %>%
  filter(reactive_p == 1) %>%
  select(local_rate_punish_lag) 

# Cross tab
xtabs(~reactive_p + wealth_inequal, data1_pc_punishes)

## For the time pressure dataset
load("~/Documents/Projects/harming_esn/data/tp_subset/ndata1_tp.Rdata")
load("~/Documents/Projects/harming_esn/data/tp_subset/ldata4_tp.Rdata")
load("~/Documents/Projects/harming_esn/data/tp_subset/tpdata.Rdata")

tpdata = tpdata %>%
  as_tibble()

tpdata %>%
  filter(behavior == "P") #612 punishments

# Instrumental punishment: punishing to reduce inequality
# defined as punishment that occurs when ego's wealth is lower than the mean wealth of alters

# first, get the list of alters
ndata_alters_list = ndata_tp %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

length(names(ndata_alters_list))

names(ndata_alters_list)[4:17] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14")

current_wealth = tpdata %>% 
  filter(round != 0) %>%
  select(superid, game, round, cumulativePayoff, cPayoffS)

ndata_long_alters = ndata_alters_list %>%
  pivot_longer(cols = starts_with("alter"), values_to = ("alter_id")) %>%
  filter(is.na(alter_id) == 0)

ndata_long_alters_wealth = ndata_long_alters %>%
  left_join(current_wealth, by = c("round", "game", "alter_id" = "superid")) %>%
  group_by(superid, round, game) %>%
  summarize(mean_alter_wealth = mean1(cPayoffS))

tpdata_pc = tpdata %>%
  left_join(ndata_long_alters_wealth, by = c("round", "game", "superid")) %>%
  mutate(wealth_inequal = ifelse(cPayoffS < mean_alter_wealth, 1, 0)) 

tpdata_pc %>%
  filter(behavior=="P", round > 0) %>%
  group_by(wealth_inequal) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc %>%
  filter(is.na(wealth_inequal) == 1 & behavior=="P") %>%
  select(superid, round, game, cPayoffS, mean_alter_wealth, wealth_inequal) # these had no alters in this round

## 51.0% wealth inequality, 47.4% wealth equal

# Reactive punishment: punishment in response to punishment
# Defined as: punishment after an alter punished in last round

tpdata_pc = 
  tpdata_pc %>% 
  mutate(reactive_p = ifelse(behavior == "P" & prev_local_rate_punish!=0, 1, 0))

tpdata_pc %>%
  filter(behavior == "P") %>%
  group_by(reactive_p) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# 49.3% reactive, 50.7% not
# 
# 3rd party punishment - i.e.; reactive_p = 0 + wealth_inequal = 1

tpdata_pc %>%
  filter(behavior == "P") %>%
  group_by(reactive_p, wealth_inequal) %>%
  summarize(n= n()) %>%
  filter(is.na(wealth_inequal) == 0) %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

# 23.3%?

# Random punishments
# not reactive or calculated

tpdata_pc %>%
  filter(behavior == "P" & round <= 1) #46

tpdata_pc %>%
  filter(behavior == "P" & round > 1 & wealth_inequal == 0 & reactive_p == 0) #143

tpdata_only_p = tpdata_pc %>% filter(behavior == "P")

# crosstabs
xtabs(~wealth_inequal + reactive_p, tpdata_pc %>% filter(round > 1, behavior == "P"))

tp_labels = read_csv("~/Documents/Projects/breadboard/time_pressure_brb/Assignment/tp_assignment.csv")
tp_labels
tp_on = tp_labels %>% filter(time_pressure_on == 1) %>% select(game) %>% pull()

tpdata_pc$time_pressure = ifelse(tpdata_pc$game %in% tp_on, 1, 0)

tpdata_pc %>%
  group_by(time_pressure, behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n),
         total = sum(n))

# for C: 869/1564 vs. 674/1645
prop.test(x = c(869, 674), n = c(1564, 1645)) # p < 0.0001

# for D: 1048/3069 vs. 6/140
prop.test(x = c(403, 651), n = c(1564, 1645)) # p < 0.0001

# for P: 581/3069
prop.test(x = c(292, 320), n = c(1564, 1645)) # p = 0.604



xtabs(~time_pressure+wealth_inequal, tpdata_only_p)
xtabs(~time_pressure+reactive_p, tpdata_only_p)

## Task 2 - gender
tpdata_only_p %>%
  filter(gender %in% c("male", "female"), is.na(wealth_inequal) == 0) %>%
  group_by(gender, wealth_inequal, tp_on) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# interpretation: when time pressure is on, calculated punishment by females stays the same where as it goes down in males (87 -> 51)
# 

## Task 2 - gender
tpdata_only_p %>%
  filter(gender %in% c("male", "female")) %>%
  group_by(gender, reactive_p, tp_on) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# interprtation: when tp is on, reactive punishment decreases in females (67->35), and men (109->55)
# "non reactive p in women" goes up?

# Cross tab
xtabs(~reactive_p + wealth_inequal, data1_pc_punishes)

## Exp 3 data
load("~/Documents/Projects/harming_esn/data/exp3/ndata1_exp3.Rdata")
load("~/Documents/Projects/harming_esn/data/exp3/exp3data_pc.Rdata")

exp3data = exp3data_pc %>%
  as_tibble()

exp3data %>%
  filter(behavior == "P") %>%
  count() #430 punishments

# inequality aversion punishment: punishing to reduce inequality
# defined as punishment that occurs when ego's wealth is lower than the mean wealth of alters

# first, get the list of alters
ndata_alters_list = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list)[4:19] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14", "alter15", "alter16")

current_wealth = exp3data %>% 
  filter(round != 0) %>%
  select(superid, game, round, cumulativePayoff, cPayoffS)

ndata_long_alters = ndata_alters_list %>%
  pivot_longer(cols = starts_with("alter"), values_to = ("alter_id")) %>%
  filter(is.na(alter_id) == 0)

ndata_long_alters_wealth = ndata_long_alters %>%
  left_join(current_wealth, by = c("round", "game", "alter_id" = "superid")) %>%
  group_by(superid, round, game) %>%
  summarize(mean_alter_wealth = mean1(cPayoffS))

exp3data_pc = exp3data %>%
  left_join(ndata_long_alters_wealth, by = c("round", "game", "superid")) %>%
  mutate(wealth_inequal = ifelse(cPayoffS < mean_alter_wealth, 1, 0)) 

save(exp3data_pc, file = "~/Documents/Projects/harming_esn/data/exp3/exp3data_pc_v2.Rdata")

