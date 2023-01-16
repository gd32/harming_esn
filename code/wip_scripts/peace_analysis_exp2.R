# Peace analysis for exp1

# PART 1
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
  select(superid, game, round, behavior, local_rate_punish, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

pi_status = data1 %>% 
  select(superid, game, round, behavior, local_rate_punish, peace_individual, behavior_punish_lag)

punish_lag_status = data1 %>%
  select(superid, game, round, behavior_punish)

tdata_merged = ndata_long_alters %>%
  left_join(pi_status, by = c("superid", "round", "game")) %>%
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, 
            by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>%
  rename(alter_punish_lag = behavior_punish)

tdata1 = tdata_merged %>%
  group_by(superid, round, game) %>%
  summarize(alter_punish_count = sum(alter_punish_lag)) %>%
  mutate(alter_punish_flag = ifelse(alter_punish_count == 0, 0, 1)) %>%
  left_join(pi_status, by = c("superid", "game", "round")) %>%
  mutate(harm = ifelse(behavior == "P", 1, 0))

# given no punishing alters, 280/6655 decisions = 4.21% of followup decisions are made out of peace
# additionally, given presence of punishing alters, 104/1845 = 5.64% of followup decisions are made in peace
xtabs(~peace_individual + behavior_punish + alter_punish_flag, tdata1)
xtabs(~peace_individual + alter_punish_flag, tdata1)

xtabs(~peace_individual + behavior_punish, tdata1)

tdata1 =
  tdata1 %>%
  mutate(behavior_punish = ifelse(behavior == "P", 1, 0))

prop.test(280, 6655)
prop.test(104, 1845)
# no punishers in previous round are in peace
xtabs(~behavior_punish_lag + peace_individual, data1)
sum(xtabs(~behavior_punish_lag + peace_individual, data1))

xtabs(~behavior_punish_lag + alter_punish_flag + harm, tdata1)

tdata2 = tdata1 %>%
  filter(behavior_punish_lag == 0, alter_punish_flag == 0)

# Part 2 of peace analysis
# First, break down into 4 categories: harm + harmers, harm + noharmers, no harm
# + harmers, no harm + no harmers (all in round t-1)

# Get count
xtabs(~behavior_punish + peace_individual, tdata1)
xtabs(~peace_individual + current_social_env, tdata2)
# Create as variable type
tdata2 = tdata1 %>% 
  mutate(last_round_type = case_when(behavior_punish_lag == 0 & alter_punish_flag == 0 ~ "NN",
                                     behavior_punish_lag == 0 & alter_punish_flag == 1 ~ "NH",
                                     behavior_punish_lag == 1 & alter_punish_flag == 0 ~ "HN",
                                     behavior_punish_lag == 1 & alter_punish_flag == 1 ~ "HH"),
         current_social_env = ifelse(local_rate_punish == 0, "Not harming", "Harming"),
         current_round_type = case_when(behavior != "P" & current_social_env == "Not harming" ~ "NN",
                                        behavior != "P" & current_social_env == "Harming" ~ "NH",
                                        behavior == "P" & current_social_env == "Not harming" ~ "HN",
                                        behavior == "P" & current_social_env == "Harming" ~ "HH"))
tdata2 %>% 
  filter(last_round_type == "NN") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))

tdata2 %>% 
  filter(last_round_type == "NH") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 

tdata2 %>% 
  filter(last_round_type == "HN") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))

tdata2 %>% 
  filter(last_round_type == "HH") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 

tdata3 = tdata2 %>% 
  mutate(prev_social_env = ifelse(alter_punish_count == 0, "Not harming", "Harming")) %>%
  rename(behavior_punish = harm) %>%
  select(superid, round, game, prev_social_env, current_social_env, local_rate_punish, behavior_punish, behavior_punish_lag)

tdata4 = tdata3
names(tdata4)[4:8] = paste0("next_",names(tdata4)[4:8])
tdata4$round = tdata4$round-1
tdata4 = tdata4 %>%
  select(superid, round, game, next_behavior_punish)
# have last round env + current round decision - need next round decision + current round social env

tdata5 = merge(tdata3, tdata4, by = c("superid", "round", "game")) %>%
  as_tibble() %>%
  arrange(superid, round, game)

# Version 2 of peace analysis - transition from round t behavior + round t-1 
# SEnv to round t+1 behavior + round t SEnv

tdata5 = tdata5 %>%
  mutate(pre_transition_state = case_when(behavior_punish == 0 & prev_social_env == "Not harming" ~ "NN",
                                          behavior_punish == 0 & prev_social_env == "Harming" ~ "NH",
                                          behavior_punish == 1 & prev_social_env == "Not harming" ~ "HN",
                                          behavior_punish == 1 & prev_social_env == "Harming" ~ "HH"),
         post_transition_state = case_when(next_behavior_punish == 0 & current_social_env == "Not harming" ~ "NN",
                                           next_behavior_punish == 0 & current_social_env == "Harming" ~ "NH",
                                           next_behavior_punish == 1 & current_social_env == "Not harming" ~ "HN",
                                           next_behavior_punish == 1 & current_social_env == "Harming" ~ "HH"))

tdata5 %>%
  group_by(pre_transition_state) %>%
  count()

tdata5 %>%
  filter(pre_transition_state == "NN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tdata5 %>%
  filter(pre_transition_state == "NH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tdata5 %>%
  filter(pre_transition_state == "HN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tdata5 %>%
  filter(pre_transition_state == "HH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# Peace analysis for exp2 
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpoff.Rdata") #TP-
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpon.Rdata") #TP+
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_pc.Rdata")
load("~/Documents/Projects/harming_esn/code/exp2/ndata1_tp.Rdata")

tpdata_pc %>%
  group_by(tp_on, game) %>%
  count()

tp_minus_ids = tpdata_tpoff$gameID %>% unique() %>%
  str_remove_all("\\D+") %>%
  as.numeric()

tp_plus_ids = tpdata_tpon$gameID %>% unique() %>%
  str_remove_all("\\D+") %>%
  as.numeric()

tpdata_pc = tpdata_pc %>% 
  mutate(time_pressure = factor(ifelse(time_pressure == "Minus", "Minus", "Plus")),
         peace_individual = ifelse(local_rate_punish_lag == 0 &
                                   behavior_punish_lag == 0, 1, 0)) %>%
  as_tibble()

# Create the list of alters by superid
ndata_alters_list = ndata_tp %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list)[4:17] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14")

ndata_alters_lag = ndata_alters_list
names(ndata_alters_lag)[4:17] = paste0("lag_",names(ndata_alters_lag)[4:17])
ndata_alters_lag$round = ndata_alters_lag$round+1

ndata_alters = merge(ndata_alters_list, ndata_alters_lag, 
                     by = c("superid", "round", "game")) %>%
  arrange(superid, round)

ndata_alters %>%
  as_tibble()

behaviors = tpdata_pc %>% 
  filter(round != 0) %>%
  select(superid, game, round, 
         tp_on, behavior_punish, 
         local_rate_punish, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

pi_status = tpdata_pc %>% 
  as_tibble() %>%
  select(superid, game, round, time_pressure, behavior_punish, local_rate_punish, peace_individual, behavior_punish_lag) %>%
  rename(e_behavior_punish = behavior_punish, 
         e_behavior_punish_lag = behavior_punish_lag)

punish_lag_status = tpdata_pc %>%
  select(superid, game, round, e_behavior_punish)

tdata_merged_tp = ndata_long_alters %>%
  left_join(pi_status, by = c("superid", "round", "game")) %>%
  mutate(prev_round = round-1) %>%
  rename(behavior_punish = e_behavior_punish,
         behavior_punish_lag = e_behavior_punish_lag) %>%
  left_join(punish_lag_status, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>%
  rename(alter_punish_lag = e_behavior_punish) %>%
  select(superid, time_pressure, game, round, prev_round, name, alter_id, behavior_punish, local_rate_punish, behavior_punish_lag, alter_punish_lag, peace_individual)

tdata_merged_tp %>%
  group_by(superid, round, game) %>%
  summarize(local_count_punish_lag = sum(alter_punish_lag)) %>%
  mutate(prev_social_env = ifelse(local_count_punish_lag == 0, "Not harming", "Harming")) %>%
  left_join(pi_status, by = c("superid", "game", "round"))

tpdata_pc_modified = tdata_merged_tp %>%
  group_by(superid, round, game) %>%
  summarize(local_count_punish_lag = sum(alter_punish_lag)) %>%
  mutate(social_env_lag = ifelse(local_count_punish_lag == 0, "Not harming", "Harming")) %>%
  left_join(pi_status, by = c("superid", "game", "round")) %>%
  rename(behavior_punish = e_behavior_punish,
         behavior_punish_lag = e_behavior_punish_lag)

tpdata_pc_modified = tpdata_pc_modified %>%
  mutate(last_round_type = case_when(behavior_punish_lag == 0 & social_env_lag == "Not harming" ~ "NN",
                                     behavior_punish_lag == 0 & social_env_lag == "Harming" ~ "NH",
                                     behavior_punish_lag == 1 & social_env_lag == "Not harming" ~ "HN",
                                     behavior_punish_lag == 1 & social_env_lag == "Harming" ~ "HH"),
         social_env = ifelse(local_rate_punish == 0, "Not harming", "Harming"),
         current_round_type = case_when(behavior_punish == 0 & social_env == "Not harming" ~ "NN",
                                        behavior_punish == 0 & social_env == "Harming" ~ "NH",
                                        behavior_punish == 1 & social_env == "Not harming" ~ "HN",
                                        behavior_punish == 1 & social_env == "Harming" ~ "HH"))

load(file = "~/Documents/Projects/harming_esn/code/exp2/tpdata_peace_analysis.Rdata")

# Check the numbers of harming
tpdata_pc_modified %>%
  group_by(time_pressure) %>%
  filter(behavior_punish == 1) %>%
  count()

# TP+
tpdata_pc_modified %>% 
  filter(last_round_type == "NN", time_pressure == "Plus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 23 +57 = 80

tpdata_pc_modified %>% 
  filter(last_round_type == "NH", time_pressure == "Plus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 43 +16 = 59

tpdata_pc_modified %>% 
  filter(last_round_type == "HN", time_pressure == "Plus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 18+28 = 46
         
tpdata_pc_modified %>% 
  filter(last_round_type == "HH", time_pressure == "Plus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))  #harm total = 24+11 = 35

# all harm = 80+59+46+35 = 220
# 2 NAs - no current neighbors

# TP-
tpdata_pc_modified %>% 
  filter(last_round_type == "NN", time_pressure == "Minus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 29+49 = 78

tpdata_pc_modified %>% 
  filter(last_round_type == "NH", time_pressure == "Minus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 76+27 = 103

tpdata_pc_modified %>% 
  filter(last_round_type == "HN", time_pressure == "Minus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 25+62 = 87

tpdata_pc_modified %>% 
  filter(last_round_type == "HH", time_pressure == "Minus") %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n)) #harm total = 51+25 = 76

xtabs(~behavior_punish_lag + social_env_lag + time_pressure, tpdata_pc_modified)

# all harm = 78+103+87+76 = 344
# 
# Part 2 - Using the current round behavior + last round env
names(tpdata_pc_modified)

tpdata_pc_mod2 = tpdata_pc_modified %>%
  select(superid, round, game, time_pressure, behavior_punish) 
names(tpdata_pc_mod2)[4:5] = paste0("next_",names(tpdata_pc_mod2)[4:5])
tpdata_pc_mod2 = tpdata_pc_mod2 %>% 
  mutate(round = round - 1)

tpdata_pc_mod3 = merge(tpdata_pc_modified, tpdata_pc_mod2, 
                       by = c("superid", "game", "round")) %>%
  as_tibble() %>% 
  arrange(superid, game, round)

tpdata_pc_mod4 = tpdata_pc_mod3 %>%
  select(superid, game, round, time_pressure, social_env_lag, behavior_punish, local_rate_punish, next_behavior_punish) %>%
  mutate(social_env = ifelse(local_rate_punish == 0, "Not harming", "Harming"),
         pre_transition_state = case_when(behavior_punish == 0 & social_env_lag == "Not harming" ~ "NN",
                                          behavior_punish == 0 & social_env_lag == "Harming" ~ "NH",
                                          behavior_punish == 1 & social_env_lag == "Not harming" ~ "HN",
                                          behavior_punish == 1 & social_env_lag == "Harming" ~ "HH"),
         post_transition_state = case_when(next_behavior_punish == 0 & social_env == "Not harming" ~ "NN",
                                           next_behavior_punish == 0 & social_env == "Harming" ~ "NH",
                                           next_behavior_punish == 1 & social_env == "Not harming" ~ "HN",
                                           next_behavior_punish == 1 & social_env == "Harming" ~ "HH"))

# TP-
tpdata_pc_mod4 %>%
  filter(time_pressure == "Minus") %>%
  group_by(pre_transition_state) %>%
  count()

tpdata_pc_mod4 %>%
  filter(time_pressure == "Minus", pre_transition_state == "NN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Minus", pre_transition_state == "NH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Minus", pre_transition_state == "HN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Minus", pre_transition_state == "HH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

# TP+
 
tpdata_pc_mod4 %>%
filter(time_pressure == "Plus") %>%
  group_by(pre_transition_state) %>%
  count()

tpdata_pc_mod4 %>%
  filter(time_pressure == "Plus", pre_transition_state == "NN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Plus", pre_transition_state == "NH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Plus", pre_transition_state == "HN") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))

tpdata_pc_mod4 %>%
  filter(time_pressure == "Plus", pre_transition_state == "HH") %>%
  group_by(post_transition_state) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n))
