# Peace analysis for exp2 

load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpoff.Rdata") #TP-
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpon.Rdata") #TP+
load("~/Documents/Projects/harming_esn/code/exp2/ndata1_tp.Rdata")

tp_minus_ids = tpdata_tpoff$gameID %>% unique() %>%
  str_remove_all("\\D+") %>%
  as.numeric()
tp_plus_ids = tpdata_tpon$gameID %>% unique() %>%
  str_remove_all("\\D+") %>%
  as.numeric()

# For TP- only
tpdata_pc = tpdata_pc %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 &
                                   behavior_punish_lag == 0, 1, 0))

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

behaviors = tpdata_pc %>% 
  filter(round != 0) %>%
  select(superid, game, round, behavior, local_rate_punish, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

pi_status = tpdata_pc %>% 
  select(superid, game, round, behavior, local_rate_punish, peace_individual, behavior_punish_lag)

punish_lag_status = tpdata_pc %>%
  select(superid, game, round, behavior_punish)

tdata_merged_tp = ndata_long_alters %>%
  left_join(pi_status, by = c("superid", "round", "game")) %>%
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, 
            by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>%
  rename(alter_punish_lag = behavior_punish)

tdata_tp1 = tdata_merged_tp %>%
  group_by(superid, round, game) %>%
  summarize(alter_punish_count = sum(alter_punish_lag)) %>%
  mutate(alter_punish_flag = ifelse(alter_punish_count == 0, 0, 1)) %>%
  left_join(pi_status, by = c("superid", "game", "round")) %>%
  mutate(harm = ifelse(behavior == "P", 1, 0))

tdata_tp_minus = tdata_tp1 %>%
  filter(game %in% tp_minus_ids)

tdata_tp_plus = tdata_tp1 %>%
  filter(game %in% tp_plus_ids)

xtabs(~behavior_punish_lag + alter_punish_flag, tdata1)

# Create as variable type
tdata_tp2 = tdata_tp1 %>% 
  mutate(tp_on = ifelse(game %in% tp_plus_ids, 1, 0),
         last_round_type = case_when(behavior_punish_lag == 0 & alter_punish_flag == 0 ~ "NN",
                                     behavior_punish_lag == 0 & alter_punish_flag == 1 ~ "NH",
                                     behavior_punish_lag == 1 & alter_punish_flag == 0 ~ "HN",
                                     behavior_punish_lag == 1 & alter_punish_flag == 1 ~ "HH"),
         current_social_env = ifelse(local_rate_punish == 0, "Peace", "Nonpeace"),
         current_round_type = case_when(behavior != "P" & current_social_env == "Peace" ~ "NN",
                                        behavior != "P" & current_social_env == "Nonpeace" ~ "NH",
                                        behavior == "P" & current_social_env == "Peace" ~ "HN",
                                        behavior == "P" & current_social_env == "Nonpeace" ~ "HH"),
         behavior_punish = ifelse(behavior == "P", 1, 0),
         behavior_coop = ifelse(behavior == "C", 1, 0),
         behavior_defect = ifelse(behavior == "D", 1, 0))

xtabs(~behavior_punish + behavior_punish_lag, tdata_tp2)
xtabs(~peace_individual + behavior_punish, tdata_tp2)

xtabs(~behavior_punish_lag + alter_punish_flag, tdata_tp2 %>% filter(tp_on == 1))
xtabs(~behavior_punish_lag + alter_punish_flag, tdata_tp2 %>% filter(tp_on == 0))

# TP+
tdata_tp2 %>% 
  filter(last_round_type == "NN", tp_on == 1) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))

tdata_tp2 %>% 
  filter(last_round_type == "NH", tp_on == 1) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 

tdata_tp2 %>% 
  filter(last_round_type == "HN", tp_on == 1) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))
         
tdata_tp2 %>% 
  filter(last_round_type == "HH", tp_on == 1) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 

# TP-
tdata_tp2 %>% 
  filter(last_round_type == "NN", tp_on == 0) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))

tdata_tp2 %>% 
  filter(last_round_type == "NH", tp_on == 0) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 

tdata_tp2 %>% 
  filter(last_round_type == "HN", tp_on == 0) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n))

tdata_tp2 %>% 
  filter(last_round_type == "HH", tp_on == 0) %>%
  group_by(current_round_type) %>%
  summarize(n = n()) %>%
  na.omit() %>%
  mutate(perc = n/sum(n)) 
`
         `