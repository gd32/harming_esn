# Creating the formalized definition of retaliation harming
# -- Must be harming that targets a player who harmed in the last round --
# That is, the victim must be still tied to the focal player
# 
# Some questions - do players always know if they are still tied to a previous 
# harmer? -- May require visible wealth condition so players can remember a 
# wealth figure
# 
# Sensitivity analysis for all vs. visible only?

# Exp. 2 data

load("~/Documents/Projects/harming_esn/code/exp2/tpdata_peace_analysis.Rdata")

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
  as_tibble() %>%
  arrange(superid, round)

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

ndata_long_alters_lag = ndata_long_alters

# From current round behavior - check if last round alters punished
# if yes, check if last round alters are in current round alters
# if both above are yes, check if current round is harming. Then, this 
# harming is retaliation

ego_curr_behavior = pi_status %>%
  select(superid, game, round, time_pressure, e_behavior_punish) # has current round behavior

ndata_curr_alters_list = ndata_alters_list %>%
  unite(starts_with("alter"), col = "alters") %>%
  mutate(alters = str_replace_all(alters, "NA", "")) %>%
  mutate(alters = str_replace_all(alters, "_", ",")) %>%
  mutate(alters = trimws(alters, whitespace = ",")) 

comma_sep = function(x) {
  x = strsplit(as.character(x), "")
  unlist(lapply(x, paste, collapse = ','))
}

test = ndata_alters_list %>%
  unite(starts_with("alter"), col = "alters") %>%
  mutate(alters = str_replace_all(alters, "NA", "")) %>%
  mutate(alters = str_replace_all(alters, "_", ",")) %>%
  mutate(alters = trimws(alters, whitespace = ",")) %>%
  mutate(alters = str_split(alters, pattern = ",")) 

ndata_pc_rp = ndata_long_alters_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(e_behavior_punish = e_behavior_punish.x,
         a_behavior_punish = e_behavior_punish.y,
         prev_round_alter = name,
         last_round_alter_id = alter_id)  %>%
  select(superid, round, prev_round, game, time_pressure, e_behavior_punish, last_round_alter_id, a_behavior_punish) %>%
  left_join(ndata_alters_list, by = c("superid", "game", "round")) %>% # current alters
  mutate(reactive_p = case_when(e_behavior_punish == 1 & a_behavior_punish == 1 & 
                                  last_round_alter_id %in% c_across(alter1:alter14) == TRUE ~ 1,
                                TRUE ~ 0)) %>%
  select(superid, round, game, time_pressure, e_behavior_punish, reactive_p) %>%
  group_by(superid, round, game)%>%
  summarize(time_pressure = time_pressure, 
            e_behavior_punish = e_behavior_punish,
            reactive_p = sum(reactive_p, na.rm = T)) %>%
  unique() %>%
  mutate(retaliation_harm = case_when(reactive_p == 0 ~ 0,
                                      reactive_p >= 1 ~ 1)) %>%
  ungroup() %>%
  select(superid, round, game, time_pressure, retaliation_harm)

# merge the new RH numbers into the main dataset
tpdata_pc = tpdata_pc %>%
  left_join(ndata_pc_rp, by = c("superid", "game", "round")) %>%
  mutate(time_pressure = ifelse(tp_on == 1, "Plus", "Minus")) 

save(tpdata_pc, file = "~/Documents/Projects/harming_esn/code/exp2/tpdata_pc_v2.Rdata")
  
ndata_long_alters_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(e_behavior_punish = e_behavior_punish.x,
         a_behavior_punish = e_behavior_punish.y,
         prev_round_alter = name,
         last_round_alter_id = alter_id)  %>%
  select(superid, round, prev_round, game, time_pressure, e_behavior_punish, last_round_alter_id, a_behavior_punish) %>%
  left_join(ndata_alters_list, by = c("superid", "game", "round")) %>% # current alters
  mutate(reactive_p = case_when(e_behavior_punish == 1 & a_behavior_punish == 1 & 
                                  last_round_alter_id %in% c_across(alter1:alter14) == TRUE ~ 1,
                                TRUE ~ 0)) %>%
  select(superid, round, game, time_pressure, e_behavior_punish, reactive_p) %>% view()

# Exp 3 data
rm(list = ls())
load("~/Documents/Projects/harming_esn/data/exp3/tpdata_exp3.Rdata")
load("~/Documents/Projects/harming_esn/data/exp3/ndata1_exp3.Rdata")

ndata_alters_list = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list)[4:19] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14", "alter15", "alter16")

ndata_alters_lag = ndata_alters_list
names(ndata_alters_lag)[4:19] = paste0("lag_",names(ndata_alters_lag)[4:19])
ndata_alters_lag$round = ndata_alters_lag$round+1

ndata_alters = merge(ndata_alters_list, ndata_alters_lag, 
                     by = c("superid", "round", "game")) %>%
  as_tibble() %>%
  arrange(superid, round)

# behaviors = exp3data %>% 
#   filter(round != 0) %>%
#   select(superid, game, round, 
#          tp_on, behavior_punish, 
#          local_rate_punish, behavior_coop_lag, 
#          behavior_punish_lag, behavior_defect_lag)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

ndata_long_alters_lag = ndata_long_alters

# From current round behavior - check if last round alters punished
# if yes, check if last round alters are in current round alters
# if both above are yes, check if current round is harming. Then, this 
# harming is retaliation

ego_curr_behavior = exp3data %>%
  as_tibble() %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 & 
                                              behavior_punish_lag == 0, 1, 0)) %>%
  select(superid, game, round, time_pressure, behavior_punish, local_rate_punish, peace_individual, behavior_punish_lag) %>%
  rename(e_behavior_punish = behavior_punish, 
         e_behavior_punish_lag = behavior_punish_lag) %>%
  select(superid, game, round, time_pressure, e_behavior_punish) # has current round behavior

punish_lag_status = exp3data %>%
  select(superid, game, round, behavior_punish)
# 
# ndata_curr_alters_list = ndata_alters_list %>%
#   unite(starts_with("alter"), col = "alters") %>%
#   mutate(alters = str_replace_all(alters, "NA", "")) %>%
#   mutate(alters = str_replace_all(alters, "_", ",")) %>%
#   mutate(alters = trimws(alters, whitespace = ",")) 

# comma_sep = function(x) {
#   x = strsplit(as.character(x), "")
#   unlist(lapply(x, paste, collapse = ','))
# }

# test = ndata_alters_list %>%
#   unite(starts_with("alter"), col = "alters") %>%
#   mutate(alters = str_replace_all(alters, "NA", "")) %>%
#   mutate(alters = str_replace_all(alters, "_", ",")) %>%
#   mutate(alters = trimws(alters, whitespace = ",")) %>%
#   mutate(alters = str_split(alters, pattern = ",")) 

ndata_rh = ndata_long_alters_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(prev_round_alter = name,
         last_round_alter_id = alter_id,
         a_behavior_punish = behavior_punish) %>%
  select(superid, round, prev_round, game, time_pressure, e_behavior_punish, last_round_alter_id, a_behavior_punish) %>%
  left_join(ndata_alters_list, by = c("superid", "game", "round")) %>% # current alters
  mutate(reactive_p = case_when(e_behavior_punish == 1 & a_behavior_punish == 1 & 
                                  last_round_alter_id %in% c_across(alter1:alter14) == TRUE ~ 1,
                                TRUE ~ 0)) %>%
  select(superid, round, game, time_pressure, e_behavior_punish, reactive_p) %>%
  group_by(superid, round, game)%>%
  summarize(time_pressure = time_pressure, 
            e_behavior_punish = e_behavior_punish,
            reactive_p = sum(reactive_p, na.rm = T)) %>%
  unique() %>%
  mutate(retaliation_harm = case_when(reactive_p == 0 ~ 0,
                                      reactive_p >= 1 ~ 1)) %>%
  ungroup() %>%
  select(superid, round, game, time_pressure, retaliation_harm)

# merge the new RH numbers into the main dataset
exp3data_pc = exp3data %>%
  left_join(ndata_rh, by = c("superid", "game", "round"))

save(exp3data_pc, file = "~/Documents/Projects/harming_esn/data/exp3/exp3data_pc.Rdata")

ndata_long_alters_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(punish_lag_status, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(e_behavior_punish = e_behavior_punish.x,
         a_behavior_punish = e_behavior_punish.y,
         prev_round_alter = name,
         last_round_alter_id = alter_id)  %>%
  select(superid, round, prev_round, game, time_pressure, e_behavior_punish, last_round_alter_id, a_behavior_punish) %>%
  left_join(ndata_alters_list, by = c("superid", "game", "round")) %>% # current alters
  mutate(reactive_p = case_when(e_behavior_punish == 1 & a_behavior_punish == 1 & 
                                  last_round_alter_id %in% c_across(alter1:alter14) == TRUE ~ 1,
                                TRUE ~ 0)) %>%
  select(superid, round, game, time_pressure, e_behavior_punish, reactive_p) %>% view()
