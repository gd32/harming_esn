# Peace analysis for exp1
load("~/Documents/Projects/harming_esn/code/exp1/ndata_individual.Rdata")
load("~/Documents/Projects/harming_esn/code/exp1/data1_pc.Rdata")

# Create the list of alters by superid
ndata_alters_list_exp1 = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list_exp1)[4:20] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14", "alter15", "alter16",
                                   "alter17")

ndata_alters_exp1_lag = ndata_alters_list_exp1
names(ndata_alters_exp1_lag)[4:20] = paste0("lag_",names(ndata_alters_exp1_lag)[4:20])
ndata_alters_exp1_lag$round = ndata_alters_exp1_lag$round+1

ndata_alters_exp1 = merge(ndata_alters_list_exp1, ndata_alters_exp1_lag, 
                     by = c("superid", "round", "game")) %>%
  arrange(superid, round) %>%
  as_tibble()

ndata_long_alters_exp1 = ndata_alters_exp1 %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

ndata_long_alters_exp1_lag = ndata_long_alters_exp1

ego_curr_behavior = data1_pc %>%
  select(superid, game, round, behavior_punish) %>%
  rename(e_behavior_punish = behavior_punish) # has current round behavior

data1_pc_rp_exp1 = ndata_long_alters_exp1_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(ego_curr_behavior, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(e_behavior_punish = e_behavior_punish.x,
         a_behavior_punish = e_behavior_punish.y,
         prev_round_alter = name,
         last_round_alter_id = alter_id)  %>%
  select(superid, round, prev_round, game, e_behavior_punish, last_round_alter_id, a_behavior_punish) %>%
  left_join(ndata_alters_list_exp1, by = c("superid", "game", "round")) %>% # current alters
  mutate(reactive_p = case_when(e_behavior_punish == 1 & a_behavior_punish == 1 & 
                                last_round_alter_id %in% c_across(alter1:alter14) == TRUE ~ 1,
                                TRUE ~ 0)) %>%
  select(superid, round, game, e_behavior_punish, reactive_p) %>%
  group_by(superid, round, game)%>%
  summarize(e_behavior_punish = e_behavior_punish,
            reactive_p = sum(reactive_p, na.rm = T)) %>%
  unique() %>%
  mutate(retaliation_harm = case_when(reactive_p == 0 ~ 0,
                                      reactive_p >= 1 ~ 1)) %>%
  ungroup()

data1_pc = data1_pc %>%
  left_join(data1_pc_rp_exp1, by = c("superid", "game", "round")) 

save(data1_pc, file = "~/Documents/Projects/harming_esn/code/exp1/data1_pc_v2.Rdata")
###############################################################################
# Peace analysis for exp2 
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpoff.Rdata") #TP-
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_tpon.Rdata") #TP+
load("~/Documents/Projects/harming_esn/code/exp2/tpdata_pc.Rdata")
load("~/Documents/Projects/harming_esn/code/exp2/ndata1_tp.Rdata")

tpdata_pc %>%
  group_by(time_pressure) %>%
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
ndata_alters_list_exp2 = ndata_tp %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list_exp2)[4:17] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14")

ndata_alters_exp2_lag = ndata_alters_list_exp2
names(ndata_alters_exp2_lag)[4:17] = paste0("lag_",names(ndata_alters_exp2_lag)[4:17])
ndata_alters_exp2_lag$round = ndata_alters_exp2_lag$round+1

ndata_alters_exp2 = merge(ndata_alters_list_exp2, ndata_alters_exp2_lag, 
                     by = c("superid", "round", "game")) %>%
  arrange(superid, round) %>%
  as_tibble()

ndata_long_alters_exp2 = ndata_alters_exp2 %>%
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

