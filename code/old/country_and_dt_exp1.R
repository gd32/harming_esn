# Checking distribution of decision time by country of origin in exp1
load("~/Documents/Projects/harming_esn/data/exp1/harmdata.Rdata")

# restrict to only US
# 
# Exp 1
harmdata %>%
  as_tibble() %>%
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "P")) %>%
  filter(behavior %in% c("C", "D", "P"), country_3cat == "US") %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000),
            se_mean_dt = se_mean(behaviorTime/1000))


# Exp 2
tpdata_pc %>%
  as_tibble() %>%
  filter(behavior %in% c("C", "D", "P", time_pressure == "Minus")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000),
            se_mean_dt = se_mean(behaviorTime/1000)) 

exp3data_pc %>%
  as_tibble() %>%
  filter(behavior %in% c("C", "D", "P", time_pressure == "Minus")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000),
            se_mean_dt = se_mean(behaviorTime/1000))
