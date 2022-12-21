load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata

library(tidyverse)
library(lme4)

#### Unnesting all the lists for rewiring data ####

ndata_break = ndata1 %>% 
  select(breakLink, notMakeLink, notBreakLink, makeLink, round, game, 
         starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  unnest_wider(breakLink) %>%
  rename(break1 = ...1,
         break2 = ...2,
         break3 = ...3, 
         break4 = ...4,
         break5 = ...5)

ndata_wide1  = ndata_break %>%
  unnest_wider(notMakeLink) 

names(ndata_wide1)[6:15] = c("notMake1", "notMake2", "notMake3", "notMake4",
                                     "notMake5", "notMake6", "notMake7", "notMake8",
                                     "notMake9", "notMake10")

ndata_wide2 = ndata_wide1 %>%
  unnest_wider(notBreakLink)

names(ndata_wide2)[16:22] = c("noBreak1", "noBreak2", "noBreak3", "noBreak4", 
                             "noBreak5", "noBreak6", "noBreak7")

ndata_wide3 = ndata_wide2 %>%
  unnest_wider(makeLink)

names(ndata_wide3)[23:31] = c("make1", "make2", "make3", "make4", "make5", 
                              "make6", "make7", "make8", "make9")


# write.csv(ndata_wide3, "~/Documents/Projects/harming_esn/Data/ndata_wide_rewiring.csv", row.names = F)

ndata_wide3 = read_csv("~/Documents/Projects/harming_esn/Data/ndata_wide_rewiring.csv")
#### Data management for rewiring analysis ####

ndata_final = ndata_wide3 %>%
  filter(round != 0) %>%
  mutate(across(starts_with("break"), ~100*game+parse_number(.)),
         across(starts_with("no"), ~100*game+parse_number(.)),
         across(starts_with("make"), ~100*game+parse_number(.))) %>%
  pivot_longer(cols = c(starts_with("break"), starts_with("no"), 
                        starts_with("make"))) %>%
  filter(is.na(value) == F) %>% 
  rename(superid = id) %>%
  select(superid, round, game, name, value)

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

ndata_alters_list

# left_join(ndata_final, ndata_alters_list, by = c("superid", "round", "game"))

# Creating lag data for previous alters
ndata_alters_lag = ndata_alters_list
names(ndata_alters_lag)[4:20] = paste0("lag_",names(ndata_alters_lag)[4:20])
ndata_alters_lag$round = ndata_alters_lag$round+1

ndata_alters = merge(ndata_alters_list, ndata_alters_lag, 
                     by = c("superid", "round", "game")) %>%
  arrange(superid, round)

ndata_alters

ndata_base = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  pivot_longer(cols = starts_with("id2"))  %>%
  na.omit() %>%
  rename(superid = id, alter_id = value) %>%
  select(superid, game, round, alter_id)

left_join(ndata_final, ndata_base, by = c("superid", "game", "round")) %>%
  mutate(retain_tie = ifelse(str_detect(name, "noBreak") == T & value == alter_id, 1, 0),
         no_make_tie = ifelse(str_detect(name "noConn" == T & value == alter_id, 1, 0)),
         break_tie = ifelse(str_detect(name, "break" == T & value == alter_id, 1, 0)),
         )

ndata_nt = left_join(ndata_base, ndata_alters, by = c("round", "game", "superid")) %>%
  filter(round != 1) %>%
  select(superid, game, round, alter_id, starts_with("lag")) %>%
  rowwise %>%
  mutate(new_tie = ifelse(alter_id %in% c_across(starts_with("lag_")), 0, 1))

covars = data1 %>% 
  filter(round != 0) %>%
  select(superid, game, round, showScore, initial_score, behavior_coop,
         behavior_punish, behavior_defect, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag, gender, age, country_3cat,
         WealthLevel, cPayoffS, starting_dc, starting_ec)

alter_behaviors = covars %>% 
  select(superid, game, round, behavior_coop, behavior_punish,
         behavior_defect, behavior_coop_lag, 
         behavior_punish_lag, behavior_defect_lag) %>%
  arrange(game, round) %>%
  rename(alter_id = superid,
         alter_behavior_coop = behavior_coop,
         alter_behavior_punish = behavior_punish,
         alter_behavior_defect = behavior_defect,
         alter_behavior_coop_lag = behavior_coop_lag,
         alter_behavior_punish_lag = behavior_punish_lag,
         alter_behavior_defect_lag = behavior_defect_lag) %>%
  filter(round != 1)

tmp1 = left_join(ndata_nt, covars, by = c("superid", "game", "round")) %>% 
  select(-starts_with("la"))

data6 = left_join(tmp1, alter_behaviors, by = c("alter_id", "game", "round"))


# cross tabs
xtabs(~new_tie, data6)
data6 = data6 %>%
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "P"),
         alter_behavior = case_when(alter_behavior_coop == 1 ~ "C",
                                    alter_behavior_defect == 1 ~ "D",
                                    alter_behavior_punish == 1 ~ "P"),
         behavior_lag = case_when(behavior_coop_lag == 1 ~ "C",
                                  behavior_defect_lag == 1 ~ "D",
                                  behavior_punish_lag == 1 ~ "P"),
         alter_behavior_lag = case_when(alter_behavior_coop_lag == 1 ~ "C",
                                        alter_behavior_defect_lag == 1 ~ "D",
                                        alter_behavior_punish_lag == 1 ~ "P"),
         new_tie = factor(new_tie, levels = c(0, 1)))

# Want to compare the building vs. breaking

names(data6)
# simple regression
m6 = glmer(factor(new_tie, levels = c(1, 0)) ~ showScore + factor(initial_score) + factor(behavior) + factor(alter_behavior) 
       + factor(round) + (1|game) + (1|superid),
      data = data6 , family = binomial, nAGQ=0, 
      control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m6)))
# table of estimates with 95% CI
tab_m6 = cbind(Est = fixef(m6), LL = fixef(m6) - 1.96 * se, UL = fixef(m6) + 1.96 * se)
round(exp(tab_m6), digits = 3)[1:7,]

# add all the covariates
m6_long = glmer(new_tie ~ showScore + factor(initial_score) + factor(behavior) +
                factor(alter_behavior) + gender + age + country_3cat + 
                WealthLevel + cPayoffS + starting_dc + starting_ec + 
                factor(round) + (1|game) + (1|superid),data = data6 , family = binomial, nAGQ=0, 
                control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m6_long)))
# table of estimates with 95% CI
tab_m6 = cbind(Est = fixef(m6_long), LL = fixef(m6_long) - 1.96 * se, UL = fixef(m6_long) + 1.96 * se)
round(exp(tab_m6), digits = 3)[1:18,]
