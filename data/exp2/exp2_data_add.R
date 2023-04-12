# Data cleaning of Exp2 games 21-30 + brief analysis

rm(list = ls())
load("~/Documents/Projects/harming_esn/data/exp2/mdata3_tp2.Rdata")
load("~/Documents/Projects/harming_esn/data/exp2/tpdata_pc_v2.Rdata")
library(rgeolocate)
mean1 = function(x) {mean(x,na.rm=TRUE)}
median1 = function(x) {median(x, na.rm = TRUE)}
sd1 = function(x) {sd(x, na.rm = TRUE)}
se_mean = function(x) sd1(x)/sqrt(sum(is.na(x) == 0))
harmdata = mdata3

# Renaming current rates
harmdata = harmdata %>% rename(local_rate_coop = cur_local_rate_coop, 
                               local_rate_defect = cur_local_rate_defect,
                               local_rate_punish = cur_local_rate_punish)

# Create continuous happiness variable
harmdata$happ = ifelse(harmdata$satisfaction == "v_good", 2, 
                       ifelse(harmdata$satisfaction == "good", 1, 
                              ifelse(harmdata$satisfaction == "neutral", 0,
                                     ifelse(harmdata$satisfaction == "bad", -1, 
                                            ifelse(harmdata$satisfaction == "v_bad", -2, NA)))))

# Create positive wealth variable
harmdata$PosWealth = ifelse(harmdata$cumulativePayoff >= 0, 
                            harmdata$cumulativePayoff, 0)

# Create categorical WealthLevel (5 levels)
harmdata = harmdata %>%
  mutate(WealthLevel = case_when(cPayoffS < -1.5 ~ "Poorest",
                                 cPayoffS >= -1.5 & cPayoffS < -0.5  ~ "Poorer",
                                 cPayoffS >= -0.5 & cPayoffS < 0.5  ~ "Middle",
                                 cPayoffS >= 0.5 & cPayoffS < 1.5  ~ "Richer",
                                 cPayoffS >= 1.5  ~ "Richest"))

# Create wealth visibility
harmdata$showScore = ifelse(harmdata$showScore=="true",1,0)

###making the variable names the same over the two data
harmdata = harmdata[,!(names(harmdata) == "gameNumber")]

#Other variables
harmdata$age = as.numeric(unlist(harmdata$age))
harmdata$gender = as.character(unlist(harmdata$gender))
harmdata$ipAddress = as.character(unlist(harmdata$ipAddress))
harmdata$race = as.character(unlist(harmdata$race))
harmdata$is_hispanic = as.character(unlist(harmdata$is_hispanic))
harmdata$satisfaction = as.character(unlist(harmdata$satisfaction))

# Convert IPaddress to country
file = system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
ipCountries = maxmind(harmdata$ipAddress, file, "country_name")
harmdata$country = ipCountries$country_name

`%notin%` <- Negate(`%in%`)

harmdata = harmdata %>% 
  mutate(country_3cat = 
           factor(case_when(country == "United States" ~ "US",
                            country == "India" ~ "India",
                            country %notin% c("United States", 
                                              "India") ~ "Other"),
                  levels = c("US", "India", "Other")))

# Add indicator for time pressure
tp2data = harmdata %>%
  as_tibble() %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == TRUE, "Plus", "Minus"))

tp2data = tp2data %>%
  mutate(behavior = case_when(behavior == "C" ~ "C",
                              behavior == "D" ~ "D",
                              behavior == "P" ~ "P",
                              TRUE ~ NA_character_),
         race = case_when(race == "[white]" ~ "White",
                          race == "[black]" ~ "Black",
                          race == "[asian]" ~ "Asian",
                          race == "[american_indian]" ~ "AmIndian",
                          TRUE ~ NA_character_),
        behavior_punish = ifelse(behavior == "P", 1, 0),
        behavior_coop = ifelse(behavior == "C", 1, 0),
        behavior_defect = ifelse(behavior == "D", 1, 0))

data_lag = tp2data %>% select(superid, round, initial_score, payoff, 
                                   cumulativePayoff, cPayoffS, WealthLevel, 
                                   behavior_coop, local_rate_coop, 
                                   behavior_defect, local_rate_defect, 
                                   behavior_punish, local_rate_punish, 
                                   degree, happ, behaviorTime)

names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1

tp2data = tp2data %>%
  left_join(data_lag, by = c("superid", "round"))

tp2data = tp2data %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == FALSE, "Minus", "Plus")) 

# write_csv(tp2data, "~/Documents/Projects/harming_esn/data/exp2/tp2data_lag_tmp.csv")

# Creating the harming subtype data
# 
# Retaliation harming
load("~/Documents/Projects/harming_esn/data/exp2/ndata1_tp2.Rdata")

ndata_alters_list = ndata1_tp2 %>% 
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
  as_tibble() %>%
  arrange(superid, round)

ndata_long_alters = ndata_alters %>%
  select(superid, round, game, starts_with("lag")) %>%
  pivot_longer(cols = starts_with("lag"), values_to = "alter_id") %>%
  filter(is.na(alter_id) == F)

ndata_long_alters_lag = ndata_long_alters

# From current round behavior - check if last round alters punished
# if yes, check if last round alters are in current round alters
# if both above are yes, check if current round is harming. Then, this 
# harming is retaliation

ego_curr_behavior = tp2data %>%
  select(superid, game, round, time_pressure, e_behavior_punish) # has current round behavior

tp2data_rp = ndata_long_alters_lag %>% 
  left_join(ego_curr_behavior, by = c("superid", "round", "game")) %>% # current round behavior
  mutate(prev_round = round - 1) %>%
  left_join(ego_curr_behavior, by = c("alter_id" = "superid", "game", "prev_round" = "round")) %>% # alter's last behavior
  rename(e_behavior_punish = e_behavior_punish.x,
         a_behavior_punish = e_behavior_punish.y,
         prev_round_alter = name,
         time_pressure = time_pressure.x, 
         last_round_alter_id = alter_id) %>%
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
                                      reactive_p >= 1 ~ 1)) 

tp2data = tp2data %>%
  left_join(tp2data_rp, by = c("superid", "game", "round")) 

# Inequality aversion
current_wealth = tp2data %>% 
  filter(round != 0) %>%
  select(superid, game, round, cumulativePayoff, cPayoffS)

ndata_long_alters = ndata_alters_list %>%
  pivot_longer(cols = starts_with("alter"), values_to = ("alter_id")) %>%
  filter(is.na(alter_id) == 0)

ndata_long_alters_wealth = ndata_long_alters %>%
  left_join(current_wealth, by = c("round", "game", "alter_id" = "superid")) %>%
  group_by(superid, round, game) %>%
  summarize(mean_alter_wealth = mean1(cPayoffS))

tp2data = tp2data %>%
  left_join(ndata_long_alters_wealth, by = c("round", "game", "superid")) %>%
  mutate(wealth_inequal = ifelse(behavior_punish == 1 & cPayoffS < mean_alter_wealth, 1, 0))
  
# Costly punishment
tp2data = tp2data %>% 
  mutate(costly_punish = ifelse(behavior_punish == 1 & local_rate_coop_lag < 0.5, 1, 0),
         unexplained_p = ifelse(behavior_punish == 1 & wealth_inequal == 0 & retaliation_harm == 0 &
                                  costly_punish == 0, 1, 0),
         inequality_averse_neg = ifelse(behavior == "P" & wealth_inequal == 0, 1, 0),
         retaliation_neg = ifelse(behavior == "P" & retaliation_harm == 0, 1, 0),
         costly_pun_neg = ifelse(behavior == "P" & costly_punish == 0, 1, 0))

write_csv(tp2data, "~/Documents/Projects/harming_esn/data/exp2/tp2data_v1.csv")

tp2data = tp2data %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == TRUE, "Plus", "Minus"))
## Brief summary

# Number of actions
dim(tp2data)[1] #6534

# Number of players
length(unique(tp2data$superid)) #429 unique participants 

# Players per game, min, max
tp2data %>%
  group_by(game) %>%
  select(superid) %>%
  unique() %>%
  summarize(n = n()) %>%
  summarize(mean_players = mean(n),
            min_players = min(n),
            max_players = max(n)) 
# mean 14.3, min 9, max 20

# Behavior breakdown

tp2data %>% 
  group_by(time_pressure) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

tp2data %>% 
  filter(time_pressure == "Minus") %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

tp2data %>% 
  filter(time_pressure == "Plus") %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

# 33% C, 55% D, 11.8% P (708 instances)
# TP- (C/D/P): 31/59/9.65 (305 P)
# TP+ (C/D/P): 35/50/14.1 (403 P)

## Harming subtypes
## IA
tp2data %>%
  filter(time_pressure == "Minus") %>%
  group_by(wealth_inequal) %>%
  count() 

tp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(wealth_inequal) %>%
  count() 
# TP-: 228/3160 = 7.21%
# TP+: 234/2858 = 8.18%

# RE
tp2data %>%
  filter(time_pressure == "Minus") %>%
  group_by(retaliation_harm) %>%
  count() 

tp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(retaliation_harm) %>%
  count() 
# TP-: 88/3160 = 2.88%
# TP+: 201/2858 = 5.21%

# CP
tp2data %>%
filter(time_pressure == "Minus") %>%
  group_by(costly_punish) %>%
  count() 

tp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(costly_punish) %>%
  count() 
# TP-: 176/3160 - 5.56%
# TP+: 221/2858- 7.73$

# Behavior times
tp2data %>%
  filter(time_pressure == "Minus", behavior %in% c("C", "D", "P")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000))
tp2data %>%
  filter(time_pressure == "Plus", behavior %in% c("C", "D", "P")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000))
#TP-: 3.5/3.0/3.99
#TP+: 2.68/2.62/2.98

# Combine the data together
dim(tpdata_pc)
dim(tp2data)

setdiff(names(tpdata_pc), names(tp2data))
tpdata_pc = tpdata_pc %>%
  select(-c(e_behavior_punish, numeric_gameID, tp_on, reactive_p_neg, time_pressure.x, time_pressure.y, peace_individual))

tp1 = as.data.frame(tpdata_pc)
tp1$age = as.numeric(unlist(tp1$age))
tp1$gender = as.character(unlist(tp1$gender))
tp1$ipAddress = as.character(unlist(tp1$ipAddress))
tp1$race = as.character(unlist(tp1$race))
tp1$is_hispanic = as.character(unlist(tp1$is_hispanic))
tp1$satisfaction = as.character(unlist(tp1$satisfaction))
tp1 = tp1 %>% as_tibble() %>%
  mutate(behavior = case_when(behavior == "C" ~ "C",
                        behavior == "D" ~ "D",
                        behavior == "P" ~ "P",
                        TRUE ~ NA_character_),
   race = case_when(race == "[white]" ~ "White",
                    race == "[black]" ~ "Black",
                    race == "[asian]" ~ "Asian",
                    race == "[american_indian]" ~ "AmIndian",
                    TRUE ~ NA_character_))

# fixing the game and superids
tp2data_v2 = tp2data %>%
  mutate(game = game + 20, 
         superid = superid + 10000)

# create the full exp2 data
exp2data = bind_rows(tp1, tp2data_v2) 

write_csv(exp2data, file = "~/Documents/Projects/harming_esn/data/exp2/exp2data_complete.csv")

## Brief summary of full exp2

exp2data %>%
  select(gameID, time_pressure) %>%
  unique() %>%
  group_by(time_pressure) %>%
  count()

# Number of actions
dim(exp2data)[1] #10057 actions

# Number of players
length(unique(exp2data$superid)) #676 unique participants 

# Players per game, min, max
exp2data %>%
  group_by(game) %>%
  select(superid) %>%
  unique() %>%
  summarize(n = n()) %>%
  summarize(mean_players = mean(n),
            min_players = min(n),
            max_players = max(n)) 
# mean 13.5 min 6, max 20

exp2data %>%
  group_by(time_pressure) %>%
  count()

# Behavior breakdown

exp2data %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  summarize(n =n()) %>%
  mutate(perc = n/sum(n))
# 38.5/47.2/14.3 (C/D/P)

exp2data %>% 
  filter(time_pressure == "Minus") %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

exp2data %>% 
  filter(time_pressure == "Plus") %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "P")) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

# 33% C, 55% D, 11.8% P (708 instances)
# TP- (C/D/P): 38.4/47.3/14.3 (678 P)
# TP+ (C/D/P): 38.5/47.2/14.3 (642 P)

## Harming subtypes
## IA
exp2data %>%
  filter(time_pressure == "Minus", behavior_punish == 1) %>%
  group_by(wealth_inequal) %>%
  count() 

exp2data %>%
  filter(time_pressure == "Plus", behavior_punish == 1) %>%
  group_by(wealth_inequal) %>%
  count() 
# TP-: 399/5194 = 7.68%
# TP+: 375/3863 = 9.71%

# RE
exp2data %>%
  filter(time_pressure == "Minus") %>%
  group_by(retaliation_harm) %>%
  count() 

exp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(retaliation_harm) %>%
  count() 
# TP-: 267/5194 = 5.14%
# TP+: 295/3863 = 7.63%

# CP
exp2data %>%
  filter(time_pressure == "Minus") %>%
  group_by(costly_punish) %>%
  count() 

exp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(costly_punish) %>%
  count() 
# TP-: 312/5194 - 6.00%
# TP+: 354/3863 - 9.16%

# Behavior times
exp2data %>%
  filter(time_pressure == "Minus", behavior %in% c("C", "D", "P")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000))
exp2data %>%
  mutate(behavior_c_or_d = ifelse(behavior %in% c("C", "D"), 1, 0)) %>%
  filter(time_pressure == "Minus")  %>%
  group_by(behavior_c_or_d) %>%
  summarize(mean_dt = mean1(behaviorTime/1000))

exp2data %>%
  filter(time_pressure == "Plus", behavior %in% c("C", "D", "P")) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000))

file = system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
ipCountries = maxmind(exp2data$ipAddress, file, "country_name")
exp2data$country = ipCountries$country_name

exp2data %>%
  filter(time_pressure == "Minus") %>%
  group_by(country) %>%
  count()#70% US

exp2data %>%
  filter(time_pressure == "Plus") %>%
  group_by(country) %>%
  count()#75% US

#TP-: 3.6/3.2/4.2
#TP+: 3.9/2.8/3.2

### game-level punishment 

tmp = exp2data %>%
  group_by(gameID) %>%
  filter(time_pressure == "Minus") %>%
  count()

tmp2 = exp2data %>%
  group_by(gameID) %>%
  filter(behavior_punish == 1, time_pressure == "Minus") %>%
  count() %>%
  left_join(tmp, by = "gameID") %>%
  rename(p = n.x,
         total = n.y) %>%
  mutate(perc = p/total) 
  

exp2data %>%
  select(game, gameID) %>%
  unique() %>%
  view()


#######
####### regression

m0.1 = glmer(behavior_punish ~ time_pressure + factor(round) + (1|game) + (1|superid), data = tp2data, family = binomial, nAGQ=0, control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

tidy(m0.1, conf.int=TRUE, exponentiate=T) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(Term = term, Estimate = estimate, 
         LL = conf.low, UL = conf.high, p = p.value) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  select(Term, Estimate, LL, UL, p) %>%
  head(2) 

m1 = glmer(behavior_punish ~ time_pressure + factor(round) + (1|game) + (1|superid), data = exp2data, family = binomial, nAGQ=0, control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

tidy(m1, conf.int=TRUE, exponentiate=T) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(Term = term, Estimate = estimate, 
         LL = conf.low, UL = conf.high, p = p.value) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  select(Term, Estimate, LL, UL, p) %>%
  head(2) 

m2 = glmer(behavior_coop ~ time_pressure + factor(round) + (1|game) + (1|superid), data = exp2data, family = binomial, nAGQ=0, control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

tidy(m2, conf.int=TRUE, exponentiate=T) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(Term = term, Estimate = estimate, 
         LL = conf.low, UL = conf.high, p = p.value) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  select(Term, Estimate, LL, UL, p) %>%
  head(2) 

m3 = glmer(behavior_punish ~ time_pressure + age + gender + behavior_coop_lag + local_rate_coop_lag + behavior_punish_lag + local_rate_punish_lag + cPayoffS_lag + degree_lag + happ_lag +factor(round) + (1|game) + (1|superid), data = exp2data, family = binomial, nAGQ=0, control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

tidy(m3, conf.int=TRUE, exponentiate=T) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(Term = term, Estimate = estimate, 
         LL = conf.low, UL = conf.high, p = p.value) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  select(Term, Estimate, LL, UL, p) 
