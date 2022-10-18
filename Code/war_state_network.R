#### Network Analysis for Peace

## We define peace as having no punishment in at least 5 (or 3?) consecutive rounds
# Factors we think are relevant
# main - wealth visibility (there is a difference, albeit at the N=50 (network) level)
# the number of punishments (cumulative)


# Feature selection for factors contributing to peace
# Want features that apply on both the individual and network level

# FEATURES ALREADY IN THE NETWORK MODEL
# 

# try - the number of punishments in rounds 5-10
# the number of defections? 
# try - the number of defections in rounds 5-10
# can also try the number of coops
# the number of people with long sequences of C? (i.e. 3+ C in a row)
# the number of people with long sequences of P?

#### Part 1 - Game level analysis ####

rm(list = ls())

library(tidyverse)
library(lme4)
library(magrittr)
library(igraph)

# Load data
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

# Comparing local rate of punishment and # of punishers
data1 %>% 
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "P"),
         behavior_numeric = case_when(behavior == "C" ~ 1, 
                                      behavior == "D" ~ 2,
                                      behavior == "P" ~ 3)) %>%
  group_by(game, round) %>%
  arrange(game, round) %>%
  select(local_rate_punish_lag, behavior_punish_lag) %>%
  summarize(mean_local_rate = mean(local_rate_punish_lag), 
            total_punish_lag = sum(behavior_punish_lag)) %>%
  filter(is.na(mean_local_rate) == F) %>%
  ggplot() +
  geom_point(aes(x= mean_local_rate, y = total_punish_lag))

data2 = data1 %>% mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                                             behavior_defect == 1 ~ "D",
                                             behavior_punish == 1 ~ "P"),
                        behavior_numeric = case_when(behavior == "C" ~ 1, 
                                                     behavior == "D" ~ 2,
                                                     behavior == "P" ~ 3))
  

# First checking how many rounds have a punishment choice
peace_rounds = NULL
punish_rounds = NULL
for(i in 1:50){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% group_by(n) %>% summarize(round_count = n())
  peace_rounds = c(peace_rounds, 
                   sum(tmp %>% filter(n == 0) %>% pull(round_count)))
  punish_rounds = c(punish_rounds, 
                    sum(tmp %>% filter(n != 0) %>% pull(round_count)))
}
sum(peace_rounds) #377 rounds with no punish choice
sum(punish_rounds) #373 rounds with at least 1 punish choice

tmp = harmdata %>% filter(game == 1, round !=0) %>% group_by(round) %>% tally(behavior_punish) %>% pull(n)

subseq_check <- function(x,y) grepl(toString(y),toString(x),fixed = TRUE)
subseq_check(tmp, c("0", "0", "0", "0", "0"))
peace_str = c("0", "0", "0", "0", "0")

peace_games = NULL
war_games = NULL
for(i in 1:50){
  for(j in 5:11){
    tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% tally(behavior_punish) %>% pull(n)
    if(subseq_check(tmp[j:j+4], peace_str) == T){
      peace_games = c(peace_games, i)
    } else {
      war_games = c(war_games, i)
    }
  }
}


peace_games = NULL
war_games = NULL
for(i in 1:50){
for(j in 5:11){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% tally(behavior_punish) %>% pull(n)
  if(subseq_check(tmp[j:(j+4)], peace_str) == T){
    peace_games = c(peace_games, i)
  } else {
    war_games = c(war_games, i)
  }
  }
}
peace_games
length(unique(peace_games)) # Under the definition of having any 5 consequtive rounds of no punishment, 18 games had peace

# Old peace definition: # length(peace_games) #14 games had peace (last 5 rounds had no punishment)
# peace_games = NULL
# war_games = NULL
# for(i in 1:50){
#   tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
#     tally(behavior_punish) %>% pull(n)
#   if(subseq_check(tail(tmp, 5), peace_str) == T){
#     peace_games = c(peace_games, i)
#   } else {
#     war_games = c(war_games, i)
#   }
# }
# # 1 5 7 8 9 13 14 15 16 20 26 28 37 45
# length(peace_games) #14 games had peace (last 5 rounds had no punishment)

punish_counts = NULL
for(i in peace_games){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% pull(n)
  punish_counts = cbind(punish_counts, tmp)
}
names(punish_counts) = as.character(peace_games)

punish_counts = as.data.frame(punish_counts)
punish_counts %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  ggplot(aes(x = as.numeric(rowname), y = colname, fill = value)) +
  geom_tile() +
  xlab("Round") + ylab("Game ID")

# get the number of punishments per game excluding the last 5 rounds
pun_counts = harmdata %>% 
  group_by(game) %>%
  filter(round %in% 1:10) %>%
  tally(behavior_punish) %>%
  rename(punishments = n)

coop_df = harmdata %>%
  group_by(game) %>%
  filter(round %in% 1:10) %>%
  tally(behavior_coop) %>%
  rename(coop = n) 

defect_df = harmdata %>%
  group_by(game) %>%
  filter(round %in% 1:10) %>%
  tally(behavior_defect) %>%
  rename(defects = n) 

count_df = left_join(pun_counts, coop_df, by = "game") %>%
  left_join(defect_df, by = "game")

visible_df = harmdata %>%
  group_by(game) %>%
  tally(showScore) %>%
  rename(wealth_visible = n)

# Creating the network features

names(harmdata)

# can use gender, mean payoff, avg weatlh variables (PosWeatlh, WeatlhLevel), rank?, country

netdata = harmdata %>% 
  group_by(game) %>%
  select(superid, game, round, showScore, behavior_coop, behavior_defect, 
         behavior_punish,cPayoffS, behaviorTime, degree, rate_rich, 
         local_gini, initial_coop, initial_defect, initial_punish, initial_local_gini, 
         initial_degree, happ, WealthLevel, country) %>%
  arrange(game, round, superid) %>%
  mutate(peace_game = ifelse(game %in% peace_games, 1, 0)) %>%
  summarize(showScore = mean(showScore),
            n = n(), # N is the number of game actions
            mean_deg = mean(degree, na.rm = T), # The mean degree across all rounds? can do individual rounds
            mean_init_deg = mean(initial_degree, na.rm = T),  #Mean initial degree of the game
            prop_punish = mean(behavior_punish, na.rm = T), #The proportion of punishing across all game actions
            prop_coop = mean(behavior_coop, na.rm = T), # The proportion of coop across all game actions
            init_coop = mean(initial_coop, na.rm = T), #The proportion of initial cooperators
            init_punish = mean(initial_punish, na.rm = T), # The proportion of initial punishers
            mean_gini = mean(initial_local_gini, na.rm = T), # The average initial gini at the start of the game
            peace_game = mean(peace_game)) # The outcome variable 

# To do: mean degree across each round by game, mean payoff at the end of each game, 

netdata1 = left_join(netdata, count_df, by = "game")
netdata2 = netdata1 %>% left_join(count_df, by = "game")


#### Regression models ####
## Using binary peace status as the outcome
m0 = glm(peace_game ~ showScore, data = netdata1, family = "binomial")
summary(m0)

m1 = glm(peace_game ~ showScore + punishments + coop + defects, data = netdata1, family = "binomial")
summary(m1)

m1.1 = glm(peace_game ~ showScore + punishments, data = netdata1, family = "binomial")
summary(m1.1)
m1.2 = glm(peace_game ~ showScore + coop, data = netdata1, family = "binomial")

summary(m1.2)

m1.3 = glm(peace_game ~ showScore + defects, data = netdata1, family = "binomial")
summary(m1.3)

m2 = glm(peace_game ~ showScore + punishments + coop + defects + mean_deg + mean_gini +
           init_coop + init_punish, data = netdata1,
         family = "binomial")
summary(m2)

m3 = glm(peace_game ~ showScore + mean_gini + mean_init_deg, data = netdata1)
summary(m3)

m4 = glm(peace_game ~ showScore + punishments + coop + defects + mean_deg + mean_gini,
         data  = netdata1, family = "binomial")
summary(m4)

m4.1 = glm(peace_game ~ showScore + punishments + defects + mean_deg + mean_gini,
         data  = netdata1, family = "binomial")
summary(m4.1)

## Using the number of non-peace rounds as the outcome
m5 = lm(punishments ~ showScore, data = netdata1)
summary(m5)

m6 = lm(punishments ~ showScore + mean_gini + mean_init_deg, data = netdata1)
summary(m6)

m7 = glm(punishments ~ showScore + mean_deg + mean_init_deg + init_punish + init_coop, data = netdata1)

m8 = lm(punishments ~ showScore + mean_deg +  mean_init_deg + 
         prop_punish + prop_coop + init_punish + init_coop + mean_gini, 
       data = netdata1)
summary(m5)

# New reseults generally make more sense - no effect of wealth visibility on "peacefulness" as the balance is more even

xtabs(~showScore + peace_game, netdata1)






#### Part 2 - Round Level Analysis
peace_rounds = NULL
punish_rounds = NULL
for(i in 1:50){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% group_by(n) %>% summarize(round_count = n())
  peace_rounds = c(peace_rounds, 
                   sum(tmp %>% filter(n == 0) %>% pull(round_count)))
  punish_rounds = c(punish_rounds, 
                    sum(tmp %>% filter(n != 0) %>% pull(round_count)))
}

round_outcomes = harmdata %>%
  arrange(superid, game, round) %>%
  group_by(game, round) %>%
  tally(behavior_punish) %>%
  mutate(peace_round = ifelse(n == 0, 1, 0))

mean1 = function(x) {mean(x,na.rm=TRUE)} 

names(tmp)
rounddata = harmdata %>% arrange(superid, game, round) %>%
  group_by(game, round) %>%
  left_join(round_outcomes) %>%
  summarize(showScore = mean(showScore),
            mean_coop = mean1(behavior_coop),
            mean_punish = mean1(behavior_punish),
            mean_local_gini = mean1(local_gini),
            mean_degree = mean1(degree),
            mean_init_coop = mean1(initial_coop),
            mean_init_punish = mean1(initial_punish),
            mean_init_wealth = mean1(initial_local_avg_wealth),
            mean_init_deg = mean1(initial_degree),
            peace_round = mean(peace_round))

## Regression models

#crude
m3 = glmer(peace_round ~ showScore + (1|game), data = rounddata, 
           family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"),
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

se = sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se, UL = fixef(m3) + 1.96 * se)
round(exp(tab), digits = 3)

#full
m4 = glmer(peace_round ~ showScore + mean_coop + mean_punish + mean_local_gini + 
           mean_init_coop + mean_init_punish + (1|game),
           data = rounddata, 
         family = binomial, nAGQ=0, 
         control = glmerControl(optimizer = c("bobyqa"),
                                optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

se = sqrt(diag(vcov(m4)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se, UL = fixef(m4) + 1.96 * se)
round(exp(tab), digits = 3)

## Part 2 - Evaluating the difference in result between individual and network-based analysis

## Try excluding the games with heavy punishment

pun_counts %>% arrange(-punishments) # Exclude games 22 and 46 since they have the strongest punish pattern

## Do the main regression analysis excluding those games
rm(list = ls())
load("~/Documents/Projects/harming_esn/Data/data1.Rdata")

data1$local_rate_punish_cat4 = 
  case_when(data1$local_rate_punish_lag >=0 & data1$local_rate_punish_lag <= 0.05 ~ 0,
            data1$local_rate_punish_lag >0.05 & data1$local_rate_punish_lag <= 0.1 ~ 1,
            data1$local_rate_punish_lag > 0.1 & data1$local_rate_punish_lag <= 0.15 ~ 2,
            data1$local_rate_punish_lag > 0.15 & data1$local_rate_punish_lag <= 0.2 ~ 3,
            data1$local_rate_punish_lag > 0.2 & data1$local_rate_punish_lag <= 0.25 ~ 4,
            data1$local_rate_punish_lag > 0.25 ~ 5)

data1$local_rate_coop_cat4 = 
  case_when(data1$local_rate_coop_lag > 0 & data1$local_rate_coop_lag <= 0.3 ~ 4,
            data1$local_rate_coop_lag > 0.3 & data1$local_rate_coop_lag <= 0.5 ~ 3,
            data1$local_rate_coop_lag > 0.5 & data1$local_rate_coop_lag <= 0.7 ~ 2,
            data1$local_rate_coop_lag > 0.7 & data1$local_rate_coop_lag <= 0.9 ~ 1,
            data1$local_rate_coop_lag > 0.9 ~ 0)


# create a cumulative counter of behaviors
names(data1)

tmp = data1 %>%
  group_by(superid, game, round) %>%
  select(superid, game, round, behavior_punish, behavior_defect, behavior_coop)

cum_data1 = tmp %>%
  ungroup() %>%
  group_by(superid, game, behavior_punish) %>%
  mutate(punish = ifelse(behavior_punish == 0, NA, 1),
         coop = ifelse(behavior_coop == 0, NA, 1),
         defect = ifelse(behavior_coop == 0 , NA, 1)) %>%
  mutate(cumulative_punish = row_number(punish),
         cumulative_coop = row_number(coop),
         cumulative_defect = row_number(defect)) %>%
  mutate(cumulative_punish = ifelse(is.na(cumulative_punish) == T, 0, cumulative_punish),
         cumulative_coop = ifelse(is.na(cumulative_coop) == T, 0, cumulative_coop),
         cumulative_defect = ifelse(is.na(cumulative_defect) == T, 0, cumulative_defect)) %>%
  select(superid, game, round, starts_with("cum"))

data2 = left_join(data1, cum_data1, by = c("superid", "game", "round")) %>% select(-behavior_punish.y) %>%
  rename(behavior_punish = behavior_punish.x)

m1 = glmer(behavior_punish ~ showScore + age + gender + behavior_coop_lag + local_rate_coop_lag + 
             behavior_punish_lag + factor(local_rate_punish_cat4) + cPayoffS_lag + degree_lag + happ_lag + 
             cumulative_punish + cumulative_coop + cumulative_defect +
             factor(round) + (1|game) + (1|superid),
           data = data2, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 * se)
round(exp(tab), digits = 3)[1:15,]





data1_cc_tmp = data1_cc %>% filter(!(game %in% c(22, 46)))

# No longer significant using punish lag categories
m1 = glmer(behavior_punish ~ showScore + age + gender + behavior_coop_lag + local_rate_coop_lag + 
             behavior_punish_lag + factor(local_rate_punish_cat4) + cPayoffS_lag + degree_lag + happ_lag + 
             factor(round) + (1|game) + (1|superid),
           data = data1_cc_tmp, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 * se)
round(exp(tab), digits = 3)[1:15,]

names(data1_cc_tmp)

# Significance is lost when adding the coop categories + losing the heavy punish rounds
m2 = glmer(behavior_punish ~ showScore + age + gender + behavior_coop_lag + factor(local_rate_coop_cat4) + 
           behavior_punish_lag + factor(local_rate_punish_cat4) + cPayoffS_lag + degree_lag + happ_lag + 
           factor(round) + (1|game) + (1|superid),
           data = data1_cc_tmp, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m2), LL = fixef(m2) - 1.96 * se, UL = fixef(m2) + 1.96 * se)
round(exp(tab), digits = 3)[1:18,]

# ShowScore never becomes significant

# Is there something special about those two rounds?

# Could you also make a two by two table of #punishments and non-punishment (c and d) between inv and vis over the final 5 rounds? And simple testing?

pun_df_last5 = harmdata %>%
  group_by(game) %>%  
  filter(round %in% 11:15) %>%
  tally(behavior_punish) %>%
  rename(punish = n)

coop_df_last5 = harmdata %>%
  group_by(game) %>%
  filter(round %in% 11:15) %>%
  tally(behavior_coop) %>%
  rename(coop = n) 

defect_df_last5 = harmdata %>%
  group_by(game) %>%
  filter(round %in% 11:15) %>%
  tally(behavior_defect) %>%
  rename(defects = n) 

count_df_last5 = left_join(pun_df_last5, coop_df_last5, by = "game") %>%
  left_join(defect_df_last5, by = "game")

visible_df = harmdata %>% group_by(game) %>% count(showScore) %>%
  select(game, showScore) %>%
  rename(vis = showScore)

count_df_last5_final = count_df_last5 %>% 
  left_join(visible_df, by = "game") %>%
  mutate(non_punish = coop+defects)

test_df = count_df_last5_final %>%
  group_by(vis) %>%
  summarize(total_pun = sum(punish),
            total_non_pun = sum(non_punish))
test_df2 = test_df %>% column_to_rownames(var = "vis") 
test_df2
chisq.test(test_df2)

# Could you also make the 50x5 table: 50 for 50 games (1-25: vis, 26-50: inv) x rounds 11-15. Each cell contains #punishments
tmp1 = harmdata %>%  
  filter(round %in% 11:15) %>%
  group_by(game, round) %>%
  tally(behavior_punish) %>%
  rename(punish = n)

tmp2 = harmdata %>%
  filter(round %in% 11:15) %>%
  group_by(game, round) %>%
  tally(behavior_coop) %>%
  rename(coop = n) 

tmp3 = harmdata %>%
  filter(round %in% 11:15) %>%
  group_by(game, round) %>%
  tally(behavior_defect) %>%
  rename(defects = n) 

tmp_big = left_join(tmp1, tmp2, by = c("game", "round")) %>%
  left_join(tmp3, by = c("game", "round")) %>%
  left_join(visible_df, by = "game")

tmp4 = tmp_big %>% group_by(game, round) %>% summarize(n_punish = sum(punish)) %>%
  left_join(visible_df, by = "game")

tmp5 = tmp4 %>%
  group_by(game, round) %>%
  summarize(n_punish = sum(n_punish)) 

tmp6 = as.data.frame(t(tmp5 %>% spread(key = game, value = n_punish)))

names(tmp6) = c("11", "12", "13", "14", "15")
tmp7 = tmp6 %>% rownames_to_column(var = "game") %>%
  filter(game != "round") %>%
  mutate(game = as.numeric(game)) %>%
  left_join(visible_df, by = "game")

tmp8 = tmp7 %>% select(-"15") %>%
  filter(game %in% 1:25)
colSums(tmp8 == 0)
#14+14+16+17 (61)

tmp9 = tmp7 %>% select(-"15") %>%
  filter(game %in% 26:50)
colSums(tmp9 == 0)
view(tmp9)
#11+12+17+14 (66)

# Testing for the difference between visible and invisible conditions
# - limit the data to rounds where the previous round has no punishment
round_outcomes = data1_cc %>%
  arrange(superid, game, round) %>%
  group_by(game, round) %>%
  tally(behavior_punish) %>%
  mutate(peace_round = ifelse(n == 0, 1, 0))
peace_rds = round_outcomes %>% filter(peace_round == 1, round %in% 1:14) %>% select(game, round)
peace_rds = peace_rds %>% mutate(peace_rds_str = 1000+as.numeric(paste0(game, round)))
peace_rds

round_outcomes %>% filter(game == 3, round == 13)
peace_rds %>% filter(peace_rds_str == 1313)
data1_cc %>% filter(game == 3, round == 14)
data1_cc2 %>% filter(game == 3, round == 13)

data1_cc2 %>% filter(game_round_str %in% peace_rds$peace_rds_str)

data1_cc2 = data1_cc %>% mutate(round_lag = round - 1,
                                game_round_str = 1000+as.numeric(paste0(as.character(game), as.character(round_lag))))

data1_cc2
# data1_cc3 only include obs from game rounds where previous round has no punishment
data1_cc3 = data1_cc2 %>% filter(game_round_str %in% peace_rds$peace_rds_str & behavior_punish_lag == 0 & local_rate_punish_lag == 0)

# crude model with showScore only
m4 = glmer(behavior_punish ~ showScore + factor(round) + (1|game) + (1|superid),
                data = data1_cc3, family = binomial, nAGQ=0, 
                control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m4)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m4), LL = fixef(m4) - 1.96 * se, UL = fixef(m4) + 1.96 * se)
round(exp(tab), digits = 3)[1:2,]

# full model
m5 = glmer(behavior_punish ~ showScore + age + gender + behavior_coop_lag + local_rate_coop_lag + 
             behavior_punish_lag + local_rate_punish_lag + cPayoffS_lag + degree_lag + happ_lag + 
             factor(round) + (1|game) + (1|superid),
           data = data1_cc3, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m5)))
# table of estimates with 95% CI
tab = cbind(Est = fixef(m5), LL = fixef(m5) - 1.96 * se, UL = fixef(m5) + 1.96 * se)
round(exp(tab), digits = 3)[1:9,]




