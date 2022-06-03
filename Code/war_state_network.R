#### Network Analysis for Peace

## We define peace as having no punishment in at least 5 (or 3?) consecutive rounds

#### Part 1 - Game level analysis ####

library(tidyverse)
library(lme4)

# Load data
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata

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

tmp = c(1, 2, 3, 4, 5)

peace_games = NULL
war_games = NULL
for(i in 1:50){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% pull(n)
  if(subseq_check(tail(tmp, 5), peace_str) == T){
    peace_games = c(peace_games, i)
  } else {
    war_games = c(war_games, i)
  }
}

length(peace_games) #14 games had peace (last 5 rounds had no punishment)

harmdata %>% filter(game == 14, round != 0) %>% group_by(round) %>% tally(behavior_punish)

punish_counts = NULL
for(i in peace_games){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% pull(n)
  punish_counts = cbind(punish_counts, tmp)
}
names(punish_counts) = as.character(peace_games)
punish_counts = as.data.frame(punish_counts)
punish_counts
punish_counts %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  ggplot(aes(x = as.numeric(rowname), y = colname, fill = value)) +
  geom_tile() +
  xlab("Round") + ylab("Game ID")

# get the number of punishments per game
pun_counts = harmdata %>% 
  group_by(game) %>%
  tally(behavior_punish) %>%
  rename(punishments = n)

netdata = harmdata %>% 
  group_by(game) %>%
  select(superid, game, round, showScore, behavior_coop, behavior_defect, 
         behavior_punish,cPayoffS, behaviorTime, degree, rate_coop, rate_rich, 
         local_gini, initial_coop, initial_defect, initial_punish, initial_local_gini, 
         initial_degree, happ, WealthLevel, country) %>%
  arrange(game, round, superid) %>%
  mutate(peace_game = ifelse(game %in% peace_games, 1, 0)) %>%
  summarize(showScore = mean(showScore),
            n = n(),
            mean_deg = mean(degree, na.rm = T),
            mean_init_deg = mean(initial_degree, na.rm = T),
            prop_punish = mean(behavior_punish, na.rm = T),
            prop_coop = mean(behavior_coop, na.rm = T),
            init_coop = mean(initial_coop, na.rm = T),
            init_punish = mean(initial_punish, na.rm = T),
            mean_gini = mean(initial_local_gini, na.rm = T),
            peace_game = mean(peace_game))

netdata1 = left_join(netdata, pun_counts, by = "game")

#### Regression models ####
## Using binary peace status as the outcome
m0 = glm(peace_game ~ showScore, data = netdata)
summary(m0)

m1 = glm(peace_game ~ showScore + mean_deg +  mean_init_deg + prop_punish + prop_coop + init_punish + init_coop + mean_gini, data = netdata)
summary(m1)

m2 = glm(peace_game ~ showScore + mean_gini + mean_init_deg, data = netdata)
summary(m2)

xtabs(~peace_game + showScore, netdata)

## Using the number of non-peace rounds as the outcome
m3 = lm(punishments ~ showScore, data = netdata1)
summary(m3)

m4 = lm(punishments ~ showScore + mean_gini + mean_init_deg, data = netdata1)
summary(m4)

m5 = lm(punishments ~ showScore + mean_deg +  mean_init_deg + 
         prop_punish + prop_coop + init_punish + init_coop + mean_gini, 
       data = netdata1)
summary(m5)

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

table(round_outcomes$peace_round)

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




    