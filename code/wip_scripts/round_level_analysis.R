## Round-level modeling for punishment and peace

# Previously we used a game-level definition of peace (no punishment in rds 10-
# 15). Here, we apply the definition of peace to the individual (and by proxy,
# the round-level) by considering the number of people who are in the "peace"
# condition - "peaceful" people are those whose local network has no punishment.

# Import data and packages
mean1 = function(x) {mean(x, na.rm = TRUE)}

library(tidyverse)
library(lme4)
library(magrittr)
library(igraph)

load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

# Create indicator for individual-level peace
# for an individual to be in peace, the local punishment rate should be 0 and
# the ego should not be a punisher
# therefore, set the condition to local_rate_punish_lag == 0 and behavior_punish_lag == 0

data1 %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 & 
                                     behavior_punish_lag == 0, 1, 0)) %>%
  summarize(num_ind_peace = mean1(peace_individual))

data1 = data1 %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 & 
                                     behavior_punish_lag == 0, 1, 0))

# Create the round-level dataset - aggregate the variables on the round level
# using sums/medians

rounddata = data1 %>% 
  group_by(game, round) %>%
  select(superid, game, round, showScore, age, gender, country_3cat, cPayoffS, 
         behavior_coop, behavior_defect, behavior_punish, local_rate_coop,
         local_rate_defect, local_rate_punish, behaviorTime, degree, 
         behavior_coop_lag, behavior_defect_lag, behavior_punish_lag, 
         local_rate_coop_lag, local_rate_defect_lag, local_rate_punish_lag,
         peace_individual) %>%
  mutate(gender_numeric = case_when(gender == "male" ~ 0,
                                    gender == "female" ~ 1,
                                    gender == "" ~ NA_real_),
         country_numeric = case_when(country_3cat %in% c("US", "Other") ~ 0,
                                     country_3cat == "India" ~ 1))

table(rounddata$country_3cat)
table(rounddata$country_numeric)

rounddata_for_modeling = rounddata %>%
  group_by(game, round) %>%
  filter(round != 0) %>% #ignore round 0
  summarize(peace_count = sum(peace_individual),
            mean_age = mean1(age),
            showScore = mean1(showScore),
            female_proportion = mean1(gender_numeric),
            ind_proportion = mean1(country_numeric),
            mean_cpayoff = mean1(cPayoffS),
            mean_degree = mean1(degree),
            prop_coop = mean1(behavior_coop),
            prop_def = mean1(behavior_defect),
            prop_punish = mean1(behavior_punish),
            prop_coop_lag = mean1(behavior_coop_lag),
            prop_def_lag = mean1(behavior_defect_lag),
            prop_punish_lag = mean1(behavior_punish_lag),
            mean_lr_coop = mean1(local_rate_coop),
            mean_lr_def = mean1(local_rate_defect),
            mean_lr_punish = mean1(local_rate_punish),
            mean_lr_coop_lag = mean1(local_rate_coop_lag),
            mean_lr_def_lag = mean1(local_rate_defect_lag),
            mean_lr_punish_lag = mean1(local_rate_punish_lag))
            
hist(rounddata_for_modeling$peace_count) #poisson? try linear first

# Models
m1 = glm(peace_count ~ showScore, family = "gaussian", data = rounddata_for_modeling)
summary(m1)

m2 = glm(peace_count ~ showScore + prop_coop_lag + prop_punish_lag + mean_lr_coop_lag
    + mean_lr_punish_lag, family = "gaussian", data = rounddata_for_modeling)
summary(m2)

m3 = glm(peace_count ~ showScore + mean_age + female_proportion + ind_proportion +
           mean_cpayoff + mean_degree, family = "gaussian", data = rounddata_for_modeling)
summary(m3)

plot(rounddata_for_modeling$mean_degree, rounddata_for_modeling$peace_count)
plot(rounddata_for_modeling$mean_age, rounddata_for_modeling$peace_count)
plot(rounddata_for_modeling$female_proportion, rounddata_for_modeling$peace_count)

