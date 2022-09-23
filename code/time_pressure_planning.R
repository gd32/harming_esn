# For new time limit breadboard experiment planning

# Calculating the decision time distribution

mean1 = function(x) {mean(x, na.rm = TRUE)}

rm(list = ls())

library(tidyverse)
library(lme4)
library(magrittr)
library(igraph)

load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

names(data1)

dt_data = data1 %>%
  group_by(superid) %>%
  summarize(mean_dt = mean1(behaviorTime),
            mean_log_dt = mean1(log(behaviorTime)))

dt_coop = data1 %>%
  group_by(superid) %>%
  filter(behavior_coop == 1) %>%
  summarize(mean_dt_coop = mean1(behaviorTime),
            mean_log_dt_coop = mean1(log(behaviorTime)))

dt_def = data1 %>%
  group_by(superid) %>%
  filter(behavior_defect == 1) %>%
  summarize(mean_dt_defect = mean1(behaviorTime),
            mean_log_dt_defect = mean1(log(behaviorTime)))

dt_punish = data1 %>%
  group_by(superid) %>%
  filter(behavior_punish == 1) %>%
  summarize(mean_dt_punish = mean1(behaviorTime),
            mean_log_dt_punish = mean1(behaviorTime))

dt_non_punish = data1 %>%
  group_by(superid) %>%
  filter(behavior_punish == 0) %>%
  summarize(mean_dt_nonpunish = mean1(behaviorTime),
            mean_log_dt_nonpunish = mean1(log(behaviorTime)))

dt_data %>% 
  ggplot() +
  geom_histogram(aes(x = mean_log_dt), binwidth = 0.25) +
  xlab("log Decision Time (s)")+
  ylab("Frequency")

dt_data_by_behavior = left_join(dt_coop, dt_def) %>%
  left_join(dt_punish) %>%
  left_join(dt_non_punish)

dt_data_by_behavior %>% 
  select(mean_log_dt_punish, mean_log_dt_nonpunish) %>%
  mutate(dt_diff = mean_log_dt_punish - mean_log_dt_nonpunish) %>%
  filter(is.na(dt_diff) == 0) %>%
  ggplot() +
  geom_histogram(aes(x = dt_diff), bins = 15) +
  xlab("Difference in decision time (punish - nonpunish)") +
  ylab("Frequency")

