## -----------------------------------------------------------------------------
## Script name: punish_categorization.R
##
## Purpose of script: To compare punishment decision times across games
##
## Author: George Dewey
##
## Date Created: 2024-07-07
##
## Last Updated: 2024-07-07
## -----------------------------------------------------------------------------

## Load packages 
library(tidyverse)
library(lme4)
library(lmerTest)
library(stats)
library(asympTest)
library(patchwork)

mean1 = function(x) {mean(x,na.rm=TRUE)}
median1 = function(x) {median(x, na.rm = TRUE)}
sd1 = function(x) {sd(x, na.rm = TRUE)}
se_mean = function(x) sd1(x)/sqrt(sum(is.na(x) == 0))

## Load data
exp1data = read_csv("~/Documents/Projects/harming_esn/data/final/exp1data_final.csv",
                    show_col_types = FALSE)

## Experiment 2
exp2data = read_csv("~/Documents/Projects/harming_esn/data/final/exp2data_final.csv", 
                    show_col_types = FALSE)

## Evaluating the association between punishment in the local environment and 
## decision time of punishment decisions
summary(lmer(behaviorTime_sec ~ local_rate_punish_lag + round + (1|superid) +
               (1|game), data = exp1data %>% filter(round > 0, 
                                                    behavior_punish == 1)))

summary(lmer(behaviorTime_sec ~ local_rate_punish_lag + round + (1|superid) +
               (1|game), data = exp2data %>% filter(round > 0,
                                                    time_pressure == 'Minus',
                                                    behavior_punish == 1)))

summary(lmer(behaviorTime_sec ~ local_rate_punish_lag + round + (1|superid) +
               (1|game), data = exp2data %>% filter(round > 0,
                                                    time_pressure == 'Plus',
                                                    behavior_punish == 1)))

## The association is not consistent - there is sign of statistical association
## in Experiment 1, but not in either arm of Experiment 2.

punish_counts_exp1 = exp1data %>% filter(round > 0) %>%
  group_by(game) %>% summarize(total_punish_per_game = sum(behavior_punish)) %>%
  select(total_punish_per_game) %>% pull()

exp1_ranked_punish = tibble(game = 1:50,
       punish_counts = punish_counts_exp1) %>%
      mutate(punish_rank = case_when(punish_counts < 7 ~ 'Low',
                                 punish_counts >= 7 & punish_counts < 13 ~ 'Medium',
                                 punish_counts >= 13 ~ 'High')) 

exp1_ranked_punish

quantile(punish_counts_exp1, c(0.33, 0.66, 1)) # Quantiles
        
exp1data = exp1data %>% left_join(exp1_ranked_punish, by = 'game') 


summary(lmer(behaviorTime_sec ~ behavior + round + (1|superid) +
               (1|game), data = exp1data %>% filter(round > 0,
                                                    punish_rank == 'Low')))

summary(lmer(behaviorTime_sec ~ behavior + round + (1|superid) +
               (1|game), data = exp1data %>% filter(round > 0,
                                                    punish_rank == 'Medium')))

summary(lmer(behaviorTime_sec ~ behavior + round + (1|superid) +
               (1|game), data = exp1data %>% filter(round > 0,
                                                    punish_rank == 'High')))

resp_f1_e1 = exp1data %>% 
  filter(round > 0, behavior %in% c('C', 'D', 'P')) %>%
  mutate(punish_rank_fct = 
           factor(punish_rank, levels = c('Low', 'Medium', 'High'))) %>%
  group_by(punish_rank_fct, behavior) %>%
  summarize(mean_dt = mean(behaviorTime_sec, na.rm = T),
            se_mean_dt = seMean(behaviorTime_sec, na.rm = T)) %>%
  ggplot(aes(x = behavior, y = mean_dt, fill = behavior)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_dt - se_mean_dt, 
                    ymax = mean_dt + se_mean_dt, color = behavior), 
                width = 0.25) +
  facet_wrap(~punish_rank_fct) +
  scale_fill_manual(values = c("#00A5CF", "#FFBF00", "#574AE2")) +
  scale_color_manual(values = c("dodgerblue4", "darkorange3", "#574AE2")) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 5, 10)) +
  labs(x = 'Behavior', y = 'Decision Time \n(seconds)', tag = '1') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

## Experiment 2, TP-
punish_counts_exp2_tp_minus = exp2data %>% 
  filter(round > 0, time_pressure == 'Minus') %>%
  group_by(game) %>% 
  summarize(total_punish_per_game = sum(behavior_punish)) %>%
  select(total_punish_per_game) %>% pull()

quantile(punish_counts_exp2_tp_minus, c(0.33, 0.66, 1))

exp2_tp_minus_ranked_punish = tibble(game = 1:25,
                            punish_counts = punish_counts_exp2_tp_minus) %>%
  mutate(punish_rank = case_when(punish_counts < 10 ~ 'Low',
                                 punish_counts >= 10 & punish_counts < 14 ~ 'Medium',
                                 punish_counts >= 14 ~ 'High')) 

exp2_tp_minus_ranked_punish

resp_f1_e2_minus = exp2data %>% 
  filter(round > 0, time_pressure == 'Minus',
         behavior %in% c('C', 'D', 'P')) %>%
  left_join(exp2_tp_minus_ranked_punish) %>%
  mutate(punish_rank_fct = 
           factor(punish_rank, levels = c('Low', 'Medium', 'High'))) %>%
  group_by(punish_rank_fct, behavior) %>%
  summarize(mean_dt = mean1(behaviorTime_sec),
            se_mean_dt = se_mean(behaviorTime_sec)) %>%
  ggplot(aes(x = behavior, y = mean_dt, fill = behavior)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_dt - se_mean_dt, 
                    ymax = mean_dt + se_mean_dt, color = behavior), 
                width = 0.25) +
  facet_wrap(~punish_rank_fct) +
  scale_fill_manual(values = c("#00A5CF", "#FFBF00", "#574AE2")) +
  scale_color_manual(values = c("dodgerblue4", "darkorange3", "#574AE2")) +
  scale_y_continuous(limits = c(0, 5), breaks = c(0, 2.5, 5)) +
  theme_minimal() +
  labs(x = 'Behavior', y = 'Decision Time \n(seconds)', tag = '2,TP-') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

resp_f1_e2_minus

## Exp.2 , TP+
punish_counts_exp2_tp_plus = exp2data %>% 
  filter(round > 0, time_pressure == 'Plus') %>%
  group_by(game) %>% 
  summarize(total_punish_per_game = sum(behavior_punish)) %>%
  select(total_punish_per_game) %>% pull()

quantile(punish_counts_exp2_tp_plus, c(0.33, 0.66, 1))

exp2_tp_plus_ranked_punish = 
  tibble(game = 26:50, punish_counts = punish_counts_exp2_tp_plus) %>%
  mutate(punish_rank = case_when(punish_counts < 6 ~ 'Low',
                                 punish_counts >= 6 & 
                                   punish_counts < 19 ~ 'Medium',
                                 punish_counts >= 19 ~ 'High')) 

resp_f1_e2_plus = exp2data %>% 
  filter(round > 0, time_pressure == 'Plus',
         behavior %in% c('C', 'D', 'P')) %>%
  left_join(exp2_tp_plus_ranked_punish) %>%
  mutate(punish_rank_fct = 
           factor(punish_rank, levels = c('Low', 'Medium', 'High'))) %>%
  group_by(punish_rank_fct, behavior) %>%
  summarize(mean_dt = mean1(behaviorTime_sec),
            se_mean_dt = se_mean(behaviorTime_sec)) %>%
  ggplot(aes(x = behavior, y = mean_dt, fill = behavior)) +
  geom_bar(stat = 'identity', alpha = 0.5) +
  geom_errorbar(aes(ymin = mean_dt - se_mean_dt, 
                    ymax = mean_dt + se_mean_dt, color = behavior), 
                width = 0.25) +
  facet_wrap(~punish_rank_fct) +
  scale_fill_manual(values = c("#00A5CF", "#FFBF00", "#574AE2")) +
  scale_color_manual(values = c("dodgerblue4", "darkorange3", "#574AE2")) +
  scale_y_continuous(breaks = c(0, 1.5, 3), limits = c(0, 3)) +
  theme_minimal() +
  labs(x = 'Behavior', y = 'Decision Time \n(seconds)', tag = '2,TP+') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

resp_f1_e2_minus

resp_f1_e1/resp_f1_e2_minus/resp_f1_e2_plus 
