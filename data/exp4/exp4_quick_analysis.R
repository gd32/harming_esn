setwd("~/Documents/Projects/harming_esn/Data/exp4/")
load("exp4_mdata3.Rdata")

library(tidyverse)

harmdata = mdata3 %>% as_tibble() %>%
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "P"),
         time_pressure = ifelse(str_detect(gameID, "TP") == T, "Plus", "Minus"))

harmdata$timeUp = unlist(harmdata$timeUp.x)

timeout_actions = harmdata %>%
  filter(time_pressure == "Plus", timeUp == TRUE, round > 0) %>%
  group_by(game) %>%
  count() %>%
  rename(timeouts = n)

total_actions = harmdata %>%
  filter(time_pressure == "Plus", round > 0) %>%
  group_by(game) %>%
  count() %>%
  rename(total = n)
  
# timeouts: the rate of timeout per game
timeouts = left_join(timeout_actions, total_actions, by = "game") %>%
  mutate(perc_timeout = timeouts/total)

# the overall percentage of timeouts
timeouts %>% 
  ungroup() %>%
  summarize(mean_timeout = mean(perc_timeout))

# figures: decision time density by behavior in TP-
harmdata %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == T, "Plus", "Minus")) %>%
  filter(time_pressure == "Minus") %>%
  ggplot() +
  geom_density(aes(x = behaviorTime/1000, group = behavior, color = behavior)) +
  xlab("Behavior Time") +
  ylab("Density") +
  scale_x_log10(limits = c(1, 30))

harmdata %>%
  filter(time_pressure == "Minus", round > 0, is.na(behavior) == FALSE) %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000),
            se_mean_dt = se_mean(behaviorTime/1000),
            median_dt = median1(behaviorTime/1000))

harmdata %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == T, "Plus", "Minus")) %>%
  filter(time_pressure == "Plus") %>%
  ggplot() +
  geom_density(aes(x = behaviorTime/1000, group = behavior, color = behavior)) +
  xlab("Behavior Time") +
  ylab("Density") + 
  scale_x_log10(limits = c(1, 3), breaks = c(1, 2, 3))

harmdata %>% 
  ggplot() +
  geom_density(aes(x = behaviorTime/1000, group = behavior, color = behavior)) +
  xlab("Behavior Time") +
  ylab("Density")

harmdata$timeUp = unlist(harmdata$timeUp.x)

harmdata %>%
  filter(round > 0, time_pressure == "Plus") %>%
  group_by(timeUp) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = n/sum(n))

harmdata %>%
  filter(round > 0, timeUp == TRUE) %>%
  group_by()

harmdata %>%
  group_by(time_pressure) %>%
  summarize(perc_c = mean(behavior_coop, na.rm = T),
            perc_d = mean(behavior_defect, na.rm = T),
            perc_h = mean(behavior_punish, na.rm = T))


harmdata %>%
  summarize(perc_c = mean(behavior_coop, na.rm = T),
            perc_d = mean(behavior_defect, na.rm = T),
            perc_h = mean(behavior_punish, na.rm = T))

library(lme4)
library(broom.mixed)

m22 = glmer(behavior_punish ~ time_pressure + factor(round) + (1|game) + (1|superid), data = harmdata, family = binomial, nAGQ=0, control = glmerControl(optimizer = c("bobyqa"), optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

tidy(m22, conf.int=TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  rename(Term = term, Estimate = estimate, 
         LL = conf.low, UL = conf.high, p = p.value) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  select(Term, Estimate, LL, UL, p) %>%
  head(2)
