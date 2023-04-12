# Testing the "collateral damage" theory of harming in networks

# Checking for a dose-response relationship between decision time and the number of non-harmers
# x-axis: the number of non-punishers in the last round?
# alternative 1: the number of non-RP users in the last round? (issue: includes other punishers)
# alternative 2: the number of RP users in the last round? (issue: not every round has these)?

data1_pc = data1_pc %>%
  mutate(collateral_rate_lag = 1 - local_rate_punish_lag)

# crude
data1_pc %>%
  ggplot() +
  geom_point(aes(x = local_rate_punish_lag, y = behaviorTime_sec))
  
# exclude the people who only had one or no neighbors
data1_pc %>%
  filter(degree_lag > 1, is.na(degree_lag) == 0) %>%
  ggplot() +
  geom_point(aes(x = local_rate_punish_lag, y = behaviorTime_sec))
# this one didn't seem to make much difference, so leave the excluded data points in

# inverting the x-axis since we want it to go from least collateral to most
data1_pc %>%
  mutate(collateral_rate_lag = 1 - local_rate_punish_lag) %>%
  ggplot() + 
  geom_point(aes(x = collateral_rate_lag, y = behaviorTime_sec))

# including only harming - exp1
d1 = data1_pc %>%
  filter(behavior_punish == 1, is.na(degree_lag) == 0, degree > 1) %>%
  ggplot(aes(x = collateral_rate_lag, y = behaviorTime_sec)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)  + 
  theme_classic()
# p = 0.079 for linear trend

# do the same for tp+ and tp-?
tpdata_pc = tpdata_pc %>%
  mutate(collateral_rate_lag = 1 - local_rate_punish_lag,
         behaviorTime_sec = behaviorTime/1000)

# tp_on
d2 = tpdata_pc %>%
  filter(behavior_punish == 1, is.na(degree_lag) == 0, tp_on == 0, degree > 1) %>%
  ggplot(aes(x = collateral_rate_lag, y = behaviorTime_sec)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) + 
  theme(axis.title.y = element_blank()) + 
  theme_classic()

lmer(behaviorTime_sec ~ collateral_rate_lag + (1|game) + (1|superid),
     data = tpdata_pc %>% 
       filter(behavior_punish == 1, is.na(degree_lag) == 0, tp_on == 1)) %>%
  tidy() # p for linear model < 0.001

# tp_off
d3 = tpdata_pc %>%
  filter(behavior_punish == 1, is.na(degree_lag) == 0, tp_on == 1, degree > 1) %>%
  ggplot(aes(x = collateral_rate_lag, y = behaviorTime_sec)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme(axis.title.y = element_blank()) + theme_classic()

lmer(behaviorTime_sec ~ collateral_rate_lag + (1|game) + (1|superid),
     data = tpdata_pc %>% 
       filter(behavior_punish == 1, is.na(degree_lag) == 0, tp_on == 0)) %>%
  tidy() # p for linear model = 0.0231

d1 + d2 + d3 +
  plot_annotation(tag_levels = "A")


                                 