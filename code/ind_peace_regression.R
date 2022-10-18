## Regression analysis with the individual-peace status as the outcome
rm(list = ls())

# Individual peace defined as not being a punisher + no one in the local network is a punisher.

# Import functions and packages
mean1 = function(x) {mean(x, na.rm = TRUE)}

library(tidyverse)
library(lme4)
library(magrittr)
library(igraph)

load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

# Import data and packages
data1 = data1 %>%
  mutate(peace_individual = ifelse(local_rate_punish_lag == 0 & 
                                     behavior_punish_lag == 0, 1, 0))

data1 %>%
  summarize(num_ind_peace = mean1(peace_individual)) #71.8% of decisions made in individual peace

data1 %>% 
  filter(peace_individual == 1) %>%
  summarize(mean_degree = mean1(degree),
            median_degree = median(degree, na.rm = T))

# Mean/median degree = 6

data1 %>% 
  filter(peace_individual == 0) %>%
  summarize(mean_degree = mean1(degree),
            median_degree = median(degree, na.rm = T))

# Mean degree is a bit higher: 6.5 for non-peace

h0 = data1 %>%
  filter(peace_individual == 0) %>%
  ggplot() +
  geom_histogram(aes(x = degree), binwidth = 1)

h1 = data1 %>%
  filter(peace_individual == 1) %>%
  ggplot() +
  geom_histogram(aes(x = degree), binwidth = 1)

h0

# not much difference in terms of degree distribution shape
h0_data = ggplot_build(h0)$data[[1]]
h1_data = ggplot_build(h1)$data[[1]]

peace_by_degree = h0_data %>% 
  select(x, count, density) %>% 
  left_join(h1_data %>% select(x, count, density), by = "x") %>%
  rename(nonpeace_count = count.x, nonpeace_density = density.x, 
         peace_count = count.y, peace_density = density.y)
peace_by_degree

## Bivariate analysis
xtabs(~peace_individual + showScore, data = data1)

xtabs(~peace_individual + gender, data1)

xtabs(~peace_individual + behavior_coop_lag, data1) # some difference

xtabs(~peace_individual + behavior_punish_lag, data1) # no punishers in prev round are in peace

# age
data1 %>% 
  na.omit() %>%
  ggplot() + 
  geom_boxplot(aes(x = factor(peace_individual), y = age))

#local_rate_coop
data1 %>%
  na.omit() %>%
  ggplot() + 
  geom_boxplot(aes(x= factor(peace_individual), y = local_rate_coop_lag))
# people in peace had a higher local cooperation rate previously

## Logistic regression models 

# Start with the crude:
m1 = glmer(peace_individual ~ showScore + factor(round) + (1|game) + (1|superid), 
           data = data1, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab_m1 = cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 * se)
round(exp(tab_m1), digits = 3)[1:2,]

# Add demographics:
m2 = glmer(peace_individual ~ showScore + age + gender + factor(round) + 
             (1|game) + (1|superid), 
           data = data1, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab_m2 = cbind(Est = fixef(m2), LL = fixef(m2) - 1.96 * se, UL = fixef(m2) + 1.96 * se)
round(exp(tab_m2), digits = 3)[1:4,]

m3 = glmer(peace_individual ~ showScore + age + gender + behavior_coop_lag + 
             local_rate_coop_lag + cPayoffS_lag + degree_lag + happ_lag + 
             factor(round) + (1|game) + (1|superid), 
           data = data1, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

se = sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab_m1 = cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se, UL = fixef(m3) + 1.96 * se)
round(exp(tab_m1), digits = 3)[1:8,]

# Redo the analysis but only in the last 5 rounds
data2 = data1 %>% filter(round >= 10)

# Start with the crude:
m1 = glmer(peace_individual ~ showScore + factor(round) + (1|game) + (1|superid), 
           data = data2, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab_m1 = cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se, UL = fixef(m1) + 1.96 * se)
round(exp(tab_m1), digits = 3)[1:2,]

# Add demographics:
m2 = glmer(peace_individual ~ showScore + age + gender + factor(round) + 
             (1|game) + (1|superid), 
           data = data2, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))
se = sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab_m2 = cbind(Est = fixef(m2), LL = fixef(m2) - 1.96 * se, UL = fixef(m2) + 1.96 * se)
round(exp(tab_m2), digits = 3)[1:4,]

m3 = glmer(peace_individual ~ showScore + age + gender + behavior_coop_lag + 
             local_rate_coop_lag + cPayoffS_lag + degree_lag + happ_lag + 
             factor(round) + (1|game) + (1|superid), 
           data = data2, family = binomial, nAGQ=0, 
           control = glmerControl(optimizer = c("bobyqa"), 
                                  optCtrl=list(maxfun=2e5), calc.derivs=FALSE))

se = sqrt(diag(vcov(m3)))
# table of estimates with 95% CI
tab_m1 = cbind(Est = fixef(m3), LL = fixef(m3) - 1.96 * se, UL = fixef(m3) + 1.96 * se)
round(exp(tab_m1), digits = 3)[1:8,]
