## -----------------------------------------------------------------------------
## Script name: rewiring_analysis.R
##
## Purpose of script: To assess the effect of choosing punishment on rewiring
##
## Author: George Dewey
##
## Date Created: 2024-07-01
##
## Last Updated: 2024-07-03
## -----------------------------------------------------------------------------

## Load packages and data ------------------------------------------------------
library(tidyverse)
library(lme4)
library(broom.mixed)

# ndata - Exp. 1
load('~/Documents/Projects/harming_esn/data/exp1/archive/harming/exp1/ndata_individual.Rdata') 

# ndata - Exp. 2
load('~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/ndata1_exp4.Rdata')

# Load the main analysis data for exp2 to get time pressure status
exp2data = read_csv("~/Documents/Projects/harming_esn/data/final/exp2data_final.csv", 
                    show_col_types = FALSE)
exp2_tp_status = exp2data %>% select(superid, time_pressure) %>% distinct() 

# Helper functions
mean1 = function(x) {mean(x,na.rm=TRUE)}
median1 = function(x) {median(x, na.rm = TRUE)}
sd1 = function(x) {sd(x, na.rm = TRUE)}
se_mean = function(x) sd1(x)/sqrt(sum(is.na(x) == 0))

## Get rewiring data for Exp. 1 ------------------------------------------------
ndata1_rewire = ndata1 %>% as_tibble() %>%
  select(game, round, id, behavior, behavior_coop, behavior_defect, 
         behavior_punish, makeLink, breakLink) 

exp1_rewiring = unnest_wider(ndata1_rewire, c(makeLink, breakLink), 
                             names_sep = 'id') 

exp1_rewiring$superid = 100*ndata1$game+as.numeric(substr(ndata1$id,
                                                          2,
                                                          nchar(ndata1$id)))
exp1_rewiring = exp1_rewiring %>%  
  mutate(behavior = case_when(behavior_coop == 1 ~ 'C',
                              behavior_defect == 1 ~'D',
                              behavior_punish == 1 ~ 'P')) 

exp1_rewiring = exp1_rewiring %>% 
  rowwise() %>%
  mutate(rewired = ifelse(sum(!is.na(c_across(makeLinkid1:breakLinkid5))) 
                          != 0, 1, 0)) %>%
  ungroup() %>% 
  select(-contains('Link'))

## Frequency table for rewiring and behaviors for Exp. 1 -----------------------
exp1_rewiring %>%
  group_by(behavior) %>%
  filter(behavior %in% c('C', 'D', 'P')) %>%
  summarize(prop_rewire = mean(rewired),
            se_rewire = se_mean(rewired))

## Regression for rewiring and punishment for Exp. 1 ---------------------------
m_rwr_e1 = glmer(rewired ~ behavior_punish + round + (1|game) + (1|superid), 
              data = exp1_rewiring %>% filter(round > 0, behavior %in% c('C', 'D', 'P')),
      family = 'binomial', nAGQ=0, 
      control = glmerControl(optimizer = c("bobyqa"), 
                             optCtrl=list(maxfun=2e5), 
                             calc.derivs=FALSE))
tidy(m_rwr_e1, exponentiate = T) 

summary(glmer(rewired ~ behavior_defect + round + (1|game) + (1|superid), 
              data = exp1_rewiring %>% filter(round > 0),
              family = 'binomial', nAGQ=0, 
              control = glmerControl(optimizer = c("bobyqa"), 
                                     optCtrl=list(maxfun=2e5), 
                                     calc.derivs=FALSE)))


exp1_rewiring_short = exp1_rewiring %>% select(superid, game, round, rewired)
write_csv(exp1_rewiring_short, "~/Documents/Projects/harming_esn/data/exp1/exp1_rewiring.csv")

## Get rewiring data for Exp. 2 ------------------------------------------------
ndata_rewire_2 = ndata1_exp4 %>% as_tibble() %>%
  select(game, round, id, behavior, behavior_coop, behavior_defect, 
         behavior_punish, makeLink, breakLink) 

exp2_rewiring = unnest_wider(ndata_rewire_2, c(makeLink, breakLink), 
                             names_sep = 'id') 

exp2_rewiring$superid = 
  100*ndata1_exp4$game+as.numeric(substr(ndata1_exp4$id,
                                         2,
                                         nchar(ndata1_exp4$id)))
exp2_rewiring = exp2_rewiring %>% 
  mutate(behavior = case_when(behavior_coop == 1 ~ 'C',
                              behavior_defect == 1 ~'D',
                              behavior_punish == 1 ~ 'P')) 

exp2_rewiring = exp2_rewiring %>% 
  rowwise() %>%
  mutate(rewired = ifelse(sum(!is.na(c_across(makeLinkid1:breakLinkid5))) != 0, 
                          1, 0)) %>%
  ungroup() %>% 
  select(-contains('Link'))

exp2_rewiring = exp2_rewiring %>% left_join(exp2_tp_status, by = 'superid')

# Frequency table for punishment and rewiring in Exp. 2 (TP+ and TP-) 
exp2_rewiring %>%
  group_by(time_pressure, behavior) %>%
  filter(behavior %in% c('C', 'D', 'P')) %>%
  summarize(prop_rewire = mean(rewired),
            se_rewire = se_mean(rewired))

## Regression for punishment and rewiring, Exp. 2 (TP-) ------------------------
m_rwr_e2_minus = glmer(rewired ~ behavior_punish + round + (1|game) + (1|superid), 
              data = exp2_rewiring %>% filter(round > 0, 
                                              behavior %in% c('C', 'D', 'P'),
                                              time_pressure == 'Minus'),
              family = 'binomial', nAGQ=0, 
              control = glmerControl(optimizer = c("bobyqa"), 
                                     optCtrl=list(maxfun=2e5), 
                                     calc.derivs=FALSE))
tidy(m_rwr_e2_minus, exponentiate = T)
 
## Regression for punishment and rewiring, Exp. 2 (TP+) ------------------------
m_rwr_e2_plus = glmer(rewired ~ behavior_punish + round + (1|game) + (1|superid), 
              data = exp2_rewiring %>% filter(round > 0, 
                                              behavior %in% c('C', 'D', 'P'),
                                              time_pressure == 'Plus'),
              family = 'binomial', nAGQ=0, 
              control = glmerControl(optimizer = c("bobyqa"), 
                                     optCtrl=list(maxfun=2e5), 
                                     calc.derivs=FALSE))

tidy(m_rwr_e2_plus, exponentiate = T) %>%
  as.data.frame()

exp2_rewiring_short = exp2_rewiring %>% select(superid, game, round, rewired)
write_csv(exp2_rewiring_short, '~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/exp2_rewiring.csv')
