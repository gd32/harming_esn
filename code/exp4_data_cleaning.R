rm(list = ls())
load("~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/exp4data.Rdata")
library(tidyverse)
library(rgeolocate)
library(igraph)

#renaming some variable names
harmdata = harmdata %>% rename(local_rate_coop = cur_local_rate_coop, 
                               local_rate_defect = cur_local_rate_defect,
                               local_rate_punish = cur_local_rate_punish,
                               initial_defect = inital_defect)

#continuous variable for satisfaction
harmdata$happ = ifelse(harmdata$satisfaction == "v_good", 2, 
                       ifelse(harmdata$satisfaction == "good", 1, 
                              ifelse(harmdata$satisfaction == "neutral", 0,
                                     ifelse(harmdata$satisfaction == "bad", -1, 
                                            ifelse(harmdata$satisfaction == "v_bad", -2, NA)))))

#variable for positive wealth
harmdata$PosWealth = ifelse(harmdata$cumulativePayoff >= 0, 
                            harmdata$cumulativePayoff, 0)

#categorical variable for payoffs (5 levels)
harmdata = harmdata %>%
  mutate(WealthLevel = case_when(cPayoffS < -1.5 ~ "Poorest",
                                 cPayoffS >= -1.5 & cPayoffS < -0.5  ~ "Poorer",
                                 cPayoffS >= -0.5 & cPayoffS < 0.5  ~ "Middle",
                                 cPayoffS >= 0.5 & cPayoffS < 1.5  ~ "Richer",
                                 cPayoffS >= 1.5  ~ "Richest"))

#dummy variable for wealth visibility
harmdata$showScore = ifelse(harmdata$showScore=="true",1,0)

#time pressure indicator
harmdata = harmdata %>% 
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == TRUE, 
                                "Plus", "Minus"))

#matching names over the two data
harmdata = harmdata[,!(names(harmdata) == "gameNumber")]

#change variable types
harmdata$age = as.numeric(unlist(harmdata$age))
harmdata$gender = as.character(unlist(harmdata$gender))
harmdata$ipAddress = as.character(unlist(harmdata$ipAddress))

#convert IP address to country
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

# # Save the individual level data without lag
# save(harmdata, file = "~/Documents/Projects/harming_esn/Data/harmdata.RData")

#data0 - a basic dataset for regression
data0 = harmdata %>% dplyr::select(game, superid, time_pressure, round, age, gender, 
                                   country_3cat, showScore, initial_score, 
                                   payoff, cumulativePayoff, cPayoffS, 
                                   WealthLevel, behavior_coop, behavior_defect, 
                                   behavior_punish, local_rate_coop, 
                                   local_rate_defect, local_rate_punish, happ, 
                                   behaviorTime, degree, e_degree, initial_coop,
                                   initial_defect, initial_punish, timeUp.x)

#extract lagged data
##1-round lag
data_lag = data0 %>% dplyr::select(superid, round, initial_score, payoff, 
                                   cumulativePayoff, cPayoffS, WealthLevel, 
                                   behavior_coop, local_rate_coop, 
                                   behavior_defect, local_rate_defect, 
                                   behavior_punish, local_rate_punish, 
                                   degree, happ, behaviorTime, timeUp.x)
names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1

# ##2-round lag
# data_lag2 = data0 %>%  dplyr::select(superid, round, initial_score, payoff, 
#                                      cumulativePayoff, cPayoffS, WealthLevel, 
#                                      behavior_coop, local_rate_coop, 
#                                      behavior_defect, local_rate_defect, 
#                                      behavior_punish, local_rate_punish, 
#                                      degree, happ, behaviorTime)
# names(data_lag2)[-c(1,2)] = paste0(names(data_lag2)[-c(1,2)],"_lag2")
# data_lag2$round = data_lag2$round + 2

#merge the lagged datasets
# data_lag_long = merge(x = data_lag, y = data_lag2, by = c("superid", "round"))
#merge with the current-round data
data1 = merge(x=data0,y=data_lag, all.x=T,all.y=F,by=c("superid","round"))

#transform behavior time to log scale (base: 10)
data1$log_behaviorTime = log10(data1$behaviorTime)
data1$log_behaviorTime_lag = log10(data1$behaviorTime_lag)

#transform the scale of behavior time from mmsec to sec
data1$behaviorTime_sec = (data1$behaviorTime)/1000

#categorical variable for behavior (C/D/P = cooperation/defection/punishment)
data1 = data1 %>% mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                                              behavior_defect == 1 ~ "D",
                                              behavior_punish == 1 ~ "P"),
                         behavior_lag = case_when(behavior_coop_lag == 1 ~ "C",
                                                  behavior_defect_lag ==1 ~ "D",
                                                  behavior_punish_lag == 1 ~ "P"))

# Adding initial centrality measures
cent_df = tibble(superid = NULL,
                 starting_ec = NULL,
                 starting_dc = NULL)
load("~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/ldata_exp4.Rdata")
load("~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/ndata1_exp4.Rdata")
for(i in 1:50){
  ndata_r0 = ndata1 %>% filter(game == i, round == 0) %>% relocate(id)
  r0_el = ldata4 %>% filter(game == i, round == 0) %>% select(id1, id2)
  g = graph_from_data_frame(r0_el, directed = T, vertices = ndata_r0)
  tmp_df = tibble(superid = 100*i+parse_number(names(eigen_centrality(g)$vector)),
                  starting_ec = eigen_centrality(g)$vector,
                  starting_dc = degree(g))
  cent_df = rbind(cent_df, tmp_df)
}

data1 = merge(data1, cent_df, by = "superid")

exp4data = data1
exp4data = exp4data %>%
  rename(timeout = timeUp.x,
         timeout_lag = timeUp.x_lag) %>%
  unnest(cols = c(timeout, timeout_lag))
save(exp4data, file = "~/Documents/Projects/harming_esn/data/exp4/exp4data.Rdata")

#replace NaNs (from missing lag data) with NAs
data1b = data1 
data1_cc = na.omit(data1b)
dim(data1_cc)

#categorical variable for the prevalence of punishment behavior 
# in the previous round
data1_cc$local_rate_punish_cat3 =
  case_when(data1_cc$local_rate_punish_lag == 0 ~ 1,
            data1_cc$local_rate_punish_lag > 0 & 
              data1_cc$local_rate_punish_lag <= 0.15 ~ 2,
            data1_cc$local_rate_punish_lag > 0.15 ~ 3)

data1_cc$local_rate_punish_cat6 = 
  case_when(data1_cc$local_rate_punish_lag <= 0.05 ~ 1,
            data1_cc$local_rate_punish_lag > 0.05 &
              data1_cc$local_rate_punish_lag <= 0.1 ~ 2,
            data1_cc$local_rate_punish_lag > 0.1 &
              data1_cc$local_rate_punish_lag <= 0.15 ~ 3,
            data1_cc$local_rate_punish_lag > 0.15 &
              data1_cc$local_rate_punish_lag <= 0.2 ~ 4,
            data1_cc$local_rate_punish_lag > 0.2 &
              data1_cc$local_rate_punish_lag <= 0.25 ~ 5,
            data1_cc$local_rate_punish_lag > 0.25 ~ 6) %>% as.factor()

round(addmargins
      (prop.table(xtabs(~local_rate_punish_cat3 + behavior_punish, data1_cc))),
      digits = 3)

round(addmargins
      (prop.table(xtabs(~local_rate_punish_cat6 + behavior_punish, data1_cc))),
      digits = 3)

data1_cc$local_rate_coop_cat5 = 
  case_when(data1_cc$local_rate_coop_lag > 0 & data1_cc$local_rate_coop_lag <= 0.1 ~ 5,
            data1_cc$local_rate_coop_lag > 0.1 & data1_cc$local_rate_coop_lag <= 0.3 ~ 4,
            data1_cc$local_rate_coop_lag > 0.3 & data1_cc$local_rate_coop_lag <= 0.6 ~ 3,
            data1_cc$local_rate_coop_lag > 0.6 & data1_cc$local_rate_coop_lag <= 0.8 ~ 2,
            data1_cc$local_rate_coop_lag > 0.8  ~ 1)