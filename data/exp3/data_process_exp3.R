## Data Processing
library(tidyverse)
library(igraph)
library(rgeolocate)

# Main data: mdata3
rm(list = ls())
load("~/Documents/Projects/harming_esn/data/exp3/mdata3_exp3.Rdata")

### Set up convenience functions
mean1 = function(x) {mean(x,na.rm=TRUE)}
median1 = function(x) {median(x, na.rm = TRUE)}
sd1 = function(x) {sd(x, na.rm = TRUE)}
se_mean = function(x) sd1(x)/sqrt(sum(is.na(x) == 0))

harmdata = mdata3

# Renaming current rates
harmdata = harmdata %>% rename(local_rate_coop = cur_local_rate_coop, 
                               local_rate_defect = cur_local_rate_defect,
                               local_rate_punish = cur_local_rate_punish)

# Create continuous happiness variable
harmdata$happ = ifelse(harmdata$satisfaction == "v_good", 2, 
                       ifelse(harmdata$satisfaction == "good", 1, 
                              ifelse(harmdata$satisfaction == "neutral", 0,
                                     ifelse(harmdata$satisfaction == "bad", -1, 
                                            ifelse(harmdata$satisfaction == "v_bad", -2, NA)))))

# Create positive wealth variable
harmdata$PosWealth = ifelse(harmdata$cumulativePayoff >= 0, 
                            harmdata$cumulativePayoff, 0)

# Create categorical WealthLevel (5 levels)
harmdata = harmdata %>%
  mutate(WealthLevel = case_when(cPayoffS < -1.5 ~ "Poorest",
                                 cPayoffS >= -1.5 & cPayoffS < -0.5  ~ "Poorer",
                                 cPayoffS >= -0.5 & cPayoffS < 0.5  ~ "Middle",
                                 cPayoffS >= 0.5 & cPayoffS < 1.5  ~ "Richer",
                                 cPayoffS >= 1.5  ~ "Richest"))

# Create wealth visibility
harmdata$showScore = ifelse(harmdata$showScore=="true",1,0)

###making the variable names the same over the two data
harmdata = harmdata[,!(names(harmdata) == "gameNumber")]

#Other variables
harmdata$age = as.numeric(unlist(harmdata$age))
harmdata$gender = as.character(unlist(harmdata$gender))
harmdata$ipAddress = as.character(unlist(harmdata$ipAddress))
harmdata$race = as.character(unlist(harmdata$race))
harmdata$is_hispanic = as.character(unlist(harmdata$is_hispanic))
harmdata$satisfaction = as.character(unlist(harmdata$satisfaction))

# Convert IPaddress to country
file = system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
ipCountries = maxmind(harmdata$ipAddress, file, "country_name")
harmdata$country = ipCountries$country_name

`%notin%` <- Negate(`%in%`) 

prop.table(table(harmdata$country)) #31.0% AUS, 39.2% USA; 8.5% IND, 7.5% SRB, 6.3% RUS

harmdata = harmdata %>% 
  mutate(country_3cat = 
           factor(case_when(country == "United States" ~ "US",
                            country == "India" ~ "India",
                            country %notin% c("United States", 
                                              "India") ~ "Other"),
                  levels = c("US", "India", "Other")))

# Add indicator for time pressure
exp3data = harmdata %>%
  as_tibble() %>%
  mutate(time_pressure = ifelse(str_detect(gameID, "TP") == TRUE, "Plus", "Minus"))

# Fixing categorizations
exp3data = exp3data %>%
  mutate(behavior = case_when(behavior == "C" ~ "C",
                              behavior == "D" ~ "D",
                              behavior == "P" ~ "P",
                              TRUE ~ NA_character_),
         race = case_when(race == "[white]" ~ "White",
                          race == "[black]" ~ "Black",
                          race == "[asian]" ~ "Asian",
                          race == "[american_indian]" ~ "AmIndian",
                          TRUE ~ NA_character_),
         behavior_punish = ifelse(behavior == "P", 1, 0),
         behavior_coop = ifelse(behavior == "C", 1, 0),
         behavior_defect = ifelse(behavior == "D", 1, 0))

data_lag = exp3data %>% select(superid, round, initial_score, payoff, 
                              cumulativePayoff, cPayoffS, WealthLevel, 
                              behavior_coop, local_rate_coop, 
                              behavior_defect, local_rate_defect, 
                              behavior_punish, local_rate_punish, 
                              degree, happ, behaviorTime)


names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1

exp3data = exp3data %>%
  left_join(data_lag, by = c("superid", "round"))

# Transform behavior time to log scale
exp3data$log_behaviorTime = log10(exp3data$behaviorTime)
exp3data$log_behaviorTime_lag = log10(exp3data$behaviorTime_lag)

# Adding initial centrality measures
load("~/Documents/Projects/harming_esn/data/exp3/ldata4_exp3.Rdata") # link data
load("~/Documents/Projects/harming_esn/data/exp3/ndata1_exp3.Rdata") # node data

cent_df = tibble(superid = NULL,
                 starting_ec = NULL,
                 starting_dc = NULL)
for(i in 1:20){
  ndata_r0 = ndata1 %>% filter(game == i, round == 0) %>% relocate(id)
  r0_el = ldata4 %>% filter(game == i, round == 0) %>% select(id1, id2)
  g = graph_from_data_frame(r0_el, directed = T, vertices = ndata_r0)
  tmp_df = tibble(superid = 100*i+parse_number(names(eigen_centrality(g)$vector)),
                  starting_ec = eigen_centrality(g)$vector,
                  starting_dc = degree(g))
  cent_df = rbind(cent_df, tmp_df)
}

exp3data = merge(exp3data, cent_df, by = "superid")

#replace NaNs (from missing lag data) with NAs
exp3data[is.na(exp3data) == T] = NA

# save(exp3data, file = "tpdata_exp3.Rdata")

