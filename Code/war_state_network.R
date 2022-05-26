#### Network Analysis for Peace

## We define peace as having no punishment in at least 5 (or 3?) consecutive rounds

library(tidyverse)

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

if(tmp[-1] == "0" & tmp[-2] == "0" & tmp[-3] == "0" & tmp[-4] == "0" & tmp[-5] == "0"){
  peace_games = c(peace_games, i)
} else {
  war_games = c(war_games, i)
}

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
# How many punishments in each of these games? and in what round?

harmdata %>% filter(game == 14, round != 0) %>% group_by(round) %>% tally(behavior_punish)

punish_counts = NULL
for(i in peace_games){
  tmp = harmdata %>% filter(game == i, round !=0) %>% group_by(round) %>% 
    tally(behavior_punish) %>% pull(n)
  punish_counts = cbind(punish_counts, tmp)
}
names(punish_counts) = as.character(peace_games)
punish_counts = as.data.frame(punish_counts)
punish_counts %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  ggplot(aes(x = as.numeric(rowname), y = colname, fill = value)) +
  geom_tile() +
  xlab("Round") + ylab("Game ID")

names(harmdata)

load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1

view(harmdata %>% 
  group_by(game) %>%
  select(superid, game, round, showScore, behavior_coop, behavior_defect, 
         behavior_punish,cPayoffS, behaviorTime, degree, rate_coop, rate_rich, 
         local_gini, initial_defect, initial_punish, initial_local_gini, 
         initial_degree, happ, WealthLevel, country) %>%
  arrange(game, round, superid) %>%
  mutate(peace_game = ifelse(game %in% peace_games, 1, 0)) %>%
  summarize(showScore = mean(showScore),
            mean_deg = mean(degree),
            n = n(),
            peace_game = mean(peace_game)))
