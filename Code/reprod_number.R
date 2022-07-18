##### Treating punishment as an infection - what is the r0 of punishment?
#### 1. Identify how many punishments are created by each punisher in the next round

rm(list = ls())
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata

library(tidyverse)
library(lme4)
library(igraph)

harmdata %>% filter(showScore == 1) #visible = 26-50, invisible = 1-25
 
punishers = data.frame(game = numeric(),r1 = numeric(),r2 = numeric(),r3 = numeric(),r4 = numeric(),r5 = numeric(),
                   r6 = numeric(),r7 = numeric(),r8 = numeric(),r9 = numeric(),r10 = numeric(),
                   r11 = numeric(),r12 = numeric(),r13 = numeric(),r14 = numeric())
for(gm in 1:50){
  ps = NULL
  for (r in 1:14){
      verts = ndata1 %>% filter(game == gm, round == r) %>% relocate(id)
      el = ldata4 %>% filter(game == gm, round == r) %>% select(id1, id2)
      g = graph_from_data_frame(el, directed = T, vertices = verts) #the graph in round t
      punishers_t = verts %>% filter(behavior_punish == 1) %>% pull(id) #these are the people who punished in round t
      verts2 = ndata1 %>% filter(game == gm, round == r+1) %>% relocate(id)
      el2 = ldata4 %>% filter(game == gm, round == r+1) %>% select(id1, id2)
      punishers_t_plus_one = verts2 %>% filter(behavior_punish == 1) %>% pull(id) # these are the punishers in round t+1
      g2 = graph_from_data_frame(el2, directed = T, vertices = verts2)
      adj = adjacent_vertices(g, V(g)[V(g)$behavior_punish == 1], mode = "all")
      N = unlist(adj)
      connected_vertices = unique(V(g)[N])
      connected_punishers = sum(V(g2)[V(g2)$behavior_punish == 1] %in% connected_vertices) # this is the number of punishers in round t+1 that were connected to the punishers in round t
      ps = c(ps, connected_punishers) #punishers is the round-by-round count of punishers that are connected to punishers in the current round
  }
  tmp_row = c(gm, ps)
  punishers = rbind(punishers, tmp_row)
  }
names(punishers) = c("game", paste0("created_r", 1:14))


count_df = data.frame(game = 1:50)
for(r in 1:14){
  tmp_count = harmdata %>% group_by(game, round) %>%
    filter(round == r) %>%
    tally(behavior_punish) %>% pull(n)
  count_df = cbind(count_df, tmp_count)
}
names(count_df) = c("game", paste0("puns_r", 1:14))
count_df

# based on the column totals, the most "new" punishers are created in the early rounds, but there is a new peak around rd 10
pun_counts = left_join(count_df, punishers, by = "game")
pc1 = count_df %>% pivot_longer(cols = -game, names_prefix = "puns_")
pc2 = punishers %>% pivot_longer(cols = -game, names_prefix = "created_")
pun_counts
count_df
test1 = count_df %>%
  pivot_longer(cols = -game, names_to = "round", names_prefix = c("puns_r")) %>%
  filter(game == 15)
test2 = punishers %>%
  pivot_longer(cols = -game, names_to = "round", names_prefix = c("created_r")) %>%
  filter(game == 15)

ggplot() + 
  geom_point(aes(x = as.numeric(round), y = value, group = 1, color = "punishers"), data = test1) +
  geom_point(aes(x = as.numeric(round), y = value, group = 1, color = "con_punishers"), data = test2) +
  scale_color_manual(values=c("punishers" = "blue", "con_punishers" = "orange")) 

pc1 %>%
  ggplot() + 
  geom_line(aes(x = rep(1:14, 50), y = value, group = game, color = game)) +
  geom_line(data = pc2, aes(x = rep(1:14, 50), y = value, group = game, color = game))

punishers
count_df
count_df_long = count_df %>% pivot_longer(cols = -game,
                          names_to = c("class", "round"),
                          names_sep = "_r",
                          values_to = "punishers") 
punishers_long = punishers %>% pivot_longer(cols = -game,
                                           names_to = c("class", "round"),
                                           names_sep = "_r",
                                           values_to = "connected_punishers") 


pun_counts_long = left_join(count_df_long, punishers_long, by = c("game", "round")) %>% 
  mutate(wealth = ifelse(game %in% 1:25, "invisible", "visible"))

pun_counts_long %>% 
  ggplot() +
  geom_jitter(aes(x = punishers, y = connected_punishers, group = game, color = wealth)) 

library(Hmisc)

pun_counts_long %>%
  filter(game %nin% c(22, 46)) %>%
  ggplot() +
  geom_jitter(aes(x = punishers, y = connected_punishers, group = game, color = wealth)) +
  geom_abline()


 
