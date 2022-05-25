# Animation script for Harming Experiments
# 4/12/2022
# By: George Dewey

# 1. Load link and node data
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1

# 2. Load packages for making network visuals
set.seed(123)
library(animation)
library(igraph)
library(tidygraph)
library(ggraph)
library(RColorBrewer)
library(tidyverse)

for(i in 1:50){
  # Create ndata for each round
  # ndata1_r0 = ndata1 %>% filter(game == i, round == 0) %>% relocate(id) 
  ndata1_r1 = ndata1 %>% filter(game == i, round == 1) %>% relocate(id)
  ndata1_r2 = ndata1 %>% filter(game == i, round == 2) %>% relocate(id) 
  ndata1_r3 = ndata1 %>% filter(game == i, round == 3) %>% relocate(id) 
  ndata1_r4 = ndata1 %>% filter(game == i, round == 4) %>% relocate(id) 
  ndata1_r5 = ndata1 %>% filter(game == i, round == 5) %>% relocate(id) 
  ndata1_r6 = ndata1 %>% filter(game == i, round == 6) %>% relocate(id) 
  ndata1_r7 = ndata1 %>% filter(game == i, round == 7) %>% relocate(id) 
  ndata1_r8 = ndata1 %>% filter(game == i, round == 8) %>% relocate(id) 
  ndata1_r9 = ndata1 %>% filter(game == i, round == 9) %>% relocate(id) 
  ndata1_r10 = ndata1 %>% filter(game == i, round == 10) %>% relocate(id) 
  ndata1_r11 = ndata1 %>% filter(game == i, round == 11) %>% relocate(id) 
  ndata1_r12 = ndata1 %>% filter(game == i, round == 12) %>% relocate(id) 
  ndata1_r13 = ndata1 %>% filter(game == i, round == 13) %>% relocate(id) 
  ndata1_r14 = ndata1 %>% filter(game == i, round == 14) %>% relocate(id) 
  ndata1_r15 = ndata1 %>% filter(game == i, round == 15) %>% relocate(id) 
  
  # Decision is made prior to rewiring. Graph structure is the structure AFTER 
  # rewiring at the end of the round. So how should we assign colors to our
  # behavior categories/decision choices?
  
  # Meanings: The network structure of round X is post-rewiring. Thus if we use
  # the choice categories for round X + 1, the network graph shows the input of
  # the rewiring on choice in the following round
  
  # Plotting schema: make graph from edgelist (ID columns from ldata) and ndata
  
  # Each animation will start at round 1 (since in round 0,  no decisions are
  # made and no wealth is exchanged in either visible or invisible condition)
  
  # game1_round0_edgelist = ldata4 %>% filter(game == 1, round == 0) %>% dplyr::select(id1, id2)
  # g0 = graph_from_data_frame(game1_round0_edgelist, directed = F, vertices = ndata1_r0)
  # pal = brewer.pal(length(unique(V(g0)$behavior)), "Set1") #Red = D, Blue = C, Green = P
  
  round1_edgelist = ldata4 %>% filter(game == i, round == 1) %>% dplyr::select(id1, id2)
  g1 = graph_from_data_frame(round1_edgelist, directed = F, vertices = ndata1_r1)
  pal = brewer.pal(length(unique(V(g1)$behavior)), "Set1") #Red = D, Blue = C, Green = P
  coords = layout.auto(g1)
  
  round2_edgelist = ldata4 %>% filter(game == i, round == 2) %>% dplyr::select(id1, id2)
  g2 = graph_from_data_frame(round2_edgelist, directed = F, vertices = ndata1_r2)
  
  round3_edgelist = ldata4 %>% filter(game == i, round == 3) %>% dplyr::select(id1, id2)
  g3 = graph_from_data_frame(round3_edgelist, directed = F, vertices = ndata1_r3)
  
  round4_edgelist = ldata4 %>% filter(game == i, round == 4) %>% dplyr::select(id1, id2)
  g4 = graph_from_data_frame(round4_edgelist, directed = F, vertices = ndata1_r4)
  
  round5_edgelist = ldata4 %>% filter(game == i, round == 5) %>% dplyr::select(id1, id2)
  g5 = graph_from_data_frame(round5_edgelist, directed = F, vertices = ndata1_r5)
  
  round6_edgelist = ldata4 %>% filter(game == i, round == 6) %>% dplyr::select(id1, id2)
  g6 = graph_from_data_frame(round6_edgelist, directed = F, vertices = ndata1_r6)
  
  round7_edgelist = ldata4 %>% filter(game == i, round == 7) %>% dplyr::select(id1, id2)
  g7 = graph_from_data_frame(round7_edgelist, directed = F, vertices = ndata1_r7)
  
  round8_edgelist = ldata4 %>% filter(game == i, round == 8) %>% dplyr::select(id1, id2)
  g8 = graph_from_data_frame(round8_edgelist, directed = F, vertices = ndata1_r8)
  
  round9_edgelist = ldata4 %>% filter(game == i, round == 9) %>% dplyr::select(id1, id2)
  g9 = graph_from_data_frame(round9_edgelist, directed = F, vertices = ndata1_r9)
  
  round10_edgelist = ldata4 %>% filter(game == i, round == 10) %>% dplyr::select(id1, id2)
  g10 = graph_from_data_frame(round10_edgelist, directed = F, vertices = ndata1_r10)
  
  round11_edgelist = ldata4 %>% filter(game == i, round == 11) %>% dplyr::select(id1, id2)
  g11 = graph_from_data_frame(round11_edgelist, directed = F, vertices = ndata1_r11)
  
  round12_edgelist = ldata4 %>% filter(game == i, round == 12) %>% dplyr::select(id1, id2)
  g12 = graph_from_data_frame(round12_edgelist, directed = F, vertices = ndata1_r12)
  
  round13_edgelist = ldata4 %>% filter(game == i, round == 13) %>% dplyr::select(id1, id2)
  g13 = graph_from_data_frame(round13_edgelist, directed = F, vertices = ndata1_r13)
  
  round14_edgelist = ldata4 %>% filter(game == i, round == 14) %>% dplyr::select(id1, id2)
  g14 = graph_from_data_frame(round14_edgelist, directed = F, vertices = ndata1_r14)
  
  round15_edgelist = ldata4 %>% filter(game == i, round == 15) %>% dplyr::select(id1, id2)
  g15 = graph_from_data_frame(round15_edgelist, directed = F, vertices = ndata1_r15)
  
  setwd("~/Documents/Projects/harming_esn/Figures/Animations")
  saveGIF({
    plot(simplify(g1), 
         layout = coords, 
         vertex.color = pal[as.numeric(factor(vertex_attr(g2, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g2), 
         layout = coords, 
         vertex.color = pal[as.numeric(factor(vertex_attr(g3, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g3), 
         layout = coords, 
         vertex.color = pal[as.numeric(factor(vertex_attr(g4, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g4), 
         layout = coords, 
         vertex.color = pal[as.numeric(factor(vertex_attr(g5, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g5), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g6, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g6), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g7, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g7), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g8, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g8), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g9, "behavior"), levels = c("D", "C", "P")))]) 
    plot(simplify(g9), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g10, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g10), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g11, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g11), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g12, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g12), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g13, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g13), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g14, "behavior"), levels = c("D", "C", "P")))])
    plot(simplify(g14), 
         layout = coords,  
         vertex.color = pal[as.numeric(factor(vertex_attr(g15, "behavior"), levels = c("D", "C", "P")))])
  },
  interval = 0.5, movie.name = paste0("game",i,".gif"))

}
