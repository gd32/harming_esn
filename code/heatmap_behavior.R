## Looking at sequencing of behavior

# Load data and packages
rm(list = ls())
load("~/Documents/Projects/harming_esn/Data/harming_jsons/ldata4_0316X.Rdata") #ldata4
load("~/Documents/Projects/harming_esn/Data/ndata_individual.Rdata") #ndata1
load("~/Documents/Projects/harming_esn/Data/harmdata.Rdata") #harmdata
load("~/Documents/Projects/harming_esn/Data/data1.Rdata") #data1

library(tidyverse) 
library(magrittr)
library(lme4)
library(igraph)
library(pheatmap)
library(cowplot)

# Create the dataframes for visuals

# 1. the table for behaviors by game + round
behavior_table = harmdata %>% mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                                                behavior_defect == 1 ~ "D",
                                                behavior_punish == 1 ~ "P"),
                                     behavior_numeric = case_when(behavior == "C" ~ 1, 
                                                                  behavior == "D" ~ 2,
                                                                  behavior == "P" ~ 3)) %>%
  group_by(superid, round) %>%
  arrange(superid, round) %>%
  select(behavior_numeric) %>%
  pivot_wider(id_cols = superid, names_from = round, values_from = behavior_numeric) %>%
  select(-"0") %>%
  mutate(game = superid %/% 100) 

# 2. the table for local punish rate in the previous round by game + round
rate_table = data1 %>% 
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "P"),
  behavior_numeric = case_when(behavior == "C" ~ 1, 
                                       behavior == "D" ~ 2,
                                       behavior == "P" ~ 3)) %>%
  group_by(superid, round) %>%
  arrange(superid, round) %>%
  select(local_rate_punish_lag)  %>%
  pivot_wider(id_cols = superid, names_from = round, 
              values_from = local_rate_punish_lag) %>%
  select(-"0") %>%
  mutate(game = superid %/% 100)

# color code: 1 = coop (g), 2 = defect (b), 3 = punish (r)

# Invisible games: 1-25

behavior_table %>% filter(game == 1) %>% ungroup() %>% select(-game) %>% 
  column_to_rownames(var = "superid") %>% as.matrix()

breakslist = c(0, 1, 2, 3)

i1 = pheatmap(behavior_table %>% filter(game == 1) %>% ungroup() %>% select(-game) %>% 
    column_to_rownames(var = "superid") %>% as.matrix(), 
    labels_row = "",
    labels_col = "",
    cluster_cols = F, 
    cluster_rows = F, 
    # display_numbers = 
    #   round(rate_table, 3) %>% filter(game == 1) %>% ungroup() %>% select(-game) %>%
    #   column_to_rownames(var = "superid") %>% as.matrix(),
    color = c("lightgreen", "white", "orange"),
    breaks = breakslist,
    legend = F,
    legend_labels = c("Cooperate", "Defect", "Punish"),
    legend_breaks = c(0.5, 1.5, 2.5))

i2 = pheatmap(behavior_table %>% filter(game == 2) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         labels_row = "",
         labels_col = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 2) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i3 = pheatmap(behavior_table %>% filter(game == 3) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
            labels_row = "",
            labels_col = "",
           cluster_cols = F, 
           cluster_rows = F, 
           # display_numbers = 
           #   round(rate_table, 3) %>% filter(game == 3) %>% ungroup() %>% select(-game) %>%
           #   column_to_rownames(var = "superid") %>% as.matrix(),
           color = c("lightgreen", "white", "orange"),
           breaks = breakslist,
          legend = F,
           legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i4 = pheatmap(behavior_table %>% filter(game == 4) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
           labels_row = "",
           labels_col = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 4) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i5 = pheatmap(behavior_table %>% filter(game == 5) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
           labels_row = "",
           labels_col = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 5) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i6 = pheatmap(behavior_table %>% filter(game == 6) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 6) %>% ungroup() %>% select(-game) %>%
           # column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i7 = pheatmap(behavior_table %>% filter(game == 7) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 7) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i8 = pheatmap(behavior_table %>% filter(game == 8) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 8) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i9 = pheatmap(behavior_table %>% filter(game == 9) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 9) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i10 = pheatmap(behavior_table %>% filter(game == 10) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
          labels_row = "",
          labels_col = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 10) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         legend = F,
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))


# First 10
plot_grid(i1[[4]], i2[[4]], i3[[4]], i4[[4]], i5[[4]], i6[[4]], i7[[4]], i8[[4]], i9[[4]], i10[[4]],
          nrow = 2, 
          labels = 1:10)

i11 = pheatmap(behavior_table %>% filter(game == 11) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
           labels_col = "",
           labels_row = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 11) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         legend = F,
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i12 = pheatmap(behavior_table %>% filter(game == 12) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
           labels_col = "",
           labels_row = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 12) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i13 = pheatmap(behavior_table %>% filter(game == 13) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 13) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i14 = pheatmap(behavior_table %>% filter(game == 14) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 14) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i15 = pheatmap(behavior_table %>% filter(game == 15) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 15) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend  = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i16 = pheatmap(behavior_table %>% filter(game == 16) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 16) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i17 = pheatmap(behavior_table %>% filter(game == 17) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 17) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i18 = pheatmap(behavior_table %>% filter(game == 18) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 18) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i19 = pheatmap(behavior_table %>% filter(game == 19) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 19) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i20 = pheatmap(behavior_table %>% filter(game == 20) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 20) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

# Second 10
plot_grid(i11[[4]], i12[[4]], i13[[4]], i14[[4]], i15[[4]], i16[[4]], i17[[4]], i18[[4]], i19[[4]],
          i20[[4]], 
          nrow = 2,
          labels = 11:20)

i21 = pheatmap(behavior_table %>% filter(game == 21) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 21) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i22 = pheatmap(behavior_table %>% filter(game == 22) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 22) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i23 = pheatmap(behavior_table %>% filter(game == 23) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 23) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i24 = pheatmap(behavior_table %>% filter(game == 24) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 24) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

i25 = pheatmap(behavior_table %>% filter(game == 25) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 25) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

# Last 5
plot_grid(i21[[4]], i22[[4]], i23[[4]], i24[[4]], i25[[4]],
          labels = 21:25)

# All plots
plot_grid(i1[[4]], i2[[4]], i3[[4]], i4[[4]], i5[[4]], i6[[4]], i7[[4]], i8[[4]], i9[[4]], i10[[4]],
          nrow = 2, 
          labels = 1:10)

plot_grid(i11[[4]], i12[[4]], i13[[4]], i14[[4]], i15[[4]], i16[[4]], i17[[4]], i18[[4]], i19[[4]],
          i20[[4]], 
          nrow = 2,
          labels = 11:20)

plot_grid(i21[[4]], i22[[4]], i23[[4]], i24[[4]], i25[[4]],
          labels = 21:25)

plot_grid(i1[[4]], i2[[4]], i3[[4]], i4[[4]], i5[[4]], i6[[4]], i7[[4]], i8[[4]], i9[[4]], i10[[4]],
          i11[[4]], i12[[4]], i13[[4]], i14[[4]], i15[[4]], i16[[4]], i17[[4]], i18[[4]], i19[[4]],
          i20[[4]], i21[[4]], i22[[4]], i23[[4]], i24[[4]], i25[[4]],
          nrow = 5, 
          ncol = 5,
          labels = 1:25)

# Visible games starts here (26-50)

v26 = pheatmap(behavior_table %>% filter(game == 26) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 26) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v27 = pheatmap(behavior_table %>% filter(game == 27) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 27) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v28 = pheatmap(behavior_table %>% filter(game == 28) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 28) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v29 = pheatmap(behavior_table %>% filter(game == 29) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 29) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v30 = pheatmap(behavior_table %>% filter(game == 30) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 30) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v31 = pheatmap(behavior_table %>% filter(game == 31) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 31) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v32 = pheatmap(behavior_table %>% filter(game == 32) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 32) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v33 = pheatmap(behavior_table %>% filter(game == 33) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 33) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v34 = pheatmap(behavior_table %>% filter(game == 34) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 34) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v35 = pheatmap(behavior_table %>% filter(game == 35) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
           labels_row = "",
           labels_col = "",
         cluster_cols = F, 
         cluster_rows = F, 
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 35) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v36 = pheatmap(behavior_table %>% filter(game == 36) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 36) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v37 = pheatmap(behavior_table %>% filter(game == 37) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 37) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v38 = pheatmap(behavior_table %>% filter(game == 38) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 38) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v39 = pheatmap(behavior_table %>% filter(game == 39) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 39) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v40 = pheatmap(behavior_table %>% filter(game == 40) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 40) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v41 = pheatmap(behavior_table %>% filter(game == 41) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 41) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v42 = pheatmap(behavior_table %>% filter(game == 42) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 42) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v43 = pheatmap(behavior_table %>% filter(game == 43) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 43) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v44 = pheatmap(behavior_table %>% filter(game == 44) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 44) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v45 = pheatmap(behavior_table %>% filter(game == 45) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 45) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend = F,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v46 = pheatmap(behavior_table %>% filter(game == 46) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col =  "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 46) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v47 = pheatmap(behavior_table %>% filter(game == 47) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_row = "",
         labels_col = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 47) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v48 = pheatmap(behavior_table %>% filter(game == 48) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 48) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v49 = pheatmap(behavior_table %>% filter(game == 49) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F, 
         labels_col = "",
         labels_row = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 49) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

v50 = pheatmap(behavior_table %>% filter(game == 50) %>% ungroup() %>% select(-game) %>% 
           column_to_rownames(var = "superid") %>% as.matrix(), 
         cluster_cols = F, 
         cluster_rows = F,
         labels_col = "",
         labels_row = "",
         legend = F,
         # display_numbers = 
         #   round(rate_table, 3) %>% filter(game == 50) %>% ungroup() %>% select(-game) %>%
         #   column_to_rownames(var = "superid") %>% as.matrix(),
         color = c("lightgreen", "white", "orange"),
         breaks = breakslist,
         legend_labels = c("Cooperate", "Defect", "Punish"),
         legend_breaks = c(0.5, 1.5, 2.5))

## all plots
plot_grid(v26[[4]], v27[[4]], v28[[4]], v29[[4]], v30[[4]], v31[[4]], v32[[4]], v33[[4]], v34[[4]],
          v35[[4]], v36[[4]], v37[[4]], v38[[4]], v39[[4]], v40[[4]], v41[[4]], v42[[4]], v43[[4]],
          v44[[4]], v45[[4]], v46[[4]], v47[[4]], v48[[4]], v49[[4]], v50[[4]],
          nrow = 5,
          labels = 26:50)

# 1 5 7 8 9 13 14 15 16 20 26 28 37 45

## peace games vs not
peace_plots = plot_grid(i1[[4]],i5[[4]],i7[[4]],i8[[4]],
                        i9[[4]], i13[[4]],i14[[4]], i15[[4]], i16[[4]],
                        i20[[4]],
                        v26[[4]],v28[[4]],v37[[4]],v45[[4]],
                        nrow = 2,
                        labels = c("1i", "5i", "7i", "8i", "9i",
                                   "13i", "14i", "15i", "16i", 
                                   "20i", "26v",
                                   "28v", "37v", "45v" 
                                   ))
peace_plots

not_peace_plots = plot_grid(i2[[4]],i3[[4]],i4[[4]],i6[[4]],
                            i10[[4]],i11[[4]],i12[[4]],
                            i17[[4]],i18[[4]],i19[[4]],
                            i21[[4]],i22[[4]],i23[[4]],i24[[4]],
                            i25[[4]],
                            v27[[4]],v29[[4]],v30[[4]],v31[[4]],v32[[4]],
                            v33[[4]],v34[[44],v35[[4]],v36[[4]],v38[[4]],
                            v39[[4]],v40[[4]],v41[[4]],v42[[4]],v43[[4]],
                            v44[[4]],
                            nrow = 6,
                            labels = c("2i", "3i", "4i", "6i",
                                       "10i", "11i", "12i",
                                       "17i", "18i", "19i", "21i",
                                       "22i", "23i", "24i", "25i",
                                       "27v"))
