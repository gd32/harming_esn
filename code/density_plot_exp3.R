library(cowplot)

# Data preparation for rug plots
data1_cc$ypos <- 0
data1_cc$ypos2 <- 0 #for sub-plots
data1_cc[behavior_coop==1, ypos:=0.06]
data1_cc[behavior_defect==1, ypos:=0.03]
data1_cc[conflict==1, ypos2:=0.1]
xtabs(~ypos+behavior, data = data1_cc)

exp3data_pc %>%
  mutate(behaviorTime_sec = behaviorTime/1000) %>%
  ggplot(aes(x = behaviorTime_sec, color = behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_continuous(limits = c(0, 10)) +
  scale_color_manual(labels = c("Cooperation", "Defection", "Punishment"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))

exp3data_pc %>%
  na.omit() %>%
  mutate(behaviorTime_sec = behaviorTime/1000) %>%
  ggplot(aes(x=behaviorTime_sec, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_y_continuous(limits = c(0, 1.3)) +
  scale_color_manual(labels = c("Cooperation", "Defection", "Punishment"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))

d_plot_f <- d_plot +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3)),
        legend.position = c(0.8, 0.8)) +
  geom_point(aes(x=behaviorTime_sec, y=ypos, colour=behavior),
             position = position_jitter(width = 0.2, height = 0),
             alpha=0.03)

d_plot_oc = data1_cc %>%
  na.omit() %>%
  filter(conflict == 0) %>%
  ggplot(aes(x=behaviorTime_sec, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_log10(limits = c(1, 110),breaks = c(1, 10, 100)) +
  scale_y_continuous(limits = c(0, 1.3)) +
  scale_color_manual(labels = c("Cooperation", "Defection", "Punishment"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3)),
        legend.position = "none") +
  geom_point(aes(x=behaviorTime_sec, y=ypos, colour=behavior),
             position = position_jitter(width = 0.2, height = 0),
             alpha=0.03)

d_plot_ic = data1_cc %>%
  na.omit() %>%
  filter(conflict == 1) %>%
  ggplot(aes(x=behaviorTime_sec, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_log10(limits = c(1, 110),breaks = c(1, 10, 100)) +
  scale_y_continuous(limits = c(0, 1.3)) +
  scale_color_manual(labels = c("Cooperation", "Defection", "Punishment"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3)),
        legend.position = "none") +
  geom_point(aes(x=behaviorTime_sec, y=ypos, colour=behavior),
             position = position_jitter(width = 0.2, height = 0),
             alpha=0.03)

grid.arrange(d_plot_f,
             d_plot_oc,
             d_plot_ic,
             layout_matrix = layout)

right_col = cowplot::plot_grid(d_plot_oc, d_plot_ic,
                               ncol = 1,
                               labels = c("No conflict", "  In conflict"))


fig2B = cowplot::plot_grid(d_plot_f, right_col, nrow = 1)

grid.arrange(fig2B,
             bottom = textGrob("Decision time (sec)",gp=gpar(fontsize=15)),
            left = textGrob("Density",gp=gpar(fontsize=15), rot = 90))


data1_cc %>%
  na.omit() %>%
  filter(conflict == 1) %>%
  ggplot(aes(x=behaviorTime_sec, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_log10(limits = c(1, 110),breaks = c(1, 10, 100)) +
  scale_y_continuous(limits = c(0, 1.3)) +
  scale_color_manual(labels = c("Cooperation", "Defection", "Punishment"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3)),
        legend.position = c(0.8, 0.8)) +
  geom_point(aes(x=behaviorTime_sec, y=ypos, colour=behavior),
             position = position_jitter(width = 0.2, height = 0),
             alpha=0.03)

data1_cc %>%
  na.omit() %>%
  filter(behavior_punish == 1) %>%
  ggplot() +
  geom_density(aes(x=behaviorTime_sec, color = local_rate_punish_cat6)) +
  scale_color_manual(labels = c("[0,5]", "(5, 10]", "(10, 15]",
                                  "(15, 20]", "(20, 25]", "(25, 100]"),
                       values = c("green", "gold", "orange", "pink", "red", "darkred"))
