load("~/Documents/Projects/harming_esn/code/exp1/data1_pc_v2.Rdata") 


data1_pc %>%
  ggplot(aes(x = behaviorTime_sec)) +
  geom_density() +
  xlim(0, 10)

data1_pc %>% 
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "H")) %>%
  ggplot(aes(x=behaviorTime_sec, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_log10(limits = c(1, 20),breaks = c(1, 10, 20)) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_color_manual(labels = c("C", "D", "H"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3))) +
  ggtitle("Exp 1")

data1_pc %>%
  mutate(behavior = case_when(behavior_coop == 1 ~ "C",
                              behavior_defect == 1 ~ "D",
                              behavior_punish == 1 ~ "H")) %>%
  group_by(behavior) %>%
  filter(behavior %in% c("C", "D", "H")) %>%
  summarize(mean_dt = mean1(behaviorTime_sec),
            median_dt = median1(behaviorTime_sec))
  

exp2data %>%
  ggplot(aes(x=behaviorTime/1000, colour=behavior)) +
  geom_density(adjust = 2) +
  theme_classic() +
  labs(color = "Behavior") +
  scale_x_log10(limits = c(1, 10),breaks = c(1, 3, 10)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  scale_color_manual(labels = c("C", "D", "H"),
                     values = c("orange2", "skyblue2", "red2")) +
  guides(colour=guide_legend(title = NULL))+
  theme(axis.title = element_blank(),
        axis.text = element_text(size = rel(1.3))) + 
  ggtitle("Exp 2")

exp2data %>%
  group_by(behavior) %>%
  summarize(mean_dt = mean1(behaviorTime/1000),
            median_dt = median1(behaviorTime/1000))
