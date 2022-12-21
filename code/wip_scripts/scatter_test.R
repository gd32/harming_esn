ggplot() +
  geom_point(aes(x=Risk_category, y=as.numeric(value)*10, size = n), data = data_fracs) +
  geom_bar(aes(x=Risk_category, y=Risk*100), stat="identity", alpha = 0.5, data=data_risk)+
  theme_classic() +
  ylab("Probability of ego \npunishment behavior (%)") +
  scale_y_continuous(breaks=seq(0,20,by=2.5)) +
  xlab('\nPrevalence of punishment among alters \nin the previous round (%)') +
  scale_x_discrete(limits=c("[0,5]\n\n 5251", "(5, 10]\n\n 219", "(10, 15]\n\n 523",
                            "(15, 20]\n\n 440", "(20, 25]\n\n 212", "(25, 100]\n\n 273")) + 
  theme(legend.position= "none",
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        strip.background = element_blank())

data_punish_props = data1_cc %>%
  mutate(alter_punish_prev= round(local_rate_punish_lag, 4)) %>%
  group_by(alter_punish_prev, behavior) %>%
  summarize(x = n()) %>%
  mutate(prevalence = x/sum(x),
         n = sum(x))

p1 = data_punish_props %>% 
  filter(behavior == "P")

confints = as_tibble(binom.confint(p1$n, p1$sum))
confints_ac = confints %>%
  filter(method == "agresti-coull")

data_p2 = data_punish_props %>%
  filter(behavior == "P") %>%
  left_join(confints_ac, by = c("x", "n")) %>%
  select(alter_punish_prev, behavior, x, n, mean, lower, upper) %>%
  mutate(risk_cat = case_when(alter_punish_prev <= 0.05 ~ 1,
                              alter_punish_prev > 0.05 & alter_punish_prev <= 0.1 ~ 2,
                              alter_punish_prev > 0.1 & alter_punish_prev <= 0.15 ~ 3,
                              alter_punish_prev > 0.15 & alter_punish_prev <= 0.2 ~ 4,
                              alter_punish_prev > 0.2 & alter_punish_prev <= 0.25 ~ 5,
                              alter_punish_prev > 0.25 ~ 6))

data_p2



library(RColorBrewer)
data_p2 %>% 
  mutate(above_threshold = ifelse(mean >= 0.075, 1, 0)) %>%
  ggplot() +
  geom_point(aes(x= alter_punish_prev, y = mean, size = x, color = factor(risk_cat))) +
  geom_vline(xintercept = 0.15, color = "red", linetype = "dotdash") +
  geom_smooth(aes(x=alter_punish_prev, y=above_threshold), method = "glm", se=FALSE,
              method.args=list(family = "binomial"), fullrange = TRUE) +
  xlab('\nPrevalence of punishment among alters \nin the previous round (%)') +
  ylab("Probability of ego \npunishment behavior (%)") +
  ylim(c(0, 1)) +
  xlim(c(0, 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(panel.grid.minor.x = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15),
        strip.text = element_text(face = "bold", size = rel(1.5)),
        strip.background = element_blank()) +
  guides(size = "none",
         color = guide_legend(title = "Risk Category"))

data_p2 %>%
  mutate(above_threshold = ifelse(mean >= 0.075, 1, 0)) %>%
  ggplot() + 
  geom_smooth(aes(x=alter_punish_prev, y=above_threshold), method = "glm", se=FALSE,
              method.args=list(family = "binomial")) 

  
data_p2
