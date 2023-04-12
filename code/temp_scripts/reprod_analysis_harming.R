# estimating r0 using the harming experiment data

library(R0)

## Try calculating R0 for all the rounds combined (so 3 total)
# Exp1
# infections
inf_cases_exp1 = data1_pc %>%
  filter(reactive_p == 1) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()
length(inf_cases_exp1) # should be 14
# imported
imp_cases_exp1 = data1_pc %>%
  filter(behavior_punish == 1 & reactive_p == 0) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()
length(imp_cases_exp1)

exp1_incidence = inf_cases_exp1 + imp_cases_exp1

estimate.R(epid = exp1_incidence, begin = 1, end = 14,
           GT = generation.time(type = c("empirical"), val = rep(1/14, 14)),
           methods = c("TD", "ML"))

# Exp2, TP+
inf_cases_tpplus = tpdata_pc %>%
  filter(tp_on == 1, reactive_p == 1) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()
imp_cases_tpplus = tpdata_pc %>%
  filter(tp_on == 1, reactive_p == 0, behavior_punish == 1) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()
length(inf_cases_tpplus)
length(imp_cases_tpplus)

exp2_tpplus_incidence = c(0,inf_cases_tpplus) + imp_cases_tpplus
estimate.R(epid = exp2_tpplus_incidence, begin = 1, end = 15,
           GT = generation.time(type = c("empirical"), val = rep(1/15, 15)),
           methods = c("TD", "ML", "SB"))


# Exp2, TP-
inf_cases_tpminus = tpdata_pc %>%
  filter(tp_on == 0, reactive_p == 1) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()

imp_cases_tpminus = tpdata_pc %>%
  filter(tp_on == 0, reactive_p == 0, behavior_punish == 1) %>%
  group_by(round) %>%
  summarize(count = n()) %>%
  dplyr::select(count) %>%
  pull()
length(inf_cases_tpminus)
length(imp_cases_tpminus)

exp2_tpminus_incidence = c(0,inf_cases_tpminus) + imp_cases_tpminus
estimate.R(epid = exp2_tpminus_incidence, begin = 1, end = 15,
           GT = generation.time(type = c("empirical"), val = rep(1/15, 15)),
           methods = c("TD", "ML"))
