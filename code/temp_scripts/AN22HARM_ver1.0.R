#
#Title: Evolution of Peace in Experimental Social Networks

#target like: https://www.nature.com/articles/nature10384

#Questions. 
#Q1: Who attacks (=performing costly sanction)? (social demographic emotional)
#(who include attack in their behavior strategy?)

#Q2: Can attack behavior spreads/sustains over social networks?  
#(dose-response?)

#Q3: Will attackers win the race (highly adaptive? evolutionarily making sense?) 
#Rand et al, "Winners do not punish", 2008, Nature

#Q4: What can stop attacks (consecutive attacks)? 

#Q5: Will attackers attack again? 

#Q6: Long-time consideration can stop attacks? (intuitive or conflictive behavior?) 

#Q7: Can we predict attacks? (machine learning, LSTM?)

#Q8: If attackers do not attack [i.e. D] (counterfactual simulation), does social welfare
#(wealth size) gain? or Does attack contribute to evolution of cooperation?

#Q9: no attack ever is the best - what can serve as an early-warning signal
#of attack? - coop rate down?

#Q10: When attacked: attack back? cut ties? or mercy cooperation?
#(not individual though)

#Q11: Evolutionary understanding of "attack": social or asocial?

#Q12: The attack behavior is attributable to self or connecting others or all?
#(causal pies)

#Q13: Does defector become attacker or does cooperator become attacker? 
#cooperator surrounded by defector - the degree of conflict, which may predict
#an attack event?

#Q14: Can no attack be "learned" over the experiment? 
#Who are really making peace?

#local gini may matter? 

#WHAT ARE THE 3 Surprising messages??
#Who causes a war is not the attacker in the cascade: but 
#a defector: fundamental cause  (needs to be tested).

#Not the first attack but the attack back (second one) determines the
#evolution of war state (needs to be tested). If attackers are ignored, 
#attackers may realize the cost of the attack.

#Selection favors "ignorers" or "mercy cooperaators" (attackers or
#non-attackers) (needs to be tested)


#REVIEWR: RAND, CENTOLA, APICELLA, 
#Co-author: BROOKMEYER?

###Install and load some packages that are going to be used.
#install.packages("reldist")
library(reldist)
library(dplyr)
library(lme4)
library(ggplot2)
library(mediation) #causal mediation analysis
library(lmerTest)
library(igraph) #for data visualization of network graphs

###Data entry
setwd("/Users/akihironishi/Dropbox/ArticlesAN/Active-AN22HARM-NatEE/Data")
load("nodedataHarming.rdata") #ndata for harming experiment
harmdata = ndata
###making superid
harmdata$superid = 20000+100*harmdata$game+as.numeric(substr(harmdata$id,2,nchar(harmdata$id)))
#2XXXX for the harming experiment

#local_rate_coop and other detailed data
#This will be done when we need other variables.
load("Harminglinknodedata.Rdata") #mdata2 for the harming expriment
harmlinknode = mdata2
harmlinknode = harmlinknode[,c("superid","round","cur_local_rate_coop","n_ties")]
names(harmlinknode)[3] = "local_rate_coop"
names(harmlinknode)[4] = "degree"
harmlinknode$superid = 20000 + harmlinknode$superid
harmdata = merge(x=harmdata,y=harmlinknode,by=c("superid","round"),all=T) #10727

###a function to standardize
standardize <- function(x) {
  mu = mean(x, na.rm = T)
  sd = sd(x, na.rm = T) 
  return ((x - mu)/sd)
}

###making a continuous variable of happiness
harmdata$happ <- ifelse(harmdata$satisfaction == "v_good", 2, 
                        ifelse(harmdata$satisfaction == "good", 1, 
                               ifelse(harmdata$satisfaction == "neutral", 0,
                                      ifelse(harmdata$satisfaction == "bad", -1, 
                                             ifelse(harmdata$satisfaction == "v_bad", -2, NA)))))

###making wealth (when <0, 0)
harmdata$PosWealth <- ifelse(harmdata$cumulativePayoff >= 0, harmdata$cumulativePayoff, 0)

###making cPayoffS (standardized cumulative payoff, gini_wealth, and gini_wellbeing)
HarmStandard <- as.data.frame(matrix(NA, ncol = ncol(harmdata) + 3, nrow = 0))
names(HarmStandard) <- c(names(harmdata), "cPayoffS", "gini_wealth", "gini_wellbeing")
for (i in 0:max(harmdata$round)){   #number of rounds 
  for (j in 1:max(harmdata$game)){ #number of games
    temp = harmdata[(harmdata$game == j & harmdata$round == i), ]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    temp$gini_wealth <- gini(temp$PosWealth)
    temp$gini_wellbeing <- gini(temp$happ)
    HarmStandard <- rbind(HarmStandard, temp)
  }
} 

###Making 5 wealth categories
HarmStandard <- HarmStandard %>%
  mutate(WealthLevel = case_when(cPayoffS < -1.5 ~ "Poorest",
                                 cPayoffS >= -1.5 & cPayoffS < -0.5  ~ "Poorer",
                                 cPayoffS >= -0.5 & cPayoffS < 0.5  ~ "Middle",
                                 cPayoffS >= 0.5 & cPayoffS < 1.5  ~ "Richer",
                                 cPayoffS >= 1.5  ~ "Richest"))

#Making a 0/1 cooperation variable
HarmStandard <- HarmStandard %>%
  mutate(coop = case_when(behavior == "C" ~ 1,behavior %in% c("D","P") ~ 0))

HarmStandard$showScore = ifelse(HarmStandard$showScore=="true",1,0)

###Renaming game ID as well (1XX for Happ, 2XX for Harm)
HarmStandard$game = 200 + HarmStandard$game

###making the variable names the same over the two data
HarmStandard = HarmStandard[,!(names(HarmStandard) == "gameNumber")]

#Harm variable
HarmStandard$harm = ifelse(HarmStandard$behavior=="P",1,0)

#Other variables
HarmStandard$age = as.numeric(unlist(HarmStandard$age))
HarmStandard$gender = as.character(unlist(HarmStandard$gender))
HarmStandard$behaviorTime = as.numeric(unlist(HarmStandard$behaviorTime))

#Making the minimum data
data0  = HarmStandard[,c("game","superid","round","age","gender","showScore",
                         "initScore","payoff","cumulativePayoff","cPayoffS",
                         "WealthLevel","coop","local_rate_coop","degree",
                         "happ","harm","behaviorTime")]

#Making a lag data
data_lag = data0[,c("superid","round",
                    "initScore","payoff","cumulativePayoff","cPayoffS",
                    "WealthLevel","coop","local_rate_coop","degree",
                    "happ","harm","behaviorTime")]
names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1

#merge
data1 = merge(x=data0,y=data_lag,all.x=T,all.y=F,by=c("superid","round"))

###############################################################################

data1$log_behaviorTime = log10(data1$behaviorTime)
data1$log_behaviorTime_lag = log10(data1$behaviorTime_lag)

summary(zelig(data = data1, harm ~ showScore + age + gender,model="logit"))
summary(zelig(data = data1[is.na(data1$harm) == 0,], harm ~ showScore,
              model="logit.gee",id="superid",corstr="exchangeable"))

summary(zelig(data = data1, harm ~ showScore +gender,
              model="logit.gee",id="game",corstr="exchangeable"))

summary(zelig(data = data1, harm ~ showScore + age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag,model="logit"))

summary(zelig(data = data1, harm ~ showScore + age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+behaviorTime+factor(round),model="logit"))

summary(zelig(data = data1, harm ~ showScore + age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+behaviorTime+factor(round),model="logit"))

data2 = data1[,c("harm","showScore","age","gender","coop_lag","harm_lag",
                 "local_rate_coop_lag","cPayoffS_lag","degree_lag","happ_lag",
                 "log_behaviorTime","game","superid","round")]
data3 = na.omit(data2)

summary(zelig(data = data3, harm ~ showScore + age + gender + coop_lag + harm_lag + 
                local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+
                log_behaviorTime+factor(round),
              model="logit.gee",id="superid",corstr="exchangeable"))
#NOTE: local_rate_coop in round 2?

summary(zelig(data = data3, harm ~ showScore + age + gender + coop_lag + harm_lag + 
                local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+
                factor(round),
              model="logit.gee",id="superid",corstr="exchangeable"))

#multi-level logit
#https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/

m <- glmer(harm ~ showScore + age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+log_behaviorTime +
             factor(round) + (1 | game) + (1 | superid), data = data3, family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
print(m, corr = FALSE)
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se)

round(exp(tab), digits=3)[1:11,]

m1 <- glmer(harm ~ age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+log_behaviorTime +
             factor(round) + (1 | game) + (1 | superid), data = data3[data3$showScore==1,], family = binomial, 
           control = glmerControl(optimizer = "bobyqa"))
print(m1, corr = FALSE)
se1 <- sqrt(diag(vcov(m1)))
# table of estimates with 95% CI
tab1 <- cbind(Est = fixef(m1), LL = fixef(m1) - 1.96 * se1, UL = fixef(m1) + 1.96 *
               se1)
round(exp(tab1), digits=3)[1:10,]

m2 <- glmer(harm ~ age + gender + coop_lag + harm_lag + local_rate_coop_lag+cPayoffS_lag+degree_lag+happ_lag+log_behaviorTime +
              factor(round) + (1 | game) + (1 | superid), data = data3[data3$showScore==0,], family = binomial, 
            control = glmerControl(optimizer = "bobyqa"))
print(m2, corr = FALSE)
se2 <- sqrt(diag(vcov(m2)))
# table of estimates with 95% CI
tab2 <- cbind(Est = fixef(m2), LL = fixef(m2) - 1.96 * se2, UL = fixef(m2) + 1.96 *
                se2)
round(exp(tab2), digits=3)[1:10,]


#Limitations: Not a individualistic attack

hist(data1$behaviorTime)

summary(data1[is.na(data1$behaviorTime)==0 & data1$behaviorTime>=1000 & data1$behaviorTime<3000,]$harm)
#0.03
summary(data1[is.na(data1$behaviorTime)==0 & data1$behaviorTime>=3000 & data1$behaviorTime<5000,]$harm)
#0.046
summary(data1[is.na(data1$behaviorTime)==0 & data1$behaviorTime>=5000 & data1$behaviorTime<9000,]$harm)
#0.069
summary(data1[is.na(data1$behaviorTime)==0 & data1$behaviorTime>=9000 & data1$behaviorTime<1000000,]$harm)
#0.079

#Time pressure can stop the war?

