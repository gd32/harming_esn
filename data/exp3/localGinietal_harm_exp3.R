rm(list = ls())
getwd()
setwd("~/Documents/Projects/harming_esn/data/exp3/")
load(file="node_1aX_exp3.Rdata") 
load(file="link_1aX_exp3.Rdata") 

library(reldist)
library(tidyverse)

rank1 = function(x) {rank(x,na.last=NA,ties.method="average")[1]} 
mean1 = function(x) {mean(x,na.rm=TRUE)} 
sum1 = function(x) {sum(x,na.rm=TRUE)} 
se1 = function(x) {sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))} 
set1 = function(x) {c(mean1(x),se1(x))}
sd1 = function(x) {sd(x,na.rm=TRUE)} 
length1 = function(x) {length(na.omit(x))}
gini1 = function(x) {gini(na.omit(x))}
se2 = function(x) {sqrt(mean1(x)*(1-mean1(x))/length1(x))}
set2 = function(x) {c(mean1(x),se2(x))}
gini2 = function(x) {gini(c(rep(x[1],x[2]),rep(x[3],x[4])))}
scale1 = function(x) {scale(x, center=T,scale=T)[,]} #standardization (x-mean)/sd (can ignore NA)

median1 = function(x) {median(x,na.rm=TRUE)} 
min1 = function(x) {min(x,na.rm=TRUE)} 
max1 = function(x) {max(x,na.rm=TRUE)} 
perc25 = function(x) {quantile(x,0.25,na.rm=TRUE)}
perc75 = function(x) {quantile(x,0.75,na.rm=TRUE)}


gmd = function(x) { #gini mean difference
  x1 = na.omit(x)
  n = length(x1)
  tmp = 0
  for (i in 1:n) {
    for (j in 1:n) {
      tmp <- tmp + abs(x1[i]-x1[j])
    }
  }
  answer = tmp/(n*n)
  return(answer)
}

ldata2 = ldata[,c("id2","id1","round","gameID","scoreA","scoreB","percentA","showScore","game")]
colnames(ldata2) = names(ldata)
ldata3 = rbind(ldata,ldata2)
ldata3$all = paste0(ldata3$game,"_r",ldata3$round,"_",ldata3$id1)
ldata4 = ldata3[order(ldata3$all),]

ldata4$id2number = NA
ldata4[1,]$id2number = 1
for (i in 1:(dim(ldata4)[1]-1)) {
  if (ldata4[i,]$all == ldata4[i+1,]$all) 
  {
    ldata4[i+1,]$id2number = ldata4[i,]$id2number + 1
  }
  else
  {
    ldata4[i+1,]$id2number = 1
  }
  print(i)
}
save(ldata4,file="ldata4_exp3.Rdata")
load("ldata4_exp3.Rdata") #ldata

ndata$behavior_coop = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "C", 1, 0)
ndata$behavior_defect = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "D", 1, 0)
ndata$behavior_punish = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "P", 1, 0)

# ndata$behavior = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "C", 1, ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "D", 0, NA))
ndata$behaviorTime = as.numeric(ndata$behaviorTime)
ldata5 = ldata4[c("id1","id2","round","gameID","game","scoreA","scoreB","percentA","showScore","id2number")]
ldata6 = reshape(ldata5, direction = "wide", idvar=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id1"), timevar="id2number")


#n of ties to calculate network density (degree)
ldata6$n_ties = as.numeric(apply(ldata6[,c("id2.1","id2.2","id2.3","id2.4","id2.5","id2.6","id2.7","id2.8","id2.9","id2.10", "id2.11","id2.12", "id2.13","id2.14","id2.15", "id2.16")],1,length1))
ndata1 = merge(x=ndata,y=ldata6,all.x=TRUE,all.y=FALSE,by.x=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id"),by.y=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id1"))

save(ndata1,file="ndata1_exp3.Rdata") #This is used for the individual-level analysis. 
load("ndata1_exp3.Rdata") #ndata1 

dim(ndata1[ndata1$round==0,]) #290 individuals 
dim(ndata1) #4533 actions

ndata1$superid = 100*ndata1$game+as.numeric(substr(ndata1$id,2,nchar(ndata1$id))) #the last two digits are the id in the game. 
ndata1[ndata1$round==0,"cumulativePayoff"] = ndata1[ndata1$round==0,"initScore"]
ndata2 = ndata1[,c("scoreA","scoreB","percentA","satisfaction","showScore","game","round","initScore","behavior_coop", "behavior_defect", "behavior_punish","behaviorTime","payoff","cumulativePayoff","n_ties","id","superid")] #Variable pruning
ndata3 = reshape(ndata2, direction = "wide", idvar=c("scoreA","scoreB","percentA","showScore","game","round"), timevar="id") 

standardize <- function(x) {
  mu = mean(x)
  sd = sd(x) 
  return ((x - mu)/sd)
}

#add standardized CumulativePayoff
ndataStandard <- as.data.frame(matrix(NA, ncol = ncol(ndata1) + 1, nrow = 0))
names(ndataStandard) <- c(names(ndata1), "cPayoffS")
for (j in 1:50){ #number of games
    temp = ndata1[(ndata1$game == j),]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    ndataStandard <- rbind(ndataStandard, temp)
}

ndata1 = ndataStandard


ndata1[ndata1$round==0,"cumulativePayoff"] = ndata1[ndata1$round==0,"initScore"]
ndata1$superid = 100*ndata1$game+as.numeric(substr(ndata1$id,2,nchar(ndata1$id)))
ndata1$superid2.1 = ifelse(is.na(ndata1$id2.1)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.1,2,nchar(as.character(ndata1$id2.1)))))
ndata1$superid2.2 = ifelse(is.na(ndata1$id2.2)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.2,2,nchar(as.character(ndata1$id2.2)))))
ndata1$superid2.3 = ifelse(is.na(ndata1$id2.3)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.3,2,nchar(as.character(ndata1$id2.3)))))
ndata1$superid2.4 = ifelse(is.na(ndata1$id2.4)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.4,2,nchar(as.character(ndata1$id2.4)))))
ndata1$superid2.5 = ifelse(is.na(ndata1$id2.5)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.5,2,nchar(as.character(ndata1$id2.5)))))
ndata1$superid2.6 = ifelse(is.na(ndata1$id2.6)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.6,2,nchar(as.character(ndata1$id2.6)))))
ndata1$superid2.7 = ifelse(is.na(ndata1$id2.7)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.7,2,nchar(as.character(ndata1$id2.7)))))
ndata1$superid2.8 = ifelse(is.na(ndata1$id2.8)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.8,2,nchar(as.character(ndata1$id2.8)))))
ndata1$superid2.9 = ifelse(is.na(ndata1$id2.9)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.9,2,nchar(as.character(ndata1$id2.9)))))
ndata1$superid2.10 = ifelse(is.na(ndata1$id2.10)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.10,2,nchar(as.character(ndata1$id2.10)))))
ndata1$superid2.11 = ifelse(is.na(ndata1$id2.11)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.11,2,nchar(as.character(ndata1$id2.11)))))
ndata1$superid2.12 = ifelse(is.na(ndata1$id2.12)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.12,2,nchar(as.character(ndata1$id2.12)))))
ndata1$superid2.13 = ifelse(is.na(ndata1$id2.13)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.13,2,nchar(as.character(ndata1$id2.13)))))
ndata1$superid2.14 = ifelse(is.na(ndata1$id2.14)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.14,2,nchar(as.character(ndata1$id2.14)))))
ndata1$superid2.15 = ifelse(is.na(ndata1$id2.15)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.15,2,nchar(as.character(ndata1$id2.15)))))
ndata1$superid2.16 = ifelse(is.na(ndata1$id2.16)==1,NA,100*ndata1$game+as.numeric(substr(ndata1$id2.16,2,nchar(as.character(ndata1$id2.16)))))

#Proc A: Preparing for the outcome data of each round
#Including: game infor, ego ID, and ego's cooperative behavior
#The identifier is gameID, round, and id. 
#behavior at round is one round lag forward.
outcome_data = ndata1[ndata1$round %in% c(0:15),c("gameID","game","scoreA","scoreB","percentA","satisfaction","behaviorTime","showScore","round","superid","payoff","cumulativePayoff","behavior","n_ties", "satisfaction", "cPayoffS", "timeUp", "rewiringSatisfaction")]
outcome_data$previousround = outcome_data$round - 1

#Proc B: Preparing for the ego's attribute and network data
#The basics needs to be connected with all the ego and alter

network_data = ndata1[,c("gameID","game","round","superid","age", "gender", "ipAddress","race","is_hispanic","behavior_coop","behavior_defect", "behavior_punish","payoff","cumulativePayoff", "satisfaction","cPayoffS","behaviorTime","rewiringSatisfaction","timeUp","superid2.1","superid2.2","superid2.3","superid2.4","superid2.5","superid2.6","superid2.7","superid2.8","superid2.9","superid2.10","superid2.11","superid2.12","superid2.13","superid2.14","superid2.15","superid2.16")]

network_data = network_data %>% rename(previousround = round,
                                       e_behavior_coop = behavior_coop,
                                       e_behavior_defect = behavior_defect,
                                       e_behavior_punish = behavior_punish,
                                       e_payoff = payoff,
                                       e_cumulativePayoff = cumulativePayoff,
                                       e_satisfaction = satisfaction,
                                       e_stdPayoff = cPayoffS,
                                       e_behaviorTime = behaviorTime,
                                       e_rewiringSatisfaction = rewiringSatisfaction,
                                       e_timeUp = timeUp)


#Proc C: Preparing for the alter's attribute data
alter_data = ndata1[,c("gameID","game","round","superid","behavior_coop", "behavior_defect", "behavior_punish","payoff","cumulativePayoff" ,"satisfaction","cPayoffS","timeUp", "rewiringSatisfaction")]
names(alter_data) = c("gameID","game","previousround","a_superid","a_behavior_coop", "a_behavior_defect", "a_behavior_punish","a_payoff","a_cumulativePayoff","a_satisfaction","a_cPayoffS","a_timeUp", "a_rewiringSatisfaction")

#and for current round
alter_data2 = ndata1[,c("gameID","game","round","superid","behavior_coop", "behavior_defect", "behavior_punish","payoff","cumulativePayoff" ,"satisfaction","cPayoffS","timeUp", "rewiringSatisfaction")]
names(alter_data2) = c("gameID","game","currentround","c_superid","c_behavior_coop", "c_behavior_defect", "c_behavior_punish","c_payoff","c_cumulativePayoff","c_satisfaction","c_cPayoffS","c_timeUp", "c_rewiringSatisfaction")

a1_data = a2_data = a3_data = a4_data = a5_data = a6_data = a7_data = a8_data = a9_data = a10_data = a11_data = a12_data = a13_data = a14_data = a15_data = a16_data = a17_data = a18_data = a19_data = a20_data = alter_data
names(a1_data) = c("gameID","game","previousround","a1_superid","a1_behavior_coop", "a1_behavior_defect", "a1_behavior_punish","a1_payoff","a1_cumulativePayoff", "a1_satisfaction","a1_cPayoffS", "a1_timeUp", "a1_rewiringSatisfaction")
names(a2_data) = c("gameID","game","previousround","a2_superid","a2_behavior_coop", "a2_behavior_defect", "a2_behavior_punish","a2_payoff","a2_cumulativePayoff", "a2_satisfaction","a2_cPayoffS", "a2_timeUp", "a2_rewiringSatisfaction")
names(a3_data) = c("gameID","game","previousround","a3_superid","a3_behavior_coop", "a3_behavior_defect", "a3_behavior_punish","a3_payoff","a3_cumulativePayoff", "a3_satisfaction","a3_cPayoffS", "a3_timeUp", "a3_rewiringSatisfaction")
names(a4_data) = c("gameID","game","previousround","a4_superid","a4_behavior_coop", "a4_behavior_defect", "a4_behavior_punish","a4_payoff","a4_cumulativePayoff", "a4_satisfaction","a4_cPayoffS", "a4_timeUp", "a4_rewiringSatisfaction")
names(a5_data) = c("gameID","game","previousround","a5_superid","a5_behavior_coop", "a5_behavior_defect", "a5_behavior_punish","a5_payoff","a5_cumulativePayoff", "a5_satisfaction","a5_cPayoffS", "a5_timeUp", "a5_rewiringSatisfaction")
names(a6_data) = c("gameID","game","previousround","a6_superid","a6_behavior_coop", "a6_behavior_defect", "a6_behavior_punish","a6_payoff","a6_cumulativePayoff", "a6_satisfaction","a6_cPayoffS", "a6_timeUp", "a6_rewiringSatisfaction")
names(a7_data) = c("gameID","game","previousround","a7_superid","a7_behavior_coop", "a7_behavior_defect", "a7_behavior_punish","a7_payoff","a7_cumulativePayoff", "a7_satisfaction","a7_cPayoffS", "a7_timeUp", "a7_rewiringSatisfaction")
names(a8_data) = c("gameID","game","previousround","a8_superid","a8_behavior_coop", "a8_behavior_defect", "a8_behavior_punish","a8_payoff","a8_cumulativePayoff", "a8_satisfaction","a8_cPayoffS", "a8_timeUp", "a8_rewiringSatisfaction")
names(a9_data) = c("gameID","game","previousround","a9_superid","a9_behavior_coop", "a9_behavior_defect", "a9_behavior_punish","a9_payoff","a9_cumulativePayoff", "a9_satisfaction","a9_cPayoffS", "a9_timeUp", "a9_rewiringSatisfaction")
names(a10_data) = c("gameID","game","previousround","a10_superid","a10_behavior_coop", "a10_behavior_defect", "a10_behavior_punish","a10_payoff","a10_cumulativePayoff", "a10_satisfaction","a10_cPayoffS",
                    "a10_timeUp", "a10_rewiringSatisfaction")
names(a11_data) = c("gameID","game","previousround","a11_superid","a11_behavior_coop", "a11_behavior_defect", "a11_behavior_punish","a11_payoff","a11_cumulativePayoff", "a11_satisfaction","a11_cPayoffS",
                    "a11_timeUp", "a11_rewiringSatisfaction")
names(a12_data) = c("gameID","game","previousround","a12_superid","a12_behavior_coop", "a12_behavior_defect", "a12_behavior_punish","a12_payoff","a12_cumulativePayoff", "a12_satisfaction","a12_cPayoffS",
                    "a12_timeUp", "a12_rewiringSatisfaction")
names(a13_data) = c("gameID","game","previousround","a13_superid","a13_behavior_coop", "a13_behavior_defect", "a13_behavior_punish","a13_payoff","a13_cumulativePayoff", "a13_satisfaction","a13_cPayoffS",
                    "a13_timeUp", "a13_rewiringSatisfaction")
names(a14_data) = c("gameID","game","previousround","a14_superid","a14_behavior_coop", "a14_behavior_defect", "a14_behavior_punish","a14_payoff","a14_cumulativePayoff", "a14_satisfaction","a14_cPayoffS",
                    "a14_timeUp", "a14_rewiringSatisfaction")
names(a15_data) = c("gameID","game","previousround","a15_superid","a15_behavior_coop", "a15_behavior_defect", "a15_behavior_punish","a15_payoff","a15_cumulativePayoff", "a15_satisfaction","a15_cPayoffS",
                    "a15_timeUp", "a15_rewiringSatisfaction")
names(a16_data) = c("gameID","game","previousround","a16_superid","a16_behavior_coop", "a16_behavior_defect", "a16_behavior_punish","a16_payoff","a16_cumulativePayoff", "a16_satisfaction","a16_cPayoffS",
                    "a16_timeUp", "a16_rewiringSatisfaction")

# rename names to right order
c1_data = c2_data = c3_data = c4_data = c5_data = c6_data = c7_data = c8_data = c9_data = c10_data = c11_data = c12_data = c13_data = c14_data = c15_data = c16_data = c17_data = c18_data = c19_data = c20_data = alter_data2
names(c1_data) = c("gameID","game","currentround","c1_superid","c1_behavior_coop", "c1_behavior_defect", "c1_behavior_punish","c1_payoff","c1_cumulativePayoff", "c1_satisfaction", "c1_cPayoffS",
                   "c1_timeUp", "c1_rewiringSatisfaction")
names(c2_data) = c("gameID","game","currentround","c2_superid","c2_behavior_coop", "c2_behavior_defect", "c2_behavior_punish","c2_payoff","c2_cumulativePayoff", "c2_satisfaction", "c2_cPayoffS",
                   "c2_timeUp", "c2_rewiringSatisfaction")
names(c3_data) = c("gameID","game","currentround","c3_superid","c3_behavior_coop", "c3_behavior_defect", "c3_behavior_punish","c3_payoff","c3_cumulativePayoff", "c3_satisfaction", "c3_cPayoffS",
                   "c3_timeUp", "c3_rewiringSatisfaction")
names(c4_data) = c("gameID","game","currentround","c4_superid","c4_behavior_coop", "c4_behavior_defect", "c4_behavior_punish","c4_payoff","c4_cumulativePayoff", "c4_satisfaction", "c4_cPayoffS",
                   "c4_timeUp", "c4_rewiringSatisfaction")
names(c5_data) = c("gameID","game","currentround","c5_superid","c5_behavior_coop", "c5_behavior_defect", "c5_behavior_punish","c5_payoff","c5_cumulativePayoff", "c5_satisfaction", "c5_cPayoffS",
                   "c5_timeUp", "c5_rewiringSatisfaction")
names(c6_data) = c("gameID","game","currentround","c6_superid","c6_behavior_coop", "c6_behavior_defect", "c6_behavior_punish","c6_payoff","c6_cumulativePayoff", "c6_satisfaction", "c6_cPayoffS",
                   "c6_timeUp", "c6_rewiringSatisfaction")
names(c7_data) = c("gameID","game","currentround","c7_superid","c7_behavior_coop", "c7_behavior_defect", "c7_behavior_punish","c7_payoff","c7_cumulativePayoff", "c7_satisfaction", "c7_cPayoffS",
                   "c7_timeUp", "c7_rewiringSatisfaction")
names(c8_data) = c("gameID","game","currentround","c8_superid","c8_behavior_coop", "c8_behavior_defect", "c8_behavior_punish","c8_payoff","c8_cumulativePayoff", "c8_satisfaction", "c8_cPayoffS",
                   "c8_timeUp", "c8_rewiringSatisfaction")
names(c9_data) = c("gameID","game","currentround","c9_superid","c9_behavior_coop", "c9_behavior_defect", "c9_behavior_punish","c9_payoff","c9_cumulativePayoff", "c9_satisfaction", "c9_cPayoffS",
                   "c9_timeUp", "c9_rewiringSatisfaction")
names(c10_data) = c("gameID","game","currentround","c10_superid","c10_behavior_coop", "c10_behavior_defect", "c10_behavior_punish","c10_payoff","c10_cumulativePayoff", "c10_satisfaction", "c10_cPayoffS",
                    "c10_timeUp", "c10_rewiringSatisfaction")
names(c11_data) = c("gameID","game","currentround","c11_superid","c11_behavior_coop", "c11_behavior_defect", "c11_behavior_punish","c11_payoff","c11_cumulativePayoff", "c11_satisfaction", "c11_cPayoffS",
                    "c11_timeUp", "c11_rewiringSatisfaction")
names(c12_data) = c("gameID","game","currentround","c12_superid","c12_behavior_coop", "c12_behavior_defect", "c12_behavior_punish","c12_payoff","c12_cumulativePayoff", "c12_satisfaction", "c12_cPayoffS",
                    "c12_timeUp", "c12_rewiringSatisfaction")
names(c13_data) = c("gameID","game","currentround","c13_superid","c13_behavior_coop", "c13_behavior_defect", "c13_behavior_punish","c13_payoff","c13_cumulativePayoff", "c13_satisfaction", "c13_cPayoffS",
                    "c13_timeUp", "c13_rewiringSatisfaction")
names(c14_data) = c("gameID","game","currentround","c14_superid","c14_behavior_coop", "c14_behavior_defect", "c14_behavior_punish","c14_payoff","c14_cumulativePayoff", "c14_satisfaction", "c14_cPayoffS",
                    "c14_timeUp", "c14_rewiringSatisfaction")
names(c15_data) = c("gameID","game","currentround","c15_superid","c15_behavior_coop", "c15_behavior_defect", "c15_behavior_punish","c15_payoff","c15_cumulativePayoff", "c15_satisfaction", "c15_cPayoffS",
                    "c15_timeUp", "c15_rewiringSatisfaction")
names(c16_data) = c("gameID","game","currentround","c16_superid","c16_behavior_coop", "c16_behavior_defect", "c16_behavior_punish","c16_payoff","c16_cumulativePayoff", "c16_satisfaction", "c16_cPayoffS",
                    "c16_timeUp", "c16_rewiringSatisfaction")
names(c17_data) = c("gameID","game","currentround","c17_superid","c17_behavior_coop", "c17_behavior_defect", "c17_behavior_punish","c17_payoff","c17_cumulativePayoff", "c17_satisfaction", "c17_cPayoffS",
                    "c17_timeUp", "c17_rewiringSatisfaction")
names(c18_data) = c("gameID","game","currentround","c18_superid","c18_behavior_coop", "c18_behavior_defect", "c18_behavior_punish","c18_payoff","c18_cumulativePayoff", "c18_satisfaction", "c18_cPayoffS",
                    "c18_timeUp", "c18_rewiringSatisfaction")
names(c19_data) = c("gameID","game","currentround","c19_superid","c19_behavior_coop", "c19_behavior_defect", "c19_behavior_punish","c19_payoff","c19_cumulativePayoff", "c19_satisfaction", "c19_cPayoffS",
                    "c19_timeUp", "c19_rewiringSatisfaction")
names(c20_data) = c("gameID","game","currentround","c20_superid","c20_behavior_coop", "c20_behavior_defect", "c20_behavior_punish","c20_payoff","c20_cumulativePayoff", "c20_satisfaction", "c20_cPayoffS",
                    "c20_timeUp", "c20_rewiringSatisfaction")

#Proc D: Merge the data
outcome_network = merge(x=outcome_data,y=network_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid"),by.y=c("gameID","game","previousround","superid"))
outcome_network_a1 = merge(x=outcome_network,y=a1_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.1"),by.y=c("gameID","game","previousround","a1_superid"))
outcome_network_a2 = merge(x=outcome_network_a1,y=a2_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.2"),by.y=c("gameID","game","previousround","a2_superid"))
outcome_network_a3 = merge(x=outcome_network_a2,y=a3_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.3"),by.y=c("gameID","game","previousround","a3_superid"))
outcome_network_a4 = merge(x=outcome_network_a3,y=a4_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.4"),by.y=c("gameID","game","previousround","a4_superid"))
outcome_network_a5 = merge(x=outcome_network_a4,y=a5_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.5"),by.y=c("gameID","game","previousround","a5_superid"))
outcome_network_a6 = merge(x=outcome_network_a5,y=a6_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.6"),by.y=c("gameID","game","previousround","a6_superid"))
outcome_network_a7 = merge(x=outcome_network_a6,y=a7_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.7"),by.y=c("gameID","game","previousround","a7_superid"))
outcome_network_a8 = merge(x=outcome_network_a7,y=a8_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.8"),by.y=c("gameID","game","previousround","a8_superid"))
outcome_network_a9 = merge(x=outcome_network_a8,y=a9_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.9"),by.y=c("gameID","game","previousround","a9_superid"))
outcome_network_a10 = merge(x=outcome_network_a9,y=a10_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.10"),by.y=c("gameID","game","previousround","a10_superid"))
outcome_network_a11 = merge(x=outcome_network_a10,y=a11_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.11"),by.y=c("gameID","game","previousround","a11_superid"))
outcome_network_a12 = merge(x=outcome_network_a11,y=a12_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.12"),by.y=c("gameID","game","previousround","a12_superid"))
outcome_network_a13 = merge(x=outcome_network_a12,y=a13_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.13"),by.y=c("gameID","game","previousround","a13_superid"))
outcome_network_a14 = merge(x=outcome_network_a13,y=a14_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.14"),by.y=c("gameID","game","previousround","a14_superid"))
outcome_network_a15 = merge(x=outcome_network_a14,y=a15_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.15"),by.y=c("gameID","game","previousround","a15_superid"))
outcome_network_a16 = merge(x=outcome_network_a15,y=a16_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.16"),by.y=c("gameID","game","previousround","a16_superid"))
# outcome_network_a17 = merge(x=outcome_network_a16,y=a17_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.17"),by.y=c("gameID","game","previousround","a17_superid"))
# outcome_network_a18 = merge(x=outcome_network_a17,y=a18_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.18"),by.y=c("gameID","game","previousround","a18_superid"))
# outcome_network_a19 = merge(x=outcome_network_a18,y=a19_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.19"),by.y=c("gameID","game","previousround","a19_superid"))
# mdata1 = merge(x=outcome_network_a19,y=a20_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","previousround","superid2.20"),by.y=c("gameID","game","previousround","a20_superid"))
mdata1 = outcome_network_a16
mdata1 = as.matrix(mdata1)
write.csv(mdata1, "mdata1_exp3.csv", quote=FALSE, row.names=FALSE) 


outcome_network_c1 = merge(x=outcome_network_a14,y=c1_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.1"),by.y=c("gameID","game","currentround","c1_superid"))
outcome_network_c2 = merge(x=outcome_network_c1,y=c2_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.2"),by.y=c("gameID","game","currentround","c2_superid"))
outcome_network_c3 = merge(x=outcome_network_c2,y=c3_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.3"),by.y=c("gameID","game","currentround","c3_superid"))
outcome_network_c4 = merge(x=outcome_network_c3,y=c4_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.4"),by.y=c("gameID","game","currentround","c4_superid"))
outcome_network_c5 = merge(x=outcome_network_c4,y=c5_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.5"),by.y=c("gameID","game","currentround","c5_superid"))
outcome_network_c6 = merge(x=outcome_network_c5,y=c6_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.6"),by.y=c("gameID","game","currentround","c6_superid"))
outcome_network_c7 = merge(x=outcome_network_c6,y=c7_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.7"),by.y=c("gameID","game","currentround","c7_superid"))
outcome_network_c8 = merge(x=outcome_network_c7,y=c8_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.8"),by.y=c("gameID","game","currentround","c8_superid"))
outcome_network_c9 = merge(x=outcome_network_c8,y=c9_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.9"),by.y=c("gameID","game","currentround","c9_superid"))
outcome_network_c10 = merge(x=outcome_network_c9,y=c10_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.10"),by.y=c("gameID","game","currentround","c10_superid"))
outcome_network_c11 = merge(x=outcome_network_c10,y=c11_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.11"),by.y=c("gameID","game","currentround","c11_superid"))
outcome_network_c12 = merge(x=outcome_network_c11,y=c12_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.12"),by.y=c("gameID","game","currentround","c12_superid"))
outcome_network_c13 = merge(x=outcome_network_c12,y=c13_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.13"),by.y=c("gameID","game","currentround","c13_superid"))
outcome_network_c14 = merge(x=outcome_network_c13,y=c14_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.14"),by.y=c("gameID","game","currentround","c14_superid"))
outcome_network_c15 = merge(x=outcome_network_c14,y=c15_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.15"),by.y=c("gameID","game","currentround","c15_superid"))
outcome_network_c16 = merge(x=outcome_network_c15,y=c16_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.16"),by.y=c("gameID","game","currentround","c16_superid"))

mdata1 = outcome_network_c16

mdata1$e_degree = as.numeric(apply(mdata1[,c("superid2.1","superid2.2","superid2.3","superid2.4","superid2.5","superid2.6","superid2.7","superid2.8","superid2.9","superid2.10","superid2.11","superid2.12","superid2.13","superid2.14","superid2.15","superid2.16")],1,length1))

#rate of richer neighbors
mdata1$richer_a1 = as.numeric(mdata1$a1_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a2 = as.numeric(mdata1$a2_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a3 = as.numeric(mdata1$a3_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a4 = as.numeric(mdata1$a4_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a5 = as.numeric(mdata1$a5_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a6 = as.numeric(mdata1$a6_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a7 = as.numeric(mdata1$a7_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a8 = as.numeric(mdata1$a8_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a9 = as.numeric(mdata1$a9_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a10 = as.numeric(mdata1$a10_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a11 = as.numeric(mdata1$a11_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a12 = as.numeric(mdata1$a12_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a13 = as.numeric(mdata1$a13_cumulativePayoff > mdata1$e_cumulativePayoff)
mdata1$richer_a14 = as.numeric(mdata1$a14_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a15 = as.numeric(mdata1$a15_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a16 = as.numeric(mdata1$a16_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a17 = as.numeric(mdata1$a17_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a18 = as.numeric(mdata1$a18_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a19 = as.numeric(mdata1$a19_cumulativePayoff > mdata1$e_cumulativePayoff)
# mdata1$richer_a20 = as.numeric(mdata1$a20_cumulativePayoff > mdata1$e_cumulativePayoff)
#
mdata1$rate_rich = as.numeric(apply(mdata1[,c("richer_a1","richer_a2","richer_a3","richer_a4","richer_a5","richer_a6","richer_a7","richer_a8","richer_a9","richer_a10","richer_a11","richer_a12","richer_a13","richer_a14")],1,mean1))
mdata1$local_gini = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,gini1))
mdata1$local_avg_wealth = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,mean1))
mdata1$local_med_wealth = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,median1))
mdata1$local_avg_stdwealth = as.numeric(apply(mdata1[,c("e_stdPayoff","a1_cPayoffS","a2_cPayoffS","a3_cPayoffS","a4_cPayoffS","a5_cPayoffS","a6_cPayoffS","a7_cPayoffS","a8_cPayoffS","a9_cPayoffS","a10_cPayoffS","a11_cPayoffS","a12_cPayoffS","a13_cPayoffS","a14_cPayoffS")],1,mean1))
mdata1$rel_wealth = mdata1$e_stdPayoff - as.numeric(apply(mdata1[,c("e_stdPayoff","a1_cPayoffS","a2_cPayoffS","a3_cPayoffS","a4_cPayoffS","a5_cPayoffS","a6_cPayoffS","a7_cPayoffS","a8_cPayoffS","a9_cPayoffS","a10_cPayoffS","a11_cPayoffS","a12_cPayoffS","a13_cPayoffS","a14_cPayoffS")],1,mean1))

mdata1$rank = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,rank1))
mdata1$rel_rank = (mdata1$rank-1)/(mdata1$e_degree) #changed the definition. 

# rate comparisons for the different behaviors
mdata1$prev_local_rate_coop = as.numeric(apply(mdata1[,c("a1_behavior_coop","a2_behavior_coop","a3_behavior_coop","a4_behavior_coop","a5_behavior_coop","a6_behavior_coop","a7_behavior_coop","a8_behavior_coop","a9_behavior_coop","a10_behavior_coop","a11_behavior_coop","a12_behavior_coop","a13_behavior_coop","a14_behavior_coop")],1,mean1))
mdata1$cur_local_rate_coop = as.numeric(apply(mdata1[,c("c1_behavior_coop","c2_behavior_coop","c3_behavior_coop","c4_behavior_coop","c5_behavior_coop","c6_behavior_coop","c7_behavior_coop","c8_behavior_coop","c9_behavior_coop","c10_behavior_coop","c11_behavior_coop","c12_behavior_coop","c13_behavior_coop","c14_behavior_coop")],1,mean1))
mdata1$prev_local_rate_defect = as.numeric(apply(mdata1[,c("a1_behavior_defect","a2_behavior_defect","a3_behavior_defect","a4_behavior_defect","a5_behavior_defect","a6_behavior_defect","a7_behavior_defect","a8_behavior_defect","a9_behavior_defect","a10_behavior_defect","a11_behavior_defect","a12_behavior_defect","a13_behavior_defect","a14_behavior_defect")],1,mean1))
mdata1$cur_local_rate_defect = as.numeric(apply(mdata1[,c("c1_behavior_defect","c2_behavior_defect","c3_behavior_defect","c4_behavior_defect","c5_behavior_defect","c6_behavior_defect","c7_behavior_defect","c8_behavior_defect","c9_behavior_defect","c10_behavior_defect","c11_behavior_defect","c12_behavior_defect","c13_behavior_defect","c14_behavior_defect")],1,mean1))
mdata1$prev_local_rate_punish = as.numeric(apply(mdata1[,c("a1_behavior_punish","a2_behavior_punish","a3_behavior_punish","a4_behavior_punish","a5_behavior_punish","a6_behavior_punish","a7_behavior_punish","a8_behavior_punish","a9_behavior_punish","a10_behavior_punish","a11_behavior_punish","a12_behavior_punish","a13_behavior_punish","a14_behavior_punish")],1,mean1))
mdata1$cur_local_rate_punish = as.numeric(apply(mdata1[,c("c1_behavior_punish","c2_behavior_punish","c3_behavior_punish","c4_behavior_punish","c5_behavior_punish","c6_behavior_punish","c7_behavior_punish","c8_behavior_punish","c9_behavior_punish","c10_behavior_punish","c11_behavior_punish","c12_behavior_punish","c13_behavior_punish","c14_behavior_punish")],1,mean1))

# Making mdata2
# mdata2 = mdata1[,c("gameID","game","scoreA","scoreB","percentA","showScore","round","superid","satisfaction","behavior","e_behavior","e_payoff","payoff","cumulativePayoff","e_cumulativePayoff","n_ties","e_degree","rate_coop","rate_rich","local_gini","local_avg_wealth","local_med_wealth","local_rate_coop","rank","rel_rank")]
names(mdata1)
mdata2 = mdata1[,c("gameID","game","scoreA","scoreB","percentA","showScore","round","superid",
                   "age", "gender", "ipAddress", "race", "is_hispanic",
                   "satisfaction","behavior","e_behavior_coop", "e_behavior_defect", "e_behavior_punish",
                   "e_payoff","cPayoffS","behaviorTime",
                   "e_behaviorTime", "payoff","cumulativePayoff","e_cumulativePayoff",
                   "n_ties","e_degree","e_satisfaction","rate_rich","local_gini",
                   "local_avg_wealth","local_avg_stdwealth","local_med_wealth","prev_local_rate_coop",
                   "cur_local_rate_coop", "prev_local_rate_defect", "cur_local_rate_defect",
                   "prev_local_rate_punish", "cur_local_rate_punish", "rank","rel_rank", "rel_wealth")]

colnames(mdata2)[colnames(mdata2) == "n_ties"] = "degree"

save(mdata1,file="exp3_mdata1.Rdata")
save(mdata2,file="exp3_mdata2.Rdata")

initial_data = mdata2[mdata2$round==1,c("superid","behavior","local_gini","local_avg_wealth","degree")]
colnames(initial_data) = c("superid","initial_coop","initial_local_gini","initial_local_avg_wealth","initial_degree")
initial_data0 = mdata2[mdata2$round==0,c("superid","cumulativePayoff")]
colnames(initial_data0) = c("superid","initial_score")
initial_data2 = merge(x=initial_data,y=initial_data0,all.x=TRUE,all.y=TRUE,by="superid")
write.csv(initial_data2, "initial_data2_exp3.csv",quote=FALSE,row.names=FALSE) 
mdata3 = merge(x=mdata2,y=initial_data2,all.x=TRUE,all.y=FALSE,by="superid")
write.csv(as.matrix(mdata3), "mdata3_exp3.csv",quote=FALSE,row.names=FALSE) #For STATA 

save(mdata3, file= "mdata3_exp3.Rdata")
# relative difference - standardize the cumulative wealth
# local gini for the past round 
# calculate 
#agent based 
#interaction variables (cooperation and happiness visible condition)

#what determines happiness, cooperation,

df <- mdata3
df$happ <- ifelse(df$satisfaction == "v_good", 4, ifelse(df$satisfaction == "good", 3, 
                                                                                         ifelse(df$satisfaction == "neutral", 2,
                                                                                                ifelse(df$satisfaction == "bad", 1, 
                                                                                                       ifelse(df$satisfaction == "v_bad", 0, NA)))))

standardize <- function(x) {
  mu = mean(x, na.rm = T)
  sd = sd(x, na.rm = T) 
  return ((x - mu)/sd)
}

dfStandard <- as.data.frame(matrix(NA, ncol = ncol(df) + 1, nrow = 0))
names(dfStandard) <- c(names(df), "cPayoffS")
for (i in 1:15){   #number of rounds 
  for (j in 1:50){ #number of games
    temp = df[(df$game == j & df$round == i), ]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    dfStandard <- rbind(dfStandard, temp)
  }
} 

dfStandard2 <- as.data.frame(matrix(NA, ncol = ncol(df) + 1, nrow = 0))
names(dfStandard2) <- c(names(df), "cPayoffS")
for (i in 1:15){   #number of rounds 
  for (j in 1:50){ #number of games
    temp = df[(df$game == j & df$round == i), ]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    dfStandard2 <- rbind(dfStandard2, temp)
  }
} 

library(lme4)
fit1 <- lmer(data = df, happ ~ showScore + (1|game) + (1|superid))
summary(fit1)

fit2 <- lmer(data = df, happ ~ showScore * cumulativePayoff + (1|game) + (1|superid))
summary(fit2)

fit2b <- lmer(data = dfStandard2, happ ~ showScore * cPayoffS + factor(round) + (1|game) + (1|superid))
summary(fit2b)

fit3 <- lmer(data = df, happ ~ showScore * local_gini + factor(round) + (1|game) + (1|superid))
summary(fit3)

fit4 <- lmer(data = dfStandard, happ ~ showScore * local_gini + cPayoffS * showScore + factor(round) +(1|game) + (1|superid))
summary(fit4)

fit5 <- lmer(data = dfStandard, happ ~ showScore * local_gini + showScore * local_avg_wealth +factor(round) + cPayoffS * showScore + factor(round) +(1|game) + (1|superid))
summary(fit5)

vis <- df[df$showScore == "true",]
invis <- df[df$showScore == "false",]


fit5v <- lmer(data = vis, happ ~ cPayoffS + local_gini + local_avg_wealth + factor(round) +(1|game) + (1|superid))
summary(fit5v)

fit5iv <- lmer(data = invis, happ ~ cPayoffS + local_gini + local_avg_wealth  + factor(round) +(1|game) + (1|superid))
summary(fit5iv)


df$happ <- ifelse(df$satisfaction == "v_good", 4, ifelse(df$satisfaction == "good", 3, 
                                                         ifelse(df$satisfaction == "neutral", 2,
                                                                ifelse(df$satisfaction == "bad", 1, 
                                                                       ifelse(df$satisfaction == "v_bad", 0, NA)))))



df$e_happ <- ifelse(df$e_satisfaction == "v_good", 4, ifelse(df$e_satisfaction == "good", 3, 
                                                         ifelse(df$e_satisfaction == "neutral", 2,
                                                                ifelse(df$e_satisfaction == "bad", 1, 
                                                                       ifelse(df$e_satisfaction == "v_bad", 0, NA)))))


df$vis <- 

fit6 <- lmer(data = df, happ ~ e_happ * showScore + cur_local_rate_coop * showScore + local_avg_stdwealth * showScore + (1|game) + factor(round) + (1|superid))
summary(fit6)
vis2 <- df[df$showScore == "true",]

mod1 <- lmer(data = vis, happ ~ e_happ + (1|game) + (1|superid) + factor(round))
mod2 <- lmer(data = vis, happ ~  e_happ + cur_local_rate_coop + behavior + (1|game) + (1|superid) + factor(round))
mod3 <- lmer(data = vis, happ ~ e_happ + cur_local_rate_coop + behavior + cPayoffS + rel_wealth + local_gini + (1|game) + (1|superid) + factor(round))

summary(mod1)
summary(mod2)
summary(mod3)

anova(mod1, mod2)
anova(mod3, mod2)


fit6b <- lmer(data = vis, happ ~ e_happ + cur_local_rate_coop + behavior + cPayoffS + rel_wealth + local_gini + (1|game) + (1|superid) + factor(round))
summary(fit6b)


mod4 <- lmer(data = vis, happ ~ e_happ + cur_local_rate_coop + behavior + cPayoffS + rel_wealth  + gini0.1.0.2 + gini0.2.0.3 + gini0.3.0.4 + gini0.4.0.5 + gini0.5 + (1|game) + (1|superid) + factor(round))
summary(mod4)

vis$gini0.0.0.1 <- (vis$local_gini > 0.0) & (vis$local_gini <= 0.1) 
vis$gini0.1.0.2 <- (vis$local_gini > 0.1) & (vis$local_gini <= 0.2) 
vis$gini0.2.0.3 <- (vis$local_gini > 0.2) & (vis$local_gini <= 0.3) 
vis$gini0.3.0.4 <- (vis$local_gini > 0.3) & (vis$local_gini <= 0.4) 
vis$gini0.4.0.5 <- (vis$local_gini > 0.4) & (vis$local_gini <= 0.5) 
vis$gini0.5 <- (vis$local_gini > 0.5)


mod1a <- lmer(data = invis, happ ~ e_happ + (1|game) + (1|superid) + factor(round))
mod2a <- lmer(data = invis, happ ~  e_happ + cur_local_rate_coop + behavior + (1|game) + (1|superid) + factor(round))
mod3a <- lmer(data = invis, happ ~ e_happ + cur_local_rate_coop + behavior + cPayoffS + rel_wealth + local_gini + (1|game) + (1|superid) + factor(round))

summary(mod1a)
summary(mod2a)
summary(mod3a)

ggplot(data = vis) + geom_jitter(aes(x = local_gini, y = happ),width = 0.05, height = 0.05) + geom_smooth(aes(x = local_gini, y = happ),se = F)


#Look at behavior times

ggplot(data = df) + geom_jitter(aes(x = behaviorTime, y = happ, color = showScore)) + stat_smooth(aes(x = behaviorTime, y = happ, col = showScore))
ggplot(data = df) + geom_jitter(aes(x = e_behaviorTime, y = happ, color = showScore)) + stat_smooth(aes(x = behaviorTime, y = happ, col = showScore))
ggplot(data = df) + stat_smooth(aes(x = behaviorTime, y = happ, col= showScore), se = T)
ggplot(data = df) + stat_smooth(aes(x = behaviorTime, y = happ, col = showScore), se = F, method = lm)
ggplot(data = df) + geom_histogram(aes(x = behaviorTime))

ggplot(data = df) + geom_boxplot(aes(y = behaviorTime, x = showScore, col = showScore))
summary(lmer(behaviorTime ~ showScore + (1|superid), data = df))
summary(lm(behaviorTime ~ showScore, data = df))

summary(lmer(behaviorTime ~ showScore + (1|superid), data = df))

vis <- df[df$showScore == "true",]
invis <- df[df$showScore == "false",]

summary(lmer(behaviorTime ~ cPayoffS + local_gini +  (1|superid) + (1|game), data = vis))
summary(lmer(behaviorTime ~ cPayoffS + local_gini +  (1|superid) + (1|game), data = invis))

ggplot(data = df) + geom_point(aes(y = behaviorTime, x = cPayoffS, col = showScore)) +  geom_smooth(aes(y = behaviorTime, x = cPayoffS, col = showScore))
ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = cPayoffS, col = showScore), method = lm)
ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = cPayoffS, col = showScore), method = loess)
ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = local_gini, col = showScore), method = loess)
ggplot(data = df) + geom_point(aes(y = behaviorTime, x = local_gini, col = showScore))


ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = cumulativePayoff, col = showScore), method = lm)
ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = cumulativePayoff, col = showScore), method = lm)

ggplot(data = df) + geom_smooth(aes(y = behaviorTime, x = prev_local_rate_coop, col = showScore), method = lm)
summary(lmer(data = df, behaviorTime ~ prev_local_rate_coop + (1|game) + (1|superid)))

ggplot(data = vis) + geom_histogram(aes(x = cPayoffS))
ggplot(data = invis) + geom_histogram(aes(x = cPayoffS)) 
ggplot(data = invis) + geom_density(aes(x = cPayoffS))
ggplot(data = vis) + geom_density(aes(x = cPayoffS))

ggplot(data = df) + geom_boxplot(aes(y = behaviorTime, x = gender))

names(ndata1)

mod1 <- svm(factor(behavior) ~ factor(e_behavior) + local_gini + behaviorTime + superid, data = df, gamma = 0.01, cost = 100, cross = 10)
mod1$accuracies
mod1$tot.accuracy
100 * sum(df$behavior == 1, na.rm = T) / sum(df$behavior == 1 | df$behavior == 0, na.rm = T)

ggplot(data = df) + geom_smooth(aes(y = happ, x = behaviorTime, col = showScore))








glmer(formula + (1|game), data, family = "binomial")
lu = function(x){length(unique(x$superid))}
dfl <- as.data.frame(matrix(NA, nrow = 50, ncol = 2))
names(dfl) <- c("game", "nparticipants")
counter = 1
for (i in unique(lastround$game)){
  dfl[counter, 1] = i
  dfl[counter, 2] = lu(lastround[lastround$game == i, ])
  counter = counter + 1
}
dfl
