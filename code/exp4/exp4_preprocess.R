rm(list = ls())
setwd("~/Documents/Projects/harming_esn/Data/exp4/exp4_subdata/")

load(file="node_exp4.Rdata") #ndata
load(file="link_exp4.Rdata") #ldata

library(reldist)
library(tidyverse)
library(rgeolocate)

# Set up functions
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

### adjust ldata ####
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
save(ldata4,file="ldata_exp4.Rdata")

###############################################################################
# Start here for creating new variables
load("~/Documents/Projects/harming_esn/Data/exp4/exp4_subdata/ldata_exp4.Rdata") #ldata
load("~/Documents/Projects/harming_esn/Data/exp4/exp4_subdata/node_exp4.Rdata") #ndata

# Adjust behavior
ndata = ndata %>% mutate(behavior = ifelse(ndata$behavior == "", NA, ndata$behavior))

# Create indicators for individual behaviors 
ndata$behavior_coop = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "C", 1, 0)
ndata$behavior_defect = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "D", 1, 0)
ndata$behavior_punish = ifelse(is.na(ndata$behavior)==0 & ndata$behavior == "P", 1, 0)

#behaviorTime as numeric
ndata$behaviorTime = as.numeric(ndata$behaviorTime)

ipList = unique(unlist(ndata$ipAddress)) # Have these
library(rgeolocate)
file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
ipCountries = maxmind(ipList, file, "country_name")
table(ipCountries) # Ok, we have some variety. Let's keep

ldata5 = ldata4[c("id1","id2","round","gameID","game","scoreA","scoreB","percentA","showScore","id2number")]
ldata6 = reshape(ldata5, direction = "wide", idvar=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id1"), timevar="id2number")
rownames(ldata6) = NULL

#n of ties to calculate network density (degree)
ldata6$n_ties = as.numeric(apply(ldata6[,c("id2.1","id2.2","id2.3","id2.4",
                                           "id2.5","id2.6","id2.7","id2.8","id2.9",
                                           "id2.10","id2.11","id2.12","id2.13","id2.14", "id2.15", "id2.16")],1,length1))

ndata1 = merge(x=ndata,y=ldata6,all.x=TRUE,all.y=FALSE,
               by.x=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id"),
               by.y=c("round","gameID","game","scoreA","scoreB","percentA","showScore","id1"))

save(ndata1, file = "ndata1_exp4.Rdata") #This is used for the individual-level analysis. 
load("~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/ndata1_exp4.Rdata") #ndata1 

dim(ndata1[ndata1$round==0,]) # 739 individuals
dim(ndata1[ndata1$round!=0,]) # 10747 actions

ndata1$superid = 100*ndata1$game+as.numeric(substr(ndata1$id,2,nchar(ndata1$id))) #the last two digits are the id in the game. 
ndata1[ndata1$round==0,"cumulativePayoff"] = ndata1[ndata1$round==0,"initScore"]
ndata2 = ndata1[,c("scoreA","scoreB","percentA","satisfaction","showScore","game","round","initScore","behavior_coop","behavior_defect", "behavior_punish","behaviorTime","payoff","cumulativePayoff","n_ties","ipAddress","timeUp","id","superid")] #Variable pruning
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
#Including: game info, ego ID, and ego's cooperative behavior
#The identifier is gameID, round, and id. 
#behavior at round is one round lag forward.
outcome_data = ndata1[ndata1$round %in% c(0:15),c("gameID","game","scoreA","scoreB","percentA",
                                                  "satisfaction","behaviorTime","showScore","round",
                                                  "superid","payoff","cumulativePayoff","behavior_coop",
                                                  "behavior_defect", "behavior_punish","n_ties","timeUp",
                                                  "satisfaction","cPayoffS")]
outcome_data$previousround = outcome_data$round - 1


#Proc B: Preparing for the ego's attribute and network data
#The basics needs to be connected with all the ego and alter
# E_ prefix means it is the value from the the previous ROUND

network_data = ndata1[,c("gameID","game","round","superid","behavior_coop",
                         "behavior_defect", "behavior_punish","payoff","cumulativePayoff", 
                         "satisfaction","cPayoffS","behaviorTime","age", 
                         "gender", "ipAddress","timeUp",
                         "superid2.1","superid2.2","superid2.3","superid2.4",
                         "superid2.5","superid2.6","superid2.7","superid2.8",
                         "superid2.9","superid2.10","superid2.11","superid2.12",
                         "superid2.13","superid2.14","superid2.15","superid2.16"
                         )]

network_data = network_data %>% rename(previousround = round,
                        e_behavior_coop = behavior_coop,
                        e_behavior_defect = behavior_defect,
                        e_behavior_punish = behavior_punish,
                        e_payoff = payoff,
                        e_cumulativePayoff = cumulativePayoff,
                        e_satisfaction = satisfaction,
                        e_stdPayoff = cPayoffS,
                        e_behaviorTime = behaviorTime
                        )


#Proc C: Preparing for the alter's attribute data for previous round
alter_data = ndata1[,c("gameID","game","round","superid","behavior_coop","behavior_defect", 
                       "behavior_punish","payoff","cumulativePayoff" ,"satisfaction","cPayoffS")]
names(alter_data) = c("gameID","game","previousround","a_superid","a_behavior_coop", "a_behavior_defect", 
                      "a_behavior_punish","a_payoff","a_cumulativePayoff","a_satisfaction","a_cPayoffS")

#and for current round
alter_data2 = ndata1[,c("gameID","game","round","superid","behavior_coop", "behavior_defect", 
                        "behavior_punish","payoff","cumulativePayoff","satisfaction","cPayoffS")]
names(alter_data2) = c("gameID","game","currentround","c_superid","c_behavior_coop", "c_behavior_defect", 
                       "c_behavior_punish","c_payoff","c_cumulativePayoff","c_satisfaction","c_cPayoffS")

a1_data = a2_data = a3_data = a4_data = a5_data = a6_data = a7_data = a8_data = a9_data = a10_data = a11_data = a12_data = a13_data = a14_data = a15_data = a16_data = a17_data = a18_data = a19_data = a20_data = alter_data
names(a1_data) = c("gameID","game","previousround","a1_superid","a1_behavior_coop", "a1_behavior_defect", "a1_behavior_punish","a1_payoff","a1_cumulativePayoff", "a1_satisfaction","a1_cPayoffS")
names(a2_data) = c("gameID","game","previousround","a2_superid","a2_behavior_coop", "a2_behavior_defect", "a2_behavior_punish","a2_payoff","a2_cumulativePayoff", "a2_satisfaction","a2_cPayoffS")
names(a3_data) = c("gameID","game","previousround","a3_superid","a3_behavior_coop", "a3_behavior_defect", "a3_behavior_punish","a3_payoff","a3_cumulativePayoff", "a3_satisfaction","a3_cPayoffS")
names(a4_data) = c("gameID","game","previousround","a4_superid","a4_behavior_coop", "a4_behavior_defect", "a4_behavior_punish","a4_payoff","a4_cumulativePayoff", "a4_satisfaction","a4_cPayoffS")
names(a5_data) = c("gameID","game","previousround","a5_superid","a5_behavior_coop", "a5_behavior_defect", "a5_behavior_punish","a5_payoff","a5_cumulativePayoff", "a5_satisfaction","a5_cPayoffS")
names(a6_data) = c("gameID","game","previousround","a6_superid","a6_behavior_coop", "a6_behavior_defect", "a6_behavior_punish","a6_payoff","a6_cumulativePayoff", "a6_satisfaction","a6_cPayoffS")
names(a7_data) = c("gameID","game","previousround","a7_superid","a7_behavior_coop", "a7_behavior_defect", "a7_behavior_punish","a7_payoff","a7_cumulativePayoff", "a7_satisfaction","a7_cPayoffS")
names(a8_data) = c("gameID","game","previousround","a8_superid","a8_behavior_coop", "a8_behavior_defect", "a8_behavior_punish","a8_payoff","a8_cumulativePayoff", "a8_satisfaction","a8_cPayoffS")
names(a9_data) = c("gameID","game","previousround","a9_superid","a9_behavior_coop", "a9_behavior_defect", "a9_behavior_punish","a9_payoff","a9_cumulativePayoff", "a9_satisfaction","a9_cPayoffS")
names(a10_data) = c("gameID","game","previousround","a10_superid","a10_behavior_coop", "a10_behavior_defect", "a10_behavior_punish","a10_payoff","a10_cumulativePayoff", "a10_satisfaction","a10_cPayoffS")
names(a11_data) = c("gameID","game","previousround","a11_superid","a11_behavior_coop", "a11_behavior_defect", "a11_behavior_punish","a11_payoff","a11_cumulativePayoff", "a11_satisfaction","a11_cPayoffS")
names(a12_data) = c("gameID","game","previousround","a12_superid","a12_behavior_coop", "a12_behavior_defect", "a12_behavior_punish","a12_payoff","a12_cumulativePayoff", "a12_satisfaction","a12_cPayoffS")
names(a13_data) = c("gameID","game","previousround","a13_superid","a13_behavior_coop", "a13_behavior_defect", "a13_behavior_punish","a13_payoff","a13_cumulativePayoff", "a13_satisfaction","a13_cPayoffS")
names(a14_data) = c("gameID","game","previousround","a14_superid","a14_behavior_coop", "a14_behavior_defect", "a14_behavior_punish","a14_payoff","a14_cumulativePayoff", "a14_satisfaction","a14_cPayoffS")
names(a15_data) = c("gameID","game","previousround","a15_superid","a15_behavior_coop", "a15_behavior_defect", "a15_behavior_punish","a15_payoff","a15_cumulativePayoff", "a15_satisfaction","a15_cPayoffS")
names(a16_data) = c("gameID","game","previousround","a16_superid","a16_behavior_coop", "a16_behavior_defect", "a16_behavior_punish","a16_payoff","a16_cumulativePayoff", "a16_satisfaction","a16_cPayoffS")

# rename names to right order
c1_data = c2_data = c3_data = c4_data = c5_data = c6_data = c7_data = c8_data = c9_data = c10_data = c11_data = c12_data = c13_data = c14_data = c15_data = c16_data = c17_data = c18_data = c19_data = c20_data = alter_data2
names(c1_data) = c("gameID","game","currentround","c1_superid","c1_behavior_coop", "c1_behavior_defect", "c1_behavior_punish","c1_payoff","c1_cumulativePayoff", "c1_satisfaction", "c1_cPayoffS")
names(c2_data) = c("gameID","game","currentround","c2_superid","c2_behavior_coop", "c2_behavior_defect", "c2_behavior_punish","c2_payoff","c2_cumulativePayoff", "c2_satisfaction", "c2_cPayoffS")
names(c3_data) = c("gameID","game","currentround","c3_superid","c3_behavior_coop", "c3_behavior_defect", "c3_behavior_punish","c3_payoff","c3_cumulativePayoff", "c3_satisfaction", "c3_cPayoffS")
names(c4_data) = c("gameID","game","currentround","c4_superid","c4_behavior_coop", "c4_behavior_defect", "c4_behavior_punish","c4_payoff","c4_cumulativePayoff", "c4_satisfaction", "c4_cPayoffS")
names(c5_data) = c("gameID","game","currentround","c5_superid","c5_behavior_coop", "c5_behavior_defect", "c5_behavior_punish","c5_payoff","c5_cumulativePayoff", "c5_satisfaction", "c5_cPayoffS")
names(c6_data) = c("gameID","game","currentround","c6_superid","c6_behavior_coop", "c6_behavior_defect", "c6_behavior_punish","c6_payoff","c6_cumulativePayoff", "c6_satisfaction", "c6_cPayoffS")
names(c7_data) = c("gameID","game","currentround","c7_superid","c7_behavior_coop", "c7_behavior_defect", "c7_behavior_punish","c7_payoff","c7_cumulativePayoff", "c7_satisfaction", "c7_cPayoffS")
names(c8_data) = c("gameID","game","currentround","c8_superid","c8_behavior_coop", "c8_behavior_defect", "c8_behavior_punish","c8_payoff","c8_cumulativePayoff", "c8_satisfaction", "c8_cPayoffS")
names(c9_data) = c("gameID","game","currentround","c9_superid","c9_behavior_coop", "c9_behavior_defect", "c9_behavior_punish","c9_payoff","c9_cumulativePayoff", "c9_satisfaction", "c9_cPayoffS")
names(c10_data) = c("gameID","game","currentround","c10_superid","c10_behavior_coop", "c10_behavior_defect", "c10_behavior_punish","c10_payoff","c10_cumulativePayoff", "c10_satisfaction", "c10_cPayoffS")
names(c11_data) = c("gameID","game","currentround","c11_superid","c11_behavior_coop", "c11_behavior_defect", "c11_behavior_punish","c11_payoff","c11_cumulativePayoff", "c11_satisfaction", "c11_cPayoffS")
names(c12_data) = c("gameID","game","currentround","c12_superid","c12_behavior_coop", "c12_behavior_defect", "c12_behavior_punish","c12_payoff","c12_cumulativePayoff", "c12_satisfaction", "c12_cPayoffS")
names(c13_data) = c("gameID","game","currentround","c13_superid","c13_behavior_coop", "c13_behavior_defect", "c13_behavior_punish","c13_payoff","c13_cumulativePayoff", "c13_satisfaction", "c13_cPayoffS")
names(c14_data) = c("gameID","game","currentround","c14_superid","c14_behavior_coop", "c14_behavior_defect", "c14_behavior_punish","c14_payoff","c14_cumulativePayoff", "c14_satisfaction", "c14_cPayoffS")
names(c15_data) = c("gameID","game","currentround","c15_superid","c15_behavior_coop", "c15_behavior_defect", "c15_behavior_punish","c15_payoff","c15_cumulativePayoff", "c15_satisfaction", "c15_cPayoffS")
names(c16_data) = c("gameID","game","currentround","c16_superid","c16_behavior_coop", "c16_behavior_defect", "c16_behavior_punish","c16_payoff","c16_cumulativePayoff", "c16_satisfaction", "c16_cPayoffS")

#Proc D: Merge the data
outcome_network = merge(outcome_data, network_data, all.x = T, by = c("gameID", "game", "superid", "previousround"))

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

mdata1 = outcome_network_a16
mdata1 = as.matrix(mdata1)
# write.csv(mdata1, "mdata1_0316X.csv", quote=FALSE, row.names=FALSE)

outcome_network_c1 = merge(x=outcome_network_a16,y=c1_data,all.x=TRUE,all.y=FALSE,by.x=c("gameID","game","round","superid2.1"),by.y=c("gameID","game","currentround","c1_superid"))
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

mdata1$e_degree = as.numeric(apply(mdata1[,c("superid2.1","superid2.2","superid2.3","superid2.4",
                                             "superid2.5","superid2.6","superid2.7","superid2.8",
                                             "superid2.9","superid2.10","superid2.11","superid2.12",
                                             "superid2.13","superid2.14", "superid2.15", "superid2.16"
                                             )],1,length1))

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
mdata1$richer_a15 = as.numeric(mdata1$a15_cumulativePayoff > mdata1$e_cumulativePayoff) 
mdata1$richer_a16 = as.numeric(mdata1$a16_cumulativePayoff > mdata1$e_cumulativePayoff) 

mdata1$rate_rich = as.numeric(apply(mdata1[,c("richer_a1","richer_a2","richer_a3","richer_a4","richer_a5","richer_a6","richer_a7","richer_a8","richer_a9","richer_a10","richer_a11","richer_a12","richer_a13","richer_a14")],1,mean1))
mdata1$local_gini = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,gini1))
mdata1$local_avg_wealth = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,mean1))
mdata1$local_med_wealth = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,median1))
mdata1$local_avg_stdwealth = as.numeric(apply(mdata1[,c("e_stdPayoff","a1_cPayoffS","a2_cPayoffS","a3_cPayoffS","a4_cPayoffS","a5_cPayoffS","a6_cPayoffS","a7_cPayoffS","a8_cPayoffS","a9_cPayoffS","a10_cPayoffS","a11_cPayoffS","a12_cPayoffS","a13_cPayoffS","a14_cPayoffS")],1,mean1))
mdata1$rel_wealth = mdata1$e_stdPayoff - as.numeric(apply(mdata1[,c("a1_cPayoffS","a2_cPayoffS","a3_cPayoffS","a4_cPayoffS","a5_cPayoffS","a6_cPayoffS","a7_cPayoffS","a8_cPayoffS","a9_cPayoffS","a10_cPayoffS","a11_cPayoffS","a12_cPayoffS","a13_cPayoffS","a14_cPayoffS")],1,mean1))

mdata1$rank = as.numeric(apply(mdata1[,c("e_cumulativePayoff","a1_cumulativePayoff","a2_cumulativePayoff","a3_cumulativePayoff","a4_cumulativePayoff","a5_cumulativePayoff","a6_cumulativePayoff","a7_cumulativePayoff","a8_cumulativePayoff","a9_cumulativePayoff","a10_cumulativePayoff","a11_cumulativePayoff","a12_cumulativePayoff","a13_cumulativePayoff","a14_cumulativePayoff")],1,rank1))
mdata1$rel_rank = (mdata1$rank-1)/(mdata1$e_degree) #changed the definition. 

# rate comparisons for the different behaviors
mdata1$prev_local_rate_coop = as.numeric(apply(mdata1[,c("a1_behavior_coop","a2_behavior_coop","a3_behavior_coop","a4_behavior_coop","a5_behavior_coop",
                                                         "a6_behavior_coop","a7_behavior_coop","a8_behavior_coop","a9_behavior_coop","a10_behavior_coop",
                                                         "a11_behavior_coop","a12_behavior_coop","a13_behavior_coop","a14_behavior_coop","a15_behavior_coop",
                                                         "a16_behavior_coop")],1,mean1))
mdata1$cur_local_rate_coop = as.numeric(apply(mdata1[,c("c1_behavior_coop","c2_behavior_coop","c3_behavior_coop","c4_behavior_coop","c5_behavior_coop"
                                                        ,"c6_behavior_coop","c7_behavior_coop","c8_behavior_coop","c9_behavior_coop","c10_behavior_coop",
                                                        "c11_behavior_coop","c12_behavior_coop","c13_behavior_coop","c14_behavior_coop","c15_behavior_coop",
                                                        "c16_behavior_coop")],1,mean1))
mdata1$prev_local_rate_defect = as.numeric(apply(mdata1[,c("a1_behavior_defect","a2_behavior_defect","a3_behavior_defect","a4_behavior_defect","a5_behavior_defect",
                                                         "a6_behavior_defect","a7_behavior_defect","a8_behavior_defect","a9_behavior_defect","a10_behavior_defect",
                                                         "a11_behavior_defect","a12_behavior_defect","a13_behavior_defect","a14_behavior_defect","a15_behavior_defect",
                                                         "a16_behavior_defect")],1,mean1))
mdata1$cur_local_rate_defect = as.numeric(apply(mdata1[,c("c1_behavior_defect","c2_behavior_defect","c3_behavior_defect","c4_behavior_defect","c5_behavior_defect"
                                                        ,"c6_behavior_defect","c7_behavior_defect","c8_behavior_defect","c9_behavior_defect","c10_behavior_defect",
                                                        "c11_behavior_defect","c12_behavior_defect","c13_behavior_defect","c14_behavior_defect","c15_behavior_defect",
                                                        "c16_behavior_defect")],1,mean1))
mdata1$prev_local_rate_punish = as.numeric(apply(mdata1[,c("a1_behavior_punish","a2_behavior_punish","a3_behavior_punish","a4_behavior_punish","a5_behavior_punish",
                                                         "a6_behavior_punish","a7_behavior_punish","a8_behavior_punish","a9_behavior_punish","a10_behavior_punish",
                                                         "a11_behavior_punish","a12_behavior_punish","a13_behavior_punish","a14_behavior_punish","a15_behavior_punish",
                                                         "a16_behavior_punish")],1,mean1))
mdata1$cur_local_rate_punish = as.numeric(apply(mdata1[,c("c1_behavior_punish","c2_behavior_punish","c3_behavior_punish","c4_behavior_punish","c5_behavior_punish"
                                                        ,"c6_behavior_punish","c7_behavior_punish","c8_behavior_punish","c9_behavior_punish","c10_behavior_punish",
                                                        "c11_behavior_punish","c12_behavior_punish","c13_behavior_punish","c14_behavior_punish","c15_behavior_punish",
                                                        "c16_behavior_punish")],1,mean1))

names(mdata1)

mdata2 = mdata1[,c("gameID","game","scoreA","scoreB","percentA","showScore","round","superid",
                   "satisfaction","behavior_coop", "behavior_defect", "behavior_punish",
                   "prev_local_rate_coop", "cur_local_rate_coop", "prev_local_rate_defect",
                   "cur_local_rate_defect", "prev_local_rate_punish", "cur_local_rate_punish",
                   "e_payoff","cPayoffS","behaviorTime", "gender", "age", "ipAddress",
                   "e_behaviorTime", "payoff","cumulativePayoff","e_cumulativePayoff",
                   "n_ties","e_degree","e_satisfaction","timeUp.x","rate_rich","local_gini",
                   "local_avg_wealth","local_avg_stdwealth","local_med_wealth", "rank","rel_rank", "rel_wealth")]

colnames(mdata2)[colnames(mdata2) == "n_ties"] = "degree"
save(mdata1,file="exp4_mdata1.Rdata") #mdata1
save(mdata2, file = "exp4_mdata2.Rdata")
initial_data = mdata2[mdata2$round==1,c("superid","behavior_coop", "behavior_defect", "behavior_punish","local_gini","local_avg_wealth","degree")]
names(initial_data)
colnames(initial_data) = c("superid","initial_coop", "inital_defect", "initial_punish","initial_local_gini","initial_local_avg_wealth","initial_degree")
initial_data0 = mdata2[mdata2$round==0,c("superid","cumulativePayoff")]
colnames(initial_data0) = c("superid","initial_score")
initial_data2 = merge(x=initial_data,y=initial_data0,all.x=TRUE,all.y=TRUE,by="superid")
mdata3 = merge(x=mdata2,y=initial_data2,all.x=TRUE,all.y=FALSE,by="superid")

save(mdata3, file= "exp4_mdata3.Rdata")

## Load from here to create new punish variables - need to create three:
## 1. negative reinforcement (punish when cooperation is low to foster more)
## 2. inequality aversion (punish when below average wealth in the local network)
## 3. copy/retaliation (punish when punishment is experienced)

load("~/Documents/Projects/harming_esn/data/exp4/exp4_subdata/ndata1_exp4.Rdata") #ndata1 
load("~/Documents/Projects/harming_esn/data/exp4/exp4data.Rdata") #exp4data

ndata_alters_list = ndata1 %>% 
  select(round, game, starts_with("id")) %>%
  filter(round != 0) %>%
  mutate(across(starts_with("id"), ~100*game+parse_number(.))) %>%
  rename(superid = id)

names(ndata_alters_list)[4:19] = c("alter1", "alter2", "alter3", "alter4", 
                                   "alter5", "alter6", "alter7", "alter8",
                                   "alter9", "alter10", "alter11", "alter12",
                                   "alter13", "alter14", "alter15", "alter16")

current_wealth = exp4data %>% 
  filter(round != 0) %>%
  select(superid, game, round, cumulativePayoff, cPayoffS)

ndata_long_alters = ndata_alters_list %>%
  pivot_longer(cols = starts_with("alter"), values_to = ("alter_id")) %>%
  filter(is.na(alter_id) == 0)

ndata_long_alters_wealth = ndata_long_alters %>%
  left_join(current_wealth, by = c("round", "game", "alter_id" = "superid")) %>%
  group_by(superid, round, game) %>%
  summarize(mean_alter_wealth = mean1(cPayoffS))

ndata_long_alters_wealth

exp4data %>%
  left_join(ndata_long_alters_wealth, by = c("round", "game", "superid")) %>%
  mutate(wealth_inequal = ifelse(cPayoffS < mean_alter_wealth, 1, 0)) 

data_lag = exp4data %>% dplyr::select(superid, round, wealth_inequal)
names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1
exp4data = merge(x=exp4data,y=data_lag, all.x=T,all.y=F,by=c("superid","round"))


## Create the three punish variables
load("~/Documents/Projects/harming_esn/data/exp4/exp4data.Rdata")
load("~/Documents/Projects/harming_esn/data/exp1/data1_pc_v2.Rdata")

data_lag = data1_pc %>% dplyr::select(superid, round, wealth_inequal)
names(data_lag)[-c(1,2)] = paste0(names(data_lag)[-c(1,2)],"_lag")
data_lag$round = data_lag$round + 1
data1_pc = merge(x=data1_pc,y=data_lag, all.x=T,all.y=F,by=c("superid","round"))


data1_pc = data1_pc %>%
  mutate(punish_type_NR = ifelse(behavior_punish == 1 & local_rate_coop_lag < 0.5, 1, 0),
         punish_type_CR = ifelse(behavior_punish == 1 & local_rate_punish_lag > 0, 1, 0),
         punish_type_IA = ifelse(behavior_punish == 1 & wealth_inequal_lag == 1, 1, 0),
         punish_type_U = ifelse(behavior_punish == 1 & punish_type_CR == 0 & punish_type_NR == 0 & punish_type_IA == 0 | behavior_punish == 1 & (is.na(punish_type_NR) == 1 & is.na(punish_type_IA) == 1 & is.na(punish_type_CR) == 1), 1, 0))

exp4data = exp4data %>%
  mutate(punish_type_NR = ifelse(behavior_punish == 1 & local_rate_coop_lag < 0.5, 1, 0),
         punish_type_CR = ifelse(behavior_punish == 1 & local_rate_punish_lag > 0, 1, 0),
         punish_type_IA = ifelse(behavior_punish == 1 & wealth_inequal_lag == 1, 1, 0),
         punish_type_U = ifelse(behavior_punish == 1 & punish_type_CR == 0 & punish_type_NR == 0 & punish_type_IA == 0 | behavior_punish == 1 & (is.na(punish_type_NR) == 1 & is.na(punish_type_IA) == 1 & is.na(punish_type_CR) == 1), 1, 0))

exp1data = data1_pc

## Do this for experiment 1 too

## Save the updated data
save(exp1data, file = "~/Documents/Projects/harming_esn/data/exp1/exp1data.Rdata")
save(exp4data, file = "~/Documents/Projects/harming_esn/data/exp4/exp4data.Rdata")
