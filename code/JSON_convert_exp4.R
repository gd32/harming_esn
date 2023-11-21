rm(list =  ls())
require(rjson)

setwd("~/Documents/Projects/harming_esn/data/exp4/json/")
data_list = list.files(pattern = "\\.json")
for (i in 1:length(data_list)){
data1 = fromJSON(file=data_list[i], method="C", unexpected.escape="error")
data2_r0 = as.data.frame(do.call("rbind", data1$result$`0`$nodes))
data2_r1 = as.data.frame(do.call("rbind", data1$result$`1`$nodes))
data2_r2 = as.data.frame(do.call("rbind", data1$result$`2`$nodes))
data2_r3 = as.data.frame(do.call("rbind", data1$result$`3`$nodes))
data2_r4 = as.data.frame(do.call("rbind", data1$result$`4`$nodes))
data2_r5 = as.data.frame(do.call("rbind", data1$result$`5`$nodes))
data2_r6 = as.data.frame(do.call("rbind", data1$result$`6`$nodes))
data2_r7 = as.data.frame(do.call("rbind", data1$result$`7`$nodes))
data2_r8 = as.data.frame(do.call("rbind", data1$result$`8`$nodes))
data2_r9 = as.data.frame(do.call("rbind", data1$result$`9`$nodes))
data2_r10 = as.data.frame(do.call("rbind", data1$result$`10`$nodes))
data2_r11 = as.data.frame(do.call("rbind", data1$result$`11`$nodes))
data2_r12 = as.data.frame(do.call("rbind", data1$result$`12`$nodes))
data2_r13 = as.data.frame(do.call("rbind", data1$result$`13`$nodes))
data2_r14 = as.data.frame(do.call("rbind", data1$result$`14`$nodes))
data2_r15 = as.data.frame(do.call("rbind", data1$result$`15`$nodes))

data2_r0$cumulativePayoff = data2_r0$behaviorTime = data2_r0$round =data2_r0$satisfaction = data2_r0$notMakeLink = data2_r0$notBreakLink = data2_r0$behavior = data2_r0$breakLink = data2_r0$makeLink = data2_r0$payoff = data2_r0$timeUp = data2_r0$rewiringSatisfaction = NA  
data2_r0 = data2_r0[,names(data2_r1)]

data2_r0$round = 0
data2_r1$round = 1
data2_r2$round = 2
data2_r3$round = 3
data2_r4$round = 4
data2_r5$round = 5
data2_r6$round = 6
data2_r7$round = 7
data2_r8$round = 8
data2_r9$round = 9
data2_r10$round = 10
data2_r11$round = 11
data2_r12$round = 12
data2_r13$round = 13
data2_r14$round = 14
data2_r15$round = 15

data2 = rbind(data2_r0,data2_r1,data2_r2,data2_r3,data2_r4,data2_r5,data2_r6,data2_r7,data2_r8,data2_r9,data2_r10, data2_r11, data2_r12, data2_r13, data2_r14, data2_r15)
data2$gameID = data1$gameId
data2$scoreA = data1$scoreA
data2$scoreB = data1$scoreB
data2$percentA = data1$percentA
data2$showScore = data1$showScore

data2$initScore = unlist(data2$initScore)
data2$cumulativePayoff = unlist(data2$cumulativePayoff)
data2$behaviorTime = unlist(data2$behaviorTime)
data2$behavior = unlist(data2$behavior)
data2$id = unlist(data2$id)
data2$payoff = unlist(data2$payoff)
data3_r0 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`0`$links))$id),ncol=2,byrow=TRUE))
data3_r1 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`1`$links))$id),ncol=2,byrow=TRUE))
data3_r2 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`2`$links))$id),ncol=2,byrow=TRUE))
data3_r3 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`3`$links))$id),ncol=2,byrow=TRUE))
data3_r4 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`4`$links))$id),ncol=2,byrow=TRUE))
data3_r5 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`5`$links))$id),ncol=2,byrow=TRUE))
data3_r6 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`6`$links))$id),ncol=2,byrow=TRUE))
data3_r7 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`7`$links))$id),ncol=2,byrow=TRUE))
data3_r8 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`8`$links))$id),ncol=2,byrow=TRUE))
data3_r9 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`9`$links))$id),ncol=2,byrow=TRUE))
data3_r10 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`10`$links))$id),ncol=2,byrow=TRUE))
data3_r11 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`11`$links))$id),ncol=2,byrow=TRUE))
data3_r12 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`12`$links))$id),ncol=2,byrow=TRUE))
data3_r13 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`13`$links))$id),ncol=2,byrow=TRUE))
data3_r14 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`14`$links))$id),ncol=2,byrow=TRUE))
data3_r15 = as.data.frame(matrix(unlist(as.data.frame(do.call("rbind", data1$result$`15`$links))$id),ncol=2,byrow=TRUE))

data3_r0$round = 0
data3_r1$round = 1
data3_r2$round = 2
data3_r3$round = 3
data3_r4$round = 4
data3_r5$round = 5
data3_r6$round = 6
data3_r7$round = 7
data3_r8$round = 8
data3_r9$round = 9
data3_r10$round = 10
data3_r11$round = 11
data3_r12$round = 12
data3_r13$round = 13
data3_r14$round = 14
data3_r15$round = 15
data3 = rbind(data3_r0,data3_r1,data3_r2,data3_r3,data3_r4,data3_r5,data3_r6,data3_r7,data3_r8,data3_r9,data3_r10, data3_r11, data3_r12, data3_r13, data3_r14, data3_r15)
colnames(data3) = c("id1","id2","round")
data3$gameID = data1$gameId
data3$scoreA = data1$scoreA
data3$scoreB = data1$scoreB
data3$percentA = data1$percentA
data3$showScore = data1$showScore
data2$game = data3$game = i
if (i ==1)
{
  data2_combined = data2
  data3_combined = data3
}

else
{
  data2_combined = rbind(data2_combined,data2)
  data3_combined = rbind(data3_combined,data3)
}

ndata = data2_combined #node data
ldata = data3_combined #link data
}


save(ndata,file="~/Documents/Projects/harming_esn/data/exp4/node_1aX_exp4.Rdata") 
save(ldata,file="~/Documents/Projects/harming_esn/data/exp4/link_1aX_exp4.Rdata")


#estbalish happ_coop
ndata$happ_coop = NA
ndata[is.na(ndata$satisfaction)==0 & ndata$satisfaction=="v_good",]$happ_coop = 2
ndata[is.na(ndata$satisfaction)==0 & ndata$satisfaction=="good",]$happ_coop = 1
ndata[is.na(ndata$satisfaction)==0 & ndata$satisfaction %in% c("average","neutral"),]$happ_coop = 0
ndata[is.na(ndata$satisfaction)==0 & ndata$satisfaction=="bad",]$happ_coop = -1
ndata[is.na(ndata$satisfaction)==0 & ndata$satisfaction=="v_bad",]$happ_coop = -2


#calculate k6 score
k6num <- function (q) {as.numeric(factor(q,c("all","most","some","a_little","none"),c(1,2,3,4,5)))}
ndata$k6Score <- k6num(ndata$nervous) + k6num(ndata$hopeless) + k6num(ndata$restless) + k6num(ndata$depressed) + k6num(ndata$effort) + k6num(ndata$worthless)



#data visualizaiton

x <- c( "igraph", "reldist", "Zelig", "ggplot2", "grid","grDevices")
lapply(x, library, character.only = TRUE)


prev_ndata = ndata[,c("game","id","round","behavior")]
prev_ndata$round = prev_ndata$round + 1
colnames(prev_ndata) = c("game","id","round","prev_coop")
ndataS = merge(x=ndata,y=prev_ndata,all.x=TRUE,all.y=FALSE,by=c("game","id","round"))


ndataA0 = ndataS[ndataS$game == 1 & ndataS$round==0,c("id","cumulativePayoff","initScore","prev_coop")]
ndataA0$color_vec = rgb(0, 0, 0, 0.4)
ndataA0$size_vec = (ndataA0$initScore)^(1/2)
ndataA0$ego_id = as.numeric(substr(ndataA0$id,2,nchar(ndataA0$id)))+1
ndataA0$label_vec = "N"
#A0-2. Link (edge) data
ldataA0 = ldata[ldata$game == 1 & ldata$round==0,c("id1","id2")]
ldataA0$color_vec = rgb(0, 0, 0, 0.2)
ldataA0s = graph.data.frame(ldataA0,directed=F)
png("/Users/christophergerman/Fig1_A0_1014.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataA0s,
     layout = layout.kamada.kawai,
     vertex.size = ndataA0$size_vec, vertex.label = ndataA0$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = ndataA0$color_vec, vertex.frame.color = NA, 
     edge.width = 0.5, edge.color = ldataA0$color_vec,
     margin = 0)
dev.off()	


#DataA10 (Round 10)
#A10-1. Node (vertex) data
ndataA10 = ndataS[ndataS$game == 1 & ndataS$round==10,c("id","cumulativePayoff","initScore","behavior")] #
ndataA10 = ndataA10[ndataA10$behavior %in% c("C","D"),]
ndataA10$color_vec = ifelse(ndataA10$behavior == "C", rgb(0, 0, 0.5, 0.5), rgb(0.5, 0, 0, 0.5)) #
ndataA10$size_vec = (ndataA10$cumulativePayoff)^(1/2) #
ndataA10$ego_id = as.numeric(substr(ndataA10$id,2,nchar(ndataA10$id)))+1
ndataA10$label_vec = "N"
#A10-2. Link (edge) data
ldataA10 = ldata[ldata$game == 1 & ldata$round==10 & ldata$id1 %in% ndataA10$id & ldata$id2 %in% ndataA10$id,c("id1","id2")] #
ldataA10$color_vec = rgb(0, 0, 0, 0.2)
ldataA10s = graph.data.frame(ldataA10,directed=F)
#A10-3. Visualization
png("/Users/christophergerman/Fig1_A10_1014.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataA10s,
     layout = layout.kamada.kawai,
     vertex.size = ndataA10$size_vec, vertex.label = ndataA10$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = ndataA10$color_vec, vertex.frame.color = NA, 
     edge.width = 0.5, edge.color = ldataA10$color_vec,
     margin = 0)
dev.off()	 


#DataB0 (Round 0)
#B0-1. Node (vertex) data
ndataB0 = ndataS[ndataS$game == 1 & ndataS$round==0,c("id","cumulativePayoff","initScore","prev_coop")]
ndataB0$color_vec = rgb(0, 0, 0, 0.4)
ndataB0$size_vec = (ndataB0$initScore)^(1/2)
ndataB0$ego_id = as.numeric(substr(ndataB0$id,2,nchar(ndataB0$id)))+1
ndataB0$label_vec = ifelse(ndataB0$initScore == 1150, "R","P")
#B0-2. Link (edge) data
ldataB0 = ldata[ldata$game == 1 & ldata$round==0,c("id1","id2")]
ldataB0$color_vec = rgb(0, 0, 0, 0.2)
ldataB0s = graph.data.frame(ldataB0,directed=F)
#B0-3. Visualization
png("/Users/christophergerman/Fig1_A10_1015b.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataB0s,
     layout = layout.kamada.kawai,
     vertex.size = ndataB0$size_vec, vertex.label = ndataB0$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = ndataB0$color_vec, vertex.frame.color = NA, 
     edge.width = 0.5, edge.color = ldataB0$color_vec,
     margin = 0)
dev.off()	


ndataB10 = ndataS[ndataS$game == 1 & ndataS$round==10,c("id","cumulativePayoff","initScore","behavior")] #
ndataB10 = ndataB10[ndataB10$behavior %in% c("C","D"),]
ndataB10$color_vec = ifelse(ndataB10$behavior == "C", rgb(0, 0, 0.5, 0.5), rgb(0.5, 0, 0, 0.5)) #
ndataB10$size_vec = (ndataB10$cumulativePayoff)^(1/2) #
ndataB10$ego_id = as.numeric(substr(ndataB10$id,2,nchar(ndataB10$id)))+1
ndataB10$label_vec = ifelse(ndataB10$initScore == 1150, "R","P")
#B10-2. Link (edge) data
ldataB10 = ldata[ldata$game == 1 & ldata$round==10 & ldata$id1 %in% ndataB10$id & ldata$id2 %in% ndataB10$id,c("id1","id2")] #
ldataB10$color_vec = rgb(0, 0, 0, 0.2)
ldataB10s = graph.data.frame(ldataB10,directed=F)
#B10-3. Visualization
png("/Users/christophergerman/Fig1_A10_1015_Round10_ShowScore.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataB10s,
     layout = layout.kamada.kawai,
     vertex.size = ndataB10$size_vec, vertex.label = ndataB10$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = ndataB10$color_vec, vertex.frame.color = NA, 
     edge.width = 0.5, edge.color = ldataB10$color_vec,
     margin = 0)
dev.off()	 


##Round 10 data for score invisible

ndataC10 = ndataS[ndataS$game == 2 & ndataS$round==10,c("id","cumulativePayoff","initScore","behavior")] #
ndataC10 = ndataC10[ndataC10$behavior %in% c("C","D"),]
ndataC10$color_vec = ifelse(ndataC10$behavior == "C", rgb(0, 0, 0.5, 0.5), rgb(0.5, 0, 0, 0.5)) #
ndataC10$size_vec = (ndataC10$cumulativePayoff)^(1/2) #
ndataC10$ego_id = as.numeric(substr(ndataC10$id,2,nchar(ndataC10$id)))+1
ndataC10$label_vec = ifelse(ndataC10$initScore == 1150, "R","P")
#B10-2. Link (edge) data
ldataC10 = ldata[ldata$game == 2 & ldata$round==10 & ldata$id1 %in% ndataC10$id & ldata$id2 %in% ndataC10$id,c("id1","id2")] #
ldataC10$color_vec = rgb(0, 0, 0, 0.2)
ldataC10s = graph.data.frame(ldataC10,directed=F)
#B10-3. Visualization
png("/Users/christophergerman/Fig1_A10_10163_Round10_NoShowScore.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataC10s,
     layout = layout.kamada.kawai,
     vertex.size = ndataC10$size_vec, vertex.label = ndataC10$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = ndataC10$color_vec, vertex.frame.color = NA, 
     edge.width = 0.5, edge.color = ldataB10$color_vec,
     margin = 0)
dev.off()	 



#for happiness colors
colVec<-rep("#000000",length(ldataB10$color_vec))
color.gradient <- function(x, colors=c("tomato","white","springgreen3"), colsteps=5) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x,na.rm=T),max(x,na.rm=T), length.out=colsteps)) ] )
}


#Happiness - No show score
ndataH10 = ndataS[ndataS$game == 2 & ndataS$round==10,c("id","cumulativePayoff","initScore","behavior","happ_coop")] #
ndataH10 = ndataH10[ndataH10$behavior %in% c("C","D"),]
ndataH10$color_vec = ifelse(ndataH10$happ_coop > 0, rgb(0.5, 0, 0, 0.5), ifelse(ndataH10$happ_coop == 0, rgb(0,0,0,0.5), rgb(0, 0, 0.5, 0.5))) #
ndataH10$size_vec = (ndataH10$cumulativePayoff)^(1/2) #
ndataH10$ego_id = as.numeric(substr(ndataH10$id,2,nchar(ndataH10$id)))+1
ndataH10$label_vec = ifelse(ndataH10$initScore == 1150, "R","P")
#B10-2. Link (edge) data
ldataH10 = ldata[ldata$game == 2 & ldata$round==10 & ldata$id1 %in% ndataH10$id & ldata$id2 %in% ndataH10$id,c("id1","id2")] #
ldataH10$color_vec = rgb(0, 0, 0, 0.2)
ldataH10s = graph.data.frame(ldataH10,directed=F)
#B10-3. Visualization
png("/Users/christophergerman/Fig1_A10_1016_HAP_NoShowScore.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataC10s,
     layout = layout.kamada.kawai,
     vertex.size = ndataH10$size_vec, vertex.label = ndataH10$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = color.gradient(ndataH10$happ_coop), vertex.frame.color = colVec, 
     edge.width = 0.5, edge.color = ldataB10$color_vec,
     margin = 0)
dev.off()	 





#happiness (red is happy, white is neutral, blue is sad) for score shown

ndataH11 = ndataS[ndataS$game == 1 & ndataS$round==10,c("id","cumulativePayoff","initScore","behavior","happ_coop")] #
ndataH11 = ndataH11[ndataH11$behavior %in% c("C","D"),]
#ndataH11$color_vec = ifelse(ndataH11$happ_coop > 0, rgb(0.5, 0, 0, 0.5), ifelse(ndataH11$happ_coop == 0, rgb(0,0,0,0.5), rgb(0, 0, 0.5, 0.5))) #
#ndataH11$color_vec = ndataH11$happ_coop
ndataH11$size_vec = (ndataH11$cumulativePayoff)^(1/2) #
ndataH11$ego_id = as.numeric(substr(ndataH11$id,2,nchar(ndataH11$id)))+1
ndataH11$label_vec = ifelse(ndataH11$initScore == 1150, "R","P")
#B10-2. Link (edge) data
ldataH11 = ldata[ldata$game == 1 & ldata$round==10 & ldata$id1 %in% ndataH11$id & ldata$id2 %in% ndataH11$id,c("id1","id2")] #
ldataH11$color_vec = rgb(0, 0, 0, 0.2)
ldataH11s = graph.data.frame(ldataH11,directed=F)
#B10-3. Visualization
png("/Users/christophergerman/Fig1_A10_1016_HAP_ShowScore.png", width=500, height=500, res=500) 
par(mar=c(0,0,0,0))
plot(ldataH11s,
     layout = layout.kamada.kawai,
     vertex.size = ndataH11$size_vec, vertex.label = ndataH11$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
     vertex.label.family = "Helvetica", vertex.color = color.gradient(ndataH11$happ_coop), vertex.frame.color = colVec, 
     edge.width = 0.5, edge.color = ldataB10$color_vec,
     margin = 0)
dev.off()	 


happ_plot <- function(gameNum = 1, roundNum = 10){
  ndataH = ndataS[ndataS$game == gameNum & ndataS$round == roundNum,c("id","cumulativePayoff","initScore","behavior","happ_coop")] #
  ndataH = ndataH[ndataH$behavior %in% c("C","D"),]
  ndataH$size_vec = (ndataH$cumulativePayoff)^(1/2) #
  ndataH$ego_id = as.numeric(substr(ndataH$id,2,nchar(ndataH$id)))+1
  ndataH$label_vec = ifelse(ndataH$initScore == 1150, "R","P")
  #B10-2. Link (edge) data
  ldataH = ldata[ldata$game == gameNum & ldata$round == roundNum & ldata$id1 %in% ndataH$id & ldata$id2 %in% ndataH$id,c("id1","id2")] #
  ldataH$color_vec = rgb(0, 0, 0, 0.2)
  ldataHs = graph.data.frame(ldataH,directed=F)
  #B10-3. Visualization
  #png("/Users/christophergerman/Fig1_A10_1016_HAP_ShowScore.png", width=500, height=500, res=500) 
  #par(mar=c(0,0,0,0))
  plot(ldataHs,
       layout = layout.kamada.kawai,
       vertex.size = ndataH$size_vec, vertex.label = ndataH$label_vec, vertex.label.cex = 0.25, vertex.label.color = "black",
       vertex.label.family = "Helvetica", vertex.color = color.gradient(ndataH$happ_coop), vertex.frame.color = colVec, 
       edge.width = 0.5, edge.color = ldataH$color_vec,
       margin = 0)
  #dev.off()	 
}

happ_plot()
happ_plot(2,9)
happ_plot(2,10)
happ_plot(2,7)
happ_plot(1,6)
happ_plot(1,10)

for (i in 1:10){
  happ_plot(1,i);
  readline()
}
for (i in 1:10){
  happ_plot(2,i);
  readline()
}


#calculate gini coef at the final round for each session
library(reldist)
nGini <- ndata[ndata$round == 15, c("cumulativePayoff", "game", "showScore", "satisfaction")]

#50 games
giniCoef <- as.data.frame(matrix(NA, ncol = 2, nrow = length(data_list)))
names(giniCoef) <- c("coef", "vis")
for (i in 1:length(data_list)){
  giniCoef$coef[i] <- gini(nGini[nGini$game == i, 1])
  giniCoef$vis[i] <- ifelse(nGini[nGini$game == i, "showScore"][1] == "true", 1, 0)
}

t.test(x = giniCoef$coef[giniCoef$vis == 0], y = giniCoef$coef[giniCoef$vis == 1])
x = giniCoef$coef[giniCoef$vis == 0]
y = giniCoef$coef[giniCoef$vis == 1]
t.test(x, y)
x
y

nGini$happ <- ifelse(nGini$satisfaction == "v_good", 5, ifelse(nGini$satisfaction == "good", 4, 
                                                               ifelse(nGini$satisfaction == "neutral", 3,
                                                                      ifelse(nGini$satisfaction == "bad", 2, 
                                                                        ifelse(nGini$satisfaction == "v_bad", 1, NA)))))
happCor <- as.data.frame(matrix(NA, ncol = 3, nrow = length(data_list)))
names(happCor) <- c("game", "vis", "pearson")
for (i in 1:length(data_list)){
  happCor$game[i] <- i
  happCor$vis[i] <- ifelse(nGini[nGini$game == i, "showScore"][1] == "true", 1, 0)
  happCor$pearson[i] <- cor(y = nGini[nGini$game == i,]$happ, x = nGini[nGini$game == i,]$cumulativePayoff, use = "na.or.complete")
}


nGini$vis<- factor(ifelse(nGini$showScore == "true", 1, 0))


model1 <- lm( happ ~  cumulativePayoff + vis + cumulativePayoff * vis, data = nGini)
summary(model1)

model1a <- lm( happ ~  cumulativePayoff , data = nGini[nGini$vis == 1, ])
summary(model1a)

model1b <- lm( happ ~  cumulativePayoff , data = nGini[nGini$vis == 0, ])
summary(model1b)

income x, happiness y, 


library(ggplot2)

ggplot(data = nGini) + geom_jitter(aes(x = cumulativePayoff, y = happ, color = vis), height = 0.05, width = 0.05) + geom_smooth(aes(x = cumulativePayoff, y = happ, color = vis), se = F, method = loess)


#standardize things 

standardize <- function(x) {
  mu = mean(x)
  sd = sd(x) 
  return ((x - mu)/sd)
}

nDataStandardized <- as.data.frame(matrix(NA, ncol = ncol(ndata) + 1, nrow = 0))
names(nDataStandardized) <- c(names(ndata), "cPayoffS")
for (i in 1:15){   #number of rounds 
  for (j in 1:50){ #number of games
    temp = ndata[((ndata$round == i) & (ndata$game == j)),]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    nDataStandardized <- rbind(nDataStandardized, temp)
  }
} 

nDataStandardized$happ <- ifelse(nDataStandardized$satisfaction == "v_good", 4, ifelse(nDataStandardized$satisfaction == "good", 3, 
                                                               ifelse(nDataStandardized$satisfaction == "neutral", 2,
                                                                      ifelse(nDataStandardized$satisfaction == "bad", 1, 
                                                                             ifelse(nDataStandardized$satisfaction == "v_bad", 0, NA)))))

nDataStandardized$vis <- (ifelse(nDataStandardized$showScore == "false", 0, 1))

nDataStandardized$vis <- factor(nDataStandardized$vis)
model2 <- lm( happ ~  cPayoffS + vis + cPayoffS * vis, data = nDataStandardized)
summary(model2)


model2 <- lmer( happ ~ cPayoffS  * vis +  (1|game) + (1|superid) + factor(round), data = nDataStandardized)
summary(model2)

model2ab <- lmer( happ ~ cPayoffS  * vis +  (1|game) + (1|id) , data = nDataStandardized[nDataStandardized$round == 15,])
summary(model2ab)

model2a <- lmer( happ ~ cPayoffS  + (1|game) + factor(round), data = nDataStandardized[nDataStandardized$vis == 1,])
summary(model2a)

model2b <- lmer( happ ~ cPayoffS  + (1|game) + (1|id)  + factor(round), data = nDataStandardized[nDataStandardized$vis == 0,])
summary(model2b)

#now finding the average happiness for visible and invisible and look at the sd and gini coef

avgHapp <- as.data.frame(matrix(NA, ncol = 6, nrow = 750))
names(avgHapp) <- c("round", "game", "happ", "vis", "gini" , "giniC")
i = 1
for (round in 1:15){   #number of rounds 
  for (game in 1:50){ #number of games
    temp = nDataStandardized[(nDataStandardized$round == round & nDataStandardized$game == game), names(nDataStandardized) %in% c("round", "game", "happ", "vis", "cumulativePayoff")]
    avgHapp$round[i] = round
    avgHapp$game[i] = game
    avgHapp$happ[i] = mean(temp$happ)
    avgHapp$vis[i] = as.numeric(temp$vis[1]) - 1
    avgHapp$gini[i] = gini(temp$happ)
    avgHapp$giniC[i] = gini(temp$cumulativePayoff)
    i = i + 1
  }
}

ggplot(data = avgHapp, aes(y = happ, x = factor(vis))) + geom_boxplot() +  ylab("Happiness (0 = very bad, 4 = very good") + xlab("Visibility (0 = Invisible, 1 = Visible)")

t.test(x = avgHapp[avgHapp$vis ==1,]$happ, y = avgHapp[avgHapp$vis == 0,]$happ)

ggplot(data = avgHapp, aes(y = happ, x = gini)) + geom_point(aes(color = factor(vis)))

ggplot(data = avgHapp, aes(y = happ, x = gini)) + geom_point(aes(color = factor(vis)))

ggplot(data = avgHapp, aes(y = gini, x = factor(vis))) + geom_boxplot() + ylab("Gini Coefficient of Happiness") + xlab("Visibility (0 = Invisible, 1 = Visible)")

t.test(x = avgHapp[avgHapp$vis ==1,]$gini, y = avgHapp[avgHapp$vis == 0,]$gini)

#make super id by ip address

#calculate countries' IP addresses
IPS <- as.data.frame(IP_country(unlist(ndata[ndata$round == 15,]$ipAddress)))
names(IPS) <- "IP"
library(dplyr)
b <- IPS %>%
  group_by(IP) %>%
  summarise(no_rows = length(IP))



ggplot(ndata[is.na(ndata$happ_coop)==0,], aes(x=cumulativePayoff,y=happ_coop)) +
  geom_point(alpha=0.8,color="white") +
  ylim(0,4) + xlim(-2.5,2.5) + 
  theme_bw() + theme(axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border=element_blank(),panel.background=element_blank(),axis.line = element_line(color = 'black',size=0.3),axis.text.x=element_text(size=15),axis.text.y=element_text(size=15)) +
  ylab("Emotional wellbeing (Very bad = 0, Very good = 4)\n") + xlab("\nWealth (Standardized Cumulative Points)") +
  annotate("segment",x=-2.5,xend=2.5,y= (3.17290 - 0.32665) - 3*(0.05300 + 0.09002),yend= (3.17290 - 0.32665)+3*(0.05300 + 0.09002),color="skyblue",size=1) +
  annotate("segment",x=-2.5,xend=2.5,y=3.17290  - 3 * (0.05300),yend=3.17290 + 3 *0.05300,color="orange",size=1) +
  annotate("text",x=-1,y=0.5,size=5,label="Blue line: Visible condition",parse=FALSE,color="skyblue")+
  annotate("text",x=-1,y=1,size=5,label="Orange line: Invisible condition",parse=FALSE,color="orange") +
  annotate("text",x=-1,y=.3,size=3,label="y = 2.85 + 0.14*StandardWealth ",parse=FALSE,color="skyblue") +
  annotate("text",x=-1,y=.8,size=3,label="y = 3.17 + 0.05*StandardWealth",parse=FALSE,color="orange") 

#super id

nDataStandardized$superid <- as.numeric(factor(unlist(nDataStandardized$ipAddress)))
avgHapp$vis <- factor(avgHapp$vis)

model_gini <- lmer(data = avgHapp, giniC ~ vis + factor(round) + (1|game))
summary(model_gini)




mat1 <- matrix(1:12, ncol = 3)
mat1[2:4,] <- matrix(rep(mat1[1,],each = nrow(mat1) - 1))  #), nrow = nrow(mat1), ncol = ncol(mat1))


superIDS <- as.data.frame(matrix(NA, ncol = 2, nrow = length(unique(nDataStandardized$superid))))
names(superIDS) <- c("superid","nrow")
k = 1
for (i in unique(nDataStandardized$superid)){
  superIDS$superid[k] <- i
  superIDS$nrow[k]<- nrow(nDataStandardized[nDataStandardized$superid == i,])
  k = k + 1
}

View(nDataStandardized[nDataStandardized$superid == 331,])
View(nDataStandardized[nDataStandardized$superid == 504,])

View(nDataStandardized[nDataStandardized$superid == 308,])

ndata$ID <- cumsum(!duplicated(ndata[,c("id","game")]))

modtime<- lmer(happ ~ as.numeric(behaviorTime) + (1|ID), data = nDataStandardized)
summary(modtime)

summary(lm(happ ~ as.numeric(behaviorTime), data = nDataStandardized))

ndata %>% 
  mutate(ID = group_indices_(ndata, .dots=c("game", "id"))) 

ndata2 <- ndata


ndata2 <- ndata2[with(ndata2, order(ndata2$game, ndata2$id)),]
ndata2$ID <- cumsum(!duplicated(ndata2[,c("id","game")]))
length(unique(ndata2$ID))

games <- as.data.frame(matrix(NA, ncol = 3, nrow = 50))
names(games) <- c("game", "n0", "n1")
i = 1
for (game in 1:50){
  games$game[i] = game
  games$n0[i] = nrow(ndata2[(ndata2$round == 0 & ndata2$game == game),])
  games$n1[i] = nrow(ndata2[(ndata2$round == 15 & ndata2$game == game),])
  i = i + 1
}
summary(lm(happ ~ log(as.numeric(behaviorTime),10), data = nDataStandardized[nDataStandardized$behaviorTime < 20000,]))
ndata2[ndata2$behaviorTime < 20000,]



nDataStandardized$initScore <- factor(nDataStandardized$initScore)
modelHappInit <- lmer(happ ~ initScore * vis + (1|game) + (1|ID) + factor(round), data = nDataStandardized2)
summary(modelHappInit)

nDataStandardized2 <- as.data.frame(matrix(NA, ncol = ncol(ndata2) + 1, nrow = 0))
names(nDataStandardized2) <- c(names(ndata2), "cPayoffS")
for (i in 1:15){   #number of rounds 
  for (j in 1:50){ #number of games 
    temp = ndata2[((ndata2$round == i) & (ndata2$game == j)),]
    temp$cPayoffS <- standardize(temp$cumulativePayoff)
    nDataStandardized2 <- rbind(nDataStandardized2, temp)
  }
} 
nDataStandardized2$happ <- ifelse(nDataStandardized2$satisfaction == "v_good", 4, ifelse(nDataStandardized2$satisfaction == "good", 3, 
                                                                                       ifelse(nDataStandardized2$satisfaction == "neutral", 2,
                                                                                              ifelse(nDataStandardized2$satisfaction == "bad", 1, 
                                                                                                     ifelse(nDataStandardized2$satisfaction == "v_bad", 0, NA)))))

nDataStandardized2$vis <- (ifelse(nDataStandardized2$showScore == "false", 0, 1))

nDataStandardized2$vis <- factor(nDataStandardized2$vis)
