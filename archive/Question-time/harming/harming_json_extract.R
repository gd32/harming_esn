rm(list = ls())
library(rjson)

setwd("~/Documents/Projects/harming_esn/data/exp1/exp1_raw/harming_w_response_time/json")
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
if (i ==1){
  data2_combined = data2
  data3_combined = data3
}else
  {
  data2_combined = rbind(data2_combined,data2)
  data3_combined = rbind(data3_combined,data3)
  ndata = data2_combined #node data
  ldata = data3_combined #link data
  }
}

ndata

save(ndata,file="~/Documents/Projects/harming_esn/Question-time/harming/node_harming.Rdata") 
save(ldata,file="~/Documents/Projects/harming_esn/Question-time/harming/link_harming.Rdata") 
