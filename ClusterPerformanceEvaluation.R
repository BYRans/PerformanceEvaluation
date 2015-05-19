#5 4
rm(list=ls())
gc()
#数据读入
memory.limit(4000)
library(corrplot)
library(rpart)
data_drop<-read.csv("D:/logData/isp5_sys12_core4_drop_20140918-20141124.txt",header=FALSE,sep="\t",na.strings="None")
data_no_drop<-read.csv("D:/logData/isp5_sys12_core4_noDrop_20140918-20141124.txt",header=FALSE,sep="\t",na.strings="None")
colnames(data_drop) <- c("host" ,"sys" ,"isp" ,"didtrict" ,"itype" ,"conS" ,"cpuRate" ,"allMem" ,"memRate" ,"recBytes" ,"recPkts" ,"dropPkts", "1RecBytes" ,"1RecPkts" ,"1DropPkts" ,"2SendBytes" ,"2SendSendPkts" ,"2SendUdps" ,"mainProcessNO" ,"config" ,"nSnmp" ,"ntp_n" ,"crond_n" ,"mainProcessCpu" ,"mainProcessMem" ,"otherCpu" ,"otherMem" ,"sysUptime" ,"checkTime" ,"dfRestart" ,"sn" ,"ifinerrors" ,"sysString" ,"averageLength")  #列改名
colnames(data_no_drop) <- c("host" ,"sys" ,"isp" ,"didtrict" ,"itype" ,"conS" ,"cpuRate" ,"allMem" ,"memRate" ,"recBytes" ,"recPkts" ,"dropPkts", "1RecBytes" ,"1RecPkts" ,"1DropPkts" ,"2SendBytes" ,"2SendSendPkts" ,"2SendUdps" ,"mainProcessNO" ,"config" ,"nSnmp" ,"ntp_n" ,"crond_n" ,"mainProcessCpu" ,"mainProcessMem" ,"otherCpu" ,"otherMem" ,"sysUptime" ,"checkTime" ,"dfRestart" ,"sn" ,"ifinerrors" ,"sysString" ,"averageLength")  #列改名
data_drop<-data_drop[!is.na(data_drop$allMem), ] 
data_no_drop<-data_no_drop[!is.na(data_no_drop$allMem), ] 
data_drop$totalPkts <- data_drop$recPkts + data_drop$dropPkts
data_no_drop$totalPkts <- data_no_drop$recPkts + data_no_drop$dropPkts
data_drop$totalBytes <- data_drop$totalPkts * data_drop$averageLength
data_no_drop$totalBytes <- data_no_drop$totalPkts * data_no_drop$averageLength
data_drop[data_drop$dropPkts>0,][,"dropPkts"]=1

#决策树
n <- nrow(data_drop)*0.7
dataNewTraining<-rbind(data_drop[1:n,],data_no_drop[sample(1:(nrow(data_no_drop[1:(nrow(data_no_drop)*0.7),])),n),])
dataNewTest<-rbind(data_drop[-(1:n),],data_no_drop[-(1:(nrow(data_no_drop)*0.7)),])
pdf(file='D:/logData/score_tmp/isp5_sys12_core4/treeisp5_sys12_core4.pdf',family="GB1")
ppsTree <- rpart(dropPkts~ totalBytes + totalPkts,data = dataNewTraining, method = "class")  #less features
allFeaturesTree <- rpart(dropPkts~ recBytes + recPkts + mainProcessCpu + mainProcessMem + averageLength + totalPkts + conS,data = dataNewTraining, method = "class")  
plot(ppsTree,main="isp5_sys12_core4 totalBytes、totalPkts");text(ppsTree)
plot(allFeaturesTree,main="isp5_sys12_core4 加上属性totalPkts、conS");text(allFeaturesTree)
dev.off()
ppsTreePredict = predict(ppsTree,dataNewTest)
write.table(ppsTreePredict, file="D:/logData/score_tmp/isp5_sys12_core4/DT_ppsTreePredict.txt",row.names= F ,col.names= F , sep="\t") 
allFeaTreePredict = predict(allFeaturesTree,dataNewTest)
write.table(allFeaTreePredict, file="D:/logData/score_tmp/isp5_sys12_core4/DT_allFeaTreePredict.txt",row.names= F ,col.names= F , sep="\t") 
write.table(data.frame(dataNewTest$recBytes,dataNewTest$recPkts ,dataNewTest$mainProcessCpu , dataNewTest$mainProcessMem ,dataNewTest$averageLength ,dataNewTest$totalPkts , dataNewTest$conS,dataNewTest$dropPkts), file="D:/logData/score_tmp/isp5_sys12_core4/DT_dataNewTest.txt",row.names= F ,col.names= F ,sep="\t") 


