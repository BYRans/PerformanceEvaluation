memory.limit(4000)
library(corrplot)
library(rpart)
library(rpart.plot)

data<-read.csv("C:/Documents and Settings/js4/less24_300w.csv",header=FALSE,sep="\t",na.strings="None")

colnames(data) <- c("host" ,"sys" ,"isp" ,"didtrict" ,"itype" ,"conS" ,"cpuRate" ,"allMem" ,"memRate" ,"recBytes" ,"recPkts" ,"dropPkts", "1RecBytes" ,"1RecPkts" ,"1DropPkts" ,"2SendBytes" ,"2SendSendPkts" ,"2SendUdps" ,"mainProcessNO" ,"config" ,"nSnmp" ,"ntp_n" ,"crond_n" ,"mainProcessCpu" ,"mainProcessMem" ,"otherCpu" ,"otherMem" ,"sysUptime" ,"checkTime" ,"dfRestart" ,"sn" ,"ifinerrors" ,"sysString" ,"averageLength")  #???

data=data[!is.na(data$allMem), ]   
data_drop<-data[data$dropPkts!=0,]
data_no_drop<-data[data$dropPkts==0,]

pdf(file='d:/factors300w.pdf')

corrplot(cor(data$dropPkts,data[,c(7, 10, 11, 16, 17, 19, 20, 22, 23, 24, 25, 31, 32, 34)], method="pearson"), method = "number", main=" all data set,dropPktsrelate factors")

corrplot(cor(data_drop$dropPkts,data_drop[,c(7, 10, 11, 16, 17, 19, 20, 22, 23, 24, 25, 31, 32, 34)] ,method="pearson"), method = "number", main="drop data set,dropPktsrelate factors")

corrplot(cor(data$dropPkts/(data$recPkts + data$dropPkts),data[,c(7, 10, 11, 16, 17, 19, 20, 22, 23, 24, 25, 31, 32, 34)], method="pearson"), method = "number", main="all data set,drop rate relate factors ")

corrplot(cor(data_drop$dropPkts/(data_drop$recPkts + data_drop$dropPkts), data_drop [,c(7, 10, 11, 16, 17, 19, 20, 22, 23, 24, 25, 31, 32, 34)], method="pearson"), method = "number", main="drop data set,drop rate relate factors ")

dev.off()
