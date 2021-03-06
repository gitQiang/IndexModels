setwd("E:/HQ/IndexModels/价格指标Wind")
datapath <- "E:/HQ/IndexModels/价格指标Wind"
Indexs <- read.delim("E:/HQ/IndexModels/价格指标Wind/IDs_LABs.txt",sep="\t")
IDs <- paste(Indexs[,2],sep="",collapse = ",")

day <- as.character(Sys.Date())

library(WindR)
w.start()
### price indexes
w_edb_data<-w.edb(IDs,'2012-01-01',day,'')
newData <- w_edb_data$Data
w.stop()

write.csv(newData,file=paste(datapath,"/WindPriceIndex_",day,".csv",sep=""),row.names=FALSE,quote=FALSE)


## PTA 华东地区市场平均价
w.start()
w_edb_data<-w.edb('S5442369','2012-01-01',day,'')
newData <- w_edb_data$Data
w.stop()
write.table(newData,file=paste(datapath,"/WindPTAPrice_",day,".txt",sep=""),row.names=FALSE,quote=FALSE,sep="\t")

