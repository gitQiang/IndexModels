rm(list=ls())
gc()

#source("//BJB-327/IndexModels/misc.R")
#datapath <- "E:/HQ/IndexModels/价格指标Wind"

source("misc.R")
datapath <- "//BJB-155/IndexModels/价格指标Wind"


##### the initial Index model
Indexs <- read.delim(paste(datapath,"/IDs_LABs.txt",sep=""),sep="\t")
day <- '2016-08-26' ##as.character(Sys.Date())
rawData <- read.csv(paste(datapath,"/WindPriceIndex_",day,".csv",sep=""))
rawData <- as.matrix(rawData)


#weekI <- which(Indexs[,7]=="周")
kv <- sapply(2:ncol(rawData), function(i) sum(is.na(rawData[,i]))/sum(!is.na(rawData[,i])))
weekI <- which(kv>=4)
## newData <- fillWeek(Indexs,rawData) # method 1: not suitable for week data
## newData <- rawData # method 2
subs <- curveOne(rawData,Indexs,weekI) ## method 3
newData <- rawData[,c(1,subs+1)]
Indexs <- Indexs[subs, ]


kv <- sapply(2:ncol(newData), function(i) sum(is.na(newData[,i]))/sum(!is.na(newData[,i])))
weekI <- which(kv>=4)
dimL <- c("day","week","month","season")
for(i in 1:length(dimL)){
        rates <- rateOne(newData,weekI,dimL[i])
        r1 <- IndexModel_1(Indexs,rates)
        print(r1)
}

r0 <- pta_rates()
print(r0)

