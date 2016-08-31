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
subs <- sapply(2:ncol(rawData), function(i) sd(as.numeric(rawData[!is.na(rawData[,i]),i]))>0)
rawData <- rawData[ ,c(TRUE,subs)]
Indexs <- Indexs[subs, ]

#weekI <- which(Indexs[,7]=="周")
kv <- sapply(2:ncol(rawData), function(i) sum(is.na(rawData[,i]))/sum(!is.na(rawData[,i])))
weekI <- which(kv>=4)
## newData <- fillWeek(Indexs,rawData) # method 1: not suitable for week data
## newData <- rawData # method 2
tmpr <- curveOne(rawData,Indexs,weekI,flag=2) ## method 3
subs <- tmpr$subs
ws <- tmpr$ws
newData <- rawData[,c(1,subs+1)]
Indexs <- Indexs[subs, ]

kv <- sapply(2:ncol(newData), function(i) sum(is.na(newData[,i]))/sum(!is.na(newData[,i])))
weekI <- which(kv>=4)
r1 <- matrix(0,4,4,dimnames = list(c("min","mean","median","max"),c("day","week","month","season")))
r2 <- matrix(0,4,4,dimnames = list(c("min","mean","median","max"),c("day","week","month","season")))
for(d in 1:4){
        rates <- rateOne(newData,weekI,d,flag=2)
        for(j in 1:4){
                #tmp <- IndexModel_1(Indexs,rates[j,])
                #tmp <- IndexModel_2(Indexs,rates[j,],ws,flag=2)
                
                ws <- 1/Indexs[,8]
                tmp <- IndexModel_2(Indexs,rates[j,],ws,flag=1)
                
                r1[j,d] <- tmp[1]
                r2[j,d] <- tmp[2]
        }
}
print(r1)
print(r2)

r0 <- pta_rates()
print(r0)
