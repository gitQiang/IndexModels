rm(list=ls())
gc()
setwd("D:/code/IndexModels")
source("misc.R")

### read input data =====
datapath <- "//BJB-155/IndexModels/价格指标Wind"
Indexs <- read.delim(paste(datapath,"/IDs_LABs.txt",sep=""),sep="\t")
day <- '2016-09-05' ##as.character(Sys.Date())
tday <- "2016-08-31"
rawData <- read_rawData(day,tday,Indexs,flag=1)

### delete constant variable =====
subs <- sapply(2:ncol(rawData), function(i) sd(as.numeric(rawData[!is.na(rawData[,i]),i]))>0)
rawData <- rawData[ ,c(TRUE,subs)]
Indexs <- Indexs[subs, ]

## selected features and weights =====
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

### get the Index supply-demand =====
kv <- sapply(2:ncol(newData), function(i) sum(is.na(newData[,i]))/sum(!is.na(newData[,i])))
weekI <- which(kv>=4)
n.method <- 5
r1 <- matrix(0,n.method,4,dimnames = list(c("min","mean","median","max","start-end"),c("day","week","month","season")))
r2 <- matrix(0,n.method,4,dimnames = list(c("min","mean","median","max","start-end"),c("day","week","month","season")))
for(d in 1:4){
        rates <- rateOne(newData,weekI,d,flag=2)
        for(j in 1:n.method){
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

# r0 <- pta_rates(tday)
# print(r0)
