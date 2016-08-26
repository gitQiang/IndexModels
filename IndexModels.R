source("//BJB-327/IndexModels/misc.R")
datapath <- "E:/HQ/IndexModels/价格指标Wind"


##### the initial Index model
Indexs <- read.delim(paste(datapath,"/IDs_LABs.txt",sep=""),sep="\t")
day <- as.character(Sys.Date())
rawData <- read.csv(paste(datapath,"/WindPriceIndex_",day,".csv",sep=""))
rawData <- as.matrix(rawData)

newData <- fillWeek(Indexs,rawData)
dimL <- c("day","week","month","season")

for(i in 1:length(dimL)){
        rates <- rateOne(newData,dimL[i])
        r1 <- IndexModel_1(Indexs,rates)
        print(r1)
}


