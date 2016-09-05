read_rawData <- function(day,tday,Indexs,flag=1){
        
        datapath <- "//BJB-155/IndexModels/价格指标Wind"
        rawData <- read.csv(paste(datapath,"/WindPriceIndex_",day,".csv",sep=""))
        if(flag==2){
                subs <- which(colnames(rawData) %in% Indexs[Indexs[,9]==1,2])
                rawData <- rawData[, c(1, subs)]
        }
        
        rawData <- rawData[rawData[,1] <= tday,  ]
        rawData <- as.matrix(rawData)   
        rawData        
}

rateOne <- function(newData,weekI,d,flag=2){
        if(flag==1){
                rates <- matrix(0,4,ncol(newData)-1)
                L <- c(1,7,30,90)
                Lweek <- c(1,1,4,12)
                
                subs2 <- weekI+1
                subs1 <- setdiff(2:ncol(newData), subs2)
                
                Lone <- L[d]
                Ltwo <- Lweek[d]
                rates[,subs1-1] <- rate0(newData,Lone,subs1)
                rates[,subs2-1] <- rate0(newData,Ltwo,subs2)    
        }else if(flag==2){
                gDate <- groupDate(newData[,1])
                gs <- gDate[,d]
                rates <- rate1(newData,gs)
                rates[is.na(rates)] <- 0
        }
        rownames(rates) <- c("min","mean","median","max","start-end")
        
        rates
}

rate0 <- function(newData,Lone,subs){
        sapply(subs, function(i){
                x <- as.numeric(newData[,i])
                subs <- which(!is.na(x))
                n <- length(subs)
                
                seq1 <- subs[(n-Lone+1):n]
                seq2 <- subs[max(0,n-2*Lone+1):(n-Lone)]
                
                c(
                        ( min(x[seq1]) - min(x[seq2]) )/( min(x[seq2]) ),
                        ( mean(x[seq1]) - mean(x[seq2]) )/( mean(x[seq2]) ),
                        ( median(x[seq1]) - median(x[seq2]) )/( median(x[seq2]) ),
                        ( max(x[seq1]) - max(x[seq2]) )/( max(x[seq2]) ),
                        ( x[max(seq1)] - x[min(seq1)] )/x[min(seq1)]
                )
        })    
}

rate1 <- function(newData,gs){
        
        sapply(2:ncol(newData), function(i) {
                xsub <- !is.na(as.numeric(newData[,i]))
                x <- as.numeric(newData[xsub,i])
                onet <- gs[xsub]
                seq1 <- which(onet==max(onet))
                seq2 <- which( onet==max(onet[onet!=max(onet)]) )
                
                c(
                ( min(x[seq1]) - min(x[seq2]) )/( min(x[seq2]) ),
                ( mean(x[seq1]) - mean(x[seq2]) )/( mean(x[seq2]) ),
                ( median(x[seq1]) - median(x[seq2]) )/( median(x[seq2]) ),
                ( max(x[seq1]) - max(x[seq2]) )/( max(x[seq2]) ),
                ( x[max(seq1)] - x[min(seq1)] )/x[min(seq1)]
                )
                })
        
}

IndexModel_1 <- function(Indexs,rates){
        
        mer1 <- as.matrix(aggregate(rates, by=list(Indexs[,4]), mean))
        lab2  <- cbind(mer1[,1], Indexs[match(mer1[,1], Indexs[,4]),5])
        mer2 <- aggregate(mer1[,2],by=list(lab2[,2]), mean)
        Ind0 <- (sum(mer2[,2]>0)-sum(mer2[,2]<0))/nrow(mer2)
        
        c(Ind0,mean(mer2[,2]))    

}

IndexModel_2 <- function(Indexs,rates,ws,flag=1){
        
        if(flag==1){
                tmp <- weighted.mean(rates,ws)
                c(tmp,(sum(rates>0)-sum(rates<0))/length(rates))
        }else if(flag==2){
                labs <- unique(Indexs[,4])
                r0 <- rep(0,length(labs))
                for(i in 1:length(labs)){
                        tmpsub <- which(Indexs[,4]==labs[i])
                        r0[i] <- weighted.mean(rates[tmpsub],ws[tmpsub])
                }
                
                ws1 <- as.matrix(aggregate(ws, by=list(Indexs[,4]), mean))
                ws2M  <- cbind(ws1[,1], Indexs[match(ws1[,1], Indexs[,4]),5])
                lab2 <- unique(ws2M[,2])
                r1 <- rep(0,length(lab2))
                for(i in 1:length(lab2)){
                        tmpsub <- which(labs %in% ws2M[ws2M[,2]==lab2[i],1])
                        r1[i] <- weighted.mean(r0[tmpsub],ws1[match(labs[tmpsub],ws1[,1]),2])
                }
                c((sum(r1>0)-sum(r1<0))/length(r1), mean(r1))
        }
        
}

fillWeek <- function(Indexs,rawData){
        subs <- which(Indexs[,7]!="日")
        newData <- rawData
        
        for(i in subs+1){
            tmp <- which(!is.na(newData[,i]))
            for(j in 2:length(tmp)){
                 newData[(tmp[j-1]+1):(tmp[j]-1),i] <- newData[tmp[j],i] 
            }
        }
        
        newData
}

curveOne <- function(rawData, Indexs,weekI, flag=2){
        
        ptaP <- pta_pricef()
        gDate <- groupDate(ptaP[,1])
        ptaW <- aggregate(as.numeric(ptaP[,2]), by=list(gDate[,2]), mean)
        
        subs2 <- weekI+1
        subs1 <- setdiff(2:ncol(rawData), subs2)
        gDate1 <- groupDate(rawData[,1])
        
        corV <- rep(-2,ncol(rawData)-1)
        
        ###
        Lone=90
        inDay <- intersect(gDate[,1],gDate1[,1])
        y <- as.numeric(ptaP[match(inDay,ptaP[,1]), 2])
        sub0 <- match(inDay,rawData[,1])
        corV[subs1-1] <- sapply(subs1, function(i){
                #print(i)
                x <- as.numeric(rawData[sub0,i])
                subs <- which(!is.na(x))
                
                if(flag==1){
                        n <- length(subs)
                        seq1 <- subs[max(0,n-2*Lone+1):n]
                        cor(x[seq1],y[seq1])
                }else{
                        cor(x[subs],y[subs])
                }
        })
        
        Lone=12
        corV[subs2-1] <- sapply(subs2, function(i){
                #print(i)
                xsub <- !is.na(as.numeric(rawData[,i]))
                if(flag==1 | flag==3){
                        tmpx <- rawData[xsub,i]
                        inweek <- intersect(gDate[,2],gDate1[xsub,2])
                        
                        xg <- aggregate(as.numeric(tmpx), by=list(gDate1[xsub,2]),mean)
                        x <- xg[match(inweek,xg[,1]),2]
                        y <- as.numeric(ptaW[match(inweek,ptaW[,1]), 2]) 
                }
                
                if(flag==1){
                        n <- length(x)
                        seq1 <- max(0,n-2*Lone+1):n
                        if(sd(x[seq1])==0) print(i)
                        cor(x[seq1],y[seq1])
                }else if(flag==2){
                        tmpx <- rawData[xsub,c(1,i)]
                        inD <- intersect(tmpx[,1],ptaP[,1])
                        x <- as.numeric(tmpx[match(inD,tmpx[,1]),2])
                        y <- as.numeric(ptaP[match(inD,ptaP[,1]),2])
                        cor(x,y)
                }else if(flag==3){
                        cor(x,y)
                }
                #print(cor(x,y))
        })
        
        corV[is.na(corV)] <- 0
        #plot(corV)
        ## random 1000 cor 3*sd ~~ 0.2
        subs <- which(corV>0.2)
        
        list(subs=subs,ws=corV[subs])
}

groupDate <- function(date1){
        library(lubridate)
        useDates <- as.Date(date1)
        
        w1 <- as.numeric(week(useDates))
        w1[w1<10] <- paste(0,w1[w1<10],sep="")
        m1 <- as.numeric(month(useDates))
        m1[m1<10] <- paste(0,m1[m1<10],sep="")
        
        gs1 <- paste(year(useDates),w1,sep="-")
        gs2 <- paste(year(useDates),m1,sep="-")
        gs3 <- paste(year(useDates),quarter(useDates),sep="-")
        
        useDates <- as.character(useDates)
        useDates <- cbind(useDates,gs1,gs2,gs3)
        useDates
}

pta_pricef <- function(tday=""){

        options(stringsAsFactors = FALSE)
        #ptaP <- read.delim("//BJB-155/IndexModels/data/PTA华东市场价格.txt",sep="\t")
        #ptaP <- ptaP[-1, ]
        day <- as.character(Sys.Date())
        ptaP <- read.delim(paste("//BJB-155/IndexModels/价格指标Wind/WindPTAPrice_",day,".txt",sep=""), sep="\t")
        ptaP[,2] <- gsub(",","",ptaP[,2])
        ptaP[,2] <- gsub(" ","",ptaP[,2])
        ptaP <- ptaP[!grepl("数据来源",ptaP[,1]), ]
        ptaP <- ptaP[ptaP[,1]!="", ]
        ptaP <- ptaP[!is.na(as.numeric(ptaP[,2])), ]
        ptaP <- as.matrix(ptaP)
        if(tday!="") ptaP <- ptaP[ptaP[,1] <= tday, ]
        
        ptaP
}

pta_rates <- function(day=""){
        
        ptaP <- pta_pricef(day)
        gDate <- groupDate(ptaP[,1])
        x <- as.numeric(ptaP[,2])
        
        L=4
        rone <- matrix(0,5,L)
        for(i in 1:L){
                onet <- gDate[,i]
                seq1 <- which(onet==max(onet))
                seq2 <- which( onet==max(onet[onet!=max(onet)]) )
                
                rone[1,i] <- ( min(x[seq1]) - min(x[seq2]) )/( min(x[seq2]) )
                rone[2,i] <- ( mean(x[seq1]) - mean(x[seq2]) )/( mean(x[seq2]) )
                rone[3,i] <- ( median(x[seq1]) - median(x[seq2]) )/( median(x[seq2]) )
                rone[4,i] <- ( max(x[seq1]) - max(x[seq2]) )/( max(x[seq2]) )
                rone[5,i] <- ( x[max(seq1)] - x[min(seq1)] )/x[min(seq1)]
        }
        colnames(rone) <- c("day","week","month","season")
        rownames(rone) <- c("min","mean","median","max","start-end")
        
        rone
}

