rateOne <- function(newData,weekI,dim="day"){
        L <- c(1,7,30,90)
        Lweek <- c(1,1,4,12)
        ds <- c("day","week","month","season")
        Lone <- L[ds==dim]
        Ltwo <- Lweek[ds==dim]
        
        rates <- rep(0,ncol(newData)-1)
        subs2 <- weekI+1
        subs1 <- setdiff(2:ncol(newData), subs2)
        
        rates[subs1-1] <- rate0(newData,Lone,subs1)
        rates[subs2-1] <- rate0(newData,Ltwo,subs2)
        
        rates
}

rate0 <- function(newData,Lone,subs){
        sapply(subs, function(i){
                x <- as.numeric(newData[,i])
                subs <- which(!is.na(x))
                n <- length(subs)
                
                seq1 <- subs[(n-Lone+1):n]
                seq2 <- subs[max(0,n-2*Lone+1):(n-Lone)]
                
                if(Lone < 20){
                        ( mean(x[seq1]) - mean(x[seq2]) )/( mean(x[seq2]) )
                }else{
                        ( median(x[seq1]) - median(x[seq2]) )/( median(x[seq2]) )
                }
        })    
}

IndexModel_1 <- function(Indexs,rates){
        
        mer1 <- as.matrix(aggregate(rates, by=list(Indexs[,4]), mean))
        lab2  <- cbind(mer1[,1], Indexs[match(mer1[,1], Indexs[,4]),5])
        mer2 <- aggregate(mer1[,2],by=list(lab2[,2]), mean)
        Ind0 <- (sum(mer2[,2]>0)-sum(mer2[,2]<0))/nrow(mer2)
        
        c(Ind0,mean(mer2[,2]))    

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

curveOne <- function(newData, Indexs,weekI){
        
        ptaP <- pta_pricef()
        ptaweek <- as.matrix(ptaP[,2,drop=FALSE])
        rownames(ptaweek) <- ptaP[,1]
        mode(ptaweek) <- "numeric"
        weekp <- groupPredict(ptaweek)  
        
        subs2 <- weekI+1
        subs1 <- setdiff(2:ncol(newData), subs2)
        corV <- rep(-2,ncol(newData)-1)
        
        Lone=90
        interD <- intersect(newData[,1],ptaP[,1])
        y <- as.numeric(ptaP[ptaP[,1] %in% interD,2])
        corV[subs1-1] <- sapply(subs1, function(i){
                #print(i)
                x <- as.numeric(newData[newData[,1] %in% interD,i])
                subs <- which(!is.na(x) & !is.na(y))
                n <- length(subs)
                
                seq1 <- subs[max(0,n-2*Lone+1):n]
                cor(x[seq1],y[seq1])
        })
        
        Lone=12
        corV[subs2-1] <- sapply(subs2, function(i){
                #print(i)
                tmp <- !is.na(newData[,i])
                x0 <- newData[tmp,i,drop=FALSE]
                mode(x0) <- "numeric"
                rownames(x0) <- paste(year(newData[tmp,1]),week(newData[tmp,1]),sep="-")
                
                interD <- intersect(rownames(weekp),rownames(x0))
                x <- x0[interD, 1]
                y <- weekp[interD, 1]
                
                subs <- which(!is.na(x) & !is.na(y))
                n <- length(subs)
                
                seq1 <- subs[max(0,n-2*Lone+1):n]
                cor(x[seq1],y[seq1])
        })
        
        corV[is.na(corV)] <- 0
        #plot(corV)
        ## random 1000 cor 3*sd ~~ 0.2
        subs <- which(corV>0.2)
        
        subs
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
        
        useDates <- cbind(useDates,gs1,gs2,gs3)
        useDates
}

pta_pricef <- function(){

        options(stringsAsFactors = FALSE)
        #ptaP <- read.delim("E:/HQ/IndexModels/data/PTA华东市场价格.txt",sep="\t")
        ptaP <- read.delim("//BJB-155/IndexModels/data/PTA华东市场价格.txt",sep="\t")
        ptaP <- ptaP[-1, ]
        ptaP[,2] <- gsub(",","",ptaP[,2])
        ptaP[,2] <- gsub(" ","",ptaP[,2])
        ptaP <- ptaP[!grepl("数据来源",ptaP[,1]), ]
        ptaP <- ptaP[ptaP[,1]!="", ]
        ptaP <- ptaP[!is.na(as.numeric(ptaP[,2])), ]
        ptaP <- as.matrix(ptaP)
        
        ptaP
}

pta_rates <- function(){
        
        ptaP <- pta_pricef()
        gDate <- groupDate(ptaP[,1])
        x <- as.numeric(ptaP[,2])
        
        L=4
        rone <- matrix(0,4,L)
        for(i in 1:L){
                onet <- gDate[,i]
                seq1 <- which(onet==max(onet))
                seq2 <- which( onet==max(onet[onet!=max(onet)]) )
                
                rone[1,i] <- ( min(x[seq1]) - min(x[seq2]) )/( min(x[seq2]) )
                rone[2,i] <- ( mean(x[seq1]) - mean(x[seq2]) )/( mean(x[seq2]) )
                rone[3,i] <- ( median(x[seq1]) - median(x[seq2]) )/( median(x[seq2]) )
                rone[4,i] <- ( max(x[seq1]) - max(x[seq2]) )/( max(x[seq2]) )
        }
        colnames(rone) <- c("day","week","month","season")
        rownames(rone) <- c("min","mean","median","max")
        
        rone
}

