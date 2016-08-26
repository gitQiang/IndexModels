rateOne <- function(newData,dim="day"){
        L <- c(1,7,30,90)
        ds <- c("day","week","month","season")
        Lone <- L[ds==dim]
        
        sapply(2:ncol(newData), function(i){
                #print(i)
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
        subs <- which(Indexs[,7]!="ÈÕ")
        newData <- rawData
        
        for(i in subs+1){
            tmp <- which(!is.na(newData[,i]))
            for(j in 2:length(tmp)){
                 newData[(tmp[j-1]+1):(tmp[j]-1),i] <- newData[tmp[j],i] 
            }
        }
        
        newData
}