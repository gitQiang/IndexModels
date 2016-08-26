rm(list=ls())
gc()
setwd("E:/HQ/IndexModels/价格指标Wind")
options(stringsAsFactors = FALSE)
library(xlsx)

path="E:/HQ/IndexModels/价格指标Wind"
filenames <- list.files(path=path,pattern=".xls$",full.names = TRUE)
        
IndexID <- matrix(0,4,0)
for(i in 1:length(filenames)){
        aa <- read.xlsx2(filenames[i],1,as.data.frame = TRUE, header=FALSE, colClasses="character")
        aa <- aa[1:9, ]
        aa[aa[,1]=="",1] <- which(aa[,1]=="")
        rownames(aa) <- aa[,1]
        aa <- aa[, -1]
        
        ## filtered conditions
        aa <- aa[, aa["频率",] %in% c("日","周")]
        aa <- aa[, (unlist(aa["更新时间",]) > "2016-08-15")]
        
        if(ncol(aa) > 0){
             IndexID <- cbind(IndexID,aa[c(3,6,9,4), ])
        }
}
IndexID <- t(IndexID)
colnames(IndexID) <- c("指标名称","ID","更新时间","频率")

## filtered conditions
IndexID <- IndexID[!grepl("\\(停止\\)", IndexID[,1]), ]
IndexID <- IndexID[!grepl("腈纶", IndexID[,1]), ]
IndexID <- IndexID[!grepl("锦纶", IndexID[,1]), ]
IndexID <- IndexID[!grepl("\\(重复\\)", IndexID[,1]), ]
##去掉重复的指标
IndexID <- IndexID[!duplicated(IndexID[,2]), ]

print(dim(IndexID))

write.table(IndexID,file = paste(path,"/","FilteredIDs.txt",sep=""), quote=FALSE, row.names = FALSE, sep="\t")
