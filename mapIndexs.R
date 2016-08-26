rm(list=ls())
gc()
setwd("E:/HQ/IndexModels/�۸�ָ��Wind")
options(stringsAsFactors = FALSE)
library(xlsx)

path="E:/HQ/IndexModels/�۸�ָ��Wind"
filenames <- list.files(path=path,pattern=".xls$",full.names = TRUE)
        
IndexID <- matrix(0,4,0)
for(i in 1:length(filenames)){
        aa <- read.xlsx2(filenames[i],1,as.data.frame = TRUE, header=FALSE, colClasses="character")
        aa <- aa[1:9, ]
        aa[aa[,1]=="",1] <- which(aa[,1]=="")
        rownames(aa) <- aa[,1]
        aa <- aa[, -1]
        
        ## filtered conditions
        aa <- aa[, aa["Ƶ��",] %in% c("��","��")]
        aa <- aa[, (unlist(aa["����ʱ��",]) > "2016-08-15")]
        
        if(ncol(aa) > 0){
             IndexID <- cbind(IndexID,aa[c(3,6,9,4), ])
        }
}
IndexID <- t(IndexID)
colnames(IndexID) <- c("ָ������","ID","����ʱ��","Ƶ��")

## filtered conditions
IndexID <- IndexID[!grepl("\\(ֹͣ\\)", IndexID[,1]), ]
IndexID <- IndexID[!grepl("����", IndexID[,1]), ]
IndexID <- IndexID[!grepl("����", IndexID[,1]), ]
IndexID <- IndexID[!grepl("\\(�ظ�\\)", IndexID[,1]), ]
##ȥ���ظ���ָ��
IndexID <- IndexID[!duplicated(IndexID[,2]), ]

print(dim(IndexID))

write.table(IndexID,file = paste(path,"/","FilteredIDs.txt",sep=""), quote=FALSE, row.names = FALSE, sep="\t")