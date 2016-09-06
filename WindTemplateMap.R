
setwd("//bjb-155/IndexModels/data/WindÄ£°å")
filenames <- list.files(path=".",".xls$")

library(xlsx)
options(warn=-1)

indexIDM <- c()

for(i in 1:length(filenames)){
        print(i)
        aa <- read.xlsx2(filenames[i],1,as.data.frame = TRUE, header=FALSE, colClasses="character")
        aa <- as.matrix(aa)
        tmp <- aa[c(3,6), -1]
        indexIDM <- cbind(indexIDM,tmp)
}
indexIDM <- t(indexIDM)
indexIDM <- indexIDM[!duplicated(indexIDM[,2]), ]
indexIDM[,1] <- gsub("¡¾¼ÆËãÓÃ¡¿","",indexIDM[,1])
indexIDM <- indexIDM[nchar(indexIDM[,2]) > 3,  ]

write.table(indexIDM,file="IndexIDMaps_WindTemplate.txt",sep="\t",col.names = FALSE,row.names = FALSE,quote = FALSE)    