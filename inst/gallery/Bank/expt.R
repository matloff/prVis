
f <- function()                                                                 {    
  for(i in 1:ncol(bank)) { 
     b <- bank[,i] 
     if (is.factor(b)) { 
        unk <- which(b == 'unknown') 
        bank[unk,i] <- NA 
     }   
  }  
   bank   
}  
bank <-  
   read.table('~/Research/DataSets/Bank/bank-full.csv',header=T,sep=';') 
bank$y <- as.integer(bank$y == 'yes') 
bank$job <- NULL  # had data issues 
bank$poutcome <- NULL  # almost all unknown 
bnk <- f()   
bnk1 <- factorsToDummies(bnk) 
bnk1$y <- as.factor(bnk1$y) 
bnkc <- bnk1[complete.cases(bnk1),] 
prVis(bnkc[,-29]) 
prVis(bnkc,labels=T) 
library(Rtsne) 
z <- Rtsne(bnkc[,-29]) 
plot(z$Y) 
plot(z$Y,col=bnkc$y) 
library(uwot)
plot(uwot:::umap(bnkc[-29]))  
plot(uwot:::umap(bnkc[-29]),col=bnkc$y) 


