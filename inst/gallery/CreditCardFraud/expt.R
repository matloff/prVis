# https://www.kaggle.com/mlg-ulb/creditcardfraud
library(umap)
library(tsne)
CCF <- read.csv("~/desktop/creditcard.csv")
CCF[, ncol(CCF)] <- as.factor(CCF[, ncol(CCF)])
length(which(CCF$Class == "1")) #number of fruads
# get same number of frauds/non-frauds, since # of frauds is little compared to 
# number of nonfrauds 
CCFsubIN <- c(which(CCF$Class == "1"),sample(which(CCF$Class == "0"),size=492))
CCFsub <- CCF[CCFsubIN, ]
prVis (CCFsub[, -1], labels = T) # exclude time column
prVis (CCFsub[, -1], labels = T, outliersRemoved = 10)
prVis (CCFsub[, -1], labels = T, outliersRemoved = 100)
prVis (CCFsub, labels = T) # with time column

# incorporate the time data 
timeVector <- CCF[ ,1]
colnames(CCF)[1] <- "timeNeed" 
CCF[, 1] <- rep(0, nrow(CCF))
CCF[1,1] <- timeVector[1] - 0
for (i in 2:nrow(CCF))
{
  # record the time needed for that specific transaction
  # change accumulated to single
  CCF[i,1] <- timeVector[i] - timeVector[i-1]
  if (i %% 1000 == 0)
    print (i) # show the process 
}

CCFsub <- CCF[CCFsubIN, ]
prVis (CCFsub, labels = T, outliersRemoved = 100)

# prcomp
z <- prcomp(CCFsub[, -ncol(CCFsub)])
plot(z$x,col=CCFsub[, ncol(CCFsub)], pch = 15, cex = 0.5)

#umap
z <- umap(CCFsub[, -ncol(CCFsub)]) 
plot(z$layout,col=CCFsub[, ncol(CCFsub)])

#tsne
tsneResults <- tsne(CCFsub[, -ncol(CCFsub)])
plot(tsneResults,col= CCFsub[, ncol(CCFsub)])
