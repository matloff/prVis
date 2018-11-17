library(prVis)
library(uwot)
library(tsne)

data <- read.csv('c4_game_database.csv')
data[,43] <- as.factor(data[,43])

d <- data[,-43]
# without labels
prVis(d,nSubSam=2000)
# with labels
prVis(data,nSubSam=2000,labels=TRUE)

# subsample for umap and tsne
nrdata <- nrow(data)
samples <- sample(1:nrdata,2000,replace=FALSE)
subData <- data[samples,]
subD <- subData[,-43]

umapResults <- umap(subD)
tsneResults <- tsne(subD)

plot(umapResults)
plot(umapResults,col=subData[,43])
plot(tsneResults)
plot(tsneResults,col=subData[,43])
