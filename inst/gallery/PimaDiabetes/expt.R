library(prVis)
library(uwot)
library(tsne)

data <- read.csv('diabetes.csv')
data[,9] <- as.factor(data[,9])

d <- data[,-43]
# without labels
prVis(d)
prVis(data,labels=TRUE)

umapResults <- umap(d)
tsneResults <- tsne(d)

plot(umapResults)
plot(umapResults,col=data[,43])
plot(tsneResults)
plot(tsneResults,col=data[,43])
