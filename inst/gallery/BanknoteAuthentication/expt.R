library(prVis)
library(uwot)
library(tsne)

data <- read.csv('data_banknote_authentication.txt',header=FALSE)
data[,5] <- as.factor(data[,5])
d <- data[,-5]

# save results of umap and tsne
umapResults <- umap(d)
tsneResults <- tsne(d)

# graph prVis
prVis(d,saveOutputs=FALSE,labels=FALSE)
# with labels
prVis(data,saveOutputs=FALSE,labels=TRUE)

# graph umap
plot(umapResults)
# with labels
plot(umapResults,col=data[,5])

# graph tsne
plot (tsneResults)
# with labels
plot (tsneResults,col=data[,5])
