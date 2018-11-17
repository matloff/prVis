library(prVis)
library(uwot)
library(tsne)

data <- read.csv("iBeacon_RSSI_Labeled.csv")
data[,1] <- as.factor(data[,1])

d <- data[,1]

umapResults <- umap(d)

# graph prVis
prVis(d,saveOutputs=FALSE)
# with labels
prVis(data,saveOutputs=FALSE,LABELS=TRUE,labelCol=1)

# graph umap
plot(umapResults)
# with labels
plot(umapResults,col=data[,1])
