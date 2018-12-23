library(prVis)
library(uwot)
library(tsne)

data <- read.csv('abalone.data',header=FALSE)
d <- data[-1]

# without labels
prVis(d[-8])
# with labels
prVis(d,yColumn=8,labels=TRUE, nIntervals=30)
# see continuous colors
continColors(8)

umapResults <- umap(d[-8])
# without labels
plot(umapResults)
# with labels
plot(umapResults, col=d[,8])

