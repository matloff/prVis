library(prVis)
library(uwot)
library(tsne)

d <- read.csv('haberman.data',head=F)

#prVis without labels
prVis(d[,-4])
#prVis with labels
prVis(d,labels=TRUE)

umapOut <- umap(d[,-4])
tsneOut <- tsne(d[,-4])

# umap without labels
plot(umapOut)
# umap with labels
plot(umapOut,col=d[,4])

#tsne without labels
plot(tsneOut)
#tsne with labels
plot(tsneOut,col=d[,4])


