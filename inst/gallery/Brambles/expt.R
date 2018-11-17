library("boot")
library("umap")
library("tsne")
# change the last column to be a factor 
brambles[, 3] <- as.factor(brambles[, 3])
prVis(brambles, labels = T)

z <- umap(brambles[,-3])
plot(z$layout,col=brambles[,3])