# https://www.kaggle.com/uciml/breast-cancer-wisconsin-data
library(umap)
library(tsne)
BC <- read.csv("~/desktop/breastCancer.csv")
BC <- BC[, -1] #id column excluded
BC <- BC[, -32] #blank column excluded

prVis(BC, labels = T, yColumn = 1)
prVis(BC, labels = T, yColumn = 1, outliersRemoved = 50) # 50 outliers removed

#prcomp
z <- prcomp(BC[, -1])
plot(z$x,col=BC[, 1], pch = 15, cex = 0.5)

#umap
z <- umap(BC[, -1]) 
plot(z$layout,col=BC[, 1])

#tsne
tsneResults <- tsne(BC[, -1])
plot(tsneResults,col=BC[, 1])