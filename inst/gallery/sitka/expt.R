# sitka89
# https://vincentarelbundock.github.io/Rdatasets/doc/geepack/sitka89.html
sitka <- read.csv("~/desktop/sitka89.csv", header = T)
# rewmove the numebering 
sitka <- sitka[, -1]
str(sitka)
prVis(sitka, labels = T)

# umap 
library(umap)
z <- umap(sitka[,-4])
plot(z$layout,col=sitka[,4])

# tsne
library(tsne)
tsneResults <- tsne(sitka[, -4])
plot(tsneResults,col=sitka[,4])
