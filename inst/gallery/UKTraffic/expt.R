setwd("yourdirectory")
myTableLeicester<-read.csv("~/Leicester.csv")
summary(myTableLeicester)
col_headings <- c("year","cp", "estimationmethod", "estimationmethoddetailed", "region", "localauthority","road","roadcategory","easting","northing","startjunction","endjunction","linklengthmiles","pedalcycles","motorcycles","carstaxis","busescoaches","lightgoodsvehicles","v2axlerigidhgv","v3axlerigidhgv","v4or5axlerigidhgv","v3or4axleartichgv","v5axleartichgv","v6ormoreaxleartichgv","allhgvs","allmotorvehicles")
myTableLeicester
names(myTableLeicester) <- col_headings
attach(myTableLeicester)

myTableLeicester$routes <- paste(myTableLeicester$startjunction,myTableLeicester$endjunction)
as.data.frame(table(myTableLeicester$routes))

busescoaches <- myTableLeicester$busescoaches
linklengthmiles <- myTableLeicester$linklengthmiles
frequency=busescoaches/linklengthmiles
dataset<-data.frame(busescoaches,linklengthmiles,frequency)
attach(dataset)
alldata<-data.frame(myTableLeicester$routes,busescoaches,linklengthmiles,frequency)
attach(alldata)

#Max-Min Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# normalize our dataset
#maxmindf is a shrinked version of original dataset
maxmindf <- as.data.frame(lapply(dataset, normalize))

#pca
column_names<-colnames(maxmindf)
#scale, with default settings, will calculate the mean and standard deviation of the entire vector,
#then "scale" each element by those values by subtracting the mean and dividing by the sd.
pcamatrix<-scale(maxmindf[,column_names])
#clustering
# C(1501,2), calculating the elucidean distance between every pair of points
d<-dist(pcamatrix, method="euclidean")
#Ward's minimum variance method aims at finding compact, spherical clusters
pfit<-hclust(d,method="ward.D")
#try to plot the clustering tree, does not go well
#plot(pfit,labels=myTableLeicester$routes)
# draw rectangles around clusters around pfit (an object returned by hclust)
#rect.hclust(pfit,k=3)
# k = the number of clusters the tree should be cut into.
groups<-cutree(pfit,k=3)

dataset$lab <- as.factor(groups)
a <- prVis  (dataset, labels = T, saveOutput = T)
#sometimes for some unbknow reason, x.pca.prout$x has no row names, this line make up to that deficit
row.names(a$prout$x) <- as.character(1:nrow(a$prout$x))
addRowNums(16, a)

library("umap")
z <- umap(dataset[,-4])
plot(z$layout,col=dataset[,4])

#cluster1:Short road length/low traffic for edinburge case
#cluster2:Long road length/high traffic
#cluster3:Short road length/high traffic
# green = c3
# red = c2



