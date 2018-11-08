library(prVis)
library(uwot)
library(tsne)

# script for generatring plots with prVis, tsne, and umap
# args:
#       dataFile: name of data file
#       header: header arg for read.csv
#       sep: sep arg for read.csv
#       labelCol: which column will be used for colors
# use:
#       call genPlots from inside a folder with your dataset file
#       and it will automatically name files for you
genPlots <- function(dataFile,header=FALSE,sep=",",labelCol){
  data <- read.csv(dataFile,header=header,sep=sep)
  lastCol <- ncol(data)
  # interpret predict column as factor
  data[,labelCol] <- as.factor(data[,predictCol])
  #switch predict column into last place
  data[,c(labelCol,lastCol)] <- data[,c(lastCol,predictCol)]

  d <- data[,-lastCol]

  if (nrow(d) > 2000) {
    nSamples <- 2000
  } else {
    nSamples <- nrow(d)
  }

  dirName <- strsplit(getwd(),"/")
  dirName <- dirName[[1]][length(dirName[[1]])]

  set.seed(9999)
  umapResults <- umap(d)
  set.seed(9999)
  tsneResults <- tsne(d)
  
  #without labels
  imageName <- paste(dirName,"_prVis",".png",sep="")
  png(filename=imageName)
  set.seed(9999)
  z <- prVis(d,nSubSam=nSamples,saveOutputs=FALSE,labels=FALSE)
  dev.off()
  
  imageName <- paste(dirName,"_umap",".png",sep="")
  png(filename=imageName)
  plot(umapResults)
  dev.off()
  
  imageName <- paste(dirName,"_tsne",".png",sep="")
  png(filename=imageName)
  plot(tsneResults)
  dev.off()

  #with labels
  imageName <- paste(dirName,"_prVis","_labels",".png",sep="")
  png(filename=imageName)
  set.seed(9999)
  z <- prVis(data,nSubSam=nSamples,saveOutputs=FALSE,labels=TRUE)
  dev.off()
  
  imageName <- paste(dirName,"_umap","_labels",".png",sep="")
  png(filename=imageName)
  plot(umapResults,col=data[,labelCol])
  dev.off()
  
  imageName <- paste(dirName,"_tsne","_labels",".png",sep="")
  png(filename=imageName)
  plot(tsneResults,col=data[,labelCol])
  dev.off()
} 
