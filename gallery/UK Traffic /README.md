## Data
The dataset collects the total volume of traffic on the stretch of road for the whole year in Cardiff, UK. Data can be found at [here](http://www.dft.gov.uk/traffic-counts/download.php).

```r
# preprocess the data, including only two columns from the original data, and derive the frequency column
busescoaches <- myTable$busescoaches
linklengthmiles <- myTable$linklengthmiles
frequency=busescoaches/linklengthmiles
dataset<-data.frame(busescoaches,linklengthmiles,frequency)
# And then clustering the data into 3 clusters
d<-dist(pcamatrix, method="euclidean")
pfit<-hclust(d,method="ward.D")
groups<-cutree(pfit,k=3)
# Appending the lable column to the dataset
dataset$lab <- as.factor(groups)
# Finally, plotting it
a <- prVis  (dataset, labels = T, saveOutput = T)
```
## Graphs

### prVis
![]()

### tsne
![]()

### umap
![]()
