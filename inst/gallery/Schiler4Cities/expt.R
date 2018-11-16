
# data from http://www.econ.yale.edu/~shiller/data.htm

# the 4 cities were concatenated together into one file, and officially
# designated outliers removed, then read in to the data frame all4

rall4 <- all4 
lm1 <- lm(all4$V1 ~ all4$V3) 
rall4$V1 <- lm1$residuals 
lm2 <- lm(all4$V2 ~ all4$V4) 
rall4$V2 <- lm2$residuals 
rall4a <- rall4[sample(1:nrow(rall4),1000),] 
prVis(rall4a,scale=T,outliersRemoved=10) 
plot(uwot:::umap(rall4a)) 
plot(umap(rall4a)$layout) 

