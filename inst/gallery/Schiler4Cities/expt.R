
rall4 <- all4 
lm1 <- lm(all4$V1 ~ all4$V3) 
rall4$V1 <- lm1$residuals 
lm2 <- lm(all4$V2 ~ all4$V4) 
rall4$V2 <- lm2$residuals 
rall4a <- rall4[sample(1:nrow(rall4),1000),] 
prVis(rall4a,scale=T,outliersRemoved=2) 
plot(uwot:::umap(rall4a)) 
plot(umap(rall4a)$layout) 

