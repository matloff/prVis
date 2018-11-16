
wheat <- read.table("~/desktop/Res/DATA/wheat/wheatkernal.txt")
colnames(wheat) <- c("Area", "Perimeter", "compactness", "length of kernal", "width", "asymmetry coefficient", "length of groove", "category")
wheat[, 8] <- as.factor(wheat[, 8])
# prVis
prVis(wheat, labels = T)

# umap 
z <- umap(wheat[,-8]) 
plot(z$layout,col=wheat[,8])

# tsne
tsneResults <- tsne(wheat[, -8])
plot(tsneResults,col=wheat[,8])