# data from 
# https://archive.ics.uci.edu/ml/datasets/seeds
wheat <- read.table("~/desktop/wheatkernal.txt")
colnames(wheat) <- c("Area", "Perimeter", "compactness", "length_of_kernal",
                     "width", "asymmetry_coefficient", "length_of_groove",
                     "category")
wheat[, 8] <- as.factor(wheat[, 8])
# prVis
prVis(wheat, labels = T)

# umap
z <- umap(wheat[,-8])
plot(z$layout,col=wheat[,8])

# tsne
tsneResults <- tsne(wheat[, -8])
plot(tsneResults,col=wheat[,8])
