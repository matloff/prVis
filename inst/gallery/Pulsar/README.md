
Pulsar data (HTRU2) from UCI Machine Learning Repository.  Goal is to predict
pulsar or non-pulsar star.

All plots give good separation of red and black. But look at the one
using **prVis()** at degree 3!  Even without labels, one would see that
"something is happening."

This also shows that one should get almost perfect prediction using a
polynomial regression model (say logistic) of degree 3.

![tsne with labels](tsneLabels.png "tsne with labels")
![umap with labels](umapLabels.png "umap with labels")

![prvis with labels](prvisDeg2Labels.png "prvis with labels")
![prvis without labels](prvisDeg3.png "prvis without labels")
![prvis with labels](prvisDeg3Labels.png "prvis with labels")

