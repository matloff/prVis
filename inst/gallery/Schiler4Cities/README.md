
From Schiller's real estate data, http://www.econ.yale.edu/~shiller/data.htm

Four cities, Atlanta, Chicago, Dallas, Oakland.  Put them all together,
removed the officially-designated outliers.

Shows sales at 2 different times (first 2 columns); the time in quarters
from 1970 for the 2 different times.

Took random subset of 1000.

Regressed sales against quarter, replaced sales by residuals, to remove
time trend.

Expect that we might have 4 clusters, for 4 cities.

Below, in order, are the plots from **prVis**, UMAP from **uwot** and
UMAP from **umap**.  The **prVis** plot seems to have 4 "prongs,"
reflecting the 4 cities.

![alt text](PrVis.png)

![alt text](Uwot.png)

![alt text](Umap.png)

