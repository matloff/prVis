# prVis, a visualization tool based on polyreg
Development of a new package to extend the functionality of [the **polyreg** package](https://github.com/matloff/polyreg) and introduce one particular new feature, a visualization tool.

**Motivation**:
A simpler, equally effective alternative to various non-linear dimension reduction (generalized Principal Components Analysis) methods, such as t-SNE and UMAP, by applying PCA to “polynomial-ized” versions.

**Usage:**
The main function in this package is prVis(), which provides a two dimensional visualization of the X data in classification problems. Users have the option to display the color coded labels (Y, and it can be both in regression cases and in classification cases.)

*Example:* Swiss Roll data derived from [Dinoj Surendran's site](http://people.cs.uchicago.edu/~dinoj/manifold/swissroll.html). The preprocessed data can be downloaded [here](https://github.com/matloff/prVis/tree/master/inst/data/SwissRoll)

``` r
# swiss roll data has 4 columns, 1600 rows
sw <- read.table('Surendran.txt',header=T)
# The last column of the original data is the "label" column.
# So it is a classification problem! In this case,
# we need to transform the last column to an R factor
sw[,4] <- as.factor(sw[,4])
# We "forget" the labels for each row by excluding the label columnm and plot it "
prVis(sw[,-4],labels=F)
```
![](https://github.com/matloff/prVis/blob/Readme/inst/data/SwissRoll/SWwithnoY.png)
It suggests 4 layers of data, which indicates 4 different components in the graph above

```r
# And we now "unforget" the lables by including them into the data set and
# plot it to see how many componentsa(labels) are actually in the data set
prVis(sw,labels=T)
```
![](https://github.com/matloff/prVis/blob/Readme/inst/data/SwissRoll/SWwithY.png)
And yeah, the result (indicated by the different colorings) corresponds to our prediction before

*Example:* to be added
