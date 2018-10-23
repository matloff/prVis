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
```r
#In the above plot, it suggests 4 layers of data, which may indicate 4 different components
```
```r
# And we now "unforget" the lables by including them into the data set and
# plot it to see how many componentsa(labels) are actually in the data set
prVis(sw,labels=T)
```
![](https://github.com/matloff/prVis/blob/Readme/inst/data/SwissRoll/SWwithY.png)
```r
And yeah, the result (indicated by the different colorings) corresponds to our prediction before
```

*Example:* Programmer/engineer 2000 Census data, Silicon Valley.
Built into [the **regtools** package](https://github.com/matloff/regtools). Install package or download directly [here](https://raw.githubusercontent.com/matloff/regtools/master/data/prgeng.txt). In the former case, getPE() reads in the dataset and does some preprocessing, producing a data frame named **pe**.
```r
getPE() # get the dataset
# Choose some of the variables: age, gender, education level, and occupation
pe1 <-[, c(1,2,6,7, 12:16)]
#plot the graph, and save the output to z for later use
z  = prVis(pe1, saveOutputs = T)
#The plot below consists about a dozen of separated streaks, making us wonder why that happens (Note: the data set is not artificial, we do not know any grouping before hand)
```
