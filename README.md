# prVis, a visualization tool based on polyreg
Development of a new package to extend the functionality of [the **polyreg** package](https://github.com/matloff/polyreg) and introduce one particular new
feature, a visualization tool.

**Motivation**:
A simpler, equally effective alternative visulaization tool to various non-linear
dimension reduction (generalized Principal Components Analysis) methods,
such as t-SNE and UMAP, by applying PCA to “polynomial-ized” versions.

**Usage:**
The main function in this package is prVis(), which provides a two dimensional
visualization of the X data in classification problems. Users have the option to
display the color coded labels (Y, and it can be both in regression cases and in
classification cases.)

*Example:* Swiss Roll data derived from [Dinoj Surendran's site](http://people.cs.uchicago.edu/~dinoj/manifold/swissroll.html).
The preprocessed data can be downloaded [here](https://github.com/matloff/prVis/blob/master/inst/data/SwissRoll/Surendran.txt)

``` r
# Read in the data
sw <- read.table('Surendran.txt',header=T)
# The last column of the original data is the "label" column.
# In this case, we need to transform the last column to an R factor
sw[,4] <- as.factor(sw[,4])
# We "forget" the labels for each row by excluding the label columnm and plot it "
prVis(sw[,-4],labels=F)
```
![](https://github.com/matloff/prVis/blob/master/inst/data/SwissRoll/SWwithnoY.png)
```r
#In the above plot, it suggests 4 different components
```
```r
# And we now "unforget" the lables by including them into the data set and
# plot it to see how many components(labels) are actually in the data set
prVis(sw,labels=T)
```
![](https://github.com/matloff/prVis/blob/master/inst/data/SwissRoll/SWwithY.png)
```r
And yeah, the result (indicated by the different colorings) corresponds
to our prediction before
```

*Example:* Programmer/engineer 2000 Census data, Silicon Valley.
Built into [the **regtools** package](https://github.com/matloff/regtools).
Install package or download directly [here](https://raw.githubusercontent.com/matloff/regtools/master/data/prgeng.txt).
In the former case, getPE() reads in the dataset and does some preprocessing,
producing a data frame named **pe**.
```r
getPE() # get the dataset
# Choose some of the variables: age, gender, education level, and occupation
pe1 <-[, c(1,2,6,7, 12:16)]
# Plot the graph, and save the output to z for later use
z  = prVis(pe1, saveOutputs = T)
axis(side=1)
# The plot below consists about a dozen of separated streaks, making us wonder
# why that happens (Note: the data set is not artificial, we do not know any
# grouping before hand)
```
![](https://github.com/matloff/prVis/blob/master/inst/data/PE/pe.png)
```r
# To investigate that question, we call another utility function in the package:
addRowNums(16,z)
# This will write the row numbers of 16 random points from the data set on to
# the graph above.
# The function addRowNums will also ouput the highlighted row numbers to the R console:
[1] "highlighted rows:"
[1] 31
[1] 2020
[1] 2846
[1] 3412
[1] 6555
[1] 7044
[1] 7183
[1] 9404
[1] 11761
[1] 14259
[1] 14922
[1] 17307
[1] 17798
[1] 17824
[1] 17831
[1] 18066
```
![](https://github.com/matloff/prVis/blob/master/inst/data/PE/peNum.png)

The plot above, along with the output row numbers, shows that rows 7183 and 14922 seem to be on the same streak.
Let's dive in to it.
```r
pe1[7183,]
#          age sex ms phd occ1
#7183 51.10664   0  0   0    0
#     occ2 occ3 occ4 occ5
#7183    0    1    0    0
pe1[14922,]
#          age sex ms phd occ1
#14922 45.02447   0  0   0    0
#     occ2 occ3 occ4 occ5
#14922   0    1    0    0
```
Yeah! The streaks do reveal some patterns: except for age, these two workers
share the same occupational type (occ3), same gender, and same education level.
The insight we get from this visulaization is that each streak may represent a
certain combination of categorical variables!

**Options**
*(1)* RSpectra extension: We give user an option to use RSpectra (require package
RSpectra) method to do PCA (Principal Component Analysis), instead of using prcomp.
We are expecting a speed up by doing so.
      Default: pcaMethod="prcomp"

*(2)* Outlier removal: We provide user the option to specify the number of
outliers to be removed from the output graph. (mahalanobis distance for now)
Default: outliersRemoved=0

*(3)* Alpha Blending: User can specify the alpha blending value
(require package ggplot2).
