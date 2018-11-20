# two-dimensional visualization of the X data in classification problems,
# similar in spirit to ordinary PCA and t-sne, color coded by Y values
# (class IDs in classification case, subinternvals of Y in regression
# case)

# t-sne, e.g. in the Rtsne package, applied in dimension k, attempts to
# find a k-dimensional manifold for which most of the data are "near";
# for visualization purposes, typically k = 2, which is assumed here

# the idea here is to expand the data with polynomial terms, using
# getPoly(), then apply PCA to the result

# typically these methods are applied only to a subsample of the data,
# due both to the lengthy computation time and the "black screen
# problem" (having a lot of points fills the screen, rendering the plot
# useless)

# arguments:

#    xy:  data frame
#    labels:  if TRUE, last column is Y for a classification problem;
#             must be an R factor, unless nIntervals is non-NULL, in
#             which case Y will be discretized to make labels
#    deg:  degree of polynomial expansion
#    scale:  if TRUE, first call scale() on the X data
#    nSubSam:  number of rows to randomly select; 0 means get all
#    nIntervals: in regression case, number of intervals to use for
#                partioning Y range to create labels
#    outliersRemoved: specify how many outliers to remove from
#                     the plot, calculated using mahalanobis distance. if
#                     outliersRemoved is between 0 and 1, a corresponding
#                     percentage of the data will be removed
#    pcaMethod: specify how eigenvectors will be calculated, using
#               prcomp or RSpectra
#    saveOutputs: if TRUE, return list with gpOut = output of getPoly(),
#                 prout = output of prcomp()
#    cex: argument to R plot(), controlling point size

prVis <- function(xy,labels=FALSE,yColumn = ncol (xy), deg=2,
   scale=FALSE,nSubSam=0,nIntervals=NULL,
   outliersRemoved=0,pcaMethod="prcomp",
   saveOutputs=FALSE,cex=0.5, alpha=0)
{
  # safety check
  if (!pcaMethod %in% c('prcomp','RSpectra'))
    stop("pcaMethod should be either NULL, prcomp, or RSpectra")

  nrxy <- nrow(xy)
  ncxy <- ncol(xy)
  if (labels) {
    if (yColumn > ncol(xy) || yColumn <= 0)
      stop("The column specified is out of range")
    tmp <- xy[, ncxy]
    xy[, ncxy] <- xy[, yColumn]
    # swapping the last column with the user-specified column
    xy[, yColumn] <- tmp

  }
  rns <- row.names(xy)
  if (scale) {
    if (labels) {
      xy[,-ncxy] <- scale(xy[,-ncxy])
    } else xy <- scale(xy)
    row.names(xy) <- rns
  }

  if (nSubSam < nrxy && nSubSam > 0)
    xy <- xy[sample(1:nrxy,nSubSam),]

  if (labels) {
    ydata <- xy[,ncxy]
    if (is.null(nIntervals) && !is.factor(ydata))
      stop('Y must be a factor for classif.; set nIntervals for regress.')
    if (!is.null(nIntervals)) {
      rng <- range(ydata)
      increm <- (rng[2] - rng[1]) / nIntervals
      ydata <- round((ydata - rng[1]) / increm)
      ydata <- as.factor(ydata)
    }
    xdata <- xy[,-ncxy, drop=FALSE]
  } else xdata <- xy

  xdata <- as.matrix(xdata)
  polyMat <- getPoly(xdata, deg)$xdata
  if (pcaMethod == "prcomp") {
    x.pca <- prcomp(polyMat,center=TRUE)
    xdata <- x.pca$x[,1:2]
  } else {
    require(RSpectra)
    x.cov <- cov(polyMat)
    x.eig <- eigs(x.cov,2)
    x.pca <- x.eig
    xdata <- as.matrix(polyMat) %*% x.eig$vectors[,1:2]
    colnames(xdata) <- c("PC1","PC2")
  }

  if (outliersRemoved > 0 && outliersRemoved <= nrow(xdata)){
    # percentage based outlier removal
    if (outliersRemoved < 1){
      outliersRemoved = floor(outliersRemoved * nrow(xdata))
    }
    # calculate mahalanobis distances for each data point
    xdataCov <- var(xdata)
    distances <- mahalanobis(xdata,colMeans(xdata),xdataCov)
    # find which row the max distances correspond to
    rownames(xdata) <- 1:nrow(xdata)
    if (labels) names(ydata) <- 1:nrow(xdata)
    names(distances) <- rownames(xdata)
    sortedDistances <- sort(distances, decreasing=TRUE)
    outliers <- names(sortedDistances)[1:outliersRemoved]
    # remove outliers
    xdata <- xdata[!rownames(xdata) %in% outliers,]
    if (labels) ydata <- ydata[!names(ydata) %in% outliers]
  }

  if (alpha) {
    require(ggplot2)
    if (labels)  {
      plotObject <-  qplot(x=xdata[,1],y=xdata[,2],xlab="PC1",ylab="PC2",
         alpha=alpha,col=ydata,size=I(cex))
    } else {
      plotObject <- qplot(x=xdata[,1],y=xdata[,2],xlab="PC1",ylab="PC2",
         alpha=alpha,size=I(cex))
    }
    print(plotObject)

  } else {
    if (labels)  {
      plot(xdata, col=ydata, pch=15, cex=cex)
    } else {
      plot(xdata, pch=15, cex=cex)
    }
  }
  if (saveOutputs){
    return(list(gpOut=polyMat,prout=x.pca))
  }
}

# intended to be used when a plot produced by prVis() is on the screen;
# chooses np points at random from the PCA output, writing their row
# numbers on the plot; these are the numbers from the full dataset, even
# if nSubSam > 0; the argument savedPrVisOut is the return value of
# prVis()
#
# arguments:
#       np: the number of points to add row numbers to. if no value of np is
#           provided, rownumbers will be added to all datapoints
#       savedPrVisOut: a list returned from a previous call to prVis with
#                      saveOutputs=TRUE
#       specifyArea: if TRUE, will prompt the user to specify the area in
#                    the plot to add row numbers. The user will be prompted
#                    for four numbers corresponding to the four corners of
#                    the area to be specified. Each number input should be
#                    a number between 0 and 1. A PCA1 interval of (0,1)
#                    along with a PCA2 interval of (0,1) corresponds to the
#                    full original plot. A PCA1 interval of (0.25,0.75)
#                    along with a PCA2 interval of (0.25,0.75) corresponds
#                    to a square selection centered around the middle of
#                    the plot. The square's height would be 50% of the
#                    graph's height and the square's width would be 50% of
#                    the graph's width.

addRowNums <- function(np=0,savedPrVisOut,specifyArea=FALSE)
{
  pcax <- savedPrVisOut$prout$x[,1:2]
  if(is.null(row.names(pcax)))
    row.names(savedPrVisOut$prout$x) <- as.character(1:nrow(savedPrVisOut$prout$x))

  if(specifyArea){
    # get boundaries of graph
    xMin <- min(savedPrVisOut$prout$x[,1])
    xMax <- max(savedPrVisOut$prout$x[,1])
    yMin <- min(savedPrVisOut$prout$x[,2])
    yMax <- max(savedPrVisOut$prout$x[,2])
    # error checking on inputs
    xI <- as.numeric(readline(prompt="starting x location (float from 0 to
                                  1):"))
    xF <- as.numeric(readline(prompt="ending x location (float from 0 to
                                   1):"))
    yI <- as.numeric(readline(prompt="starting y location (float from 0 to
                                  1):"))
    yF <- as.numeric(readline(prompt="ending y location (float from 0 to
                                   1):"))
    if (is.na(xI)){
      stop('starting x location must be a number')
    }
    if (is.na(xF)){
      stop('ending x location must be a number')
    }
    if (is.na(yI)){
      stop('starting y location must be a number')
    }
    if (is.na(yF)){
      stop('ending y location must be a number')
    }
    if (xI < 0 | xI > 1 | xF < 0 | xF > 1 | xI > xF |
        yI < 0 | yI > 1 | yF < 0 | yF > 1 | yI > yF){
      stop('invalid boundaries (must be between 0 and 1, start must be less
           than finish')
    }

    # scale x interval
    xI <- (xMax - xMin)*xI + xMin
    xF <- (xMax - xMin)*xF  + xMin
    # scale y interval
    yI <- (yMax - yMin)*yI + yMin
    yF <- (yMax - yMin)*yF  + yMin
    # filter to datapoints within specified range
    pcax <- pcax[which(pcax[,1] <= xF & pcax[,1] >= xI & pcax[,2] <=
                  yF & pcax[,2] > yI),]
  }

  npcax <- nrow(pcax)
  tmp <- sample(1:npcax,np,replace=FALSE)
  rowNames <- row.names(pcax[tmp,])
  print('highlighted rows:')
  sorted <- sort(as.numeric(rowNames))
  for (i in 1:length(rowNames)) {
    rn <- rowNames[i]
    print(sorted[i])
    coords <- pcax[rn,]
    text(coords[1],coords[2],rn)
  }
}

# intended to produce different grouping methods based on user input;
# prVis uses the color coding paradigm specified by the factor column (if no
# factor in the dataset, then no coloring), this function will create a factor
# column and replace the default one in the original data frame (if there is any)
# If there is no factor column in xy, then the created factor column is placed
# in the last column. The factor levels are specified by expressions. Each
# expression accounts for one factor level (one color in the output of the graph)
# User can enter multiple expressions to produce mutiple levels(labels) of a
# factor column, but they must be mutually exclusive (don't need to be exhautive,
# the unlabeled data points will be grouped as "other")
# Example: if an user wants to highlight the group which contains all people who
# is male **and** under 25 years old, he or she may want to input in this format:
# male == 1 + age < 25
# if the user wants to highlight the group which contains people
# who is male **or** who is under 25 years old, he or she may want to input in
# this format:
# male == 1 * age < 25
# arguments:
#       xy: data frame, the same argument that passed into the function prVis


createGroup (xy)
{
  UserIn <- readline(
        prompt="Your expression(followed by '+/*' for more constraints): ")

}
