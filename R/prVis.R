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
#    bigData: use bigmemory package to store matrices if specified as true
#    saveOutputs: specify the name of the file where the results will be saved.
#                 default file is 'lastPrVisOut'. set to the empty string to
#                 not save results.
#    cex: argument to R plot(), controlling point size
#    alpha: a number between 0 and 1 that can be used to specify transparency
#           for alpha blending. If alpha is specified ggplot2 will be used to
#           create the plot
prVis <- function(xy,labels=FALSE,yColumn = ncol (xy), deg=2,
   scale=FALSE,nSubSam=0,nIntervals=NULL,
   outliersRemoved=0,pcaMethod="prcomp",bigData=FALSE,
   saveOutputs="lastPrVisOut",cex=0.5, alpha=0)
{
  # safety check
  if (!pcaMethod %in% c('prcomp','RSpectra'))
    stop("pcaMethod should be either NULL, prcomp, or RSpectra")

  # use bigmemory if user specifies bigData
  if(bigData){
    xymat <- as.big.matrix(xy)
    xy <- xymat[,]
  }

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

  # specify xdata as big matrix if bigData specified
 # xdata <- as.matrix(xdata)
  polyMat <- as.matrix(getPoly(xdata, deg)$xdata)
  
  if(bigData){
    polyMat<- as.big.matrix(polyMat)
    polyMat <-polyMat[,]
  }
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
  if (saveOutputs != ""){
    # ensure that data is saved correctly for continuous columns
    if (labels && is.factor(ydata))
      outputList <- list(gpOut=polyMat,prout=x.pca,
        colName=colnames(xy[, -ncxy]), yCol = ydata, yname=colnames(xy)[ncxy])
        # yCol and yname are the names of the factor column
    # if ydata is not a factor
    else
      outputList <- list(gpOut=polyMat,prout=x.pca, colName=colnames(xy))
    save(outputList,file=saveOutputs)
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
#       savedPrVisOut: the name of the file where a previous call to prVis was
#                      stored.
#       area: vector with in the form of [x_start, x_finish, y_start, y_finish].
#             x_start, x_finish, y_start, and y_finish should all be between 0
#             and 1. These values correspond to percentages of the graph from
#             left to right and bottom to top. [0,1,0,1] specifies the entirety
#             of the graph. [0,0.5,0.5,1] specifies upper-left quadrant. x_start
#             must be less than x_finish and y_start must be less than y_finish

addRowNums <- function(np=0,area=c(0,1,0,1),savedPrVisOut="lastPrVisOut")
{
  load(savedPrVisOut)
  if(is.null(row.names(outputList$prout$x[,1:2])))
    row.names(outputList$prout$x) <-
      as.character(1:nrow(outputList$prout$x))
  pcax <- outputList$prout$x[,1:2]

  if(identical(area, c(0,1,0,1))){
    # get boundaries of graph
    xMin <- min(outputList$prout$x[,1])
    xMax <- max(outputList$prout$x[,1])
    yMin <- min(outputList$prout$x[,2])
    yMax <- max(outputList$prout$x[,2])
    # error checking on inputs
    xI <- area[1]
    xF <- area[2]
    yI <- area[3]
    yF <- area[4]
    if (xI < 0 | xI > 1 | xF < 0 | xF > 1 | xI > xF)
    if (yI < 0 | yI > 1 | yF < 0 | yF > 1 | yI > yF){
      stop('invalid area boundaries, 0 < x_start < x_finish < 1, 0 < y_start <
           y_finish < 1. area is in the form of c(x_start,x_finish,y_start,
                                                  y_finish)')
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

# colorCode: display color coding for user-specified vairables or expressions.
# Normally called after prVis, produce a new coloring of the same plot produced
# by prVis.
# arguments:
#          colName: user can specify the column that he or she wants to produce
#                   the color on. The column specified must be a continuous one.
#          colorVec: a column of continuous data, with the first entry in the
#                    vector corresponding to the first row in the dataframe
#                    used in savedPrVisOut. Can be used when wanting to color
#                    by a column that was not included in the prVis call.
#          n: The number of shades used to color code the values of colName.
#             n and exps should not both be specified at the same time.
#          exps: expressions that create a label column that produces coloring.
#                If user specifies colName, he or she cannot provide arguments
#                for exps, since they are two ways to produce coloring (should
#                be mutually exclusive). User can supply several expressions,
#                each one corresponding to a group (a label, a color, a level),
#                and concatenating by c(). The expression should be mutually
#                exclusive, since a data point cannot be in two colors.
#                expression format:
#                <exp> ::= <subexpression> [(+|*) <subexpression>]
#                <subexpression> ::= columnname relationalOperator value
#                Note: * represents logic and, + represents logic or
#          savedPrVisOut: the file that stores a prVis object

colorCode <- function(colName="",colorVec=NULL, n=256,exps="",
savedPrVisOut="lastPrVisOut", cex = 0.5)
{
  load(savedPrVisOut)
  xdata <- outputList$gpOut[,1:length(outputList$colName)]
  plotData <- outputList$prout$x[,1:2]
  isColorDeclared <- xor(!is.null(colorVec),colName != "")

  # cases covered by continColor:
  if (isColorDeclared && exps == "")
  {
    if (!is.null(colorVec)) # colorVec spcified
      d <- colorVec
    else # colName spcified
    {
      if (!colName %in% outputList$colName) # colName is not in the dataframe
        stop("The column specified is not a continuous one or not found")
      else # normal
      {
        colNum = which(colName == outputList$colName)
        d <- xdata[,colNum]
      }

    }
    minVal <- min(d)
    maxVal <- max(d)
    diffVal <- maxVal - minVal
    colorPalette <- rev(rainbow(n,start=0,end=0.7))
    colorIndexes <- sapply(d, function(x) ((x - minVal) * n) %/% diffVal)
    plot(plotData, col=colorPalette[colorIndexes])
  }

  else if (is.null(colorVec) && colName == "" && exps != "")
  {
    xdata <- as.data.frame(xdata)
    # create a label column with potentially more than one labels
    numberOfRows <- length(outputList$prout$x[,1])
    userCol <- rep(NA, numberOfRows) # initialize label column
    hasY <- !(is.null(outputList$yname))
    if (hasY) #original dataset has a factor column
      factorCol <- outputList$yCol

    hasLabel <- c() # track rows that has already had a label, check for relable
    for (i in 1:length(exps)) {
      # delete all white spaces (compress the string)
      exp <- gsub(" ", "", exps[i], fixed = TRUE)
      labelName <- exp #use the expression as the label name
      subExp <- unlist(strsplit(exp, "\\+|\\*"))
      for (m in 1:length(subExp))
        exp <- sub(subExp[m], "", exp, fixed = T)
      exp <- unlist(strsplit(exp, split="")) #string to vector of characters
      #number of +/* operators should be one less than the number of constraints
      if (length(exp) != length(subExp) - 1)
        stop (length(exp)," +/* not match ",length(subExp)," constraints")
      for (j in 1:length(subExp)) { # solve all of an expressions constraints
        # Ex has one constraint but the relational operator is extracted
        # EX : "Male", "1"
        Ex <- unlist(strsplit(subExp[j],"(==|>=|<=|>|<|!=)")) #relational ops
        if (length(Ex) != 2) # Ex should have two components: column name, value
          stop ("The constraint must follow the format: 'yourCol'
          'relationalOperator' 'value'")
        else {
          tmp <- trimws(Ex[1])
          columnNum <- grep(tmp, outputList$colName)
          if (!length(columnNum) && (!hasY || tmp != outputList$yname))
            stop("The specified column ",Ex[1],
                 " is not found in the data frame xy")
          # restore the relational operator
          relationalOp <- sub(Ex[1], "", subExp[j], fixed= TRUE)
          relationalOp <- sub(Ex[2], "", relationalOp, fixed= TRUE)

          if (hasY && tmp == outputList$yname) # Ex[1] is the factorcol
          {
            if (!Ex[2] %in% levels(factorCol))
              stop ("The label ", Ex[2], " is not in the factor column")
            # when ecounter operations between labels, only == and != make sense
            if (!relationalOp %in% c("==", "!="))
              stop ("Use of the inappropriate operator ", relationalOp)
            # get the row numbers of data that satisfy the constraint userExp[i]
            rowBelong <- switch(relationalOp, "==" = which(factorCol == Ex[2]),
            "!=" = which (factorCol != Ex[2]))
          }
          else { # EX[1] is a continuous column, so Ex[2] should be a number
            val <- as.double(Ex[2])

            if (is.null(val)|| val< min(xdata[[columnNum]])
                ||val > max(xdata[[columnNum]]))
              stop("The value ", Ex[2], " is out of the range")
            # get the row numbers of data that satisfy the constraint userExp[i]
            rowBelong <- switch(relationalOp,
              "==" = which(xdata[[columnNum]] == val),
              "!=" = which (xy[[columnNum]] != val),
              ">="= which(xdata[[columnNum]]>=val),
              "<="=which(xdata[[columnNum]] <= val),
              ">" = which (xdata[[columnNum]] > val),
               "<" = which(xdata[[columnNum]] < val))
          }
        }

        if (j == 1) # initialize labelData
          labelData <- rowBelong
        else {
          if (exp[j-1] == "*") # And, get the intersection of the row numbers
            labelData <- intersect(labelData, rowBelong)
          else  # Or, get the union
            labelData <- union(labelData, rowBelong)
        }
      } # end for loop
      # check for overlaps! will cause relabel of certain data that satisfy more
      # than two expressions. Enforcing mutual exclusivity between expressions
      if (length(intersect(labelData, hasLabel)) != 0)
        stop ("The expression ", i, " tries to relabel some data,
        the groups must be mutually exclusive")
      # gives the label to data that satisfy the expression
      userCol[labelData] <- labelName
      # update hasLabel to keep track of all data has been labeled
      hasLabel <- union(labelData, hasLabel)
   } # end big for loop
   if (length(hasLabel) == 0) # no matching data of the expressions
     stop ("Expression(s) match no data points")

   userCol[-hasLabel] <- "others"
   userCol <- as.factor(userCol)
   plot (plotData, col=userCol, pch=15, cex=cex)
  } # end createCol

  else
  {
    if (colName == "" && exps == "" && is.null(colorVec)) # spcified nothing
      stop("colName, expressions(exps), or colorVec must be specified")

    else
      stop ("colorVec, colName and exps should not be specified
            at the same time")
  }
}
