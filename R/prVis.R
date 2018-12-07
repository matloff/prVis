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
#    parDims: number of parallel PCA dimensions to graph. If parDims > 0, the 
#             a parallel coordinate graph will be displayed using the
#             cdparcoord package.
#    nLevels: number of levels to discretize the principal components into. Will
#             be used when parDims > 0.
#    saveOutputs: specify the name of the file where the results will be saved.
#                 default file is 'lastPrVisOut'. set to the empty string to
#                 not save results.
#    cex: argument to R plot(), controlling point size
#    alpha: a number between 0 and 1 that can be used to specify transparency
#           for alpha blending. If alpha is specified ggplot2 will be used to
#           create the plot
prVis <- function(xy,labels=FALSE,yColumn = ncol (xy), deg=2,
   scale=FALSE,nSubSam=0,nIntervals=NULL,
   outliersRemoved=0,pcaMethod="prcomp",parDims=0,nLevels=10,
   saveOutputs="lastPrVisOut",cex=0.5, alpha=0)
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

  # parallel coordinate graphing
  if (parDims > 0) {
    require(cdparcoord)
    dims <- x.pca[,1:parDims]
    pcdata <- discretize(dims,nlevels=nLevels)
    discparcoord(pcdata)
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
    if (labels && is.factor(ydata)) #xy has factor column, colName stores the name for all continuou
                #yname stores the name for the factor col
      outputList <- list(gpOut=polyMat,prout=x.pca,
        colName=colnames(xy[, -ncxy]), yCol = ydata, yname=colnames(xy)[ncxy])
    else # xy has no factor column
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
  pcax <- outputList$prout$x[,1:2]
  if(is.null(row.names(pcax)))
    row.names(outputList$prout$x) <-
      as.character(1:nrow(outputList$prout$x))

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

# intended to produce different grouping methods based on user input;
# prVis uses the color coding paradigm specified by the factor column (if no
# factor in the dataset, then no coloring), this function will create a factor
# column and replace the original factor column in the original data frame (if
# there is any)
# If there is no factor column in xy, then the created factor column is placed
# in the last column. The factor levels are specified by expressions. Each
# expression accounts for one factor level (one color in the output of the graph)
# User can enter multiple expressions to produce mutiple levels(labels) of a
# factor column, but they must be mutually exclusive (don't need to be exhautive,
# the unlabeled data points will be grouped as "others")
# Example: if an user wants to highlight the group which contains all people who
# is male **and** under 25 years old, he or she may want to input in this format:
# male == 1 * age < 25
# if the user wants to highlight the group which contains people
# who is male **or** who is under 25 years old, he or she may want to input in
# this format:
# male == 1 + age < 25
# arguments:
#       xy: data frame, the same argument that passed into the function prVis
# return value:
#       A data frame that has a factor column and intended to be passed to prVis
# Note: I use terms like group, level, label interchangebly, they all mean
#       represents certain characteristics of the data within the group (or
#       with same level, same label)

colorCode <- function(colName="",n=256,exps="", savedPrVisOut="lastPrVisOut",
cex = 0.5)
{
  load(savedPrVisOut)
  xdata <- outputList$gpOut[,1:length(outputList$colName)]
  plotData <- outputList$prout$x[,1:2]
  if (colName == "" && exps == "")
    stop("colName and expressions(exps) not specified")
  if (colName != "" && exps == "")
  {
    if (!colName %in% outputList$colName)
      stop("The column specified is not a continuous one or not found")
    else { # do continue color (rainbow)
      colNum = which(colName == outputList$colName)
      d <- xdata[,colNum]
      minVal <- min(d)
      maxVal <- max(d)
      diffVal <- maxVal - minVal
      colorPalette <- rev(rainbow(n,start=0,end=0.7))
      colorIndexes <- sapply(d, function(x) ((x - minVal) * n) %/% diffVal)
      plot(plotData, col=colorPalette[colorIndexes])
    }
  }
  else if(colName != "" && exps != "") # illegal specify both
    stop("colName for rainbow, exps for createFactor column")

  else { #original createGroup, only 1 or 0 factor cols, not reuseable, not interactive
    numberOfRows <- length(outputList$prout$x[,1])
    userCol <- rep(NA, numberOfRows)
    hasY <- !(is.null(outputList$yname))
    if (hasY) #original dataset has a factor column
      factorCol <- outputList$yCol

    hasLabel <- c() # track rows that has already had a label, check for relable
    for (i in 1:length(exps)) {
      # delete all white spaces (compress the string)
      exp <- gsub(" ", "", exps[i], fixed = TRUE)
      labelName <- exp
      subExp <- unlist(strsplit(exp, "\\+|\\*"))
      for (m in 1:length(subExp))
        exp <- sub(subExp[m], "", exp, fixed = T)
      exp <- unlist(strsplit(exp, split="")) #string to vector of characters
      #number of +/* operators should be one less than the number of constraints
      if (length(exp) != length(subExp) - 1)
        stop (length(exp)," +/* not match ",length(subExp)," constraints")
      for (j in 1:length(subExp)) { # solve one expression by solving all constraints
        # Ex has one constraint but the relational operator is extracted
        # EX : "Male", "1"
        Ex <- unlist(strsplit(subExp[j],"(==|>=|<=|>|<|!=)")) #relational ops
        if (length(Ex) != 2) # Ex should have two components: column name, value
          stop ("The constraint must follow the format: 'yourCol'
          'relationalOperator' 'value'")
        else {
          tmp <- paste("\\b", Ex[1], sep="")
          tmp <- paste(tmp, "\\b", sep="")
          columnNum <- grep(tmp, outputList$colName)
          if (!length(columnNum) && (!hasY || tmp != outputList$yname))
            stop("The specified column ",Ex[1]," is not found in the data frame xy")
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

            if (is.null(val)||val< min(xdata[[columnNum]])||val > max(xdata[[columnNum]]))
              stop("The value ", Ex[2], " is out of the range")
            # get the row numbers of data that satisfy the constraint userExp[i]
            rowBelong <- switch(relationalOp, "==" = which(xdata[[columnNum]] == val),
            "!=" = which (xy[[columnNum]] != val),">="= which(xdata[[columnNum]]>=val),
            "<="=which(xdata[[columnNum]] <= val), ">" = which (xdata[[columnNum]] > val),
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
      # check for overlaps! will cause relabel of certain data that satisfy two or
      # more expressions. Enforcing mutually exclusiveness between expressions
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
}



createCol <- function(xy)
{
  factorCol <- length(which(sapply(xy, is.factor) == T)) # number of factor cols
  columnname<-readline(prompt="Please specify the name of the column you created: ")
  if (factorCol > 0) {
    if (length(factorCol) > 1) # if there is more than one factor cols, error!
      stop("The data frame cannot have more than one factor column")
    # get index of the factor column
    factorCol <- as.numeric(which(sapply(xy, is.factor) == T))
    if (colnames(xy)[factorCol] == columnname)#if user specified name exist already
      stop("Duplicate names")
  }
  expressionNum <- 0 # track the expression, later use for error message
  xy[[columnname]] <- NA # initilize the factor column defined by user
  hasLabel <- c() # track rows that has already had a label, check for relable
  repeat { # ask user for multiple expressions
           # (each expression represent one unique group in the factor column)
    expressionNum <- expressionNum + 1
    userIn <- readline(
          prompt="Your expression(followed by '+/*' for more constraints): ")
    # delete all white spaces (compress the string)
    userIn <- gsub(" ", "", userIn, fixed = TRUE)
    #label is defined by the compressed version of the user input expression
    labelName <- userIn
    #userExp contains a vector of constraints (sub-expressions)
    #userIn now has a vector of +/* operators. Together with userExp,
    # they form the the whole expression
    userExp <- unlist(strsplit(userIn, "\\+|\\*"))
    for (i in 1:length(userExp))
      userIn <- sub(userExp[i], "", userIn, fixed = T)
    userIn <- unlist(strsplit(userIn, split="")) #string to vector of characters
    #number of +/* operators should be one less than the number of constraints
    if (length(userIn) != length(userExp) - 1)
      stop (length(userIn)," +/* not match ",length(userExp)," constraints")
    for (i in 1:length(userExp)) { # solve one expression by solving all constraints
      # Ex has one constraint but the relational operator is extracted
      # EX : "Male", "1"
      Ex <- unlist(strsplit(userExp[i],"(==|>=|<=|>|<|!=)")) #relational ops
      if (length(Ex) != 2) # Ex should have two components: column name, value
        stop ("The constraint must follow the format: 'columnName'
        'relationalOperator' 'value'")
      else {
        tmp <- paste("\\b", Ex[1], sep="")
        tmp <- paste(tmp, "\\b", sep="")
        columnNum <- grep(tmp, colnames(xy))
        if (!length(columnNum)) # xy should have the specified column
          stop("The specified column ",Ex[1]," is not found in the data frame xy")
      }
      # restore the relational operator
      relationalOp <- sub(Ex[1], "", userExp[i], fixed= TRUE)
      relationalOp <- sub(Ex[2], "", relationalOp, fixed= TRUE)
      if (columnNum == factorCol) # the user spcified column is the factor col
      {
        # check to see if the label specified is in the factor column
        if (!Ex[2] %in% levels(xy[[columnNum]]))
          stop ("The label ", Ex[2], " is not in the factor column")
        # when ecounter operations between labels, only == and != make sense
        if (!relationalOp %in% c("==", "!="))
          stop ("Use of the inappropriate operator ", relationalOp)
        # get the row numbers of data that satisfy the constraint userExp[i]
        rowBelong <- switch(relationalOp, "==" = which(xy[[columnNum]] == Ex[2]),
        "!=" = which (xy[[columnNum]] != Ex[2]))
      }
      else { # EX[1] is a continuous column, so Ex[2] should be a number
        val <- as.double(Ex[2])
        if (is.null(val)||val< min(xy[[columnNum]])||val > max(xy[[columnNum]]))
          stop("The value ", Ex[2], " is out of the range")
        # get the row numbers of data that satisfy the constraint userExp[i]
        rowBelong <- switch(relationalOp, "==" = which(xy[[columnNum]] == val),
        "!=" = which (xy[[columnNum]] != val),">="= which(xy[[columnNum]]>=val),
        "<="=which(xy[[columnNum]] <= val), ">" = which (xy[[columnNum]] > val),
         "<" = which(xy[[columnNum]] < val))
      }

      if (i == 1) # initialize labelData
        labelData <- rowBelong # labelData has the row numbers of data
                              # that satisfy the expression, rowBelong has the
                              # row numbers statisfy the constraint, hasLabel
                              # has the row numbers satisfy all expressions
      else {
        if (userIn[i-1] == "*") # And, get the intersection of the row numbers
          labelData <- intersect(labelData, rowBelong)
        else  # Or, get the union
          labelData <- union(labelData, rowBelong)
      }
    } # end for loop
    # check for overlaps! will cause relabel of certain data that satisfy two or
    # more expressions. Enforcing mutually exclusiveness between expressions
    if (length(intersect(labelData, hasLabel)) != 0)
      stop ("The expression ", expressionNum, " tries to relabel some data,
      the groups must be mutually exclusive")
    # gives the label to data that satisfy the expression
    xy[[columnname]][labelData] <- labelName
    # update hasLabel to keep track of all data has been labeled
    hasLabel <- union(labelData, hasLabel)
    # check if usr wants more levels/groups by providing more expressions
    # The levels/groups/labels and expressions are one-to-one corresponded
    # one expression creates one group, there is no intersection between groups
    moreIn <- readline(prompt="Do you want more levels(y/n): ")
    if (tolower(moreIn) != 'y')
      break;
  } # end repeat
  if (length(hasLabel) == 0) # no matching data of the expressions
    stop ("Expression(s) match no data points")
  # replace all NAs (data that has not yet been labeled) with label "others"
  xy[[columnname]][-hasLabel] <- "others"
  if (factorCol != 0) { # xy originally has one factor column, replace that
    xy[, factorCol] <- xy[[columnname]]
    xy[[columnname]] <- NULL # delete the column after data is transfered
    colnames(xy)[factorCol] <- columnname # rename the column
  }
  xy[[columnname]] <- as.factor (xy[[columnname]])
  # return the revised xy
  xy
}
