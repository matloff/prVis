extract <- function (instruction) {
    if (grepl(":", instruction)) { # x has : in it
        vector <- strtoi(unlist(strsplit(instruction, ":")))
        return (vector)
    } else {
        return (strtoi(instruction))
    }
}

polyGet <- function (X, deg=2, maxInteractDeg=deg) {
    if (sum(sapply(X, is.factor)) > 0)
        X <- factorsToDummies(X)

    instructions <- generateMoves(names(X), deg, maxInteractDeg)
    Result <- cbind(data.matrix(X), matrix(nrow=nrow(X),ncol=length(instructions) - ncol(X)))
    start <- ncol(X) + 1
    remove(X)
    for (i in start:length(instructions)) {
        numbers <- extract(instructions[i])
        Result[, i] <- Result[,numbers[1]] * Result[,numbers[2]]
    }
    return (Result)
}

polyGet_big <- function (X, deg=2, maxInteractDeg=deg) {
    if (sum(sapply(X, is.factor)) > 0)
        X <- factorsToDummies(X)
    
    if(file.exists("polyMat.bk"))
        file.remove("polyMat.bk")
    
    require(bigstatsr)
    
    instructions <- generateMoves(names(X), deg, maxInteractDeg)
    Result <- cbind(data.matrix(X), matrix(nrow=nrow(X),ncol=length(instructions) - ncol(X)))
    Result <- as_FBM(Result, type="double", backingfile = "polyMat")
    start <- ncol(X) + 1
    remove(X)
    for (i in start:length(instructions)) {
        numbers <- extract(instructions[i])
        Result[, i] <- Result[,numbers[1]] * Result[,numbers[2]]
    }
    return (Result)
}


generateMoves <- function(name, deg=2, maxInteractDeg=deg) 
{
    if (deg <= 1)
        stop("deg should be bigger than 1")
    if (maxInteractDeg > deg)
        stop("Interaction deg should be bounded by deg")
    
    numVar <- length(name)
    factors <- grep("factor[[:digit:]]_", name)  
    nonfactors <- setdiff(1:numVar, factors) 
    
    # cutoffs[i] + 1 : cutoffs[i + 1] are the index of ith deg 
    cutoffs <- vector(length=deg+1)
    cutoffs[1] <- 0
    cutoffs[2] <- numVar
    
    indices <- 1:numVar
    instructions <- sapply(1:numVar, FUN=toString)
    labels <- vector(length = numVar)
    labels[nonfactors] <- sapply(-(1:length(nonfactors)), FUN=toString)
    labels[factors] <- sub("\\D*(\\d+).*","\\1", name[factors]) 
    sums <- products <- 1:numVar
    
    xIndices <- indices 
    xInstructions <- instructions
    xLabels <- labels
    xSums <- sums
    xProducts <- products
    
    for (i in 2:deg) 
    {
        indexRange <- (cutoffs[i-1] + 1) : (cutoffs[i])
        yIndices <- indices[indexRange] 
        yInstructions <- instructions[indexRange]
        yLabels <- labels[indexRange]
        ySums <- sums[indexRange]
        yProducts <- products[indexRange]
        
        newInstructions <- vector()
        newLabels <- vector()
        newSums <- vector()
        newProducts <- vector()

        for (j in 1:numVar) {
            if (maxInteractDeg < i) { # no interaction
                # case 1: no interaction + x[j] is factor var
                if(strtoi(xLabels[j]) > 0)
                    next
                for (m in 1:length(yIndices)) {
                    # case 2: no interaction + x[j] is not factor + y[k] is not its descendents
                    if (xLabels[j] != yLabels[m]) 
                        next
                    # case 3: no inter + x[j] find its desendent 
                    newInstructions <-c(newInstructions, paste0(xIndices[j], ":", yIndices[m]))
                    newSums <- c(newSums, xIndices[j] + ySums[m])
                    newProducts <- c(newProducts, xIndices[j] * yProducts[m])
                    newLabels <- c(newLabels, xLabels[j])
                    break
                } # end m
            } else {
                if (j > length(yIndices)) 
                    break
                    
                for (k in j:length(yIndices)) {
                    # other cases:
                    
                    # case 8: ..., x[j] is numeric, y[k] is also numeric variable of that same x[j]
                    # case 9: ..., x[j] is numeric, y[k] is pure numeric interaction
                    # case 10: ... x[j] is numeric, y[k] is an interaction
                    if(strtoi(xLabels[j]) < 0) {
                        if (i == 2 && strtoi(yLabels[k]) < 0 && xLabels[j] != yLabels[k])
                            newLabel <- "0"
                        else
                            newLabel <- yLabels[k]
                    } else {
                        # case 4: has int, x[j] is factor, y[k] is also a factor of the same class
                        if (xLabels[j] == yLabels[k])
                            next
                        # case 5: has int, x[j] is factor, y[k] is an interaction containing 
                        # a component which is the same class as x[j]
                        if (grepl(paste0("^",xLabels[j],"_"), yLabels[k], fixed=F) ||
                            grepl(paste0("_",xLabels[j], "$"), yLabels[k], fixed = F) || 
                            grepl(paste0("_", xLabels[j], "_"), yLabels[k], fixed = F))
                            next
                        # case 7: ..., x[j] is a factor, y[k] is a interaction term  with 
                        # no component of class of that of x[j] or another factor class
                        if (is.na(strtoi(yLabels[k])) || strtoi(yLabels[k]) > 0)
                            newLabel <- paste0(xLabels[j], "_", yLabels[k])
                        # case 6: ..., y[k] is a numeric var
                        else
                            newLabel <- xLabels[j]
                    }
                    
                    tempSum <- xIndices[j] + ySums[k]
                    tempProduct <- xIndices[j] * yProducts[k]
                    if (tempSum %in% newSums) {
                        if (tempProduct %in% newProducts) {
                             if (length(intersect(which(newSums == tempSum), which(newProducts == tempProduct))) > 0)
                                 next
                        }
                    } 
                    
                    newInstructions <-c(newInstructions, paste0(xIndices[j], ":", yIndices[k]))
                    newSums <- c(newSums, tempSum)
                    newProducts <- c(newProducts, tempProduct)
                    newLabels <- c(newLabels, newLabel)
        
                } # end k
            } # end else
        } # end j
        
        # update everything
        instructions <- c(instructions, newInstructions)
        labels <- c(labels, newLabels)
        sums <- c(sums, newSums)
        products <- c(products, newProducts)
        cutoffs[i + 1] <- cutoffs[i] + length(newInstructions)
        indices <- c(indices, (cutoffs[i] + 1):(cutoffs[i+1]))
    } # end i
    return(instructions)
}

factorsToDummies <- function(df) 
{
    require(dummies)
    outDF <- data.frame(rep(0,nrow((df))))  # filler start
    numFactor <- 0
    for (i in 1:ncol(df)) {
        dfi <- df[,i]
        if (!is.factor(dfi)) {
            outDF <- cbind(outDF,dfi) 
            names(outDF)[ncol(outDF)] <- names(df)[i]
        } else {
            numFactor <- numFactor + 1
            dumms <- factorToDummies(dfi,names(df)[i], numFactor)
            outDF <- cbind(outDF,dumms)
        }
    }
    outDF[,1] <- NULL  # delete filler
    outDF
}



factorToDummies <- function (f,fname, factorNum) 
{
    n <- length(f)
    fl <- levels(f)
    ndumms <- length(fl) - 1
    dms <- matrix(nrow = n, ncol = ndumms)
    for (i in 1:ndumms) dms[, i] <- as.integer(f == fl[i])
    colnames(dms) <- paste("factor", toString(factorNum), "_", fname,'.', 
                           fl[-(length(fl))], sep = "")
    dms
}




