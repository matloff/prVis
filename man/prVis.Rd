\name{prVis}
\alias{prVis}
\alias{addRowNums}

\title{Polynomial-Based Manifold Exploration }

\description{
Polynomial-based alternative to t-SNE, UMAP etc.}

\usage{
prVis(xy, labels = FALSE, yColumn = ncol (xy), deg = 2, scale = FALSE,
nSubSam = 0, nIntervals = NULL, outliersRemoved = 0, pcaMethod = "prcomp",
saveOutputs="lastPrVisOut", cex = 0.5, alpha=0)
addRowNums(np, savedPrVisOut, specifyArea = FALSE)
colorCode(colName="",n=256,exps="", savedPrVisOut="lastPrVisOut", cex = 0.5)
}

\arguments{
  \item{xy}{Data frame with labels, if any, in the last column or can be
  specified in the yColumn.}
  \item{labels}{If TRUE, have class labels.}
  \item{yColumn}{The column number of the labeled column, default to be the last}
  \item{deg}{Degree of polynomial.}
  \item{scale}{If TRUE, call \code{scale} on nonlabels data before
     generating polynomial terms.}
  \item{nSubSam}{Number of random rows of \code{xy} to sample; 0 means
     use the full dataset.}
  \item{nIntervals}{If labels column is continuous, discretize into this
     many levels.}
  \item{outliersRemoved}{Specify how many outliers or how much the percentage
     corresponding to the data to be removed from the plot, calculated using
     mahalanobis distance.}
  \item{pcaMethod}{Specify how eigenvectors will be calculated, using prcomp
     or RSpectra}
  \item{saveOutputs}{Save prVis object to a file}
  \item{cex}{Point size for plot.}
  \item{alpha}{transparency scale parameter, used in ggplot2}
  \item{np}{Number of points to label in plot.}
  \item{savedPrVisOut}{Output save from a previous call, so can avoid
     duplicate computation.}
  \item{specifyArea}{If set to true, user will be prompted to specify
     the area that row numbers to be added into}
  \itme{colName}{Continuous column that user wants to produce color}
  \item{n}{rainbow parameter}
  \item{exps}{expressions that create a label column that produces coloring}
}

\details{

A number of "nonlinear" analogs of Principle Components Analysis (PCA)
have emerged, such as ICA, t-SNE, UMAP and so on. Intuitively, an
approach based on polynomials may be effective too.  Specifically,
\code{prVis} first expands \code{xy} to polynomial terms, then applies
PCA to the result.

Once a plot is displayed, \code{addRowNums} can be used to add
row-number IDs of random points, to gain further insight into the data.

}

\value{
If \code{saveOutputs} is set, an R list is returned, with comppnents
\code{gpOut}, the generated polynomial matrix, and \code{prout}, the
return value from the call to \code{prcomp}.
}

\examples{

getPE()  # prgeng data, included in pkg
# may want to predict wage; look at some predictors
pe1 <- pe[,c(1,2,6:16)]
z <- prVis(pe1,nSubSam=2000,labels=FALSE)
# get a bunch of streaks; why?
# call addRowNums() (not shown); discover that points on the same streak
# tend to have same combination of sex, education and occupation; moving
# along a streak mainly consists of variying age
colorCode(exps=c("sex==1*occ3==1", "sex==1* occ2==1"))
print('see data/SwissRoll for another example')

}
