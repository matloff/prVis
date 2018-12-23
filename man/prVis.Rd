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
  \item{labels}{If TRUE, have class labels. The column specified by yColumn
     must be an R factor, unless nIntervals in non-NULL, in which case Y will
     be discretized to make labels.}
  \item{yColumn}{The column number of the labeled column, the default for
     yColumn is the last column of xy}
  \item{deg}{Degree of polynomial expansion.}
  \item{scale}{If TRUE, call \code{scale} on nonlabels data before
     generating polynomial terms.}
  \item{nSubSam}{Number of random rows of \code{xy} to sample; 0 means
     use the full dataset.}
  \item{nIntervals}{If Y column is continuous, transform it into a factor with
     n many levels}
  \item{outliersRemoved}{Specify how many outliers will be removed from the plot
     calculated using mahalanobis distance. Values between 0 and 1 will be
     interpreted as percentages (0.99 means remove the 99 percent of the data
     with the largest mahalanobis distances)}
  \item{pcaMethod}{A string that specifies the method of eigenvector
     computation, prcomp or RSpectra}
  \item{saveOutputs}{name of the file where prVis object will be saved. Use the
     empty string in order to not save results}
  \item{cex}{Controls the point size for plotting.}
  \item{alpha}{ a number between 0 and 1 that that specifies the level of 
     transparency for alpha blending. If alpha is specified then ggplot2 will
     be used to create the plot.}
  \item{np}{Number of points to add row numbers to in the plot. If no value is
     provided, rownumbers will be added to all datapoints in the selection}
  \item{savedPrVisOut}{the name of the file where a previous call to prVis was
     stored}
  \item{area}{A vector in the form of [x_start, x_finish, y_start, y_finish].
     x_start, x_finish, y_start, and y_finish should all be between 0 and 1.
     These values correspond to percentages of the graph from left to right and
     bottom to top. [0,1,0,1] would specify the entirety of the graph. [0,0.5,
     0.5,1] specifies the upper-left quadrant. x_start and y_start must be less
     than x_finish and y_finish respectivelly.}
  \item{colName}{The name of the column of continuous data that will be used for
     color coding}
  \item{n}{The number of shades used to color code the values of colName.
           n and exps should not both be specified at the same time.}
  \item{exps}{a vector of string expressions that will be used to create a
              factor column for coloring. If user specifies colName, they
              can not specify exps. Each expression corresponds to a group
              of the factor that will be created. The format for expression is
              listed below as a context free grammar.\cr\cr
              <expression> ::= <subexpression> [(+|*) <subexpression>]\cr
              <subexpression> ::= columnName relationalOperator value\cr\cr
              Note: * represents the logical and, + represents the logical or.}
}

\details{

A number of "nonlinear" analogs of Principle Components Analysis (PCA)
have emerged, such as ICA, t-SNE, UMAP and so on. Intuitively, an
approach based on polynomials may be effective too.  Specifically,
\code{prVis} first expands \code{xy} to polynomial terms, then applies
PCA to the result.

Once a plot is displayed, \code{addRowNums} can be used to add
row-number IDs of random points, to gain further insight into the data.

\code{colorCode} can be used to display color coding for user-specified
expressions using the exps argument. \code{colorCode} can also be used to color
based upon a column of continuous data by using the colName argument.
}

\value{
If \code{saveOutputs} is set, a file is R list is created, with the components
contained inside of a list called \code{outputList}. Two of the components,
\code{gpOut}, the generated polynomial matrix, and \code{prout}, the
return value from the call to \code{prcomp} will always be contained inside of
\code{outputList}. Additional information may be included in \code{outputList}
regarding the y column in \code{colName}, \code{yCol}, or \code{yname}.
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
