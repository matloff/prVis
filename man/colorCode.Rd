\name{colorCode}
\alias{colorCode}
\title{colorCode}

\description{
\code{colorCode} can be used to display color coding for user-specified
expressions using the exps argument. \code{colorCode} can also be used to color
based upon a column of continuous data by using the colName argument.
}

\usage{
colorCode(colName="", colorVec = NULL, n=256, exps="", 
          savedPrVisOut="lastPrVisOut", cex = 0.5)
}

\arguments{
  \item{colName}{The name of the column of continuous data that will be used for
     color coding}
  \item{colorVec}{An alternate way of specifying a column of continuous data,
    colorVec can be used to specify a vector of continuous data to color code
    the graph}
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

}
