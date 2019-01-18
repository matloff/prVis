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
              factor column for coloring. If user specifies colName or
              colorVec, they can not specify exps. Each expression corresponds
              to a group of the factor that will be created.
              The format for expression is
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
# colorCode and addRowNums (not shown here) can explore graph produced by
# prVis to see the characteristics of the data points from each streak

# There are three options (they are mutually exclusive in a way that only one
# of them can be spcified at a time):

# option "colName":
# "age" is an existing column in the dataset pe1. In prVis, we can only
# color the points by a factor column. But option colName allows coloring by
# a continuous one and it produces a "rainbow" of color
colorCode(colName="age")

# option "colorVec":
# Under the consideration that it is "unnatural" that we plot the data
# dependent on the same column that subsequently colors the plot. (like
# what we did in the above), we allow user to specify an "external" vector to
# color the plot
# We did not include the third column of dataset pe in creating pe1, so column 3
# of pe is "invisible" to pe1
colorCode(colorVec=pe[,3])

# option "exps":
# The code below specifes two expressions, delimited by a coma.
# The "*" operator "ands" the two "subexpressions" together
# the exps is essentially saying that: I want a color to denotes points
# in the plot that is a male and he has occupation 3, a color to denotes points
# that is a male and he has occupation 2, and all other points are denoted by
# the third color.
colorCode(exps=c("sex==1*occ3==0", "sex==1*occ2==1"))

}
