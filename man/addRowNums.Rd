\name{addRowNums}
\alias{addRowNums}
\title{addRowNums}

\description{
Once a plot is displayed, \code{addRowNums} can be used to add row-number IDs 
to random points to gain further insight into the data.
}

\usage{
addRowNums(np, area = c(0,1,0,1), savedPrVisOut)
}

\arguments{
  \item{np}{Number of points to add row numbers to in the plot. If no value is
     provided, rownumbers will be added to all datapoints in the selection}
  \item{area}{A vector in the form of [x_start, x_finish, y_start, y_finish].
     x_start, x_finish, y_start, and y_finish should all be numbers between 0 
     and 1.  These values correspond to percentages of the graph from left to 
     right and bottom to top. [0,1,0,1] would specify the entirety of the graph.
     [0,0.5,0.5,1] specifies the upper-left quadrant. x_start and y_start must 
     be less than x_finish and y_finish respectively.}
  \item{savedPrVisOut}{the name of the file where a previous call to prVis was
     stored}
}


