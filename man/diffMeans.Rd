\name{diffMeans}
\alias{diffMeans}
\title{Single Pairwise Difference of Mean Outcomes}
\description{Calculates the difference of mean outcomes for a specified treatment factor and specified pair of comparison levels.}
\usage{diffMeans(y, w, calcOptions = NULL)}
\arguments{
  \item{y}{
    a vector of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{calcOptions}{
    a list of options for calculating the difference of mean outcomes (if necessary).
    \code{calcOptions$factor} is a number denoting the treatment factor of interest (defaults to \code{1}).
    \code{calcOptions$pair} is a vector (of length 2) denoting the pair of levels for comparison (defaults to \code{c(0,1)}).
    If \code{calcOptions} is \code{NULL}, then \code{calcOptions$factor} and \code{calcOptions$pair} take on their default values.
  }
}

\value{The difference of mean outcomes.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{diffMeansVector}}}

\examples{
## 1 treatment factor with 2 levels
## Assignments and outcomes
w = c(0,0,0,0,0,1,1,1,1,1)
y = c(4,6,5,6,4,11,11,9,10,9)
diffMeans(y, w) ## Equals 5

## 2 treatment factors, each with 3 levels
## Assignments and outcomes
w1 = c(1,2,3,1,2,3,1,2,3)
w2 = c(1,2,3,2,3,1,3,1,2)
w = cbind(w1,w2)
y = c(4,6,5,7,4,7,11,9,10)
diffMeans(y, w,
    calcOptions = list(factor = 2, pair = c(1,3))) ## Equals 0
}

\keyword{methods}
