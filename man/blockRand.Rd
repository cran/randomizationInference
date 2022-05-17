\name{blockRand}
\alias{blockRand}
\title{Random Treatment Assignments for Randomized Block Designs}
\description{Randomly draws a specified number of assignment vectors or matrices according to a randomized block design.}
\usage{blockRand(w, nrand, block)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nrand}{
    a number specifying the desired number of random assignments.
  }
  \item{block}{
    a vector of block designations.
  }
}

\details{
Assignments are randomly permuted within each block.

If \code{w} is a matrix, the permutations occur by row.
}

\value{A list of random assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{completeRand}}, \code{\link{latinRand}}}

\examples{
w1 <- c(0, 1, 0, 1, 0, 1, 0, 1)
block <- c(0, 0, 0, 0, 1, 1, 1, 1)
blockRand(w1, nrand = 5, block)

w2 <- c(0, 0, 1, 1, 0, 0, 1, 1)
blockRand(w = cbind(w1, w2), nrand = 5, block)
}

\keyword{methods}
