\name{blockRand}
\alias{blockRand}
\title{Permutations of Treatment Assignments for Randomized Block Designs}
\description{Randomly permutes an assignment vector or matrix according to a randomized block design, for a specified number of permutations.}
\usage{blockRand(w, nperm, block)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nperm}{
    a number specifying the desired number of random permutations.
  }
  \item{block}{
    a vector of block designations.
  }
}

\details{
Assignments are permuted within each block.

If \code{w} is a matrix, the permutations occur by row.
}

\value{A list of permuted assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{completeRand}}, \code{\link{latinRand}}}

\examples{
w1 = c(0,1,0,1,0,1,0,1)
block = c(0,0,0,0,1,1,1,1)
blockRand(w1, nperm = 5, block)

w2 = c(0,0,1,1,0,0,1,1)
blockRand(w = cbind(w1,w2), nperm = 5, block)
}

\keyword{methods}
