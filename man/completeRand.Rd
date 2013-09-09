\name{completeRand}
\alias{completeRand}
\title{Permutations of Treatment Assignments for Completely Randomized Designs}
\description{Randomly permutes an assignment vector or matrix according to a completely randomized design, for a specified number of permutations.}
\usage{completeRand(w, nperm)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nperm}{
    a number specifying the desired number of random permutations.
  }
}

\details{If \code{w} is a matrix, the permutations occur by row.}

\value{A list of permuted assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{blockRand}}, \code{\link{latinRand}}}

\examples{
w1 = c(0,0,0,0,1,1,1,1)
completeRand(w1, nperm = 5)

w2 = c(0,1,0,1,0,1,0,1)
completeRand(w = cbind(w1,w2), nperm = 5)}

\keyword{methods}
