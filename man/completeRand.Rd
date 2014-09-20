\name{completeRand}
\alias{completeRand}
\title{Random Treatment Assignments for Completely Randomized Designs}
\description{Randomly draws a specified number of assignment vectors or matrices according to a completely randomized design.}
\usage{completeRand(w, nrand)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nrand}{
    a number specifying the desired number of random assignments.
  }
}

\details{If \code{w} is a matrix, the permutations occur by row.}

\value{A list of random assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{blockRand}}, \code{\link{latinRand}}}

\examples{
w1 = c(0,0,0,0,1,1,1,1)
completeRand(w1, nrand = 5)

w2 = c(0,1,0,1,0,1,0,1)
completeRand(w = cbind(w1,w2), nrand = 5)}

\keyword{methods}
