\name{latinRand}
\alias{latinRand}
\title{Permutations of Treatment Assignments for Latin Square Designs}
\description{Randomly permutes an assignment vector or matrix according to a Latin square design, for a specified number of permutations.}
\usage{latinRand(w, nperm, row, col)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nperm}{
    a number specifying the desired number of random permutations.
  }
  \item{row}{
    a vector of row designations.
  }
  \item{col}{
    a vector of column designations.
  }
}

\details{
Assignments are permuted such that the Latin square design is preserved.

If \code{w} is a matrix, the permutations occur by row.
}

\value{A list of permuted assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{completeRand}}, \code{\link{blockRand}}}

\examples{
w = c("C","D","B","A","A","B","D","C","D","C","A","B","B","A","C","D")
row = rep(1:4,4)
col = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
latinRand(w, nperm = 5, row, col)
}

\keyword{methods}
