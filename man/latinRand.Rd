\name{latinRand}
\alias{latinRand}
\title{Random Treatment Assignments for Isomorphic Latin Square Designs}
\description{Randomly draws an assignment vector or matrix according to the isomorphic Latin square design, for a specified number of permutations.}
\usage{latinRand(w, nrand, row, col)}
\arguments{
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{nrand}{
    a number specifying the desired number of random assignments.
  }
  \item{row}{
    a vector of row designations.
  }
  \item{col}{
    a vector of column designations.
  }
}

\details{
Assignments are randomly permuted along rows and columns such that the Latin square design is preserved.

If \code{w} is a matrix, the permutations occur by row.
}

\value{A list of random isomorphic assignment vectors or matrices.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{completeRand}}, \code{\link{blockRand}}}

\examples{
w <- c(
  "C", "D", "B", "A", "A", "B", "D", "C",
  "D", "C", "A", "B", "B", "A", "C", "D"
)
row <- rep(1:4, 4)
col <- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4))
latinRand(w, nrand = 5, row, col)
}

\keyword{methods}
