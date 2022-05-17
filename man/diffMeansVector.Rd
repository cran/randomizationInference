\name{diffMeansVector}
\alias{diffMeansVector}
\title{Vector of Pairwise Differences of Mean Outcomes}
\description{Calculates the differences of mean observed outcomes for multiple specified treatment factors and specified pairs of comparison levels.}
\usage{diffMeansVector(y, w, calcOptions)}
\arguments{
  \item{y}{
    a vector or matrix of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{calcOptions}{
    a list of options for calculating the vector of differences of mean outcomes.
    \code{calcOptions$factors} is a numeric vector denoting the treatment factors of interest.
    \code{calcOptions$pairs} is a matrix of pairs (specified by row) of levels for comparison.
  }
}

\details{
If unspecified, \code{calcOptions$pairs} defaults to \code{c(0,1)}.

If \code{calcOptions$factors} is specified, its length must equal the number of rows specified in \code{calcOptions$pairs}.
If unspecified, \code{calcOptions$factors} defaults to an appropriately-sized vector of \code{1}'s.
}

\value{A vector of differences of mean observed outcomes.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\seealso{\code{\link{diffMeans}}}

\examples{
# 2 treatment factors, each with 3 levels
# Assignments, outcomes, and desired pairs
w1 <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
w2 <- c(1, 2, 3, 2, 3, 1, 3, 1, 2)
w <- cbind(w1, w2)
y <- c(4, 6, 5, 7, 4, 7, 11, 9, 10)
diffMeansVector(
  y,
  w,
  calcOptions = list(
    factors = c(1, 1, 2),
    pairs = rbind(c(1, 2), c(2, 3), c(1, 3))
  )
) # Equals c(-1, 1, 0)
}


\keyword{methods}
