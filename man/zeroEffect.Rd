\name{zeroEffect}
\alias{zeroEffect}
\title{Potential Outcomes With Zero Treatment Effects}
\description{Calculates potential outcomes under modified assignments, assuming zero treatment effect(s).}
\usage{zeroEffect(y, w, w_new)}
\arguments{
  \item{y}{
    a vector or matrix of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{w_new}{
    a vector or matrix of modified assignments.
  }
}

\value{A vector of potential outcomes under the modified assignments.}

\seealso{\code{\link{constEffect}}}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\examples{
# Assignments
w <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
# Modified Assignments
w_new <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
# Outcomes
y <- c(4, 6, 5, 7, 4, 7, 11, 9, 8, 12)
zeroEffect(y, w, w_new) # Returns y as is
}

\keyword{methods}
