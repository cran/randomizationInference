\name{constEffect}
\alias{constEffect}
\title{Potential Outcomes With Constant Treatment Effects}
\description{Calculates potential outcomes under modified assignments, according to the specified constant treatment effect(s).}
\usage{constEffect(y, w, w_new, poOptions)}
\arguments{
  \item{y}{
    a vector of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{w_new}{
    a vector or matrix of modified assignments.
  }
  \item{poOptions}{
    a list of options for calculating potential outcomes.
    \code{poOptions$tau} is a number or numeric vector denoting the constant treatment effect(s).
  }
}

\value{A vector of potential outcomes under the modified assignments.}

\seealso{\code{\link{zeroEffect}}}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\examples{
## 1 treatment factor with 2 levels
## Assignments
w = c(0,0,0,0,0,1,1,1,1,1)
## Modified Assignments
w_new = c(1,1,1,1,1,0,0,0,0,0)
## Outcomes
y = c(4,6,5,7,4,7,11,9,8,12)
constEffect(y, w, w_new, poOptions = list(tau = 2))

## 2 treatment factors, each with 2 levels
## Assignments
w1 = c(0,0,0,0,0,1,1,1,1,1)
w2 = c(0,1,0,1,0,1,0,1,0,1)
w = cbind(w1,w2)
## Modified assignments
w1_new = c(1,1,1,1,1,0,0,0,0,0)
w2_new = c(1,0,1,0,1,0,1,0,1,0)
w_new = cbind(w1_new,w2_new)
## Outcomes
y = c(4,6,5,7,4,7,11,9,8,12)
constEffect(y, w, w_new, poOptions = list(tau = c(2,-1)))
}


\keyword{methods}
