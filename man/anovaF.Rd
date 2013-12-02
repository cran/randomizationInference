\name{anovaF}
\alias{anovaF}
\title{Analysis of Variance F statistic}
\description{Calculates the analysis of variance F statistic.}
\usage{anovaF(y, w, calcOptions = NULL)}
\arguments{
  \item{y}{
    a vector or matrix of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{calcOptions}{
    a list of options for calculating the analysis of variance (ANOVA) F statistic (if necessary).
    \code{calcOptions$block} can denote a block variable to be accounted for.
    \code{calcOptions$row} and \code{calcOptions$col} can denote rows and columns to be accounted for in a Latin square design.
  }
}

\details{Returns the F statistic calculated in an analysis of variance of a linear model with no interaction terms.}

\value{An analysis of variance F statistic.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\examples{
## 1 treatment factor with 3 levels
## Assignments and outcomes
w = c(1,1,1,1,2,2,2,2,3,3,3,3)
y = c(4,6,5,7,4,7,11,9,8,12,9,9)
anovaF(y, w)

## 1 treatment factor with 3 levels, with block
## Assignments, blocks, and outcomes
w = c(1,1,1,1,2,2,2,2,3,3,3,3)
x = c(1,1,2,2,1,1,2,2,1,1,2,2)
y = c(4,6,5,7,4,7,11,9,8,12,9,9)
anovaF(y, w, calcOptions = list(block = x))
}


\keyword{methods}
