\name{withinBlockEffects}
\alias{withinBlockEffects}
\title{Pairwise Differences of Mean Outcomes Within Blocks}
\description{Calculates the differences of mean outcomes for multiple specified treatment factors and specified pairs of comparison levels, within the specified blocks.}
\usage{withinBlockEffects(y, w, calcOptions)}
\arguments{
  \item{y}{
    a vector or matrix of outcomes.
  }
  \item{w}{
    a vector or matrix of assignments.
  }
  \item{calcOptions}{
    a list of options for calculating the difference of mean outcomes within blocks.
    \code{calcOptions$block} is a vector denoting the block designations.
    The numeric vector \code{calcOptions$factors} denotes the treatment factors of interest.
    \code{calcOptions$pairs} is a matrix of pairs (specified by row) of levels for comparison.
    \code{calcOptions$blockindex} is a vector of indices denoting the blocks within which the pairs should be compared.
  }
}

\details{
\code{calcOptions$block} should have the same length as \code{y} and \code{w}.

If unspecified, \code{calcOptions$pairs} defaults to \code{c(0,1)}.

If \code{calcOptions$factors} is specified, its length must equal the number of rows specified in \code{calcOptions$pairs}.
If unspecified, \code{calcOptions$factors} defaults to an appropriately-sized vector of \code{1}'s.

\code{calcOptions$blockindex} should have the same length as \code{calcOptions$factors}.
}

\value{A vector of differences of mean outcomes within blocks.}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\references{Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics. Original source: study conducted by Jim Baumann and Leah Jones of the Purdue University Education Department.}

\seealso{\code{\link{diffMeansVector}}}

\examples{
# Reading comprehension pre- and post-test example
data(reading)
withinBlockEffects(
  y = reading$Diff1,
  w = reading$Group,
  calcOptions = list(
    block = reading$Block,
    pairs = rbind(
      c("Basal", "DRTA"),
      c("Basal", "Strat"),
      c("DRTA", "Strat"),
      c("Basal", "DRTA"),
      c("Basal", "Strat"),
      c("DRTA", "Strat")
    ),
    blockindex = c(rep(1, 3), rep(2, 3))
  )
)
}

\keyword{methods}
