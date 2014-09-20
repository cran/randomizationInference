\name{randInterval}
\alias{randInterval}
\title{Randomization-Based Null Interval}
\description{Calculates randomization-based intervals under the null hypothesis for the specified test statistics and coverage levels.}
\usage{randInterval(results, coverage = 0.95)}
\arguments{
  \item{results}{
    a resultant object of the \code{randTest} function. 
  }
  \item{coverage}{
    a number specifying the desired coverage level (defaults to \code{0.95}).
  }
}

\details{If multiple tests are conducted simultaneously, users should be wary of multiple comparisons and make adjustments accordingly (e.g., Bonferroni corrections).}

\value{A randomization-based interval (or multiple intervals) for the test statistic(s) under the null hypothesis with the specified coverage level.}

\seealso{\code{\link{randTest}}, \code{\link{randPlot}}}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\examples{
## Completely randomized design example
## with one treatment factor at two levels
w = c(rep(0,5), rep(1,5))
y = rnorm(10, mean = 0, sd = 1)
## Two-sided test
twoSidedTest = randTest(y, w, nrand = 50, calcTestStat = diffMeans)
randInterval(twoSidedTest)

## Reading comprehension pre- and post-test example
data(reading)
## Testing within-block pairwise effects
readingTest=randTest(y = reading$Diff1, w = reading$Group, nrand = 50,
    calcTestStat = withinBlockEffects,
    calcOptions = list(block = reading$Block,
    pairs = rbind(c("Basal", "DRTA"), c("Basal", "Strat"),
        c("DRTA", "Strat"), c("Basal", "DRTA"),
        c("Basal", "Strat"), c("DRTA", "Strat")),
    blockindex = c(rep(1,3), rep(2,3))),
    randOptions = list(type = "block", block = reading$Block))
randInterval(readingTest)
}

\keyword{methods}
