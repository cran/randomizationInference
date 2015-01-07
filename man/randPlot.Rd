\name{randPlot}
\alias{randPlot}
\title{Randomization-Based Plot}
\description{Plots observed test statistics against their randomization distributions and against the randomization-based intervals for their corresponding estimands.}
\usage{randPlot(results, coverage = 0.95, breaks = 10,
    plotDim = c(length(results$obs_stat), 1))}
\arguments{
  \item{results}{
    a resultant object of the \code{randTest} function. 
  }
  \item{coverage}{
    a number specifying the desired coverage level (defaults to \code{0.95}).
  }
  \item{breaks}{
    a value specifying the histogram break pattern (defaults to 10).
  }
  \item{plotDim}{
    a numeric vector (of length 2) specifying the desired plot dimensions (if necessary, defaults to \code{c(length(results$obs_stat), 1)}).
  }
}

\details{
One plot will be displayed for each test statistic specified by \code{results}.

Observed test statistic values are demarcated by solid red lines.
Comparison values under the null hypothesis are demarcated by solid black lines.
Randomization-based interval bounds for the specified coverage level are demarcated by dotted blue lines.

Setting \code{plotDim = c(a, b)} is equivalent to specifying \code{par(mfrow=c(a, b))}.  Plot dimensions are automatically reset to \code{c(1, 1)} afterward.
If multiple test statistics are tested simultaneously, \code{plotDim} may need to be specified suitably for the plots to be displayed.

If multiple tests are conducted simultaneously, users should be wary of multiple comparisons and make adjustments accordingly (e.g., Bonferroni corrections).
}

\value{The plot(s) described in \emph{details} will be displayed.}

\seealso{\code{\link{randTest}}, \code{\link{randInterval}}}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\examples{
## Completely randomized design example
## with one treatment factor at two levels
w = c(rep(0,5), rep(1,5))
y = rnorm(10, mean = 0, sd = 1)
## Two-sided test
twoSidedTest = randTest(y, w, nrand = 50, calcTestStat = diffMeans)
randPlot(twoSidedTest)

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
randPlot(readingTest, breaks = 20, plotDim = c(2,3))
}

\keyword{methods}
