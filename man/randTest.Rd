\name{randTest}
\alias{randTest}
\title{Randomization-Based Hypothesis Testing}
\description{Conducts randomization-based hypothesis tests according to the specified test statistic(s), assignment mechanism, and null and alternative hypotheses.}
\usage{randTest(y, w, nrand, calcTestStat, calcOptions = NULL,
    calcPO = zeroEffect, poOptions = NULL,
    randOptions=list(type = c("complete", "block", "Latin", "user.defined"),
        block = NULL, row = NULL, col = NULL), userRand = NULL, userOptions = NULL,
    null = NULL, alternative = c("two.sided", "greater", "less"))}
\arguments{
  \item{y}{
    a vector or matrix of outcomes.
  }
  \item{w}{
    a vector (or matrix, when multiple treatment factors are present) of assignments.
  }
  \item{nrand}{
    a number specifying the desired number of random hypothetical assignments.
  }
  \item{calcTestStat}{
    a function to calculate the specified test statistic(s).
  }
  \item{calcOptions}{
    a list of options for calculating the test statistic(s) (if necessary).
  }
  \item{calcPO}{
    a function to calculate potential outcomes (defaults to \code{\link{zeroEffect}}).
  }
  \item{poOptions}{
    a list of options for calculating potential outcomes (if necessary).
  }
  \item{randOptions}{
    a list of options governing the random assignment mechanism.
  }
  \item{userRand}{
    a function to generate random assignments (if necessary).
  }
  \item{userOptions}{
    a list of options governing the user-defined random assignment mechanism (if necessary).
  }
  \item{null}{
    a number or numeric vector specifying the comparison value(s) under the null hypothesis.
  }
  \item{alternative}{
    a character string specifying the alternative hypothesis, must be one of the following: \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
  }
}

\details{
  The inputs to \code{calcTestStat} are \code{y}, \code{w}, and \code{calcOptions} (if necessary).
  The output of \code{calcTestStat} is a number or numeric vector representing the test statistic value(s).
  Several common test statistics (\code{\link{diffMeans}}, \code{\link{anovaF}}, \code{\link{diffMeansVector}}) are built-in for convenience.
  \code{calcTestStat} can also be a custom function, as long as the inputs and output are as described above.
  If multiple test statistics are tested simultaneously, users should be wary of multiple comparisons and make appropriate adjustments (e.g., Bonferroni corrections).

  The inputs to \code{calcPO} are \code{y}, \code{w}, \code{w_new} (a vector or matrix of modified assignments), and \code{poOptions} (if necessary).
  The output of \code{calcPO} is a vector of potential outcomes under the modified assignments.
  Two common potential outcome calculations (\code{\link{zeroEffect}} (default) and \code{\link{constEffect}}) are built-in for convenience.
  \code{calcPO} can also be a custom function, as long as the inputs and output are as described above.

  If unspecified, \code{randOptions$type} defaults to \code{"complete"}.
  If \code{randOptions$type} equals \code{"block"}, then \code{randOptions$block} must be specified.
  If \code{randOptions$type} equals \code{"Latin"}, then \code{randOptions$row} and \code{randOptions$col} must be specified.
  If \code{randOptions$type} equals \code{"user.defined"}, then \code{userRand} must be specified.

  The inputs to \code{userRand} are \code{nrand} and \code{userOptions} (if necessary).

  If \code{null} is specified, its length must equal the length of the output of \code{calcTestStat}.
  If unspecified, \code{null} defaults to \code{0} or an appropriately sized vector of \code{0}'s.

  \code{alternative="greater"} is the alternative that the test statistic is greater than the comparison value.
}

\value{A list containing the following elements:
  \item{perm_stats}{
    a vector or matrix of hypothetical test statistic values.
  }
  \item{obs_stat}{
    the observed test statistic value(s).
  }
  \item{null}{
    a number or numeric vector specifying the comparison value(s) under the null hypothesis.
  }
  \item{alternative}{
    a character string specifying the alternative hypothesis.
  }
  \item{pvalue}{
    the randomization-based p-value(s).
  }
}

\seealso{
\code{\link{diffMeans}}, \code{\link{anovaF}}, \code{\link{diffMeansVector}},
\code{\link{zeroEffect}}, \code{\link{constEffect}},
\code{\link{completeRand}}, \code{\link{blockRand}}, \code{\link{latinRand}},
\code{\link{randInterval}}, \code{\link{randPlot}}
}

\author{Joseph J. Lee and Tirthankar Dasgupta}

\references{
Wu, C. F. J. and Hamada, M. (2009) Experiments, Planning, Analysis and Optimization (2nd ed), Wiley.

Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics. Original source: study conducted by Jim Baumann and Leah Jones of the Purdue University Education Department.
}

\examples{
## Completely randomized design example
## with one treatment factor at two levels
w = c(rep(0,5), rep(1,5))
y = rnorm(10, mean = 0, sd = 1)
## Two-sided test
twoSidedTest = randTest(y, w, nrand = 50, calcTestStat = diffMeans)
## One-sided test
oneSidedTest = randTest(y, w, nrand = 50, calcTestStat = diffMeans,
    alternative = "greater")
## Two=sided test with non-zero null hypothesis
nonZeroTest = randTest(y, w, nrand = 50, calcTestStat = diffMeans,
    calcPO = constEffect, poOptions = list(tau = 2), null = 2)

## Randomized block design example
## with one treatment factor at three levels
x = rep(1:3,4)
w_block = rep(1:4,3)
y_block = rnorm(12, mean = x, sd = 1)
blockTest = randTest(y_block, w_block, nrand = 50,
    calcTestStat = anovaF,
    calcOptions = list(block = x),
    randOptions = list(type = "block", block = x))

## 4x4 Latin square example (from the Wu/Hamada reference)
row = rep(1:4,4)
col = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
w_latin = c("C","D","B","A","A","B","D","C",
    "D","C","A","B","B","A","C","D")
y_latin = c(235,236,218,268,251,241,227,229,
    234,273,274,226,195,270,230,225)
latinTest = randTest(y_latin, w_latin, nrand = 50,
    calcTestStat = anovaF,
    calcOptions = list(row = row, col = col),
    randOptions = list(type = "Latin", row = row, col = col))

## 2^3 factorial design example
## three treatment factors (OT, CP, and ST) at two levels each
OT = c(-1,-1,-1,-1,1,1,1,1)
CP = c(-1,-1,1,1,-1,-1,1,1)
ST = rep(c(-1,1), 4)
w_fac = cbind(OT, CP, ST)
y_fac = c(67,79,61,75,59,90,52,87)
## Testing the main effect of factor "OT"
facTest1 = randTest(y_fac, w_fac, nrand = 50, calcTestStat = diffMeans,
    calcOptions = list(factor = 1, pair = c(-1,1)))
## Testing all three main effects simultaneously
facTest2 = randTest(y_fac, w_fac, nrand = 50,
    calcTestStat = diffMeansVector, calcOptions = list(factors = 1:3,
        pairs = matrix(rep(c(-1,1),3),ncol=2,byrow=TRUE)))
## Testing all contrasts simultaneously
w_facNew = cbind(OT, CP, ST, OT*CP, OT*ST, CP*ST, OT*CP*ST)
facTest3 = randTest(y_fac, w_facNew, nrand = 50,
    calcTestStat = diffMeansVector, calcOptions = list(factors = 1:7,
        pairs = matrix(rep(c(-1,1),7),ncol=2,byrow=TRUE)))

## Reading comprehension pre- and post-test example
data(reading)
## Ignoring blocks
readingTest1=randTest(y = reading$Diff1, w = reading$Group, nrand = 50,
    calcTestStat = anovaF)
## Testing within-block pairwise effects
readingTest2=randTest(y = reading$Diff1, w = reading$Group, nrand = 50,
    calcTestStat = withinBlockEffects,
    calcOptions = list(block = reading$Block,
        pairs = rbind(c("Basal", "DRTA"), c("Basal", "Strat"),
            c("DRTA", "Strat"), c("Basal", "DRTA"),
            c("Basal", "Strat"), c("DRTA", "Strat")),
        blockindex = c(rep(1,3), rep(2,3))),
    randOptions = list(type = "block", block = reading$Block))
}

\keyword{methods}
