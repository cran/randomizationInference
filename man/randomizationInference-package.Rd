\name{randomizationInference}
\alias{randomizationInference-package}
\alias{randomizationInference}
\docType{package}
\title{Flexible Randomization-Based Inference}
\description{Randomization-based p-values and null intervals for a wide variety of experimental scenarios, with corresponding visualizations.}
\details{
  \tabular{ll}{
    Package: \tab randomizationInference\cr
    Type: \tab Package\cr
    Version: \tab 1.0.1\cr
    Date: \tab 2013-12-02\cr
    License: \tab GPL-2\cr
    Main functions: \tab \code{\link{randTest}}, \code{\link{randPlot}}, \code{\link{randInterval}}\cr
  }

  The \code{randomizationInference} package conducts randomization-based inference for a wide variety
  of experimental scenarios. The package leverages a potential outcomes framework to output
  randomization-based p-values and null intervals for any estimand of interest, according to the specified
  null and alternative hypotheses.  The package also creates visualizations of randomization
  distributions and can test multiple test statistics simultaneously.
}

\author{
  Joseph J. Lee and Tirthankar Dasgupta

  Maintainer: Joseph J. Lee <lee26@fas.harvard.edu>
}
\references{
Wu, C. F. J. and Hamada, M. (2009) Experiments, Planning, Analysis and Optimization (2nd ed), Wiley.

Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics.
}

\examples{
## Completely randomized design example
## with one treatment factor at two levels
w = c(rep(0,5), rep(1,5))
y = rnorm(10, mean = 0, sd = 1)
## Two-sided test
twoSidedTest = randTest(y, w, nperm = 50, calcTestStat = diffMeans)
randInterval(twoSidedTest)
randPlot(twoSidedTest)
## One-sided test
oneSidedTest = randTest(y, w, nperm = 50, calcTestStat = diffMeans,
    alternative = "greater")
## Two=sided test with non-zero null hypothesis
nonZeroTest = randTest(y, w, nperm = 50, calcTestStat = diffMeans,
    calcPO = constEffect, poOptions = list(tau = 2), null = 2)

## Randomized block design example
## with one treatment factor at three levels
x = rep(1:3,4)
w_block = rep(1:4,3)
y_block = rnorm(12, mean = x, sd = 1)
blockTest = randTest(y_block, w_block, nperm = 50,
    calcTestStat = anovaF, calcOptions = list(block = x),
    randOptions = list(type = "block", block = x))
randInterval(blockTest)
randPlot(blockTest)

## 4x4 Latin square example (from the Wu/Hamada reference)
row = rep(1:4,4)
col = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
w_latin = c("C","D","B","A","A","B","D","C",
    "D","C","A","B","B","A","C","D")
y_latin = c(235,236,218,268,251,241,227,229,
    234,273,274,226,195,270,230,225)
latinTest = randTest(y_latin, w_latin, nperm = 50,
    calcTestStat = anovaF,
    calcOptions = list(row = row, col = col),
    randOptions = list(type = "Latin", row = row, col = col))
randInterval(latinTest)
randPlot(latinTest)

## 2^3 factorial design example
## three treatment factors (OT, CP, and ST) at two levels each
OT = c(-1,-1,-1,-1,1,1,1,1)
CP = c(-1,-1,1,1,-1,-1,1,1)
ST = rep(c(-1,1), 4)
w_fac = cbind(OT, CP, ST)
y_fac = c(67,79,61,75,59,90,52,87)
## Testing the main effect of factor "OT"
facTest1 = randTest(y_fac, w_fac, nperm = 50, calcTestStat = diffMeans,
    calcOptions = list(factor = 1, pair = c(-1,1)))
## Testing all three main effects simultaneously
facTest2 = randTest(y_fac, w_fac, nperm = 50,
    calcTestStat = diffMeansVector, calcOptions = list(factors = 1:3,
        pairs = matrix(rep(c(-1,1),3),ncol=2,byrow=TRUE)))
## Testing all contrasts simultaneously
w_facNew = cbind(OT, CP, ST, OT*CP, OT*ST, CP*ST, OT*CP*ST)
facTest3 = randTest(y_fac, w_facNew, nperm = 50,
    calcTestStat = diffMeansVector, calcOptions = list(factors = 1:7,
        pairs = matrix(rep(c(-1,1),7),ncol=2,byrow=TRUE)))
randInterval(facTest3)
randPlot(facTest3, plotDim = c(2, 4))

## Reading comprehension pre- and post-test example
data(reading)
## Ignoring blocks
readingTest1=randTest(y = reading$Diff1, w = reading$Group, nperm = 50,
    calcTestStat = anovaF)
## Testing within-block pairwise effects
readingTest2=randTest(y = reading$Diff1, w = reading$Group, nperm = 50,
    calcTestStat = withinBlockEffects,
    calcOptions = list(block = reading$Block,
        pairs = rbind(c("Basal", "DRTA"), c("Basal", "Strat"),
            c("DRTA", "Strat"), c("Basal", "DRTA"),
            c("Basal", "Strat"), c("DRTA", "Strat")),
        blockindex = c(rep(1,3), rep(2,3))),
    randOptions = list(type = "block", block = reading$Block))
randInterval(readingTest2)
randPlot(readingTest2, plotDim = c(2, 3))
}

\keyword{package}