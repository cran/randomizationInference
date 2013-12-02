\name{reading}

\docType{data}
\alias{reading}

\title{
  Reading Data
}

\description{
  Pre- and post-treatment reading comprehension test scores for 66 students randomly assigned to one of three methods for teaching reading comprehension.
}

\usage{data(reading)}

\format{
  A data set of 66 students with 6 variables:
  \describe{
    \item{\code{Subject}}{student ID numbers}
    \item{\code{Block}}{block designations}
    \item{\code{Group}}{treatment group assignments}
    \item{\code{Pre1}}{pre-treatment reading comprehension test scores}
    \item{\code{Post1}}{post-treatment reading comprehension test scores}
    \item{\code{Diff1}}{changes in test score (improvement if positive)}
  }
}

\source{
Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics. Original source: study conducted by Jim Baumann and Leah Jones of the Purdue University Education Department.
}

\examples{
  data(reading)
}

\keyword{datasets}
