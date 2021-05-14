\name{bell}
\alias{bell}
\title{
  Bell Numbers
}
\description{
  Generate Bell numbers.
}
\usage{
bell(n)
}
\arguments{
  \item{n}{integer, asking for the n-th Bell number.}
}
\details{
  Bell numbers, commonly denoted as \eqn{B_n}, are defined as the number of
  partitions of a set of \code{n} elements. They can easily be calculated
  recursively.

  Bell numbers also appear as moments of probability distributions, for example
  \code{B_n} is the n-th momentum of the Poisson distribution with mean 1.
}
\value{
  A single integer, as long as \code{n<=22}.
}
\examples{
sapply(0:10, bell)
#      1      1      2      5     15     52    203    877   4140  21147 115975
}
