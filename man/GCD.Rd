\name{GCD, LCM}
\alias{GCD}
\alias{LCM}
\alias{mGCD}
\title{GCD and LCM Integer Functions}
\description{
  Greatest common divisor and least common multiple
}
\usage{
GCD(n, m)
LCM(n, m)

mGCD(x)
}
\arguments{
  \item{n, m}{integer scalars.}
  \item{x}{a vector of integers.}
}
\details{
  Computation based on the Euclidean algorithm without using the extended
  version.

  \code{mGCD} (the multiple GCD) computes the greatest common divisor for
  all numbers in the integer vector \code{x} together.
}
\value{
  A numeric (integer) value.
}
\note{
  The following relation is always true:

  \code{n * m = GCD(n, m) * LCM(n, m)}
}
\seealso{
\code{\link{extGCD}}, \code{\link{coprime}}
}
\examples{
GCD(12, 10)
GCD(46368, 75025)  # Fibonacci numbers are relatively prime to each other

LCM(12, 10)
LCM(46368, 75025)  # = 46368 * 75025

mGCD(c(2, 3, 5, 7) * 11)
mGCD(c(2*3, 3*5, 5*7))
}
