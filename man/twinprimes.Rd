\name{twinPrimes}
\alias{twinPrimes}
\title{Twin Primes}
\description{
  Generate a list of twin primes between \code{n1} and \code{n2}.
}
\usage{
  twinPrimes(n1, n2)
}
\arguments{
  \item{n1, n2}{natural numbers with \code{n1 <= n2}.}
}
\details{
  \code{twinPrimes} uses \code{Primes} and uses \code{diff} to find all
  twin primes in the given interval.

  In double precision arithmetic integers are represented exactly only up to
  2^53 - 1, therefore this is the maximal allowed value.
}
\value{
  Returnes a \code{nx2}-matrix, where \code{n}is the number of twin primes
  found, and each twin tuple fills one row.
}
\seealso{
  \code{\link{Primes}}
}
\examples{
twinPrimes(1e6+1, 1e6+1001)
}
