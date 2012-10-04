\name{factorize}
\alias{factorize}
\title{Prime Factors}
\description{
  Returns a vector containing the prime factors of \code{n}.
}
\usage{
  factorize(n)
}
\arguments{
  \item{n}{nonnegative integer}
}
\details{
  Computes the prime factors of \code{n} in ascending order,
  each one as often as its multiplicity requires, such that 
  \code{n == prod(factorize(n))}.
}
\value{
  Vector containing the prime factors of \code{n}.
}
\seealso{
  \code{\link{isPrime}, \link{Primes}}
}
\examples{
  factorize(1002001)       # 7  7  11  11  13  13
  factorize(65537)         # is prime
  # Euler's calculation
  factorize(2^32 + 1)      # 641  6700417
}
