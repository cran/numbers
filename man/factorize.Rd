\name{primeFactors}
\alias{primeFactors}
\title{Prime Factors}
\description{
  Returns a vector containing the prime factors of \code{n}.
}
\usage{
  primeFactors(n)
}
\arguments{
  \item{n}{nonnegative integer}
}
\details{
  Computes the prime factors of \code{n} in ascending order,
  each one as often as its multiplicity requires, such that 
  \code{n == prod(primeFactors(n))}.
}
\value{
  Vector containing the prime factors of \code{n}.
}
\seealso{
  \code{\link{factorize}}
}
\examples{
  primeFactors(1002001)         # 7  7  11  11  13  13
  primeFactors(65537)           # is prime
  # Euler's calculation
  primeFactors(2^32 + 1)        # 641  6700417
}
