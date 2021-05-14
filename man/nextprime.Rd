\name{nextPrime}
\alias{nextPrime}
\title{Next Prime}
\description{
  Find the next prime above \code{n}.
}
\usage{
  nextPrime(n)
}
\arguments{
  \item{n}{natural number.}
}
\details{
  \code{nextPrime} finds the next prime number greater than \code{n}, while
  \code{previousPrime} finds the next prime number below \code{n}.
  In general the next prime will occur in the interval \code{[n+1,n+log(n)]}.

  In double precision arithmetic integers are represented exactly only up to
  2^53 - 1, therefore this is the maximal allowed value.
}
\value{
  Integer.
}
\seealso{
  \code{\link{Primes}}, \code{\link{isPrime}}
}
\examples{
p <- nextPrime(1e+6)  # 1000003
isPrime(p)            # TRUE
}
