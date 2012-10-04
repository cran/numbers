\name{previousPrime}
\alias{previousPrime}
\title{Previous Prime}
\description{
  Find the next prime below \code{n}.
}
\usage{
  previousPrime(n)
}
\arguments{
  \item{n}{natural number.}
}
\details{
  \code{previousPrime} finds the next prime number smaller than \code{n}, 
  while \code{nextPrime} finds the next prime number below \code{n}.
  In general the previousn prime will occur in the interval 
  \code{[n-1,n-log(n)]}.

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
p <- previousPrime(1e+6)  # 999983
isPrime(p)                # TRUE
}
