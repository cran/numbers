\name{divisors}
\alias{divisors}
\title{
  List of Divisors
}
\description{
  Generates a list of divisors of an integer number.
}
\usage{
divisors(n)
}
\arguments{
  \item{n}{integer whose divisors will be generated.}
}
\details{
  The list of all divisors of an integer \code{n} will be calculated
  and returned in ascending order, including 1 and the number itself.
  For \code{n>=1000} the list of prime factors of \code{n} will be
  used, for smaller \code{n} a total search is applied.
}
\value{
  Returns a vector integers.
}
\seealso{
  \code{\link{primeFactors}}, \code{\link{Sigma}}, \code{\link{tau}}
}
\examples{
divisors(1)          # 1
divisors(2)          # 1 2
divisors(2^5)        # 1  2  4  8 16 32
divisors(1000)       # 1  2  4  5  8 10 ... 100 125 200 250 500 1000
divisors(1001)       # 1  7 11 13 77 91 143 1001
}
