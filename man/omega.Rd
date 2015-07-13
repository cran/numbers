\name{omega}
\alias{omega}
\alias{Omega}
\title{Number of Prime Factors}
\description{
  Sum of all exponents of prime factors in the prime decomposition.
}
\usage{
omega(n)
Omega(n)
}
\arguments{
  \item{n}{Positive integer.}
}
\details{
  Compute the number of prime factors of \code{n} resp. the sum of their
  exponents in the prime decomposition.

  \code{(-1)^Omega(n)} is the Liouville function.
}
\value{
  Natural number.
}
\note{
  Works well up to \code{10^9}.
}
\seealso{
  \code{\link{Sigma}}
}
\examples{
omega(2*3*5*7*11*13*17*19)  #=> 8
Omega(2 * 3^2 * 5^3 * 7^4)  #=> 10

\dontshow{
stopifnot(identical(sapply(1:16, omega),
                    c(0, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1)))
stopifnot(identical(sapply(1:16, Omega),
                    c(0, 1, 1, 2, 1, 2, 1, 3, 2, 2, 1, 3, 1, 2, 2, 4)))
}
}
