\name{mersenne}
\alias{mersenne}
\title{
  Mersenne Numbers
}
\description{
  Determines whether \eqn{p} is a Mersenne number, that is such that
  \eqn{2^p - 1} is prime.
}
\usage{
mersenne(p)
}
\arguments{
  \item{p}{prime number, not very large.}
}
\details{
  Applies the Lucas-Lehmer test on \code{p}. Because intermediate numbers will
  soon get very large, uses `gmp' from the beginning.
}
\value{
  Returns TRUE or FALSE, indicating whether \code{p} is a Mersenne number or not.
}
\references{
  http://mathworld.wolfram.com/Lucas-LehmerTest.html
}
\author{
  HwB <hwborchers@googlemail.com>
}
\examples{
P <- Primes(32)
M <- c()
for (p in P)
    if (mersenne(p)) M <- c(M, p)
# Next Mersenne numpers with primes are 521 and 607 (below 1200)
M                       # 2  3   5    7    13     17   19   31   61  89  107  127
gmp::as.bigz(2)^M - 1   # 3  7  31  127  8191 131071  ...
}
