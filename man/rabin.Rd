\name{rabin_miller}
\alias{rabin_miller}
\title{Rabin-Miller Test}
\description{
  Probabilistic Rabin-Miller primality test.
}
\usage{
  rabin_miller(n)
}
\arguments{
  \item{n}{natural number.}
}
\details{
  The Rabin-Miller test is an efficient probabilistic primality test based
  on strong pseudoprimes. This implementation uses the first seven prime
  numbers (if necessary) as test cases. It is thus exact for all numbers
  \code{n < 341550071728321}.
}
\value{
  Returns TRUE or FALSE.
}
\references{
  \url{http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html}
}
\seealso{
  \code{\link{isPrime}}
}
\examples{
  rabin_miller(4294967297)  #=> FALSE
  rabin_miller(4294967311)  #=> TRUE

  # Rabin-Miller 10 times faster than nextPrime()
  N <- n <- 2^32 + 1
  system.time(while (!rabin_miller(n)) n <- n + 1)  # 0.003
  system.time(p <- nextPrime(N))                    # 0.029

\dontrun{
  N <- c(2047, 1373653, 25326001, 3215031751, 2152302898747,
          3474749660383, 341550071728321)
  for (n in N) {
      p <- nextPrime(n)
      T <- system.time(r <- rabin_miller(p))
      cat(n, p, r, T[3], "\n")}}
}
