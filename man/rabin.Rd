\name{miller_rabin}
\alias{miller_rabin}
\title{Miller-Rabin Test}
\description{
  Probabilistic Miller-Rabin primality test.
}
\usage{
  miller_rabin(n)
}
\arguments{
  \item{n}{natural number.}
}
\details{
  The Miller-Rabin test is an efficient probabilistic primality test based
  on strong pseudoprimes. This implementation uses the first seven prime
  numbers (if necessary) as test cases. It is thus exact for all numbers
  \code{n < 341550071728321}.
}
\value{
  Returns TRUE or FALSE.
}
\references{
  \url{https://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html}
}
\note{
  \code{miller_rabin()} will only work if package \code{gmp} has been loaded
  by the user separately.
}
\seealso{
  \code{\link{isPrime}}
}
\examples{
miller_rabin(2)

\dontrun{
  miller_rabin(4294967297)  #=> FALSE
  miller_rabin(4294967311)  #=> TRUE

  # Rabin-Miller 10 times faster than nextPrime()
  N <- n <- 2^32 + 1
  system.time(while (!miller_rabin(n)) n <- n + 1)  # 0.003
  system.time(p <- nextPrime(N))                    # 0.029

  N <- c(2047, 1373653, 25326001, 3215031751, 2152302898747,
          3474749660383, 341550071728321)
  for (n in N) {
      p <- nextPrime(n)
      T <- system.time(r <- miller_rabin(p))
      cat(n, p, r, T[3], "\n")}}
}
