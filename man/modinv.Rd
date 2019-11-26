\name{modinv, modsqrt}
\alias{modinv}
\alias{modsqrt}
\title{Modular Inverse and Square Root}
\description{
  Computes the modular inverse of \code{n} modulo \code{m}.
}
\usage{
modinv(n, m)

modsqrt(a, p)
}
\arguments{
  \item{n, m}{integer scalars.}
  \item{a, p}{integer modulo p, p a prime.}
}
\details{
  The modular inverse of \code{n} modulo \code{m} is the unique natural
  number \code{0 < n0 < m} such that \code{n * n0 = 1 mod m}. It is a
  simple application of the extended GCD algorithm.

  The modular square root of \code{a} modulo a prime \code{p} is a number
  \code{x} such that \code{x^2 = a mod p}. If \code{x} is a solution, then
  \code{p-x} is also a solution module \code{p}. The function will always
  return the smaller value.

  \code{modsqrt} implements the Tonelli-Shanks algorithm which also works
  for square roots modulo prime powers. The general case is NP-hard.
}
\value{
  A natural number smaller \code{m}, if \code{n} and \code{m} are coprime,
  else \code{NA}. \code{modsqrt} will return 0 if there is no solution.
}
\seealso{
\code{\link{extGCD}}
}
\examples{
modinv(5, 1001)  #=> 801, as 5*801 = 4005 = 1 mod 1001

Modinv <- Vectorize(modinv, "n")
((1:10)*Modinv(1:10, 11)) \%\% 11     #=> 1 1 1 1 1 1 1 1 1 1

modsqrt( 8, 23)  # 10 because 10^2 = 100 = 8 mod 23
modsqrt(10, 17)  #  0 because 10 is not a quadratic residue mod 17
}

