\name{primeFactors}
\alias{primeFactors}
\alias{radical}
\title{Prime Factors}
\description{
  \code{primeFactors} computes a vector containing the prime factors of 
  \code{n}. \code{radical} returns the product of those unique prime
  factors.
}
\usage{
  primeFactors(n)
  radical(n)
}
\arguments{
  \item{n}{nonnegative integer}
}
\details{
  Computes the prime factors of \code{n} in ascending order,
  each one as often as its multiplicity requires, such that 
  \code{n == prod(primeFactors(n))}.

  ## radical() is used in the abc-conjecture:

  # abc-triple: 1 <= a < b, a, b coprime, c = a + b

  # for every e > 0 there are only finitely many abc-triples with

  # c > radical(a*b*c)^(1+e)

}
\value{
  Vector containing the prime factors of \code{n}, resp.
  the product of unique prime factors.
}
\seealso{
  \code{\link{divisors}}, \code{gmp::factorize}
}
\examples{
  primeFactors(1002001)         # 7  7  11  11  13  13
  primeFactors(65537)           # is prime
  # Euler's calculation
  primeFactors(2^32 + 1)        # 641  6700417

  radical(1002001)              # 1001

\dontrun{
  for (i in 1:99) {
    for (j in (i+1):100) {
      if (coprime(i, j)) {
        k = i + j
        r = radical(i*j*k)
        q = log(k) / log(r)  # 'quality' of the triple
        if (q > 1)
          cat(q, ":\t", i, ",", j, ",", k, "\n")
        }
      }
    }}
}