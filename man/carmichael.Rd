\name{Carmichael numbers}
\alias{carmichael}
\title{
  Carmichael Numbers
}
\description{
  Checks whether a number is a Carmichael number.
}
\usage{
  carmichael(n)
}
\arguments{
  \item{n}{natural number}
}
\details{
  A natural number \code{n} is a Carmichael number if it is a Fermat
  pseudoprime for every \code{a}, that is \code{a^(n-1) = 1 mod n}, but
  is composite, not prime.

  Here the Korselt criterion is used to tell whether a number \code{n} is
  a Carmichael number.
}
\value{
  Returns TRUE or FALSE
}
\note{
  There are infinitely many Carmichael numbers, specifically there should
  be at least \code{n^(2/7)} Carmichael numbers up to n (for n large enough).
}
\references{
  R. Crandall and C. Pomerance. Prime Numbers - A Computational Perspective.
  Second Edition, Springer Science+Business Media, New York 2005.
}
\seealso{
  \code{\link{primeFactors}}
}
\examples{
carmichael(561)  # TRUE

\dontrun{
for (n in 1:100000)
    if (carmichael(n)) cat(n, '\n')
##    561     2821    15841    52633 
##   1105     6601    29341    62745 
##   1729     8911    41041    63973 
##   2465    10585    46657    75361 
}
}
