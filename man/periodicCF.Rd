\name{periodicCF}
\alias{periodicCF}
\title{
  Periodic continued fraction
}
\description{
  Generates a periodic continued fraction.
}
\usage{
periodicCF(d)
}
\arguments{
  \item{d}{positive integer that is not a square number}
}
\details{
  The function computes the periodic continued fraction of the square root of
  an integer that itself shall not be a square (because otherwise the integer
  square root will be returned). Note that the continued fraction of an
  irrational quadratic number is always a periodic continued fraction.

  The first term is the biggest integer below \code{sqrt(d)} and the rest is
  the period of the continued fraction. The period is always exact, there is
  no floating point inaccuracy involved (though integer overflow may happen
  for very long fractions).

  The underlying algorithm is sometimes called "The Fundamental Algorithm
  for Quadratic Numbers". The function will be utilized especially when
  solving Pell's equation.
}
\value{
  Returns a list with components
  \item{cf }{the continued fraction with integer part and first period.}
  \item{plen }{the length of the period.}
}
\references{
  Mak Trifkovic. Algebraic Theory of Quadratic Numbers. Springer Verlag,
  Universitext, New York 2013.
}
\author{
  Hans Werner Borchers
}
\note{
  Integer overflow may happen for very long continued fractions.
}
\seealso{
  \code{\link{solvePellsEq}}
}
\examples{
  periodicCF(2)    # sqrt(2) = [1; 2,2,2,...] = [1; (2)]

  periodicCF(1003)
  ## $cf
  ## [1] 31  1  2 31  2  1 62
  ## $plen
  ## [1] 6
}
