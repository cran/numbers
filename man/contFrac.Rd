\name{contfrac}
\alias{contfrac}
\title{
  Continued Fractions
}
\description{
  Evaluate a continued fraction or generate one.
}
\usage{
contfrac(x, tol = 1e-12)
}
\arguments{
  \item{x}{a numeric scalar or vector.}
  \item{tol}{tolerance; default \code{1e-12}.}
}
\details{
  If \code{x} is a scalar its continued fraction will be generated up to
  the accuracy prescribed in \code{tol}. If it is of length greater 1, the
  function assumes this to be a continued fraction and computes its value
  and convergents.

  The continued fraction \eqn{[b_0; b_1, \ldots, b_{n-1}]} is assumed to be
  finite and neither periodic nor infinite. For implementation uses the
  representation of continued fractions through 2-by-2 matrices
  (i.e. Wallis' recursion formula from 1644).
}
\value{
  If \code{x} is a scalar, it will return a list with components \code{cf}
  the continued fraction as a vector, \code{rat} the rational approximation,
  and \code{prec} the difference between the value and this approximation.

  If \code{x} is a vector, the continued fraction, then it will return a list
   with components \code{f} the numerical value, \code{p} and \code{q} the
   convergents, and \code{prec} an estimated precision.
}
\references{
  Hardy, G. H., and E. M. Wright (1979). An Introduction to the Theory of
  Numbers. Fifth Edition, Oxford University Press, New York.
}
\note{
  This function is \emph{not} vectorized.
}
\seealso{
\code{\link{cf2num}}, \code{\link{ratFarey}}
}
\examples{
contfrac(pi)
contfrac(c(3, 7, 15, 1))        # rational Approx: 355/113

contfrac(0.555)                 #  0  1  1  4 22
contfrac(c(1, rep(2, 25)))      #  1.414213562373095, sqrt(2)
}
