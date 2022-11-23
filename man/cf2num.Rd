\name{cf2num}
\alias{cf2num}
\title{
  Generalized Continous Fractions
}
\description{
  Evaluate a generalized continuous fraction as an alternating sum.
}
\usage{
cf2num(a, b = 1, a0 = 0, finite = FALSE)
}
\arguments{
  \item{a}{numeric vector of length greater than 2.}
  \item{b}{numeric vector of length 1 or the same length as a.}
  \item{a0}{absolute term, integer part of the continuous fraction.}
  \item{finite}{logical; shall Algorithm 1 be applied.}
}
\details{
  Calculates the numerical value of (simple or generalized) continued
  fractions of the form
  \deqn{ a_0 + \frac{b1}{a1+} \frac{b2}{a2+} \frac{b3}{a3+...} }
  by converting it into an alternating sum and then applying the
  accelleration Algorithm 1 of Cohen et al. (2000).

  The argument \eqn{b} is by default set to \eqn{b = (1, 1, ...)},
  that is the continued fraction is treated in its simple form.

  With \code{finite=TRUE} the accelleration is turned off.
}
\value{
  Returns a numerical value, an approximation of the continued fraction.
}
\references{
  H. Cohen, F. R. Villegas, and Don Zagier (2000).
  Experimental Mathematics, Vol. 9, No. 1, pp. 3-12.
  <www.emis.de/journals/EM>
}
\note{
  This function is \emph{not} vectorized.
}
\seealso{
  \code{\link{contfrac}}
}
\examples{
##  Examples from Wolfram Mathworld
print(cf2num(1:25), digits=16)  # 0.6977746579640077, eps()

a = 2*(1:25) + 1; b = 2*(1:25); a0 = 1  # 1/(sqrt(exp(1))-1)
cf2num(a, b, a0)                        # 1.541494082536798

a <- b <- 1:25                          # 1/(exp(1)-1)
cf2num(a, b)                            # 0.5819767068693286

a <- rep(1, 100); b <- 1:100; a0 <- 1   # 1.5251352761609812
cf2num(a, b, a0, finite = FALSE)        # 1.525135276161128
cf2num(a, b, a0, finite = TRUE)         # 1.525135259240266
}
