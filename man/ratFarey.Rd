\name{ratFarey}
\alias{ratFarey}
\title{
    Farey Approximation
}
\description{
    Rational approximation of real numbers through Farey fractions.
}
\usage{
ratFarey(x, n, upper = TRUE)
}
\arguments{
  \item{x}{real number.}
  \item{n}{integer, highest allowed denominator in a rational approximation.}
  \item{upper}{logical; shall the Farey fraction be grater than \code{x}.}
}
\details{
    Rational approximation of real numbers through Farey fractions,
    i.e. find for \code{x} the nearest fraction in the Farey series of
    rational numbers with denominator not larger than \code{n}.
}
\value{
    Returns a vector with two natural numbers, nominator and denominator.
}
\references{
    Hardy, G. H., and E. M. Wright (1979). An Introduction to the Theory of
    Numbers. Fifth Edition, Oxford University Press, New York.
}
\seealso{
    \code{contFrac}
}
\examples{
    ratFarey(pi, 100)                          # 22/7    0.0013
    ratFarey(pi, 100, upper = FALSE)           # 311/99  0.0002
    ratFarey(-pi, 100)                         # -22/7
    ratFarey(pi - 3, 70)                       # pi ~= 3 + (3/8)^2
    ratFarey(pi, 1000)                         # 355/113
    ratFarey(pi, 10000, upper = FALSE)         # id.
    ratFarey(pi, 1e5, upper = FALSE)           # 312689/99532 - pi ~= 3e-11

    ratFarey(4/5, 5)                           # 4/5
    ratFarey(4/5, 4)                           # 1/1
    ratFarey(4/5, 4, upper = FALSE)            # 3/4
}
