\name{Farey Numbers}
\alias{ratFarey}
\alias{farey_seq}
\title{
    Farey Approximation and Series
}
\description{
    Rational approximation of real numbers through Farey fractions.
}
\usage{
ratFarey(x, n, upper = TRUE)

farey_seq(n)
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

    \code{farey_seq(n)} generates the full Farey sequence of rational
    numbers with denominators not larger than \code{n}. Returns the
    fractions as 'big rational' class in 'gmp'.
}
\value{
    Returns a vector with two natural numbers, nominator and denominator.
}
\note{
    \code{farey_seq} is very slow even for \code{n > 40}, due to the
    handling of rational numbers as 'big rationals'.
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

    farey_seq(5)
    # Big Rational ('bigq') object of length 11:
    #  [1] 0   1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1  
}
