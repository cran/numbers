\name{mod}
\alias{mod}
\alias{modq}
\title{Modulo Operator}
\description{
  Modulo operator.
}
\usage{
mod(n, m)

modq(a, b, k)
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{integer vector (positive, zero, or negative)}
  \item{a,b}{whole numbers (scalars)}
  \item{k}{integer greater than 1}
}
\details{
  \code{mod(n, m)} is the modulo operator and returns \code{n mod m}.
  \code{mod(n, 0)} is \code{n}, and the result always has the same sign
  as \code{m}.

  \code{modq(a, b, k)} is the modulo operator for rational numbers and
  returns \code{a/b mod k}. \code{b} and \code{k} must be coprime,
  otherwise \code{NA} is returned. 
}
\value{
  a numeric (integer) value or vector/matrix, resp. an integer number
}
\note{
  The following relation is fulfilled (for \code{m != 0}):

  \code{mod(n, m) = n - m * floor(n/m)}
}
\seealso{
\code{\link{rem}}, \code{\link{div}}
}
\examples{
mod(c(-5:5), 5)
mod(c(-5:5), -5)
mod(0, 1)         #=> 0
mod(1, 0)         #=> 1

modq(5, 66, 5)    # 0  (Bernoulli 10)
modq(5, 66, 7)    # 4
modq(5, 66, 13)   # 5
modq(5, 66, 25)   # 5
modq(5, 66, 35)   # 25
modq(-1,  30, 7)  # 3  (Bernoulli 8)
modq( 1, -30, 7)  # 3

# Warning messages:
# modq(5, 66, 77)       : Arguments 'b' and 'm' must be coprime.
# Error messages
# modq(5, 66, 1)        : Argument 'm' mustbe a natural number > 1.
# modq(5, 66, 1.5)      : All arguments of 'modq' must be integers.
# modq(5, 66, c(5, 7))  : Function 'modq' is *not* vectorized.
}
