\name{mod}
\alias{mod}
\title{Modulo Operator}
\description{
  Modulo operator.
}
\usage{
mod(n, m)
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{integer vector (positive, zero, or negative)}
}
\details{
  \code{mod(n, m)} is the modulo operator and returns \eqn{n\,mod\,m}.
  \code{mod(n, 0)} is \code{n}, and the result always has the same sign
  as \code{m}.
}
\value{
  a numeric (integer) value or vector/matrix
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
}
