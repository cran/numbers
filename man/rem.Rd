\name{rem}
\alias{rem}
\title{Integer Remainder}
\description{
  Integer remainder function.
}
\usage{
rem(n, m)
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{must be a scalar integer (positive, zero, or negative)}
}
\details{
  \code{rem(n, m)} is the same modulo operator and returns \eqn{n\,mod\,m}.
  \code{mod(n, 0)} is \code{NaN}, and the result always has the same sign
  as \code{n} (for \code{n != m} and \code{m != 0}).
}
\value{
  a numeric (integer) value or vector/matrix
}
\seealso{
\code{\link{mod}}, \code{\link{div}}
}
\examples{
rem(c(-5:5), 5)
rem(c(-5:5), -5)
rem(0, 1)         #=> 0
rem(1, 1)         #=> 0  (always for n == m)
rem(1, 0)         #   NA  (should be NaN)
rem(0, 0)         #=> NaN
}
