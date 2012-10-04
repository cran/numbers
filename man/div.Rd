\name{div}
\alias{div}
\title{Integer Division}
\description{
  Integer division.
}
\usage{
div(n, m)
}
\arguments{
  \item{n}{numeric vector (preferably of integers)}
  \item{m}{integer vector (positive, zero, or negative)}
}
\details{
  \code{div(n, m)} is integer division, that is discards the fractional part, 
  with the same effect as \code{n \%/\% m}.
  It can be defined as \code{floor(n/m)}.
}
\value{
  A numeric (integer) value or vector/matrix.
}
\seealso{
\code{\link{mod}}, \code{\link{rem}}
}
\examples{
div(c(-5:5), 5)
div(c(-5:5), -5)
div(c(1, -1), 0)  #=> Inf -Inf
div(0,c(0, 1))    #=> NaN  0      
}
