\name{ordpn}
\alias{ordpn}
\title{
  Order in Faculty
}
\description{
  Calculates the order of a prime number \code{p} in \code{n!}, i.e. the 
  highest exponent \code{e} such that \code{p^e|n!}.
}
\usage{
ordpn(p, n)
}
\arguments{
  \item{p}{prime number.}
  \item{n}{natural number.}
}
\details{
  Applies the well-known formula adding terms \code{floor(n/p^k)}.
}
\value{
  Returns the exponent \code{e}.
}
\examples{
  ordpn(2, 100)         #=> 97
  ordpn(7, 100)         #=> 16
  ordpn(101, 100)       #=>  0
  ordpn(997, 1000)      #=>  1
}
