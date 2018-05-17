\name{isPrimroot}
\alias{isPrimroot}
\title{
  Primitive Root Test
}
\description{
  Determine whether \code{g} generates the multiplicative group
  modulo p.
}
\usage{
  isPrimroot(g, p)
}
\arguments{
  \item{g}{integer greater 2 (and smaller than p).}
  \item{p}{prime number.}
}
\details{
  Test is done by determining the order of \code{g} modulo \code{p}.
}
\value{
  Returns TRUE or FALSE.
}
\examples{
isPrimroot(2, 7)
isPrimroot(2, 71)
isPrimroot(7, 71)
}
