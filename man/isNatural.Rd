\name{isNatural}
\alias{isNatural}
\title{
  Natural Number
}
\description{
  Natural number type.
}
\usage{
  isNatural(n)
}
\arguments{
  \item{n}{any numeric number.}
}
\details{
  Returns \code{TRUE} for natural (or: whole) numbers between 1 and 2^53-1.
}
\value{
  Boolean
}
\author{
  HwB  email: <hwborchers@googlemail.com>
}
\examples{
IsNatural <- Vectorize(isNatural)
IsNatural(c(-1, 0, 1, 5.1, 10, 2^53-1, 2^53, Inf, NA))
}
