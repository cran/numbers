\name{isIntpower}
\alias{isIntpower}
\alias{isSquare}
\alias{isSquarefree}
\title{
  Powers of Integers
}
\description{
  Determine whether \code{p} is the power of an integer.
}
\usage{
  isIntpower(p)

  isSquare(p)
  isSquarefree(p)
}
\arguments{
  \item{p}{any integer number.}
}
\details{
  \code{isIntpower(p)} determines whether \code{p} is the power of an integer
  and returns a tupel \code{(n, m)} such that \code{p=n^m} where \code{m} is
  as small as possible. E.g., if \code{p} is prime it returns \code{c(p,1)}.

  \code{isSquare(p)} determines whether \code{p} is the square of an integer;
  and \code{isSquarefree(p)} determines if \code{p} contains a square number
  as a divisor.
}
\value{
  A 2-vector of integers.
}
\examples{
isIntpower(1)    #  1  1
isIntpower(15)   # 15  1
isIntpower(17)   # 17  1
isIntpower(64)   #  8  2
isIntpower(36)   #  6  2
isIntpower(100)  # 10  2
\dontrun{
  for (p in 5^7:7^5) {
      pp <- isIntpower(p)
      if (pp[2] != 1) cat(p, ":\t", pp, "\n")
  }}
}
