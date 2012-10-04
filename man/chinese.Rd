\name{chinese}
\alias{chinese}
\title{Chinese Remainder Theorem}
\description{
  Executes the Chinese Remainder Theorem (CRT).
}
\usage{
chinese(a, m)
}
\arguments{
  \item{a}{sequence of integers, same length as \code{m}.}
  \item{m}{sequence of integers, relatively prime to each other.}
}
\details{
  The Chinese Remainder Theorem says that given integers \eqn{a_i} and
  natural numbers \eqn{m_i}, relatively prime (i.e., coprime) to each other,
  there exists a unique solution \eqn{x = x_i} such that the following
  system of linear modular equations is satisfied:

  \deqn{x_i = a_i \, mod \, m_i, \quad 1 \le i \le n }

  More generally, a solution exists if the following condition is satisfied:

  \deqn{a_i = a_j \, mod \, gcd(m_i, m_j)}

  This version of the CRT is not yet implemented.
}
\value{
  Returns th (unique) solution of the system of modular equalities as an
  integer between \code{0} and \code{M=prod(m)}.
}
\seealso{
  \code{\link{extGCD}}
}
\examples{
m <- c(3, 4, 5)
a <- c(2, 3, 1)
chinese(a, m)    #=> 11

# ... would be sufficient
# m <- c(50, 210, 154)
# a <- c(44,  34, 132)
# x = 4444
}
