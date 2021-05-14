\name{egyptian_methods}
\alias{egyptian_methods}
\title{
  Egyptian Fractions - Specialized Methods
}
\description{
  Generate Egyptian fractions with specialized methods.
}
\usage{
  egyptian_methods(a, b)
}
\arguments{
  \item{a, b}{integers, a != 1, a < b and a, b relatively prime.}
}
\details{
  For a rational number \code{0 < a/b < 1}, generates Egyptian fractions
  that is finds integers \code{x1, x2, ..., xk} such that

  \code{a/b = 1/x1 + 1/x2 + ... + 1/xk}

  using the following methods:
  \itemize{
    \item{`greedy'}
    \item{Fibonacci-Sylvester}
    \item{Golomb (same as with Farey sequences)}
    \item{continued fractions (not yet implemented)}
  }
}
\value{
  No return value, all solutions found will be printed to the console.
}
\references{
  \url{https://www.ics.uci.edu/~eppstein/numth/egypt/}
}
\seealso{
  \code{\link{egyptian_complete}}
}
\examples{
egyptian_methods(8, 11)
# 8/11 = 1/2 +  1/5 + 1/37 + 1/4070  (Fibonacci-Sylvester)
# 8/11 = 1/2 +  1/6 + 1/21 + 1/77    (Golomb-Farey)

# Other solutions
# 8/11 = 1/2 +  1/8 + 1/11 + 1/88
# 8/11 = 1/2 + 1/12 + 1/22 + 1/121
}
