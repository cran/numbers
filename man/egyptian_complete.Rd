\name{egyptian_complete}
\alias{egyptian_complete}
\title{
  Egyptian Fractions - Complete Search
}
\description{
  Generate all Egyptian fractions of length 2 and 3.
}
\usage{
  egyptian_complete(a, b, show = TRUE)
}
\arguments{
  \item{a, b}{integers, a != 1, a < b and a, b relatively prime.}
  \item{show}{logical; shall solutions found be printed?}
}
\details{
  For a rational number \code{0 < a/b < 1}, generates all Egyptian fractions
  of length 2 and three, that is finds integers \code{x1, x2, x3} such that

  \code{a/b = 1/x1 + 1/x2}\cr
  \code{a/b = 1/x1 + 1/x2 + 1/x3}.
}
\value{
 All solutions found will be printed to the console if \code{show=TRUE};
 returns invisibly the number of solutions found. 
}
\references{
  \url{https://www.ics.uci.edu/~eppstein/numth/egypt/}
}
\seealso{
  \code{\link{egyptian_methods}}
}
\examples{
egyptian_complete(6, 7)         # 1/2 + 1/3 + 1/42
egyptian_complete(8, 11)        # no solution with 2 or 3 fractions

# TODO
# 2/9 = 1/9 + 1/10 + 1/90       # is not recognized, as similar cases,
                                # because 1/n is not considered in m/n.
}
