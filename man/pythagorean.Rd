\name{pythagorean_triples}
\alias{pythagorean_triples}
\title{
  Pythagorean Triples
}
\description{
  Generates all primitive Pythagorean triples \eqn{(a, b, c)} of integers
  such that \eqn{a^2 + b^2 = c^2}, where \eqn{a, b, c} are coprime (have no
  common divisor) and \eqn{c_1 \le c \le c_2}.
}
\usage{
pythagorean_triples(c1, c2)
}
\arguments{
  \item{c1, c2}{lower and upper limit of the hypothenuses \code{c}.}
}
\details{
  If \eqn{(a, b, c)} is a primitive Pythagorean triple, there are integers
  \eqn{m, n} with \eqn{1 \le n < m} such that
  \deqn{a = m^2 - n^2, b = 2 m n, c = m^2 + n^2}
  with \eqn{gcd(m, n) = 1} and \eqn{m - n} being odd.
}
\value{
  Returns a matrix, one row for each Pythagorean triple, of the form
  \code{(m  n  a  b  c)}.
}
\references{
  \url{http://mathworld.wolfram.com/PythagoreanTriple.html}
}
\examples{
pythagorean_triples(100, 200)
##       [,1] [,2] [,3] [,4] [,5]
##  [1,]   10    1   99   20  101
##  [2,]   10    3   91   60  109
##  [3,]    8    7   15  112  113
##  [4,]   11    2  117   44  125
##  [5,]   11    4  105   88  137
##  [6,]    9    8   17  144  145
##  [7,]   12    1  143   24  145
##  [8,]   10    7   51  140  149
##  [9,]   11    6   85  132  157
## [10,]   12    5  119  120  169
## [11,]   13    2  165   52  173
## [12,]   10    9   19  180  181
## [13,]   11    8   57  176  185
## [14,]   13    4  153  104  185
## [15,]   12    7   95  168  193
## [16,]   14    1  195   28  197
}

