\name{Bernoulli numbers}
\alias{bernoulli_numbers}
\title{
  Bernoulli Numbers
}
\description{
  Generate the Bernoulli numbers.
}
\usage{
bernoulli_numbers(n, big = FALSE)
}
\arguments{
  \item{n}{integer; starting from 0.}
  \item{big}{logical; shall double or GMP big numbers be returned?}
}
\details{
  Generate the \code{n+1} Bernoulli numbers \code{B_0,B_1, ...,B_n},
  i.e. from 0 to \code{n}. We assume \code{B1 = +1/2}.

  With \code{big=FALSE} double integers up to \code{2^53-1} will be used,
  with \code{big=TRUE} GMP big rationals (through the 'gmp' package).
  \code{B_25} is the highest such number that can be expressed as an
  integer in double float.
}
\value{
  Returns a matrix with two columns, the first the numerator, the second
  the denominator of the Bernoulli number.
}
\references{
  M. Kaneko. The Akiyama-Tanigawa algorithm for Bernoulli numbers.
  Journal of Integer Sequences, Vol. 3, 2000.

  D. Harvey. A multimodular algorithm for computing Bernoulli numbers.
  Mathematics of Computation, Vol. 79(272), pp. 2361-2370, Oct. 2010.
  arXiv 0807.1347v2, Oct. 2018.
}
\seealso{
  \code{\link{pascal_triangle}}
}
\examples{
bernoulli_numbers(3); bernoulli_numbers(3, big=TRUE)
##                    Big Integer ('bigz') 4 x 2 matrix:
##      [,1] [,2]          [,1] [,2]
## [1,]    1    1     [1,] 1    1   
## [1,]    1    2     [2,] 1    2   
## [2,]    1    6     [3,] 1    6   
## [3,]    0    1     [4,] 0    1   

\dontrun{
bernoulli_numbers(24)[25,]
## [1] -236364091       2730

bernoulli_numbers(30, big=TRUE)[31,]
## Big Integer ('bigz') 1 x 2 matrix:
##      [,1]          [,2] 
## [1,] 8615841276005 14322
}
}
