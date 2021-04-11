\name{Pascal triangle}
\alias{pascal_triangle}
\title{
  Pascal Triangle
}
\description{
  Generates the Pascal triangle in rectangular form.
}
\usage{
pascal_triangle(n)
}
\arguments{
  \item{n}{integer number}
}
\details{
  Pascal numbers will be generated with the usual recursion formula and
  stored in a rectangular scheme.

  For \code{n>50} integer overflow would happen, so use the arbitrary
  precision version \code{gmp::chooseZ(n, 0:n)} instead for calculating
  binomial numbers.
}
\value{
  Returns the Pascal triangle as an (n+1)x(n+1) rectangle with zeros filled in.
}
\references{
  See Wolfram MathWorld or the Wikipedia.
}
\examples{
n <- 5; P <- pascal_triangle(n)
for (i in 1:(n+1)) {
    cat(P[i, 1:i], '\n')
}
## 1 
## 1 1 
## 1 2 1 
## 1 3 3 1 
## 1 4 6 4 1 
## 1 5 10 10 5 1 

\dontrun{
P <- pascal_triangle(50)
max(P[51, ])
## [1] 126410606437752
}
}
