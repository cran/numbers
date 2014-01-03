\name{catalan}
\alias{catalan}
\title{
  Catalan Numbers
}
\description{
  Generate Catalan numbers.
}
\usage{
catalan(n)
}
\arguments{
  \item{n}{integer, asking for the n-th Catalan number.}
}
\details{
  Catalan numbers, commonly denoted as \eqn{C_n}, are defined as
  \deqn{C_n = \frac{1}{n+1} {2 n \choose n}}
  and occur regularly in all kinds of enumeration problems.
}
\value{
  A single integer, as long as \code{n<=30}.
}
\examples{
C <- numeric(10)
for (i in 1:10) C[i] <- catalan(i)
C[5]                                #=> 42
}
