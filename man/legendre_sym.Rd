\name{legendre_sym}
\alias{legendre_sym}
\alias{jacobi_sym}
\title{Legendre and Jacobi Symbol}
\description{
  Legendre and Jacobi Symbol for quadratic residues.
}
\usage{
legendre_sym(a, p)

jacobi_sym(a, n)
}
\arguments{
  \item{a, n}{integers.}
  \item{p}{prime number.}
}
\details{
  The Legendre Symbol \code{(a/p)}, where \code{p} must be a prime number,
  denotes whether \code{a} is a quadratic residue modulo \code{p} or not.

  The Jacobi symbol \code{(a/p)} is the product of \code{(a/p)} of all prime
  factors \code{p} on \code{n}.
}
\value{
  Returns 0, 1, or -1 if \code{p} divides \code{a}, \code{a} is a quadratic 
  residue, or if not.
}
\seealso{
\code{\link{quadratic_residues}}
}
\examples{
Lsym <- Vectorize(legendre_sym, 'a')

# all quadratic residues of p = 17
qr17 <- which(Lsym(1:16, 17) == 1)      #  1  2  4  8  9 13 15 16
sort(unique((1:16)^2 \%\% 17))            #  the same

\dontrun{
# how about large numbers?
p <- 1198112137                         #  isPrime(p) TRUE
x <- 4652356
a <- mod(x^2, p)                        #  520595831
legendre_sym(a, p)                      #  1
legendre_sym(a+1, p)                    # -1
  }

jacobi_sym(11, 12)                      # -1
}