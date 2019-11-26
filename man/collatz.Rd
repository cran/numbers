\name{collatz}
\alias{collatz}
\title{
  Collatz Sequences
}
\description{
  Generates Collatz sequences with n -> k*n+l for n odd.
}
\usage{
collatz(n, k = 3, l = 1, short = FALSE, check = TRUE)
}
\arguments{
  \item{n}{integer to start the Collatz sequence with.}
  \item{k, l}{parameters for computing \code{k*n+l}.}
  \item{short}{logical, abbreviate stps with \code{(k*n+l)/2}}
  \item{check}{logical, check for nontrivial cycles.}
}
\details{
  Function \code{n, k, l} generates iterative sequences starting with
  \code{n} and calculating the next number as \code{n/2} if \code{n} is
  even and \code{k*n+l} if \code{n} is odd. It stops automatically when
  1 is reached.

  The default parameters \code{k=3, l=1} generate the classical Collatz
  sequence. The Collatz conjecture says that every such sequences will end
  in the trivial cycle \code{...,4,2,1}. For other parameters this does not
  necessarily happen.

  \code{k} and \code{l} are not allowed to be both even or both odd -- to make
  \code{k*n+l} even for \code{n} odd. Option \code{short=TRUE} calculates 
  \code{(k*n+l)/2} when \code{n} is odd (as \code{k*n+l} is even in this case),
  shortening the sequence a bit.

  With option \code{check=TRUE} will check for nontrivial cycles, stopping
  with the first integer that repeats in the sequence. The check is disabled
  for the default parameters in the light of the Collatz conjecture.
}
\value{
  Returns the integer sequence generated from the iterative rule.
  
  Sends out a message if a nontrivial cycle was found (i.e. the sequence 
  is not ending with 1 and end in an infinite cycle). Throws an error if
  an integer overflow is detected.
}
\references{
  See the Wikipedia entry on the 'Collatz Conjecture'.
}
\note{
  The Collatz or \code{3n+1}-conjecture has been experimentally verified
  for all start numbers \code{n} up to \code{10^20} at least.
}
\examples{
collatz(7)  # n -> 3n+1
## [1]  7 22 11 34 17 52 26 13 40 20 10  5 16  8  4  2  1
collatz(9, short = TRUE)
## [1]  9 14  7 11 17 26 13 20 10  5  8  4  2  1

collatz(7, l = -1)  # n -> 3n-1
## Found a non-trivial cycle for n = 7 !
##     [1]  7 20 10  5 14  7

\dontrun{
collatz(5, k = 7, l = 1)  # n -> 7n+1
## [1]  5 36 18  9 64 32 16  8  4  2  1
collatz(5, k = 7, l = -1)  # n -> 7n-1
## Info: 5 --> 1.26995e+16 too big after 280 steps.
## Error in collatz(5, k = 7, l = -1) : 
##     Integer overflow, i.e. greater than 2^53-1
}
}
