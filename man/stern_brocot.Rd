\name{Stern-Brocot}
\alias{stern_brocot_seq}
\title{
  Stern-Brocot Sequence
}
\description{
  The function generates the Stern-Brocot sequence up to 
  length \code{n}.
}
\usage{
  stern_brocot_seq(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{integer; length of the sequence.}
}
\details{
  The Stern-Brocot sequence is a sequence \code{S} of natural 
  numbers beginning with

  \code{1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, ...}

  defined with \code{S[1] = S[2] = 1} and the following rules:

  \code{S[k] = S[k/2]} if \code{k} is even \cr
  \code{S[k] = S[(k-1)/2] + S[(k+1)/2]} if \code{k} is not even

  The Stern-Brocot has the remarkable properties that

  (1) Consecutive values in this sequence are coprime;\cr 
  (2) the list of rationals \code{S[k+1]/S[k]} (all in reduced
  form) covers all positive rational numbers once and once only.
}
\value{
  Returns a sequence of length \code{n} of natural numbers.
}
\references{
  N. Calkin and H.S. Wilf. Recounting the rationals.
  The American Mathematical Monthly, Vol. 7(4), 2000.

  Graham, Knuth, and Patashnik. Concrete Mathematics - 
  A Foundation for Computer Science. Addison-Wesley, 1989.
}
\seealso{
  \code{\link{fibonacci}}
}
\examples{
( S <- stern_brocot_seq(92) )
# 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1,  5, 4, 7, 
# 3, 8, 5, 7, 2, 7, 5, 8, 3, 7, 4, 5, 1, 6, 5, 9, 4, 11, 7, 10, 
# 3, 11, 8, 13, 5, 12, 7, 9, 2, 9, 7, 12, 5, 13, 8, 11, 3, 10, 7, 11, 
# 4, 9, 5, 6, 1, 7, 6, 11, 5, 14, 9, 13, 4, 15, 11, 18, 7, 17, 10, 13, 
# 3, 14, 11, 19, 8, 21, 13, 18, 5, 17, 12, 19, 7, ...

table(S)
## S
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 17 18 19 21 
##  7  5  9  7 12  3 11  5  5  3  7  3  5  2  1  2  2  2  1 

which(S == 1)  # 1  2  4  8 16 32 64

\dontrun{
# Find the rational number p/q in S
# note that 1/2^n appears in position S[c(2^(n-1), 2^(n-1)+1)]
occurs <- function(p, q, s){
    # Find i such that (p, q) = s[i, i+1]
    inds <- seq.int(length = length(s)-1)
    inds <- inds[p == s[inds]]
    inds[q == s[inds + 1]]
}
p = 3; q = 7        # 3/7
occurs(p, q, S)     # S[28, 29]

'\%//\%' <- function(p, q) gmp::as.bigq(p, q)
n <- length(S)
S[1:(n-1)] \%//\% S[2:n]
## Big Rational ('bigq') object of length 91:
##  [1] 1     1/2  2     1/3   3/2   2/3   3     1/4   4/3   3/5   
## [11] 5/2   2/5  5/3   3/4   4     1/5   5/4   4/7   7/3   3/8   ...

as.double(S[1:(n-1)] \%//\% S[2:n])
## [1] 1.000000 0.500000 2.000000 0.333333 1.500000 0.666667 3.000000
## [8] 0.250000 1.333333 0.600000 2.500000 0.400000 1.666667 0.750000 ...
}
}
