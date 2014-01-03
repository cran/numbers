\name{sigma}
\alias{sigma}
\alias{tau}
\title{Divisor Functions}
\description{
  Sum of powers of all divisors of a natural number.
}
\usage{
sigma(n, k = 1, proper = FALSE)

tau(n)
}
\arguments{
  \item{n}{Positive integer.}
  \item{k}{Numeric scalar, the exponent to be used.}
  \item{proper}{Logical; if \code{TRUE}, n will \emph{not} be considered
                as a divisor of itself; default: FALSE.}
}
\details{
  Total sum of all integer divisors of \code{n} to the power of \code{k},
  including \code{1} and \code{n}.

  For \code{k=0} this is the number of divisors, for \code{k=1}
  it is the sum of all divisors of \code{n}.

  \code{tau} is Ramanujan`s \emph{tau} function, here computed using
  \code{sigma(., 5)} and \code{sigma(., 11)}.

  A number is called \emph{refactorable}, if \code{tau(n)} divides \code{n},
  for example \code{n=12} or \code{n=18}.
}
\value{
  Natural number, the number or sum of all divisors.
}
\references{
\code{http://en.wikipedia.org/wiki/Divisor_function}

\code{http://en.wikipedia.org/wiki/Tau-function}
}
\note{
  Works well up to \code{10^9}.
}
\seealso{
\code{\link{primeFactors}}
}
\examples{
sapply(1:16, sigma, k = 0)
sapply(1:16, sigma, k = 1)
sapply(1:16, sigma, proper = TRUE)

\dontshow{
stopifnot(identical(sapply(1:16, sigma, k = 0),
                    c(1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5)))
stopifnot(identical(sapply(1:16, sigma, k = 1),
            c(1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31)))
stopifnot(identical(sapply(1:16, sigma, proper = TRUE),
            c(0, 1, 1, 3, 1, 6, 1, 7, 4, 8, 1, 16, 1, 10, 9, 15)))

stopifnot(all.equal(sapply(1:10, tau),
        c(1, -24, 252, -1472, 4830, -6048, -16744, 84480, -113643, -115920)))
  }
}
