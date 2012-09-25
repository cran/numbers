\name{moebius}
\alias{moebius}
\alias{mertens}
\title{Moebius Function}
\description{
  The classical Moebius and Mertens functions in number theory.
}
\usage{
moebius(n)
mertens(n)
}
\arguments{
  \item{n}{Positive integer.}
}
\details{
  \code{moebius(n)} is \code{+1} if n is a square-free positive integer
  with an even number of prime factors, or \code{+1} if there are an odd
  of prime factors. It is \code{0} if \code{n} is not square-free.

  \code{mertens(n)} is the aggregating summary function, that sums up all
  values of \code{moebius} from \code{1} to \code{n}. 
}
\value{
  For \code{moebius}, \code{0, 1} or \code{-1}, depending on the prime
  decomposition of \code{n}.

  For \code{mertens} the values will very slowly grow.
}
\note{
  Works well up to \code{10^9}, but will become very slow for the Mertens
  function.
}
\seealso{
\code{\link{factorize}}, \code{\link{eulersPhi}}
}
\examples{
sapply(1:16, moebius)
sapply(1:16, mertens)

\dontrun{
x <- 1:50; y <- sapply(x, moebius)
plot(c(1, 50), c(-3, 3), type="n")
grid()
points(1:50, y, pch=18, col="blue")

x <- 1:100; y <- sapply(x, mertens)
plot(c(1, 100), c(-5, 3), type="n")
grid()
lines(1:100, y, col="red", type="s")}
}

