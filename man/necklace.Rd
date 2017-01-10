\name{necklace}
\alias{necklace}
\alias{bracelet}
\title{Necklace and Bracelet Functions}
\description{
  Necklace and bracelet problems in combinatorics.
}
\usage{
necklace(k, n)

bracelet(k, n)
}
\arguments{
  \item{k}{The size of the set or alphabet to choose from.}
  \item{n}{the length of the necklace or bracelet.}
}
\details{
  A necklace is a closed string of length \code{n} over a set of size
  \code{k} (numbers, characters, clors, etc.), where all rotations are
  taken as equivalent. A bracelet is a necklace where strings may also
  be equivalent under reflections.

  Polya's enumeration theorem can be utilized to enumerate all necklaces
  or bracelets. The final calculation involves Euler's Phi or totient
  function, in this package implemented as \code{eulersPhi}.
}
\value{
  Returns the number of necklaces resp. bracelets.
}
\references{
  \url{https://en.wikipedia.org/wiki/Necklace_(combinatorics)}
}
\examples{
necklace(2, 5)
necklace(3, 6)

bracelet(2, 5)
bracelet(3, 6)
}
