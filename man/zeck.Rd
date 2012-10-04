\name{zeck}
\alias{zeck}
\title{
  Zeckendorf Representation
}
\description{
  Generates the Zeckendorf representation of an integer as a sum of
  Fibonacci numbers.
}
\usage{
zeck(n)
}
\arguments{
  \item{n}{integer.}
}
\details{
  According to Zeckendors theorem from 1972, each integer can be uniquely
  represented as a sum of Fibonacci numbers such that no two of these are 
  consecutive in the Fibonacci sequence.

  The computation is simply the greedy algorith of finding the highest
  Fibonacci number below \code{n}, subtracting it and iterating.
}
\value{
  List with components \code{fibs} the Fibonacci numbers that add sum up to
  \code{n}, and \code{inds} their indices in the Fibonacci sequence.
}
\examples{
zeck(  10)  #=> 2 + 8 = 10
zeck( 100)  #=> 3 + 8 + 89 = 100
zeck(1000)  #=> 13 + 987 = 1000
}
