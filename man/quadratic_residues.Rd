\name{quadratic_residues}
\alias{quadratic_residues}
\title{Quadratic Residues}
\description{
  List all quadratic residues of an integer.
}
\usage{
quadratic_residues(n)
}
\arguments{
  \item{n}{integer.}
}
\details{
  Squares all numbers between 0 and \code{n/2} and generate a unique list of 
  all these numbers modulo \code{n}.
}
\value{
  Vector of integers.
}
\seealso{
\code{\link{legendre_sym}}
}
\examples{
quadratic_residues(17)
}
