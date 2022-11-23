\name{solvePellsEq}
\alias{solvePellsEq}
\title{
  Solve Pell's Equation
}
\description{
  Find the basic, that is minimal, solution for Pell's equation, applying
  the technique of (periodic) continued fractions.
}
\usage{
solvePellsEq(d)
}
\arguments{
  \item{d}{non-square integer greater 1.}
}
\details{
  Solving Pell's equation means to find integer solutions \code{(x,y)}
  for the Diophantine equation \deqn{x^2 - d\,y^2 = 1} for \eqn{d} a
  non-square integer. These solutions are important in number theory and
  for the theory of quadratic number fields.

  The procedure goes as follows: First find the periodic continued
  fraction for \eqn{\sqrt{d}}, then determine the convergents of this
  continued fraction. The last pair of convergents will provide the
  solution for Pell's equation.

  The solution found is the minimal or \emph{fundamental} solution.
  All other solutions can be derived from this one -- but the numbers
  grow up very rapidly.
}
\value{
  Returns a list with components
    \item{x, y }{solution (x,y) of Pell's equation.}
    \item{plen }{length of the period.}
    \item{doubled}{logical: was the period doubled?}
    \item{msg}{message either "Success" or "Integer overflow".}

  If 'doubled' was TRUE, there exists also a solution for the
  \emph{negative} Pell equation
}
\references{
  H.W. Lenstra Jr. Solving the Pell Equation. Notices of the AMS,
  Vol. 49, No. 2, February 2002.

  See the "List of fundamental solutions of Pell's equations" in the
  Wikipedia entry for "Pell's Equation".
}
\author{
  Hans Werner Borchers
}
\note{
  Integer overflow may happen for the convergents, but very rarely.
  More often, the terms \code{x^2} or \code{y^2} can overflow the
  maximally representable integer \code{2^53-1} and checking Pell's
  equation may end with a value differing from \code{1}, though in
  reality the solution is correct.
}
\seealso{
  \code{\link{periodicCF}}
}
\examples{
  s = solvePellsEq(1003)                # $x = 9026, $y = 285
  9026^2 - 1003*285^2 == 1
  # TRUE
}
