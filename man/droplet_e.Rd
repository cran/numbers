\name{dropletPi}
\alias{dropletPi}
\alias{dropletE}
\title{
  Droplet Algorithm for pi and e
}
\description{
  Generates digits for pi resp. the Euler number e.
}
\usage{
dropletPi(n)
dropletE(n)
}
\arguments{
  \item{n}{number of digits after the decimal point; should not exceed 1000
           much as otherwise it will be \emph{very} slow.}
}
\details{
  Based on a formula discovered by S. Rabinowitz and S. Wagon.

  The droplet algorithm for pi uses the Euler transform of the alternating
  Leibniz series and the socalled "radix conversion".
}
\value{
  String containing ``3.1415926..." resp. ``2.718281828..." with
  \code{n} digits after the decimal point (i.e., internal decimal places).
}
\references{
  Borwein, J., and K. Devlin (2009). The Computer as Crucible: An Introduction
  to Experimental Mathematics. A K Peters, Ltd.

  Arndt, J., and Ch. Haenel (2000). Pi -- Algorithmen, Computer, Arithmetik.
  Springer-Verlag, Berlin Heidelberg.
}
\examples{
##  Example
dropletE(20)                    # [1] "2.71828182845904523536"
print(exp(1), digits=20)        # [1]  2.7182818284590450908

dropletPi(20)                   # [1] "3.14159265358979323846"
print(pi, digits=20)            # [1]  3.141592653589793116

\dontrun{
E <- dropletE(1000)
table(strsplit(substring(E, 3, 1002), ""))
#    0   1   2   3   4   5   6   7   8   9 
#  100  96  97 109 100  85  99  99 103 112

Pi <- dropletPi(1000)
table(strsplit(substring(Pi, 3, 1002), ""))
#   0   1   2   3   4   5   6   7   8   9 
#  93 116 103 102  93  97  94  95 101 106 }
}

