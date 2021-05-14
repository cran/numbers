\name{iNthroot}
\alias{iNthroot}
\title{
  Integer N-th Root
}
\description{
  Determine the integer \code{n}-th root of \code{}.
}
\usage{
  iNthroot(p, n)
}
\arguments{
  \item{p}{any positive number.}
  \item{n}{a natural number.}
}
\details{
  Calculates the highest natural number below the \code{n}-th root of
  \code{p} in a more integer based way than simply \code{floor(p^{1/n})}.
}
\value{
  An integer.
}
\examples{
iNthroot(0.5, 6)    # 0
iNthroot(1, 6)      # 1
iNthroot(5^6, 6)    # 5
iNthroot(5^6-1, 6)  # 4
\dontrun{
# Define a function that tests whether 
isNthpower <- function(p, n) {
    q <- iNthroot(p, n)
    if (q^n == p) { return(TRUE)
    } else { return(FALSE) }
  }
  }
}
