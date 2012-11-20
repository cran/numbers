\name{fibonacci}
\alias{fibonacci}
\alias{lucas}
\title{
  Fibonacci and Lucas Series
}
\description{
  Generates single Fibonacci numbers or a Fibonacci sequence; generates
  a Lucas series based on the Fibonacci series.
}
\usage{
fibonacci(n, sequence = TRUE)
lucas(n)
}
\arguments{
  \item{n}{integer.}
  \item{sequence}{logical; default: True.}
}
\details{
  Generates the \code{n}-th Fibonacci number, or the whole Fibonacci sequence
  from the first to the \code{n}-th number. Generates only the sequence of
  Lucas numbers.

  The recursive version (\code{sequence=FALSE}) is extremely slow for values
  \code{n>=30}. To get the \code{n}-th Fibonacci number for larger \code{n}
  values, use \code{fibonacci(n)[n]}.
}
\value{
  A single integer, or a vector of integers.
}
\examples{
fibonacci(0)                            # 1
fibonacci(2)                            # 2
fibonacci(2, sequence = TRUE)           # 1 2

lucas(0)                                # 2
lucas(2)                                # 1 3

# Golden ratio
F <- fibonacci(25, sequence = TRUE)     # ... 75025 121393
f25 <- F[25]/F[24]                      #     1.618033989
phi <- (sqrt(5) + 1)/2
abs(f25 - phi)                          # 7.945178e-11

# Fibonacci numbers w/o iteration
  fibo <- function(n) {
    phi <- (sqrt(5) + 1)/2
    fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
    round(fib)
  }
fibo(30:33)                             # 1346269 2178309 3524578 5702887

  luca <- function(n) {
    phi <- (sqrt(5) + 1)/2
    floor(phi^n + 1/2)
  }
luca(30:33)                             # 1860498 3010349 4870847 7881196

# Compare recursive with iterative approach:
# system.time(F30 <- fibonacci(30, sequence = FALSE)      # user: 17.075 s
# system.time(F30 <- fibonacci(30, sequence = TRUE)[30])  # user:  0.006 s
}
