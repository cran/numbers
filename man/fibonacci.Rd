\name{fibonacci}
\alias{fibonacci}
\alias{lucas}
\title{
  Fibonacci and Lucas Series
}
\description{
  Generates single Fibonacci numbers or a Fibonacci sequence;
  or generates a Lucas series based on the Fibonacci series.
}
\usage{
fibonacci(n, sequence = FALSE)
lucas(n)
}
\arguments{
  \item{n}{an integer.}
  \item{sequence}{logical; default: FALSE.}
}
\details{
  Generates the \code{n}-th Fibonacci number, or the whole Fibonacci sequence
  from the first to the \code{n}-th number; starts with (1, 1, 2, 3, ...).
  Generates only single Lucas numbers. The Lucas series can be extenden to
  the left and starts as (... -4, 3, -1, 2, 1, 3, 4, ...).

  The recursive version is too slow for values \code{n>=30}. Therefore, an
  iterative approach is used. For numbers \code{n > 78} Fibonacci numbers
  cannot be represented exactly in R as integers (\code{>2^53-1}).
}
\value{
  A single integer, or a vector of integers.
}
\examples{
fibonacci(0)                            # 0
fibonacci(2)                            # 1
fibonacci(2, sequence = TRUE)           # 1 1
fibonacci(78)                           # 8944394323791464 < 9*10^15

lucas(0)                                # 2
lucas(2)                                # 3
lucas(76)                               # 7639424778862807

# Golden ratio
F <- fibonacci(25, sequence = TRUE)     # ... 46368 75025
f25 <- F[25]/F[24]                      # 1.618034
phi <- (sqrt(5) + 1)/2
abs(f25 - phi)                          # 2.080072e-10

# Fibonacci numbers w/o iteration
  fibo <- function(n) {
    phi <- (sqrt(5) + 1)/2
    fib <- (phi^n - (1-phi)^n) / (2*phi - 1)
    round(fib)
  }
fibo(30:33)                             # 832040 1346269 2178309 3524578

for (i in -8:8) cat(lucas(i), " ")
# 47  -29  18  -11  7  -4  3  -1  2  1  3  4  7  11  18  29  47

# Lucas numbers w/o iteration
  luca <- function(n) {
    phi <- (sqrt(5) + 1)/2
    luc <- phi^n + (1-phi)^n
    round(luc)
  }
luca(0:10)
# [1]   2   1   3   4   7  11  18  29  47  76 123

# Lucas primes
#   for (j in 0:76) {
#     l <- lucas(j)
#     if (isPrime(l)) cat(j, "\t", l, "\n")
#   }
# 0   2
# 2   3
# ...
# 71  688846502588399
}
