##
##  f i b o n a c c i . R  Fibonacci Sequence
##


fibonacci <- function(n, sequence = TRUE) {
    if (!is.numeric(n) || length(n) != 1 || floor(n) != ceiling(n) || n < 0)
        stop("Argument 'n' must be a single integer >= 0.")
    if (n <= 1) return(c(1))

    if (sequence) {
        if (n == 2) return(c(1, 2))
        fib <- numeric(n)
        fib[1:2] <- c(1, 2)
        for (k in 3:n) {
            fib[k] <- fib[k-1] + fib[k-2]
        }
    } else {
        if (n <= 1) {
            return(1)
        } else {
            fib = fibonacci(n-1) + fibonacci(n-2)
        }
    }
    return(fib)
}


lucas <- function(n) {
    if (!is.numeric(n) || length(n) != 1 || floor(n) != ceiling(n) || n < 0)
        stop("Argument 'n' must be a single integer >= 0.")
    if (n == 0) return(c(2))
    if (n == 1) return(c(1))
    if (n == 2) return(c(1, 3))

    fib <- fibonacci(n, sequence = TRUE)
    luc <- fib + c(0, 1, fib[1:(n-2)])
    return(luc)
}


zeck <- function(n) {
    stopifnot(is.numeric(n))
    if (!isNatural(n) || length(n) != 1)
        stop("Argument 'n' must be an integer.")

    Fib <- c(1, 2)
    k <- 2
    f <- 3
    while (f <= n) {
        Fib <- c(Fib, f)
        f <- Fib[k] + f
        k <- k + 1
    }

    fib <- Fib
    K <- c()
    while (n > 0) {
        K <- c(K, k)
        n <- n - fib[k]
        fib <- fib[fib <= n]
        k <- length(fib)
    }

    K <- rev(K)
    return(list(fibs = Fib[K], inds = K))
}
