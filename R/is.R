##
##  i s . R  Type functions
##


isNatural <- function(n) {
    stopifnot(is.numeric(n))
    if (any(floor(n) != ceiling(n)) ||
        any(n < 1) || any(n > 2^53 - 1)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}


isPrime <- function(x) {
    if (is.null(x) || length(x) == 0)
        stop("Argument 'x' must be a nonempty vector or matrix.")
    if (!is.numeric(x) || any(x < 0) || any(x != round(x)))
        stop("All entries of 'x' must be nonnegative integers.")

    n <- length(x)
    X <- x[1:n]
    L <- logical(n)
    p <- Primes(ceiling(sqrt(max(x))))
    for (i in 1:n) {
        L[i] <- all(X[i] %% p[p < X[i]] != 0)
    }
    L[X == 1 | X == 0] <- FALSE
    dim(L) <- dim(x)
    return(L)
}


isIntpower <- function(p) {
    if (!isNatural(p))
        stop("Argument 'p' must be a natural number.")
    if (p == 1)
        return(c(1, 1))

    int_root <- function(p, b) {
        x <- 2^ceiling(log2(p+1)/b)
        while(TRUE) {
            y <- floor(((b-1)*x + floor(p/x^(b-1)))/b)
            if (y >= x) return(x)
            x <- y
        }
    }
    for (b in 2:floor(log2(p))) {
        q <- int_root(p, b)
        if (q^b == p) {
            return(c(q, b))
        }
    }
    return(c(p, 1))
}


isPrimroot <- function(g, p) {
    stopifnot(is.numeric(g), floor(g) == ceiling(g),
              is.numeric(p), floor(p) == ceiling(p))
    if (!isPrime(p)) return(FALSE)
    if (modorder(g, p) == p-1) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


isSquare <- function(p) {
    if (floor(p) != ceiling(p) || p < 0)
        stop("Argument 'p' must be a non-negative integer.")
    if (p == 0 || p == 1) return(TRUE)
    pp = isIntpower(p)
    if (pp[2] == 2) return(TRUE)
    else return(FALSE)
}


isSquarefree <- function(p) {
    # Determine whether a square is a divisor of p
    if (! isNatural(p))
        stop("Argument 'p' must be a natural number.")
    pf <- primeFactors(p)
    if (length(unique(pf)) < length(pf)) {
        return(FALSE)
    } else
        return(TRUE)
}
