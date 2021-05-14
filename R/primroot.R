##
##  p r i m r o o t . R  Primitive Root
##


modpower <- function(n, k, m) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 0,
              is.numeric(k), floor(k) == ceiling(k), k >= 0,
              is.numeric(m), floor(m) == ceiling(m), m >= 0)

    if (m^2 > 2^53-1)
        stop("Modulus 'm' too big for integer arithmetic in R.")
    if (k == 0) return(1)
    if (n == 0) return(0)

    b <- n %% m
    r <- 1
    while (k != 0) {
        if (k %% 2 == 1) {
            r <- (b * r) %% m
            k <- k - 1
        }
        k <- k / 2
        b <- (b * b) %% m
    }
    return(r)
}


modorder <- function(n, m) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 0,
              is.numeric(m), floor(m) == ceiling(m), m >= 0)

    if (!coprime(n, m)) return(0)
    r <- n %% m; k <- 1
    if (r == 0) return(NA)
    while (r != 1) {
        r <- (n*r) %% m; k <- k + 1
    }
    return(k)
}


primroot <- function (m, all = FALSE) {
    stopifnot(is.numeric(m), floor(m) == ceiling(m), m >= 0)
    if (!isPrime(m)) return(NA)
    if (m == 2) return(1)
    
    if (all) {
        res <- c()
        for (r in 2:(m-1)) {
            k <- modorder(r, m)
            if (k == m-1) res <- c(res, r)
        }
    } else {
        for (r in 2:(m-1)) {
            k <- modorder(r, m)
            if (k == m - 1) break
        }
        res <- r
    }
    return(res)
}
