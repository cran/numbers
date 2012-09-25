##
##  m o d u l a r . R  Modular functions
##

mod <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(m) != 1 || floor(m) != ceiling(m))
        stop("Argument 'm' must be an integer scalar.")
    if (m == 0) return(n)
    else        return(n %% m)
}


rem <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    if (length(m) != 1 || floor(m) != ceiling(m))
        stop("Argument 'm' must be an integer scalar.")
    if (m == 0) return(NaN)
    k <- mod(n, m)
    if (sign(n) * sign(m) < 0) {
        k <- k - m
    }
    return(k)
}


modinv <- function(n, m) {
    v <- extGCD(n, m)
    if (v[1] == 0 || v[1] > 1) return(NA)
    if (v[2] >= 0) v[2] else v[2] + m
}

modlin <- function(a, b, n) {
    stopifnot(is.numeric(a), is.numeric(b), is.numeric(n))
    if (length(a) != 1 || length(b) != 1 || length(n) != 1 ||
        floor(a) != ceiling(a) || floor(b) != ceiling(b) || floor(n) != ceiling(n) ||
        a < 1 || b < 1 || n < 1)
        stop("All inputs 'a', 'b', 'n' must be integers.")

    def <- extGCD(a, n)
    d <- def[1]; e <- def[2]; f <- def[3]

    x <- c()
    if (b %% d == 0) {
        x0 <- (e * (b/d)) %% n
        for (i in 0:(d-1)) {
            x <- c(x, (x0 + i*(n/d)) %% n)
        }
    }
    return(x)
}
