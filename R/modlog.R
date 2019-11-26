##
##  m o d l o g . R  Modular (or: Discrete) Logarithm
##


modlog <- function(g, x, p) {
    stopifnot(is.numeric(g), is.numeric(x), is.numeric(p),
              length(g) == 1, length(x) == 1, length(p) == 1)
    stopifnot(floor(g)  == ceiling(g), g >= 1,
              floor(x)  == ceiling(x), x >= 1,
              floor(p)  == ceiling(p), p >= 1)
    if (!isPrime(p))
        stop("Argument 'p' must be a prime number.")
    if (modorder(g, p) != p-1)
        stop("Argument 'g' must be a primitive root mod p.")
    if (mod(x, p) == 0)
        stop("Arguments (x' and 'p' are not coprime.")
    
    q  <- ceiling(-0.5 + sqrt(p+0.25))   # min q*(q+1) >= p
    gq <- modpower(g, q, p)
    Q  <- numeric(q+1); Q[1] <- 1
    for (j in 1:q)
        Q[j+1] <- mod(gq * Q[j], p)
    
    g1 <- modinv(g, p)
    
    for (i in 0:q) {
        gi <- mod(x * modpower(g1, i, p), p)
        k  <- which(gi == Q)
        if (length(k) > 0) {
            n <- (k-1) * q + i
            break
        }
    }
    return(n)
}


modinv <- function(n, m) {
    stopifnot(is.numeric(n), is.numeric(m))
    v <- extGCD(n, m)
    if (v[1] == 0 || v[1] > 1) return(NA)
    if (v[2] >= 0) v[2] else v[2] + m
}


modsqrt <- function(a, p) {
    stopifnot(is.numeric(a), length(a) == 1,
              is.numeric(p), length(p) == 1)
    if (ceiling(a) != floor(a) || a < 0)
        stop("Argument 'a' must be an integer greater or equal 0.")
    if(ceiling(p) != floor(p) || !isPrime(p))
        stop("Argument 'p' must be a prime number.")

    if (a == 0 || p == 2) {
        return(0)
    } else if (legendre_sym(a, p) != 1) {
        return(0)
    } else if (mod(p, 4) == 3) {
        x <- modpower(a, (p+1)/4, p)
        return(min(x, p-x))
    }
    s <- p - 1
    e <- 0
    while (s %% 2 == 0) { s <- s/2; e <- e + 1 }
    n <- 2
    while (legendre_sym(n, p) != -1) { n <- n + 1 }

    x <- modpower(a, (s+1)/2, p)
    b <- modpower(a, s, p)
    g <- modpower(n, s, p)
    r <- e
    while (TRUE) {
        t <- b
        m <- 0
        for (m in (0:(r-1))) {
            if (t == 1) break
            t <- modpower(t, 2, p)
        }
        if (m == 0) break

        gs <- modpower(g, 2^(r-m-1), p)
        g <- (gs * gs) %% p
        x <- (x * gs) %% p
        b <- (b * g) %% p
        r <- m
    }
    return( min(x, p-x))
}
