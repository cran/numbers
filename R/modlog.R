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
