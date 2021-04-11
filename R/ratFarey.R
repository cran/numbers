##
##  r a t F a r e y . R  Farey Approximation
##


ratFarey <- function(x, n, upper = TRUE) {
    sgn <- sign(x); x <- abs(x)
    xn <- trunc(x); x <- x - xn

    F1 <- c(0, 1); f1 <- F1[1]/F1[2]    # Farey series
    F2 <- c(1, 1); f2 <- F2[1]/F2[2]    #  and mediant
    repeat {
        M <- c(F1[1] + F2[1], F1[2] + F2[2]) 
        m <- M[1]/M[2] 
        if (M[2] > n) {
            if (upper) {
                F <- F2; break
            } else {
                if (x < (f1+f2)/2) {F <- F1; break}
                else               {F <- F2; break}
            }
        } else {
            if (x < m) {
                F2 <- M
                f2 <- F2[1]/F2[2]
            } else if (x > m) {
                F1 <- M
                f1 <- F1[1]/F1[2]
            } else {
                {F <- M; break}
            }
        }
    }
    F[1] <- sgn * (xn * F[2] + F[1])

    return(F)
}


farey_seq <- function(n) {
    stopifnot(is.numeric(n), length(n) == 1, 
              floor(n) == ceiling(n), n >= 1)
    if (!requireNamespace("gmp", quietly = TRUE)) {
        stop("Package 'gmp' needed: Please install separately.",
             call. = FALSE)
    }
    F <- c(gmp::as.bigq(c(0)), gmp::as.bigq(1))
    if (n == 1) return( F )
    for (k in 2:n) {
        for (h in 1:(k-1)) {
            if (gmp::gcd(h, k) == 1) {
                r <- gmp::as.bigq(h, k)
                F <- c(F, r)
            }
        }
    }
    return( sort(F) )
}
