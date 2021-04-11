##
##   b e r n o u l l i _ n u m b e r s
##


bernoulli_numbers <- function(n, big = FALSE) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 0)
    if (! big) {
        # double integers n <= 24
        if (n > 24)
            stop("Integer overflow: use GMP big integers 'big=TRUE'.")
        A <- B <- matrix(0, nrow=n+1, ncol=2)
        if (n == 0) return((1))
        A[1, ] <- c(1, 1)
        for (i in 1:n) {
            A[i+1, ] <- c(1, i+1)
            for (j in i:1) {
                r <- c(A[j, 1]*A[j+1,2] - A[j, 2]*A[j+1, 1], A[j, 2]*A[j+1, 2])
                g <- GCD(r[1], r[2]); if (g > 1) r <- r/g
                g <- GCD(j, r[2]);    r <- c(j/g*r[1], r[2]/g)
                A[j, ] <- r
            }
            B[i, ] <- A[1, ]
        }
        return( rbind(c(1, 1), B[1:n, ]) )

    } else {
        # GMP big rationals
        if (!requireNamespace("gmp", quietly = TRUE)) {
            stop("Package 'gmp' needed: Please install separately.",
                 call. = FALSE)
        }
        B <- gmp::BernoulliQ(0:n)
        cbind(gmp::numerator(B), gmp::denominator(B))
    }
}
