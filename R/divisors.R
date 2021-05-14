##
##  d i v i s o r s . R
##


divisors <- function(n) {
    if (n != floor(n) || n <= 0)
        stop("Argument 'n' must be a nonnegative integer.")
    if (n == 1) {
        return(1)
    } else if (n <= 1000) {
        return( (1:n)[(n %% 1:n) == 0] )
    } else {
        pfs <- rle(primeFactors(n))
        pfs_len <- pfs$length
        pfs_val <- pfs$values

        m <- length(pfs_len)
        D <- pfs_val[1]^c(0:pfs_len[1])
        if (m == 1) return(D)

        for (k in 2:m) {
            D <- c( outer(D, pfs_val[k]^c(0:pfs_len[k])) )
        }
        return( sort(D) )
    }
}
