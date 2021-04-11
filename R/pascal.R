##
##  P a s c a l  T r i a n g l e
##


pascal_triangle <- function(n) {
    stopifnot(is.numeric(n))
    if (length(n) != 1 || floor(n) != ceiling(n) || n < 0)
        stop("Argument 'n' must be a single integer.")
    if (n > 50)
        stop("Integer overflow: Argument 'n' must be <= 50.", call. = FALSE)
    P <- matrix(0, nrow = n+1, ncol = n+1)
    P[1, 1] <- 1
    if (n == 0) return(P)
    for (i in 1:n) {
        j <- i+1
        P[j, 1] <- 1
        P[j, 2:j] <- P[i, 1:i] + P[i, 2:j]
    }
    return(P)
}
