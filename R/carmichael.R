##
##  C a r m i c h a e l  n u m b e r s
##


carmichael <- function(n) {
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n >= 1)
    if (n == 1 || n %% 2 == 0 || isPrime(n)) return(FALSE)
    P <- primeFactors(n)

    # first criterion: square-free
    if (anyDuplicated(P) > 0)
        return(FALSE)

    # second criterion: p|n ==> (p-1)|(n-1)
    return_value <- TRUE
    for (p in unique(P)) {
        if ((n-1) %% (p-1) != 0) {
            return_value <- FALSE
            break
        }
    }
    return_value
}
