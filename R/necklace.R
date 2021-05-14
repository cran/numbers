##
##  n e c k l a c e . R  Necklace and bracelet functions
##


necklace <- function(k, n) {
    stopifnot(k == floor(k), k >= 1,
              n == floor(n), n >= 1)
    d <- divisors(n)
    return(1/n * sum(sapply(d, numbers::eulersPhi) * k^(n/d)))
}

bracelet <- function(k, n) {
    stopifnot(k == floor(k), k >= 1,
              n == floor(n), n >= 1)
    if (n%%2 == 0) {
        blet <- 1/2 * necklace(k, n) + 1/4 * (k+1) * k^(n/2)
    } else {
        blet <- 1/2 * necklace(k, n) + 1/2 * k^((n+1)/2)
    }
    return(blet)
}
