##
##  i n t h r o o t . R  Integer N-th Root
##


iNthroot <- function(p, n) {
    if (!isNatural(n))
        stop("Argument 'p' must be a natural number.")
    if (p <= 0)
        stop("Argument 'p' must be a positive number.")

    x <- 2^ceiling(log2(p+1)/n)
    while(TRUE) {
        y <- floor(((n-1)*x + floor(p/x^(n-1)))/n)
        if (y >= x) break
        x <- y
    }
    return(x)
}
