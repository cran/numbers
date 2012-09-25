##
##  g c d . R  GCD and LCM
##


extGCD <- function(a, b) {
    # The Blankinship method, MAA Mathematical Monthly, Jul-Aug 1963
    stopifnot(is.numeric(a), length(a) == 1, floor(a) == ceiling(a), 
              is.numeric(b), length(b) == 1, floor(b) == ceiling(b))

    sign_ab <- sign(c(a, b))
    A <- matrix(c(abs(c(a, b)), 1, 0, 0, 1), nrow=2, ncol=3)

    while (A[1, 1]*A[2, 1] != 0) {
        if (A[1, 1] > A[2, 1]) {
            m <- A[1, 1] %/% A[2, 1]
            A[1, ] <- A[1, ] - m * A[2, ]
        } else {
            m <- A[2, 1] %/% A[1, 1]
            A[2, ] <- A[2, ] - m * A[1, ]
        }
    }

    if (A[1, 1] == 0)  g <- A[2, ]
    else               g <- A[1, ]

    g[2:3] <- sign_ab * g[2:3]
    return(g)
}


GCD <- function(n, m) return(extGCD(n, m)[1])


LCM <- function(n, m) return(n / GCD(n, m) * m)


coprime <- function(n, m) {
    if (GCD(n, m) > 1) FALSE else TRUE
}
