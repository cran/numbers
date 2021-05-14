##
##  c o n t F r a c . R  Continuous Fractions
##


contFrac <- function(x, tol = 1e-6) {
    if (!is.numeric(x) || is.matrix(x))
        stop("Argument 'x' must be a numeric scalar or vector.")

    if (length(x) > 1) {
        # Compute value of a continuous fraction
        n <- length(x)
        B <- diag(1, 2)
        for (i in seq(along=x)) {
            B <- B %*% matrix(c(x[i], 1, 1, 0), 2, 2)
        }
        return(B[1,1]/B[2,1])

    } else {
        # Generate the continuous fraction of a value
        sgnx <- sign(x)
        x <- abs(x)

        b <- floor(x)
        k <- b
        r <- x - b
        B <- matrix(c(b, 1, 1, 0), 2, 2)
        while ( abs(x - B[1,1]/B[2,1])  > tol) {
            b <- floor(1/r)
            k <- c(k, b)
            r <- 1/r - b
            B <- B %*% matrix(c(b, 1, 1, 0), 2, 2)
        }
        return(list(cf = sgnx * k, rat = c(sgnx*B[1,1], B[2,1]),
                    prec = abs(x - B[1,1]/B[2,1])))
    }
}


cf2num <- function(a, b = 1, a0 = 0, finite = FALSE) {
    stopifnot(is.numeric(a), is.numeric(b), is.numeric(a0))
    n <- length(a)
    if (length(b) != n) {
        if (length(b) == 1) b <- rep(b, n)
    } else if (length(a) != length(b)) {
        stop("length(a)==length(b) or length(b)==1 required.")
    }
    
    # Calculate CF as an alternating sum
    q <- numeric(n)  # q_{-1} = 0; q_0 = 1
    q[1] <- a[1]; q[2] <- a[2]*a[1] + b[2]*1
    for (j in 3:n) {
        q[j] <-a[j]*q[j-1] + b[j]*q[j-2]
    }
    qq <- c(1, q[1:(n-1)]) * q
    pp <- (-1)^(0:(n-1)) * cumprod(b)
    aa <- pp / qq
    
    if (finite) {
        ss <- sum(aa)
    } else {
    # Apply Algorithm 1 from Cohen et al. (2000)
        bb <- 2^(2 * n - 1)
        cc <- bb
        ss <- 0
        for (k in (n-1):0) {
            tt <- aa[k+1]
            ss <- ss + cc * tt
            bb <- bb * (2 * k + 1) * (k + 1)/(2 * (n - k) * (n + k))
            cc <- cc + bb
        }
        ss <- ss / cc
    }
    # Don't forget the absolute term
    return(a0 + ss)
}

