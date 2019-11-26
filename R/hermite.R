##
##  h e r m i t e . R  Hermite Normal Form
##


hermiteNF <- function(A) {
    stopifnot(is.numeric(A))
    if ( !is.matrix(A) || any(floor(A) !=ceiling(A)) )
        stop("Argument 'A' must be an integer matrix.")

    # Initialize Hermite and unitary matrix
    m <- nrow(A); n <- ncol(A)
    r <- min(m, n); p <- 0      # location of pivot element
    H <- A; U <- diag(1, n)

    for (i in 1:r) {
        S <- which( H[i, (p+1):n] != 0 )
        if (length(S) != 0) {
            p <- p + 1
            finished <- FALSE
            while (!finished) {
                S <- p - 1 + which(H[i, p:n] != 0)
                k <- which.min(abs(H[i, S]))  # find smallest non-zero entry
                if (S[k] != p) {
                    index <- 1:n; index[p] <- S[k]; index[S[k]] <- p
                    H <- H[, index, drop=FALSE]  # exchange columns i and k
                    U <- U[, index, drop=FALSE]  # exchange columns i and k
                }
                if (p < n) {
                    for (j in (p+1):n) {
                        q <- round(H[i,j]/H[i,p])
                        H[, j] <- H[, j, drop=FALSE] - q * H[, p, drop=FALSE]
                        U[ ,j] <- U[, j, drop=FALSE] - q * U[, p, drop=FALSE]
                    }
                }
                if ( p >= n || length(which(H[i, (p+1):n] != 0)) == 0 ) {
                    finished <- TRUE
                    # flip the sign of H[i, ] if necessary
                    if (H[i, p] < 0) {
                        H[, p] <- -H[, p, drop=FALSE]
                        U[, p] <- -U[, p, drop=FALSE]
                    }
                    # reduce the entries to the left of H[i, i]
                    if (p > 1) {
                        for (j in 1:(p-1)) {
                            q <- floor(H[i, j] / H[i, p])
                            H[, j] <- H[, j, drop=FALSE] - q * H[, p, drop=FALSE]
                            U[, j] <- U[, j, drop=FALSE] - q * U[, p, drop=FALSE]
                        }
                    }
                }   # end if
            }   # end while
        }   # end if
    }   # end for

    return(list(H = H, U = U))
}