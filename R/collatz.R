collatz <- function(n, k = 3, l = 1, short = FALSE, check = TRUE) {
    stopifnot(length(n) == 1, floor(n) == ceiling(n), n >= 1, 
              length(k) == 1, floor(k) == ceiling(k), k >= 3,
              length(l) == 1, floor(l) == ceiling(l), l != 0,
              is.logical(short), is.logical(check))
    if ((k %% 2 != 1 && l %% 2 == 0) ||
        (k %% 2 == 0 && l %% 2 == 1))
        stop("Arguments 'k' and 'l' must be both even or both odd integers.")
    if (k == 3 && l == 1) check <- FALSE
    m <- n; cseq = c(n)
    while (n > 1) {
        if (n %% 2 == 0) {
            n <- n/2
        } else {
            n <- k*n+l
            if (n > 2^53-1) {
                # cat(cseq, "\n")
                cat("Info:", m, "-->", n,
                    "too big after", length(cseq), "steps.\n")
                stop("Integer overflow, i.e. greater than 2^53-1")
            }
            if (short) n <- n/2
        }
        if (check) {
            if (length(which(cseq == n)) > 0) {  # cycle found
                cat("Found a non-trivial cycle for n =", m, "!\n")
                cseq <- c(cseq, n)
                break
            }
        }
        cseq <- c(cseq, n)
    }
    return (cseq)
}
