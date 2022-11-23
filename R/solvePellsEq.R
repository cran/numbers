#-- --------------------------------------------------------------------
#   SOLVING PELL'S Equation
#-- --------------------------------------------------------------------


solvePellsEq <- function(d) {
    # Find a minimal, non-trivial solution (x, y) for Pell's equation
    # x^2 - d*y^2 = 1  when d is a positive integer and not a square.

    stopifnot(floor(d) == ceiling(d), d > 0)    # positive integer
    if (d == 1 || isSquare(d)) return(NA)       # no non-trivial solution
    doubled = FALSE
    msg = "Success"

    # Compute the unique and exact periodic continued fraction CF
    # such that sqrt(d) = [a0; (a1, ..., ak)].
    cp = periodicCF(d)
    cf = cp$cf; plen = cp$plen      # plen the length of the period
    if (plen %% 2 == 1) {           # distinguish between odd/even
        cf = c(cf, cf[2:(plen+1)])  # add a second period to the CF
        plen = 2*plen               # if the period length is odd
        doubled = TRUE
    }

    # compute the convergents with the common recursion formula
    n <- length(cf)
    p <- q <- numeric(n)
    p[1] = cf[1]; q[1] = 1
    p[2] = cf[1]*cf[2] + 1; q[2] = cf[2]
    if (n > 2) {
        for (i in 3:n) {
            p[i] = cf[i] * p[i-1] + p[i-2]
            q[i] = cf[i] * q[i-1] + q[i-2]
        }
    }
    
    # select the solution from the convergents
    x = p[plen];  y = q[plen]       # the second last convergents of CF
    maxInt = 2^53 -1
    if (x > maxInt || y > maxInt) msg = "Integer overflow: convergents."
    if (x^2 > maxInt || y^2 > maxInt)
        msg = "Integer overflow: solution check."

    # Missing: length of the period and whether the period of the CF
    # has been doubled (so first a solution = -1 had been found.)
    if (doubled) plen = plen/2

    return(list(x = x, y = y, plen = plen,
                doubled = doubled, msg = msg))
}
