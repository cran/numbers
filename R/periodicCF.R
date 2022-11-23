#-- --------------------------------------------------------------------
#-- Compute periodic continued fraction
#-- --------------------------------------------------------------------


periodicCF <- function(d) {
    if (! isNatural(d)) stop("Argument 'd' must be a natural number.")
    if (isSquare(d)) {
        warning("Argument 'd' is a square; trivial solution returned.")
        return(c(sqrt(d)))
    }

    # Compute sqrt(d) as periodic continiued fraction
    ds = sqrt(d); dsi = floor(sqrt(d))
    cf = c(dsi)
    s1 = 1; t1 = dsi             # start position

    # Simulate the manual calculation based on the relation
    # 1 / (sqrt(d) - k) = (sqrt(d) + k) / (d - k^2)
    while (TRUE) {
        g = GCD(s1, d-t1^2)
        u = s1/g; v = (d-t1^2)/g        # u shall always be 1 !
        r = floor(u/v * (sqrt(d)+t1))
        cf = c(cf, r)

        s1 = v; t1 = r*v - u*t1         # new generation; break if
        if (s1 == 1 && t1 == dsi) break # start position is reached
    }

    # Return the CF and the length of the period
    plen = length(cf) - 1
    return(list(cf = cf, plen = plen))
 }
























