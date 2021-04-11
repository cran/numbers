##
##  S t e r n - B r o c o t Sequence
##

# Stern-Brocot sequence of length n
stern_brocot_seq <- function(n) {
    # n must be an integer n > 0
    stopifnot(is.numeric(n), floor(n) == ceiling(n), n > 0)
    if (n == 1) return( c(1) )
    if (n == 2) return( c(1,1) )
    S = rep(NA, n)
    S[1] = 1
    for (m in 1:(ceiling((n-2)/2))) {
        S[2*m]   = S[m]
        S[2*m+1] = S[m] + S[m+1]
    }
    if (n%%2 == 0) S[n] = S[n/2]
    S
}


#-----------------------------------------------------------------------
# Calculate the corresponding rational numbers
# 
# stern_brocot_Q <- function(n) {
#     S = stern_brocot(n)
#     Q = numeric(n)
#     for (i in 1:(n-1)) {
#         Q[i] = S[i]/S[i+1]
#     }
#     return( c(0, Q) )
# }
# 
# # Plot the histogram
# Q = stern_brocot_Q(200)
# opar <- par(mar=c(2,2,2,1))
# plot(Q, type='h', col=2, main = "Stern-Brocot Sequence")
# 
# # Plot the Sern-Brocot star
# q <- max(Q)
# plot(c(-q, q), c(-q,q), asp=1, type='n',
#      main = "Stern-Brocot Stern")
# for (i in 1:N) {
#     xy = pracma::pol2cart(c(i*2*pi/N, Q[i]))
#     lines(c(0,xy[1]), c(0, xy[2]), col=2)
# }
# 
#-----------------------------------------------------------------------
# Stern-Brocot Tree
# 
# library(gmp)
# 
# mediant = function(r1, r2) {
#     as.bigq(numerator(r1)+numerator(r2),
#             denominator(r1)+denominator(r2))
# }
# 
# next_row <- function(rw) {
#     n <- length(rw)
#     if (n == 1) return(c(as.bigq(1,2), as.bigq(1,1), as.bigq(2,1)))
#     nrw = as.bigq(rep(NA, 2*n+1))
#     nrw[1]     = as.bigq(numerator(rw[1]), denominator(rw[1])+1)
#     nrw[2*n+1] = as.bigq(numerator(rw[n]+1), denominator(rw[n]))
#     for (i in seq(2, 2*n, by=2)) {
#         nrw[i] = rw[i/2]
#     }
#     for (j in seq(3, 2*n-1, by=2)) {
#         k = floor(j/2)
#         nrw[j] = mediant(rw[k], rw[k+1])
#     }
#     return(nrw)
# }
# 
# # Generate the n-th row of the Stern-Brocot tree
# stern_brocot_tree <- function(n) {
#     # n must be integer >= 1
#     r <- as.bigq(1,1)
#     if (n == 1) return(r)
#     n <- n-1
#     while (n > 0) {
#         r <- next_row(r)
#         n <- n-1
#     }
#     return(r)
# }
