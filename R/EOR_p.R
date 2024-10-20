#' Calculation of probabilities for the EOR model
#'
#' Calculation of win probabilities for each individual based on the EOR model
#'
#' @param data k vector of individual indices
#' @param l  vector of lambda's - exponential rates
#' @param a  vector of offsets

#
#'
#' @returns win probabilities for each s
#' @examples
#'   EOR_p(1:3)
#'
#' @export
#'
EOR_p <- function(k, l = c(2, 1, 3), a = c(.5, 1, 2)){

    # put constants in ascending order and rearrange other variables and recalculate index k

    m <- length(l)
    si <- sort(a, index.return=TRUE)

    k <- which(si$ix==k)
    a <- si$x
    d <- a-a[k]
    d <- c(d, Inf)
    l <- l[si$ix]

    I0 <- l[k] * exp(sum(l[1:(k - 1)]*d[1:(k - 1)]))/sum(l[1:k])*(1-exp(-sum(l[1:k])*d[k + 1]))

    I <- c()

    if(k == m) I <- 0

    else{
        for(j in 1:(m - k)){
            kk <- k + j
            ll <- l[1:kk][-k]
            dd<- d[1:kk][-k]
            I[j] <- l[k] * exp(sum(ll * dd)) /sum(l[1:kk])* (exp(-sum(l[1:kk])*d[kk]) - exp(-sum(l[1:kk])*d[kk + 1]))
            if(is.nan(I[j])) I[j] <- 0
        }
    }
    I0 + sum(I)
}

EOR_p <- Vectorize(EOR_p, vectorize.args="k")


