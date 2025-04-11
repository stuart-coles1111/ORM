#' Calculation of QQQQ
#'
#' Calculation of log rank joint probabilities for approximate exponential model
#'
#' @param m m
#' @param M N
#' @param l vector of exponential rates
#' @param a vector of offsets
#' @param alpha scale parameter in Gumbel model
#
#'
#' @returns win probabilities
#'
#' @examples
#' QQ(5, 10, rep(.1,10), 1:10, .5)
#'
#' @export
#'
# QG <- function(m, N, l, a, alpha) {
#     s <- gum_mod_probs_log(l[1:2], a[1:2], alpha)[1]
#     if(m >= 2){
#         for(i in 2:m){
#             l <- l[-1]
#             a <- a[-1]
#             s <- s + gum_mod_probs_log(l[1:2], a[1:2], alpha)[1]
#         }
#     }
#     s
# }


QG <- function(m, N, l, a, alpha) {
    s <- 0
    for(i in 1:(m-1)){
        for(j in (i+1):m){
            s <- s + gum_mod_probs_log(l[c(i,j)], a[c(i,j)], alpha)[1]
        }
    }
s
}

