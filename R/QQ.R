#' Calculation of QQ
#'
#' Calculation of log rank joint probabilities for Gumbel model
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
#'  QQ(5, 10, rep(.1,10), 1:10, .5)
#'
#' @export
#'
QQ <- function(m, N, l, a, alpha) {
    s <- gum_mod_probs_log(l, a, alpha)[1]
    if(m >=2){
        for(i in 2:m){
            l <- l[-1]
            a <- a[-1]
            s <- s + gum_mod_probs_log(l, a, alpha)[1]
        }
    }
    s
}



