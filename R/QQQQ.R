#' Calculation of QQQQ
#'
#' Calculation of log rank joint probabilities for approximate exponential model
#'
#' @param m m
#' @param M N
#' @param l vector of exponential rates
#' @param a vector of offsets
#
#'
#' @returns win probabilities
#'
#' @examples
#'  QQQQ(5, 10, rep(.1,10), 1:10, .5)
#'
#' @export
#'
QQQQ <- function(m, N, l, a) {
    s <- 0
    for(i in 1:(m-1)){
        for(j in (i+1):m){
            s <- s + EOR_p(1, l[c(i,j)], a[c(i,j)]) %>% log
        }
    }
    s
}





