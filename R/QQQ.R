#' Calculation of QQQ
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
#'  QQQ(5, 10, rep(.1,10), 1:10, .5)
#'
#' @export
#'
QQQ <- function(m, N, l, a) {
    s <- EOR_p(1, l, a) %>% log
    if(m >= 2){
        for(i in 2:m){
            l <- l[-1]
            a <- a[-1]
            s <- s + EOR_p(1, l, a) %>% log
        }
    }
    s
}



