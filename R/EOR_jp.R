#' Calculation of probabilities for the EOR model
#'
#' Calculation of win probabilities for each individual based on the EOR model
#'
#' @param win_inds indices of top ranked individuals
#' @param N total number of racers
#' @param l vector of exponential rates
#' @param a vector of offsets
#
#'
#' @returns win probabilities
#' @examples
#'   EOR_jp(2:6, 10, rep(.1,10), 1:10)
#'
#' @export
#'
EOR_jp <- function(win_inds, N, l, a){
     inds <- c(win_inds, (1:N)[-win_inds])
     l <- l[inds]
     a <- a[inds]
     m <- length(win_inds)
     Q(m, N, l, a)
}
