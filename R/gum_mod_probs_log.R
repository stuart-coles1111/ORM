#' Log probabilities of Gumbel rank model
#'
#' Calculation of Log probabilities of Gumbel rank model
#'
#' @param b b
#' @param a a
#' @param alpha  alpha
#'
#'
gum_mod_probs_log  <- function(b = c(1, 2 ,3), a = c(0, 0, 0), alpha = 1){
    -(b + a) / alpha  - log(sum(exp(-(b + a) / alpha)))
}
