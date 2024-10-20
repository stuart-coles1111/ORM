#' Utility function for EOR model
#'
#' Calculation of win probabilities for each individual based on the EOR model
#'
#' @param l lambda
#' @param a offsets
#' @param i1 first index
#' @param i2 second index
#'
lexp <- function(l, a, i1 , i2) exp(- sum(l[i1])*(a[i2[1]] - a[i2[2]]))
