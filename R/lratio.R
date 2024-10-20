#' Utility function for EOR model
#'
#' Calculation of win probabilities for each individual based on the EOR model
#'
#' @param l l
#' @param i1 first index
#' @param i2 second index
#'
#'
lratio <- function(l, i1 = NULL, i2)
    ifelse(is.null(i1), 1/sum(l[i2]), sum(l[i1])/sum(l[i2]))
