#' Fit models with defaults from paper
#'
#' Wrapper to fit models with defaults from paper
#'
#' @param model_type model type "gum" for gumbel, "exp" for exponential
#' @param model_number as defined in object models
#' @param print print loglik at each iteration? (TRUE/FALSE)
#'
#' @returns fitted model
#' @examples
#'   fit_paper_model(model_number = 5, model_type = "gum")
#'   fit_paper_model(model_number = 0, model_type = "exp", print = TRUE) # note: all exponential models are slow to fit
#'
#' @export
#'

fit_paper_model <- function(model_number, model_type, print = FALSE){
fit(model_type = model_type, approx = 0, model_number = model_number, ski_data, first_Run =FALSE, allranks=FALSE,
    maxrank=15, if_hessian=FALSE, init=0.1, method="BFGS", print = print)
}
