#' Fit of EOR and GOR models
#'
#' Fit of EOR and GOR models
#'
#' @param model_type model type "gum" for gumbel, "exp" for exponential
#' @param exp_approx (TRUE/FALSE) approximate exponential model?
#' @param model_number as defined in object models
#' @param race_df dataframe of data
#' @param first_Run model first run (TRUE/FALSE)
#' @param second_Run model second run (TRUE/FALSE)
#' @param allranks model all ranks (TRUE/FALSE)
#' @param maxrank if all ranks = FALSE, how =many ranks to model?
#' @param exclude_second_Run vector of race numbers to exclude from second run
#' @param if_hessian return hessian (TRUE/FALSE)
#' @param init initial value for all parameters
#' @param method optimisation method
#' @param transform transfrom FIS points (TRUE/FALSE)
#' @param print print iteration values (TRUE/FALSE)

#'
#' @returns fitted model
#' @examples
#'   fit(model_type = "gum", exp_approx = 0, model_number = 5, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0.1, method="BFGS", print=FALSE)
#'
#' @export
#'
fit <- function(model_type = "gum", exp_approx = 0, model_number = 1, race_df = ski_data, first_Run = TRUE, second_Run = TRUE, allranks = TRUE, maxrank = 10,  exclude_second_Run=NULL, if_hessian = FALSE, init = 0, method = "BFGS", transform = TRUE, print = FALSE){

    models <- get("models")
    model_name <- paste0(ifelse(model_type == "gum", "gum_model", "exp_model"), model_number)
    model <- models[[model_name]]

    if(transform) race_df$points <- log(race_df$points + 1)

    n_races <- length(unique(race_df$race))

    opt <- optim(rep(init, eval(parse(text=model$npar))), race_nllh, race_df = race_df, first_Run = first_Run, second_Run = second_Run, maxrank = maxrank, allranks = allranks,  exclude_second_Run = exclude_second_Run, model_type = model_type, exp_approx = exp_approx, model_number = model_number, control = list(maxit = 5000),method=method, hessian = if_hessian, print = print)

    if(if_hessian)
#        se <- opt$hessian %>% solve %>% diag %>% sqrt
    se <- opt$hessian %>% solve %>% diag %>% sqrt

    else
        se <- NULL

    list(model_type = model_type, model_number = model_number, opt = opt, par = opt$par, se = se)
}
