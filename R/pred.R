#' PredictionEOR and GOR models
#'
#' Predictionof EOR and GOR models
#'
#' @param race_df dataframe of data
#' @param race_number race number to predict
#' @param beta fitted parameters
#' @param gum_model is model gumbel (TRUE/FALSE)
#' @param model_number as defined in object models
#' @param plot plot results (TRUE/FALSE)
#' @param transform transfrom FIS points (TRUE/FALSE)

#'
#' @returns predictions
#' @examples
#'   mod_fit <- fit("gum", 0, 5, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0.1, method="BFGS", print=FALSE)
#'   pred(ski_data, 7, mod_fit)
#' @export
#'
pred <- function(race_df, race_number, fit_object, plot = TRUE, transform = TRUE){

    models <- get("models")
    model_name <- paste0(ifelse(fit_object$model_type == "gum", "gum_model", "exp_model"), fit_object$model_number)
    model <- models[[model_name]]

    beta <- fit_object$par

    if(transform) race_df$points <- log(race_df$points + 1)

    races <- unique(race_df$race)
    n_races <- length(races)
    race_df <- dplyr::arrange(race_df, bib2)

    df <- race_df[race_df$race == races[race_number],]
    nr <- nrow(df)

    eval(parse(text = model$lambda2))
    if(model$type == "gumbel") {
        eval(parse(text = model$alpha_ind))
        eval(parse(text = model$alpha))
    }

    if(model$type == "gumbel")

        pr <-  gum_mod_probs_log(lambda, df$time1, alpha = alpha[race_number]) %>% exp

    else

        pr <- EOR_p(1:nr, lambda, df$time1)

    p_df <- data.frame(surname=df$surname, bib=df$bib2, Run1_pos=df$rank1, points=df$points, prob=pr, time1=df$time1, position = df$position)
    p_df$diff <- p_df$time1 - min(p_df$time1)



    if(plot == TRUE){
        p_plot <- ggplot2::ggplot(p_df, ggplot2::aes(diff, prob)) +
            ggplot2::geom_point(col='steelblue') +
            ggplot2::xlab('Time to leading racer (secs)') + ggplot2::ylab('Race Win Probability')
        p_plot %>% print
    }

    p_df
}
