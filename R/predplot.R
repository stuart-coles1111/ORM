#' Prediction plots
#'
#' Prediction plots
#'
#' @param data dataframe containing data
#' @param point_size point size
#' @param axis_label_size axis label size
#' @param axis_title_size axis title size
#' @param legend_text_size legend text size
#
#'
#' @returns plot of data
#' @examples
#'   mod_fit <- fit("gum", 5, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0.1, method="BFGS", print=FALSE)
#'   pred(ski_data, 7, mod_fit)#'
#' @export
#'

predplot <- function(race_df, race_number, fit_object, plot = TRUE, transform = TRUE, point_size = 2, axis_label_size = 12, axis_title_size = 15, legend_text_size = 12){

    models <- get("models")
    model_name <- paste0(ifelse(fit_object$model_type == "gum", "gum_model", "exp_model"), fit_object$model_number)
    model <- models[[model_name]]

    beta <- fit_object$opt$par

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

    p_df <- data.frame(surname=df$surname, bib=df$bib2, Run1_pos=df$rank1, points=df$points, prob=pr, time1=df$time1)
    p_df$diff <- p_df$time1 - min(p_df$time1)



    if(plot == TRUE){
        p_plot <- ggplot2::ggplot(p_df, ggplot2::aes(diff, prob)) +
            ggplot2::geom_point(col='steelblue', size = point_size) +
            ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
            ggplot2::xlab('Time to leading racer (secs)') + ggplot2::ylab('Race Win Probability')
    }
    return(p_plot)
}


