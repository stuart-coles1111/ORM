#' Compares exact and approx lik fits
#'
#' Compares exact and approx lik fits
#'
#' @param data data
#' @param point_size point size
#' @param axis_label_size axis label size
#' @param axis_title_size axis title size
#' @param legend_text_size legend text size
#
#'
#' @returns plot of data
#' @examples
#' likcompar(ski_data)
#'
#' @export
#'



likcompar <- function(data, point_size = 2, axis_label_size = 12, axis_title_size = 15, legend_text_size = 12){
    f0 <- ORM::fit("gum", 0, 4, data ,first_Run =FALSE, allranks=FALSE, maxrank=30, if_hessian=TRUE, init=0.3, method="BFGS", print=FALSE)
    f2 <- ORM::fit("gum", 2, 4, data ,first_Run =FALSE, allranks=FALSE, maxrank=30, if_hessian=FALSE, init=0.3, method="BFGS", print=FALSE)
    df <- data.frame(p1=f0$par,p2=f2$par,s1=f0$se)
    df <- cbind(n = 1:11, df)
    df2 <- data.frame(n = rep(1:11, 2), p = c(f0$par, f2$par), Method=c(rep("Exact", 11), rep("Approx", 11)))
    df2[11, 2] <- exp(df2[11, 2])
    df2[22, 2] <- exp(df2[22, 2])
    df$cl <- f0$par - 1.96 * f0$se
    df$cu <- f0$par + 1.96 * f0$se
    df$cl[11] <- exp(df$cl[11])
    df$cu[11] <- exp(df$cu[11])

    sp <- ggplot2::ggplot() + ggplot2::geom_point(data=df2, ggplot2::aes(x=n, y=p, color = Method), size = point_size) +
        ggplot2::geom_errorbar(data=df, ggplot2::aes(x=n, ymin = cl, ymax = cu), width = 0.2) +
        ggplot2::scale_x_continuous(breaks=1:11,
                                    labels= c(unique(ski_data$race) %>% as.character,"Scale")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0)) +
        ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
        ggplot2::theme(legend.title =  ggplot2::element_text(size = legend_text_size),  legend.text =  ggplot2::element_text(size = legend_text_size)) +
        ggplot2::xlab("Parameter") + ggplot2::ylab("Estimate")
    sp
}

