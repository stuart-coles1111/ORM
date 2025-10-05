#' Compares fits for different numbers of athletes
#'
#' Compares fits for different numbers of athletes
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
#' mcompar(ski_data)
#'
#' @export
#'
mcompar <- function(data, axis_label_size = 12, axis_title_size = 15){
fits <- c()
for(i in 5:30){
    res <- ORM::fit("gum", 0, 4, ski_data ,first_Run =FALSE, allranks=FALSE, maxrank=i, if_hessian=TRUE, init=0.1, method="BFGS", print=FALSE)
    fits <- rbind(fits, c(i, res$par, res$se))
    cat(i, fill=T)
}

fits <- as.data.frame(fits)

fits$upper <- fits$V8 + 1.96 * fits$V19
fits$lower <- fits$V8 - 1.96 * fits$V19

p1 <- ggplot2::ggplot(data = fits,  ggplot2::aes(x = V1, y = V8) ) +  ggplot2::geom_line(color="steelblue") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha=0.2) +
    ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
    ggplot2::xlab('m') +  ggplot2::ylab('Estimate')+  ggplot2::ggtitle('Regression parameter (Chamonix)')


fits$upper <- fits$V12 + 1.96 * fits$V23
fits$lower <- fits$V12 - 1.96 * fits$V23

fits$V12 <- exp(fits$V12)
fits$upper <- exp(fits$upper)
fits$lower <- exp(fits$lower)

p2 <- ggplot2::ggplot(data = fits, ggplot2::aes(x = V1, y = V12) ) +
    ggplot2::geom_line(color="steelblue") + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha=0.2) +
    ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
    ggplot2::xlab('m') + ggplot2::ylab('Estimate') + ggplot2::ggtitle('Scale Parameter')

sp <- gridExtra::grid.arrange(p1, p2, ncol=2)

sp
}
