#' Plots ski data run total and first run times
#'
#' Total time run  first run times per race
#'
#' @param data dataframe containing data
#
#'
#' @returns plot of data
#' @examples
#' totaltimeplot(ski_data)
#'
#' @export
#'
totaltimeplot <- function(data){
    ggplot2::ggplot(data = data,  ggplot2::aes(time1 + time2 , time2)) +
        ggplot2::geom_point( ggplot2::aes(color = race))  +
        ggplot2::xlab("First run time (secs)") +
        ggplot2::ylab("Total race time (secs)") +
        ggplot2::labs(color = "Event")  +
        ggplot2::geom_abline(intercept = 0, slope = 1) +
        ggplot2::scale_shape_manual(values = 1:10)   +
        ggplot2::theme(axis.text =  ggplot2::element_text(size = 10), axis.title =  ggplot2::element_text(size = 12)) +
        ggplot2::guides(shape =  ggplot2::guide_legend(override.aes = list(size = 1))) +
        ggplot2::theme(legend.title =  ggplot2::element_text(size = 8),  legend.text =  ggplot2::element_text(size = 8))
}
