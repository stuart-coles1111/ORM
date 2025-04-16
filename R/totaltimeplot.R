#' Plots ski data run total and first run times
#'
#' Total time run  first run times per race
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
#' totaltimeplot(ski_data)
#'
#' @export
#'
totaltimeplot <- function(data, point_size = 2, axis_label_size = 12, axis_title_size = 15, legend_text_size = 12){
    ggplot2::ggplot(data = data,  ggplot2::aes(time1, time1 + time2)) +
        ggplot2::geom_point( ggplot2::aes(color = race), size = point_size)  +
        ggplot2::xlab("First run time (secs)") +
        ggplot2::ylab("Total race time (secs)") +
        ggplot2::labs(color = "Race")  +
        ggplot2::scale_shape_manual(values = 1:10)   +
        ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
        ggplot2::theme(legend.title =  ggplot2::element_text(size = legend_text_size),  legend.text =  ggplot2::element_text(size = legend_text_size))
}
