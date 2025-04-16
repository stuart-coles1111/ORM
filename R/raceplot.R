#' Plots race info
#'
#' Race info
#'
#' @param data dataframe containing data
#' @param point_size point size
#' @param axis_label_size axis label size
#' @param axis_title_size axis title size
#
#'
#' @returns plot of data
#' @examples
#' raceplot(ski_data)
#'
#' @export
#'
raceplot <- function(data, point_size = 3, axis_label_size = 12, axis_title_size = 15){
time <- c()
races <- unique(data$race) %>% as.character
for(i in 1:10){
    temp <- subset(data, race == races[i])
    time[i] <- max(temp$time1) - min(temp$time1)
}

df <-data.frame(race=races, time=time)
df$race <- factor(df$race, levels=unique(df$race), order=T)
ggplot2::ggplot(df, ggplot2::aes(x = race, xend = race, y = 0, yend = time)) + ggplot2::geom_segment() +
    ggplot2::geom_point(ggplot2::aes(race, time), colour = "steelblue", size = point_size ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust=0, size = axis_label_size)) +
    ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
    ggplot2::xlab('Race')  + ggplot2::ylab('Time (secs)') +
    ggplot2::ylim(0,5)
}
