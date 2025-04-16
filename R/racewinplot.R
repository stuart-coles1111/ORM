#' Plots race  win info
#'
#' Race win info
#'
#' @param data dataframe containing data
#' @param point_size point size
#' @param axis_label_size axis label size
#' @param axis_title_size axis title size
#
#'
#' @returns plot of data
#' @examples
#' racewinplot(ski_data)
#'
#' @export
#'
racewinplot <- function(data, point_size = 3, axis_label_size = 12, axis_title_size = 15){
mod_fit <- ORM::fit("gum", 0, 4, data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
probs <- c()
for(i in 1:10) probs[i] <- ORM::pred(ski_data,i,mod_fit, plot=FALSE)[1,5]
df <- data.frame(race = 1:10, Probability = probs)
sp <- ggplot2::ggplot(df,  ggplot2::aes(x=race, xend = race, y=0, yend = probs)) +  ggplot2::geom_segment() +
    ggplot2::geom_point(ggplot2::aes(race, probs), colour="steelblue", size = point_size) +
    ggplot2::scale_x_continuous(breaks=1:10,
                                labels= unique(ski_data$race) %>% as.character) +
    ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0)) +  ggplot2::xlab("Race") +
    ggplot2::ylab("Race Win Probability")
sp
}


