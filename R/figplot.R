#' Figures for paper
#'
#' Figures
#'
#' @param fig_number figure number
#' @param save TRUE/FALSE? save figure to file?
#' @param file_path path to save file if required
#' @param text_size text size for labels
#'
#' @returns empty but plots figure
#'
#' @examples
#'  figplot(1)
#'  for(i in 1:9) figplot(i) # generate all 9 plots from paper (fig 9 is quite slow)
#'
#' @export

figplot <- function(fig_number, save = FALSE, file_path = '~/slalom_rank/new submission/figs/', text_size = 5){

    tt <- ski_data
    tt$labs <- ""
    tt$labs[159] <- "Chamonix: Daniel Yule"

    if(fig_number == 1){
    #        text_size = 5; point_size = 2; axis_label_size = 12; axis_title_size = 15; legend_text_size = 12
    text_size = 5; point_size = 3; axis_label_size = 15; axis_title_size = 18; legend_text_size = 12
    sp <- ORM::timeplot(tt, point_size = point_size, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
    sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time2, label=labs), size = text_size,
                                   box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                   arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 2){
#        text_size = 5; point_size = 2; axis_label_size = 12; axis_title_size = 15; legend_text_size = 12
        text_size = 5; point_size = 3; axis_label_size = 15; axis_title_size = 18; legend_text_size = 12
        sp <- ORM::totaltimeplot(tt, point_size = point_size, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
        sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time1 + time2, label=labs), size = text_size,
                                             box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
            arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
        sp %>% print
    }

    if(fig_number == 3){
    point_size = 4; axis_label_size = 12; axis_title_size = 15
    sp <- ORM::raceplot(ski_data, point_size = point_size, axis_label_size = axis_label_size, axis_title_size = axis_title_size)
    sp %>% print
    }

    if(fig_number == 4){
    point_size = 4; axis_label_size = 15; axis_title_size = 18; legend_text_size = 15
    mod_fit <- ORM::fit("gum", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
    preds <-  ORM::pred(ski_data, 7 , mod_fit, plot = FALSE)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
    sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label=labs), size = text_size,
                                         box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                         arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 5){
        point_size = 4; axis_label_size = 12; axis_title_size = 15
        sp <- racewinplot(ski_data, point_size = point_size, axis_label_size = axis_label_size, axis_title_size = axis_title_size)
        sp %>% print
    }

    if(fig_number == 6){
        sp <- mcompar(ski_data)
        sp %>% print
    }


    if(fig_number == 7){
        point_size = 2; axis_label_size = 12; axis_title_size = 15; legend_text_size = 12
        sp <- likcompar(ski_data, point_size = point_size, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
        sp %>% print
    }

    # following plots not included in final version of paper


    # if(fig_number == 8){
    # point_size = 4; axis_label_size = 15; axis_title_size = 18; legend_text_size = 15
    # mod_fit <- ORM::fit("gum", 0, 2, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE,exclude=7)
    # preds <-  ORM::pred(ski_data, 7, mod_fit)
    # preds$diff <- preds$time1 - min(preds$time1)
    # preds$labs <- ""
    # preds$labs[1] <- "Daniel Yule"
    # sp <- ORM::predplot(ski_data,7,mod_fit, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
    # sp <- sp + ggrepel::geom_label_repel(data = preds, ggplot2::aes(diff ,prob, label=labs), size = text_size,
    #                                box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
    #                                arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) +
    #     ggplot2::scale_color_brewer(palette = "Paired")
    # sp %>% print
    # }
    #
    # if(fig_number == 9){
    # point_size = 4; axis_label_size = 15; axis_title_size = 18; legend_text_size = 15
    # mod_fit <-  ORM::fit("gum", 0, 7, ski_data,first_Run =TRUE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE, exclude=7)
    # preds <-  ORM::pred(ski_data,7,mod_fit)
    # preds$diff <- preds$time1 - min(preds$time1)
    # preds$labs <- ""
    # preds$labs[1] <- "Daniel Yule"
    # sp <- ORM::predplot(ski_data,7,mod_fit, axis_label_size = axis_label_size, axis_title_size = axis_title_size, legend_text_size = legend_text_size)
    # sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label = labs), box.padding   = 2.5,  size = text_size,
    #                                point.padding = .5, max.overlaps = Inf,arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) +
    #     ggplot2::scale_color_brewer(palette = "Paired")
    # sp %>% print
    # }


    if(save){
        ggplot2::ggsave(file = paste0(file_path,"fig", fig_number,".jpeg"), sp)
    }
    return()
}


