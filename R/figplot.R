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
    sp <- ORM::timeplot(tt)
    sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time2, label=labs), size = text_size,
                                   box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                   arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 2){
        sp <- ORM::totaltimeplot(ski_data)
        sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time1 + time2, label=labs), size = text_size,
                                             box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
            arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
        sp %>% print
    }

    if(fig_number == 3){
    sp <- ORM::raceplot(ski_data)
    sp %>% print
    }

    if(fig_number == 4){
    mod_fit <- ORM::fit("gum", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
    preds <-  ORM::pred(ski_data, 7 , mod_fit, plot = FALSE)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit)
    sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label=labs), size = text_size,
                                         box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                         arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 5){
        sp <- racewinplot(ski_data)
        sp %>% print
    }

    if(fig_number == 6){
    mod_fit <- ORM::fit("gum", 0, 2, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE,exclude=7)
    preds <-  ORM::pred(ski_data, 7, mod_fit)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit)
    sp <- sp + ggrepel::geom_label_repel(data = preds, ggplot2::aes(diff ,prob, label=labs), size = text_size,
                                   box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                   arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) +
        ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 7){
    mod_fit <-  ORM::fit("gum", 0, 7, ski_data,first_Run =TRUE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE, exclude=7)
    preds <-  ORM::pred(ski_data,7,mod_fit)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit)
    sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label = labs), box.padding   = 2.5,  size = text_size,
                                   point.padding = .5, max.overlaps = Inf,arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) +
        ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 8){
    sp <- likcompar(ski_data)
    sp %>% print
    }

    if(fig_number == 9){
        sp <- mcompar(ski_data)
        sp %>% print
    }
    if(save){
        ggplot2::ggsave(file = paste0(file_path,"fig", fig_number,".pdf"), sp)
    }
    return()
}


