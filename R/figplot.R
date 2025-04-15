#' Figures for paper
#'
#' Figures
#'
#' @param fig_number figure number
#' @param save TRUE/FALSE? save figure to file?
#' @param file_path path to save file if required
#'
#' @returns empty but plots figure
#'
#' @examples
#'  figplot(1)
#'  for(i in 1:9) figplot(i) # generate all 9 plots from paper (fig 9 is quite slow)
#'
#' @export

figplot <- function(fig_number, save = FALSE, file_path = '~/slalom_rank/new submission/figs/'){

    tt <- ski_data
    tt$labs <- ""
    tt$labs[159] <- "Chamonix: Daniel Yule"

    if(fig_number == 1){
    sp <- ORM::timeplot(tt)
    sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time2, label=labs),
                                   box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                   arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 2){
        sp <- ORM::totaltimeplot(ski_data)
        sp <- sp + ggrepel::geom_label_repel(data = tt, ggplot2::aes(time1,time1 + time2, label=labs), box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
            arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
        sp %>% print
    }

    if(fig_number == 3){
    time <- c()
    races <- unique(ski_data$race) %>% as.character
    for(i in 1:10){
        temp <- subset(ski_data,race==races[i])
        time[i] <- max(temp$time1)-min(temp$time1)
    }

    df <-data.frame(race=races, time=time)
    df$race <- factor(df$race, levels=unique(df$race), order=T)
    sp <- ggplot2::ggplot(df, ggplot2::aes(x = race, xend = race, y = 0, yend = time)) + ggplot2::geom_segment() +
        ggplot2::geom_point(ggplot2::aes(race, time), colour = "steelblue") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, vjust = 0.5, hjust=0)) +  ggplot2::xlab('Race')  + ggplot2::ylab('Time (secs)') +
        ggplot2::ylim(0,5)
    sp %>% print
    }

    if(fig_number == 4){
    mod_fit <- ORM::fit("gum", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
    preds <-  ORM::pred(ski_data,7,mod_fit)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit)
    sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label=labs), box.padding   = 2.5, point.padding = .5, max.overlaps = Inf,
                                         arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) + ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 5){
        mod_fit <- ORM::fit("gum", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
        probs <- c()
        for(i in 1:10) probs[i] <- ORM::pred(ski_data,i,mod_fit, plot=FALSE)[1,5]
        df <- data.frame(race = 1:10, Probability = probs)
        sp <- ggplot2::ggplot(df,  ggplot2::aes(x=race, xend = race, y=0, yend = probs)) +  ggplot2::geom_segment() +
            ggplot2::geom_point(ggplot2::aes(race, probs), colour="steelblue") +
            ggplot2::scale_x_continuous(breaks=1:10,
                               labels= unique(ski_data$race) %>% as.character) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0)) +  ggplot2::xlab("Race") +
            ggplot2::ylab("Race Win Probability")
        sp %>% print
    }

    if(fig_number == 6){
    mod_fit <- ORM::fit("gum", 0, 2, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE,exclude=7)
    preds <-  ORM::pred(ski_data, 7, mod_fit)
    preds$diff <- preds$time1 - min(preds$time1)
    preds$labs <- ""
    preds$labs[1] <- "Daniel Yule"
    sp <- ORM::predplot(ski_data,7,mod_fit)
    sp <- sp + ggrepel::geom_label_repel(data = preds, ggplot2::aes(diff ,prob, label=labs),
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
    sp <- sp + ggrepel::geom_label_repel(data=preds, ggplot2::aes(diff,prob, label = labs), box.padding   = 2.5,
                                   point.padding = .5, max.overlaps = Inf,arrow = ggplot2::arrow(length = grid::unit(0.015, "npc"))) +
        ggplot2::scale_color_brewer(palette = "Paired")
    sp %>% print
    }

    if(fig_number == 8){
    f0 <- ORM::fit("gum", 0, 4, ski_data ,first_Run =FALSE, allranks=FALSE, maxrank=30, if_hessian=TRUE, init=0.3, method="BFGS", print=FALSE)
    f2 <- ORM::fit("gum", 2, 4, ski_data ,first_Run =FALSE, allranks=FALSE, maxrank=30, if_hessian=FALSE, init=0.3, method="BFGS", print=FALSE)
    df <- data.frame(p1=f0$par,p2=f2$par,s1=f0$se)
    df <- cbind(n = 1:11, df)
    df2 <- data.frame(n = rep(1:11, 2), p = c(f0$par, f2$par), method=c(rep("exact", 11), rep("approx", 11)))
    sp <- ggplot2::ggplot() + ggplot2::geom_point(data=df2, ggplot2::aes(x=n, y=p, color = method), size = 2) +
        ggplot2::geom_errorbar(data=df, ggplot2::aes(x=n, ymin = p1 - 1.96 * s1, ymax = p1 + 1.96 * s1), width = 0.2) +
        ggplot2::scale_x_continuous(breaks=1:11,
                           labels= c(unique(ski_data$race) %>% as.character,"Scale")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0, size=10)) +
        ggplot2::xlab("Parameter") + ggplot2::ylab("Estimate")
        sp %>% print
    }

    if(fig_number == 9){
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
            ggplot2::xlab('m') +  ggplot2::ylab('Estimate')+  ggplot2::ggtitle('Regression parameter (Chamonix)')


        fits$upper <- fits$V12 + 1.96 * fits$V23
        fits$lower <- fits$V12 - 1.96 * fits$V23

        p2 <- ggplot2::ggplot(data = fits, ggplot2::aes(x = V1, y = V12) ) +
            ggplot2::geom_line(color="steelblue") + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha=0.2) +
            ggplot2::xlab('m') + ggplot2::ylab('Estimate') + ggplot2::ggtitle('Scale Parameter')

        sp <- gridExtra::grid.arrange(p1, p2, ncol=2)
        sp %>% print
    }
    if(save){
        ggplot2::ggsave(file = paste0(file_path,"fig", fig_number,".pdf"), sp)
    }
    return()
}


