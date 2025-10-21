#' plots for supplmentary analysis
#'
#' analysis for supplemetary materials
#'
#' @param nrep number of repeated samples
#' @param times model for times
#' @param top number of racers in top group for mixture model
#' @param alpha value of alpha in gumbel model
#' @param ln_alpha value of alpha in lognormal model
#' @param mix_alpha vector of alphas for mixture model
#' @param seed value of seed
#' @param show show results per iteration

#'
#' @returns diagnostic plot
#' @examples
#'   supp_analysis(nrep = 10)
#' @export
#'
supp_analysis <- function(nrep = 1000, times = "gumbel", top = 10, alpha = exp(-1.569), ln_alpha = exp(-1.569),
                          gamma = 1.377, mix_alpha = c(exp(-1.569),10*exp(-1.569)),seed = NULL, show = TRUE){
    if(!is.null(seed)) set.seed(seed)
       source('supp_fncs/fncs.R')
    out_m <- matrix(0, nr = 26, nc=6)

    for(i in 5:30){
        if(show) cat(i, fill = TRUE)
        set.seed(seed)
        if(times == "gumbel")
            out = g(alpha = alpha, gamma = gamma, nrep = nrep, m=i) %>% unlist
        if(times == "mixture")
            out = g_mix(alpha = mix_alpha, gamma = gamma, top = top, nrep = nrep, m=i) %>% unlist
        if(times == "lognormal")
            out = g_ln(alpha = ln_alpha, gamma = gamma,  nrep = nrep, m=i) %>% unlist
        if (show) print(out)
        out_m[i - 4, ] = out %>% unlist
    }

    out_m <- cbind(5:30, out_m)
    out_m <- as.data.frame(out_m)
    colnames(out_m) <- c('m','gamma_bias','log_alpha_bias','gamma_rmse','log_alpha_rmse','gamma_mad','log_alpha_mad')

    p1 <- ggplot(out_m) + geom_line(aes(m, gamma_bias), colour="steelblue") + ggtitle('gamma bias')
    p2 <- ggplot(out_m) + geom_line(aes(m, log_alpha_bias), colour="steelblue")+ ggtitle('log(alpha) bias')
    p1a <- ggplot(out_m) + geom_line(aes(m, abs(gamma_bias)), colour="steelblue")+ ggtitle('gamma absolute bias')
    p2a <- ggplot(out_m) + geom_line(aes(m, abs(log_alpha_bias)), colour="steelblue")+ ggtitle('log(alpha) absolute bias')
    p3 <- ggplot(out_m) + geom_line(aes(m, gamma_rmse), colour="steelblue")+ ggtitle('gamma rmse')
    p4 <- ggplot(out_m) + geom_line(aes(m, log_alpha_rmse), colour="steelblue")+ ggtitle('log(alpha) rmse')
    p5 <- ggplot(out_m) + geom_line(aes(m, gamma_mad), colour="steelblue")+ ggtitle('gamma mad')
    p6 <- ggplot(out_m) + geom_line(aes(m, log_alpha_mad), colour="steelblue")+ ggtitle('log(alpha) mad')

    if(times == "gumbel"){
        p_g <- grid.arrange(p1,p1a,p3,p5,ncol=2)
        p_a <- grid.arrange(p2,p2a,p4,p6,ncol=2)
        return(list(out_m, p_g, p_a))
    }
    if(times != "gumbel"){
        p_g <- grid.arrange(p1,p1a,p3,p5,ncol=2)
        return(list(out_m[,c(1,2,4,6)], p_g))
    }
}



