
 temp =  function(df, axis_label_size = 12, axis_title_size = 15, true_reg = 1.377, true_log_alpha = -1.569){
        fits <- c()
        pp1<-c()
        pp2<-c()
        for(i in 5:30){
            res <- ORM::fit("gum", 0, 2, df ,first_Run =FALSE, allranks=FALSE, maxrank=i, if_hessian=TRUE, init=0.1, method="BFGS", print=FALSE)
            fits <- rbind(fits, c(i, res$par, res$se))
            pp <- pred(df, 1, res, FALSE, FALSE)
            pp1[i-4]<-pp[1,"prob"]
            pp2[i-4]<-pp[30,"prob"]
            cat(i, fill=T)

        }
        fits <- as.data.frame(fits)

        fits$upper <- fits$V2 + 1.96 * fits$V4
        fits$lower <- fits$V2 - 1.96 * fits$V4

        p1 <- ggplot2::ggplot(data = fits,  ggplot2::aes(x = V1, y = V2) ) +  ggplot2::geom_line(color="steelblue") +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha=0.2) +
            ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
            ggplot2::xlab('m') +  ggplot2::ylab('Estimate')+  ggplot2::ggtitle('Regression parameter (Chamonix)') + geom_hline(yintercept = true_reg, colour="red")



        fits$upper <- fits$V3 + 1.96 * fits$V5
        fits$lower <- fits$V3 - 1.96 * fits$V5

        pred1 <- pred(df, 1, res, FALSE, FALSE)

        p2 <- ggplot2::ggplot(data = fits, ggplot2::aes(x = V1, y = V3) ) +
            ggplot2::geom_line(color="steelblue") + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha=0.2) +
            ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
            ggplot2::xlab('m') + ggplot2::ylab('Estimate') + ggplot2::ggtitle(' Log Scale Parameter')+ geom_hline(yintercept = true_log_alpha, colour="red")




        fits$pp1 <- pp1
        fits$pp2 <- pp2

        res_true <- res
        res_true$par <-  c(true_reg, true_log_alpha)

        true_pp1<-  pred(df, 1, res_true, FALSE, FALSE)[1,"prob"]
        true_pp2<-  pred(df, 1, res_true, FALSE, FALSE)[30,"prob"]

        p3<- ggplot2::ggplot(data = fits, ggplot2::aes(x = V1, y = pp1) ) +
            ggplot2::geom_line(color="steelblue") +
            ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
            ggplot2::xlab('m') + ggplot2::ylab('Probability') + ggplot2::ggtitle('1st run: 30th') + geom_hline(yintercept = true_pp1, colour="red") + ylim(0,1)

        p4<- ggplot2::ggplot(data = fits, ggplot2::aes(x = V1, y = pp2) ) +
            ggplot2::geom_line(color="steelblue") +
            ggplot2::theme(axis.text =  ggplot2::element_text(size = axis_label_size), axis.title =  ggplot2::element_text(size = axis_title_size)) +
            ggplot2::xlab('m') + ggplot2::ylab('Probability') + ggplot2::ggtitle('1st run: 1st') + geom_hline(yintercept = true_pp2, colour="red")+ ylim(0,1)

                sp <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)

        sp
   }

g_r_sim <- function(nracers = 30, points = fis_points, alpha=.5, beta = 5, gamma= 0, order1 = TRUE, order2 = TRUE){
# note only designed for when beta = 0. then picks first run times from chamonix
    df <- data.frame(racer=1:nracers, points=points)
    if(order1) {
        df <- arrange(df, points)
    }
    df$bib1 <- 1:nracers
#    df$time1 <- alpha*log(rexp(nracers,1)) + beta*df$points + gamma * df$bib1 / nracers
     df$time1 <- subset(ski_data, number== "Race 7")$time1
    if(order2){
        df <- arrange(df, desc(time1))
        df$bib2 <- 1:nracers
    }
    else{
        df$bib2 <- sample(1:nracers, nracers,replace=FALSE)
    }
    df$time2 <- alpha*log(rexp(nracers,1)) + beta*df$points + gamma * df$bib2 / nracers
    df$total_time <- df$time1+df$time2
    df$position <- rank(df$total_time)
    arrange(df,position)
}


g_r_sim_mix <- function(nracers = 30, points = fis_points, alpha=c(.5,.5), gamma= 0, order1 = TRUE, order2 = TRUE, top=10){
    # note only designed for when beta = 0. then picks first run times from chamonix
    df <- data.frame(racer=1:nracers, points=points)
    if(order1) {
        df <- arrange(df, points)
    }
    df$bib1 <- 1:nracers
    #    df$time1 <- alpha*log(rexp(nracers,1)) + gamma * df$bib1 / nracers
    df$time1 <- subset(ski_data, number== "Race 7")$time1
    if(order2){
        df <- arrange(df, desc(time1))
        df$bib2 <- 1:nracers
    }
    else{
        df$bib2 <- sample(1:nracers, nracers,replace=FALSE)
    }

    df$time2 <- rep(0,nracers)
    df$time2[1:(nracers-top)] <- alpha[2]*log(rexp(nracers-top,1)) + gamma * df$bib2[1:(nracers-top)] / nracers
    df$time2[(nracers-top+1):nracers] <- alpha[1]*log(rexp(top)) + gamma * df$bib2[(nracers-top+1):nracers] / nracers
    df$total_time <- df$time1+df$time2
    df$position <- rank(df$total_time)
    arrange(df,position)
}

g_r_sim_ln <- function(nracers = 30, points = fis_points, alpha=.5, gamma= 0, order1 = TRUE, order2 = TRUE){
    # note only designed for when beta = 0. then picks first run times from chamonix. lognormal version
    df <- data.frame(racer=1:nracers, points=points)
    if(order1) {
        df <- arrange(df, points)
    }
    df$bib1 <- 1:nracers
    #    df$time1 <- alpha*log(rexp(nracers,1)) + gamma * df$bib1 / nracers
    df$time1 <- subset(ski_data, number== "Race 7")$time1
    if(order2){
        df <- arrange(df, desc(time1))
        df$bib2 <- 1:nracers
    }
    else{
        df$bib2 <- sample(1:nracers, nracers,replace=FALSE)
    }

    df$time2 <- alpha*exp(rnorm(nracers,0,1)) + gamma * df$bib2 / nracers
    df$total_time <- df$time1+df$time2
    df$position <- rank(df$total_time)
    arrange(df,position)
}


runsim<-function(seed){
set.seed(seed)
df = g_r_sim(alpha=exp(-1.569),beta=0,gamma=1.377)
df$race=1
df$surname=1:30
df$rank1=rank(df$time1)
temp(df)
}

runsim_mix<-function(seed, alpha = c(exp(-1.569), exp(-1)), gamma=1.377, top=10){
    set.seed(seed)
    df = g_r_sim_mix(alpha=alpha,gamma=gamma, top=top)
    df$race=1
    df$surname=1:30
    df$rank1=rank(df$time1)
    temp(df)
}

runsim_ln<-function(seed, alpha = exp(-1.569), gamma=1.377){
    set.seed(seed)
    df = g_r_sim_ln(alpha=alpha,gamma=gamma)
    df$race=1
    df$surname=1:30
    df$rank1=rank(df$time1)
    temp(df)
}

rmse<-function(x, x0) sqrt(mean((x-x0)^2))
bias<-function(x, x0) mean(x-x0)
mad<-function(x, x0) median(abs(x-x0))

g <- function(alpha=exp(-1.569),gamma=1.377, m = 15, nrep=1000, init=1.5, plot = FALSE){
    res_mat<-matrix(0,nr=nrep,ncol=2)
for(i in 1:nrep){
    flag <-1
    while(flag != 0){
    df = g_r_sim(alpha=alpha,beta=0,gamma=gamma)
    df$race=1
    df$surname=1:30
    df$rank1=rank(df$time1)
    res <- try(
        ORM::fit("gum", 0, 2, df ,first_Run =FALSE, allranks=FALSE, maxrank=m, if_hessian=FALSE, init=init, method="BFGS", print=FALSE),
        silent = TRUE)
    if (!inherits(res, "try-error")) {
        res_mat[i,] <- res$par
        flag <- res$opt$convergence
    }
    else{
        flag < - 1
    }
    }
#    cat(i,fill=TRUE)

    }
    bias1 <- bias(res_mat[,1], gamma)
    bias2 <- bias(res_mat[,2], log(alpha))

    rmse1 <- rmse(res_mat[,1], gamma)
    rmse2 <- rmse(res_mat[,2], log(alpha))

    mad1 <- mad(res_mat[,1], gamma)
    mad2 <- mad(res_mat[,2], log(alpha))
    if(plot){
    df <- data.frame(gamma=res_mat[,1], log_alfa = res_mat[,2])
    p1 <- ggplot(df) + geom_point(aes(gamma,log_alfa)) + geom_hline(yintercept = log(alpha), colour="red") + geom_vline(xintercept = gamma, colour = "red")
    print(p1)
}
    list(c(bias1, bias2),c(rmse1, rmse2),c(mad1, mad2))
}

g_mix <- function(alpha=c(exp(-1.569),exp(-1)), gamma=1.377, top = 10, m = 15, nrep=1000, init=1.5, plot = FALSE, updates = FALSE){
    res_mat<-matrix(0,nr=nrep,ncol=2)
    for(i in 1:nrep){
        flag <-1
        while(flag != 0){
            df = g_r_sim_mix(alpha=alpha,gamma=gamma,top=top)
            df$race=1
            df$surname=1:30
            df$rank1=rank(df$time1)
            res <- try(
                ORM::fit("gum", 0, 2, df ,first_Run =FALSE, allranks=FALSE, maxrank=m, if_hessian=FALSE, init=init, method="BFGS", print=FALSE),
                silent = TRUE)
            if (!inherits(res, "try-error")) {
                res_mat[i,] <- res$par
                flag <- res$opt$convergence
            }
            else{
                flag < - 1
            }
        }
        if(updates)    cat(i,fill=TRUE)

    }
    bias1 <- bias(res_mat[,1], gamma)
    bias2 <- bias(res_mat[,2], log(alpha))

    rmse1 <- rmse(res_mat[,1], gamma)
    rmse2 <- rmse(res_mat[,2], log(alpha))

    mad1 <- mad(res_mat[,1], gamma)
    mad2 <- mad(res_mat[,2], log(alpha))
    if(plot){
        df <- data.frame(gamma=res_mat[,1], log_alfa = res_mat[,2])
        p1 <- ggplot(df) + geom_point(aes(gamma,log_alfa)) + geom_hline(yintercept = log(alpha), colour="red") + geom_vline(xintercept = gamma, colour = "red")
        print(p1)
    }
    list(c(bias1, bias2),c(rmse1, rmse2),c(mad1, mad2))
}

g_ln <- function(alpha=exp(-1.569), gamma=1.377, m = 15, nrep=1000, init=1.5, plot = FALSE, updates = FALSE){
    res_mat<-matrix(0,nr=nrep,ncol=2)
    for(i in 1:nrep){
        flag <-1
        while(flag != 0){
            df = g_r_sim_ln(alpha=alpha,gamma=gamma)
            df$race=1
            df$surname=1:30
            df$rank1=rank(df$time1)
            res <- try(
                ORM::fit("gum", 0, 2, df ,first_Run =FALSE, allranks=FALSE, maxrank=m, if_hessian=FALSE, init=init, method="BFGS", print=FALSE),
                silent = TRUE)
            if (!inherits(res, "try-error")) {
                res_mat[i,] <- res$par
                flag <- res$opt$convergence
            }
            else{
                flag < - 1
            }
        }
        if(updates)    cat(i,fill=TRUE)

    }
    bias1 <- bias(res_mat[,1], gamma)
    bias2 <- bias(res_mat[,2], log(alpha))

    rmse1 <- rmse(res_mat[,1], gamma)
    rmse2 <- rmse(res_mat[,2], log(alpha))

    mad1 <- mad(res_mat[,1], gamma)
    mad2 <- mad(res_mat[,2], log(alpha))
    if(plot){
        df <- data.frame(gamma=res_mat[,1], log_alfa = res_mat[,2])
        p1 <- ggplot(df) + geom_point(aes(gamma,log_alfa)) + geom_hline(yintercept = log(alpha), colour="red") + geom_vline(xintercept = gamma, colour = "red")
        print(p1)
    }
    list(c(bias1, bias2),c(rmse1, rmse2),c(mad1, mad2))
}


set.seed(999)
out_m <- matrix(0, nr = 26, nc=6)
for(i in 5:30){
    cat(i, fill = TRUE)
    set.seed(seed)
    out = g(nrep=1000, m=i) %>% unlist
    print(out)
    out_m[i - 4, ] = out %>% unlist
}

out_m <- cbind(5:30,out_m)
out_m <- as.data.frame(out_m)


colnames(out_m) <- c('m','gamma_bias','log_alpha_bias','gamma_rmse','log_alpha_rmse','gamma_mad','log_alpha_mad')

p1 <- ggplot(out_m) + geom_line(aes(m, gamma_bias), colour="steelblue")
p2 <- ggplot(out_m) + geom_line(aes(m, log_alpha_bias), colour="steelblue")
p3 <- ggplot(out_m) + geom_line(aes(m, gamma_rmse), colour="steelblue")
p4 <- ggplot(out_m) + geom_line(aes(m, log_alpha_rmse), colour="steelblue")
p5 <- ggplot(out_m) + geom_line(aes(m, gamma_mad), colour="steelblue")
p6 <- ggplot(out_m) + geom_line(aes(m, log_alpha_mad), colour="steelblue")

grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)



seed <- 999
out_m <- matrix(0, nr = 26, nc=6)
for(i in 5:30){
    cat(i, fill = TRUE)
    set.seed(seed)
    out = g_mix(nrep=1000, m=i) %>% unlist
    print(out)
    out_m[i - 4, ] = out %>% unlist
}

out_m <- cbind(5:30,out_m)
out_m <- as.data.frame(out_m)



=========================================
gum_preds_position_list <- list()
for(i in 1:10){
    gum_preds_position_list[[i]] <- subset(gum_preds,race_number==i)$position
}

gum_preds_rank2_list <- list()
for(i in 1:10){
        gum_preds_rank2_list[[i]] <- subset(gum_preds,race_number==i)$rank2
}

exp_preds_position_list<-c()
for(i in 1:10){
    exp_preds_position_list[[i]] <- subset(exp_preds,race_number==i)$position
}

exp_preds_rank2_list <- list()
for(i in 1:10){
    exp_preds_rank2_list[[i]] <- subset(exp_preds,race_number==i)$rank2
}

ski_data_position_list <- list()
for(i in 1:10){
    ski_data_position_list[[i]] <- subset(ski_data,number==unique(ski_data$number)[i])$position
}

ski_data_rank1_list <- list()
for(i in 1:10){
    ski_data_rank1_list[[i]] <- subset(ski_data,number==unique(ski_data$number)[i])$rank1
}

ski_temp_position_list <- list()
for(i in 1:10){
    ski_temp_position_list[[i]] <- subset(ski_data,number==unique(ski_data$number)[i])$position
}

ski_temp_rank1_rev_list <- list()
for(i in 1:10){
    ski_temp_rank1_rev_list[[i]] <- subset(ski_temp,number==unique(ski_temp$number)[i])$rank1_rev
}

tau1<-c()
for(i in 1:10)tau1[i] <- cor(gum_preds_position_list[[i]],gum_preds_rank2_list[[i]],method="kendall")
mean(tau1)
[1] 0.5515396

tau2<-c()
for(i in 1:10)tau2[i] <- cor(ski_data_position_list[[i]],ski_data_rank1_list[[i]],method="kendall")
mean(tau2)
[1] 0.5064464

tau3<-c()
for(i in 1:10)tau3[i] <- cor(ski_temp_position_list[[i]],ski_temp_rank1_rev_list[[i]],method="kendall")
mean(tau2)
0.5064464

tau4<-c()
for(i in 1:10)tau4[i] <- cor(exp_preds_position_list[[i]],exp_preds_rank2_list[[i]],method="kendall")
mean(tau4)
0.5047853


tau1w<-c()
for(i in 1:10)tau1w[i] <- wdm(gum_preds_position_list[[i]][sort(gum_preds_position_list[[i]],index=TRUE)$ix],
                              gum_preds_rank2_list[[i]][sort(gum_preds_position_list[[i]],index=TRUE)$ix],
                                  method="kendall", weights=1/(1:length(gum_preds_position_list[[i]])))
mean(tau1w)
[1]0.561245

tau2w<-c()
for(i in 1:10)tau2w[i] <- wdm(ski_data_position_list[[i]][sort(ski_data_position_list[[i]],index=TRUE)$ix],
                              ski_data_rank1_list[[i]][sort(ski_data_position_list[[i]],index=TRUE)$ix],
                              method="kendall", weights=1/(1:length(ski_data_position_list[[i]])))
mean(tau2w)
[1]  0.5475795

tau3w<-c()
for(i in 1:10)tau3w[i] <- wdm(ski_temp_position_list[[i]][sort(ski_temp_position_list[[i]],index=TRUE)$ix],
                              ski_temp_rank1_rev_list[[i]][sort(ski_temp_position_list[[i]],index=TRUE)$ix],
                              method="kendall", weights=1/(1:length(ski_temp_position_list[[i]])))
mean(tau3w)
[1]  0.5475795

tau4w<-c()
for(i in 1:10)tau4w[i]<- wdm(exp_preds_position_list[[i]][sort(exp_preds_position_list[[i]],index=TRUE)$ix],
                                          exp_preds_rank2_list[[i]][sort(exp_preds_position_list[[i]],index=TRUE)$ix],
                                          method="kendall", weights=1/(1:length(exp_preds_position_list[[i]])))
mean(tau4w)
0.5444496

wdm(gum_preds_position_list[[1]][sort(gum_preds_position_list[[1]],index=TRUE)$ix], gum_preds_rank2_list[[1]][sort(gum_preds_position_list[[1]],index=TRUE)$ix],
    method="kendall", weights=1/(1:length(gum_preds_position_list[[1]])))

