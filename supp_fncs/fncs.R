
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
#            cat(i,fill=TRUE)

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
#        print(p1)
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
