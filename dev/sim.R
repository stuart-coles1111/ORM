

expmod_race_sim <- function(n=10000,l=c(1,2,3),a=c(.5,1,2), a_eps = 0.25){
        zz <- matrix(0,nr=n,nc=2*length(l))
        for(i in 1:n){
            x <- rexp(length(l),l)
            a_r <- a + runif(length(l), -a_eps, a_eps)
            y <- x+a_r
            zz[i,] <- rbind(c(a_r, sort(y, ind=T)$ix))
        }
        zz
}

gummod_race_sim <- function(n=10000,l=c(1,2,3),a=c(.5,1,2), a_eps = 0.25){
    zz <- matrix(0,nr=n,nc=2*length(l))
    for(i in 1:n){
        x <- l*rexp(length(l),1)
        a_r <- a + runif(length(l), -a_eps, a_eps)
        y <- x+a_r
        zz[i,] <- rbind(c(a_r, sort(y, ind=T)$ix))
    }
    zz
}

nl <- function(x, l) {
    m <- ncol(x)/2
    s <- 0
    #    res <- lapply(1:nrow(x), function(i, l, a) log(ORM::Q(m, m, exp(l[x[i,]]), a[x[i,]])), l=l,a=a) %>% unlist %>% sum
    for(i in 1:nrow(x)){
        s <- s + log(ORM::Q(m, m, exp(l[x[i,-(1:m)]]), x[i, x[i, -(1:m)]]))
    }
    cat(s, fill=T)
    -s
}

nl2 <- function(x, l) {
    m <- ncol(x)/2
    s <- 0
    #    res <- lapply(1:nrow(x), function(i, l, a) log(ORM::Q(m, m, exp(l[x[i,]]), a[x[i,]])), l=l,a=a) %>% unlist %>% sum
    for(i in 1:nrow(x)){
        s <- s + ORM::QQQQ(m, m, exp(l[x[i,-(1:m)]]), x[i, x[i, -(1:m)]])
    }
    cat(s,fill=T)
    -s
}

nl_g <- function(x, p) {
    l <- p[-length(p)]
    alpha <- p[length(p)]
    m <- ncol(x)/2
    s <- 0
    #    res <- lapply(1:nrow(x), function(i, l, a) log(ORM::Q(m, m, exp(l[x[i,]]), a[x[i,]])), l=l,a=a) %>% unlist %>% sum
    for(i in 1:nrow(x)){
        s <- s + ORM::QQ(m, m, exp(l[x[i,-(1:m)]]), x[i, x[i, -(1:m)]], alpha)
    }
    cat(s, fill=T)
    -s
}
l <- c(1.3,2.4,1,.2,.9, .7)
a <- c(1,.5, .75,1.3, .4, .8)

temp <- expmod_race_sim(1000,l= l, a=a)


ot <- optim(rep(0,length(l)), nl, x = temp,  method = "BFGS", hessian = FALSE)
exp(ot$par)

ot2 <- optim(rep(0,length(l)), nl2, x = temp,  method = "BFGS", hessian = FALSE)
exp(ot2$par)
