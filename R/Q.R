#' Calculation of Q
#'
#' Calculation of recursion in EOR model
#'
#' @param m m
#' @param M N
#' @param l vector of exponential rates
#' @param a vector of offsets
#
#'
#' @returns win probabilities
#'
#' @examples
#'  Q(5, 10, rep(.1,10), 1:10)
#'
#' @export
#'
Q <- function(m, N, l, a) {

    if(m == 1){

        res <- EOR_p(1, l, a)

        return(res)

    }


    if(a[1] > a[2]) {

        l_new <- l[-1]
        a_new <- a[-1]
        a_new[1] <- a[1]
        a_new[2] <- a[3]

        l_new2 <- l[-1]
        l_new2[1] <- l[1] + l[2]

        res <- lexp(l, a, 2, 1:2) * (
            Q(m - 1, N - 1, l_new, a_new) -
                lratio(l, 2, 1:2) * Q(m - 1, N - 1, l_new2, a_new)
        )
        return(res)
    }

    else{

        l_new <- l[-1]
        a_new <- a[-1]
        a_new[1] <- a[2]
        a_new[2] <- a[3]

        l_new2 <- l[-1]
        l_new2[1] <- l[1] + l[2]

        res <- Q(m-1, N-1, l_new, a_new) -
            lexp(l, a, 1, 2:1) * lratio(l, 2, 1:2) * Q(m - 1, N - 1, l_new2, a_new)
        return(res)
    }
}



