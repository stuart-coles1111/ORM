#' Calculation of offset rank neg log lik
#'
#' Calculation of offset rank neg log lik for fitting to ski data
#'
#' @param beta model parameters
#' @param model_type model type "gum" for gumbel, "exp" for exponential
#' @param approx  approximate likelihood? (0 - no, 1 or 2 for approx type)
#' @param model_number as defined in object models
#' @param race_df dataframe of data
#' @param first_Run model first run (TRUE/FALSE)
#' @param second_Run model second run (TRUE/FALSE)
#' @param allranks model all ranks (TRUE/FALSE)
#' @param maxrank if all ranks = FALSE, how =many ranks to model?
#' @param exclude_second_Run vector of race numbers to exclude from second run
#' @param print print results (TRUE/FALSE)
#
#'
#' @returns neg log lik of specified model at given parameters
#' @examples
#'   race_nllh(c(.1,.2), "gum", FALSE, 1, ski_data, allranks=FALSE, maxrank=15, first_Run = FALSE)
#'
#' @export
#'
race_nllh <- function(beta, model_type = "gum", approx = 0, model_number = 1, race_df, first_Run=TRUE, second_Run=TRUE, allranks = TRUE, maxrank = 10, exclude_second_Run=NULL, print = FALSE){

    models <- get("models")
    model_name <- paste0(ifelse(model_type == "gum", "gum_model", "exp_model"), model_number)
    model <- models[[model_name]]

    races <- unique(race_df$race)
    n_races <- length(races)

    l1 <- 0

    if(first_Run){
        #first Run...

        if(model$type == "gumbel") {
            eval(parse(text = model$alpha_ind))
            eval(parse(text = model$alpha))
        }

        for(race_number in 1:n_races){


            df <- subset(race_df, race == races[race_number])
            df <- dplyr::arrange(df, time1) #arrange by first Run times
            nr <- nrow(df)

            m <- ifelse(allranks, nr, min(maxrank, nr - 1))

            eval(parse(text = model$lambda1))

        if(model$type == "gumbel" &  approx == 0)
            {
                l1 <- l1 - QQ(m, nr, lambda, rep(0, nr), alpha[race_number])
            }

            if(model$type == "gumbel" &  approx == 2)
            {
                l2 <- l2 - QG(m, nr, lambda, rep(0, nr), alpha[race_number])
            }

            else if(model$type == "exp" & approx == 0) {
                l1 <- l1 - log(Q(m, nr, lambda, rep(0, nr)))
            }

            else if(model$type == "exp" & approx == 1) {
                l1 <- l1 - QQQ(m, nr, lambda, rep(0, nr))
            }

            else if(model$type == "exp" & approx == 2) {
                l1 <- l1 - QQQQ(m, nr, lambda, rep(0, nr))
            }
        }
    }


    # total race...

    l2 <- 0

    if(second_Run){

        which_races <- 1:n_races
        if(!is.null(exclude_second_Run)) which_races <- which_races[-exclude_second_Run]

        if(model$type == "gumbel") {
            eval(parse(text = model$alpha_ind))
            eval(parse(text = model$alpha))
        }

        for(race_number in which_races){


            df <- subset(race_df, race == races[race_number])
            df <- dplyr::arrange(df, position) #arrange by final position

            nr <- nrow(df)

            m <- ifelse(allranks, nr, min(maxrank, nr - 1))

            eval(parse(text = model$lambda2))

            if(model$type == "gumbel" &  approx == 0)
            {
                l2 <- l2 - QQ(m, nr, lambda, df$time1, alpha[race_number])
            }

            if(model$type == "gumbel" &  approx == 2)
            {
                l2 <- l2 - QG(m, nr, lambda, df$time1, alpha[race_number])
            }

            else if(model$type == "exp" & approx == 0) {
                l2 <- l2 - log(Q(m, nr, lambda, df$time1))
            }

            else if(model$type == "exp" & approx == 1) {
                l2 <- l2 - QQQ(m, nr, lambda, df$time1)
            }

            else if(model$type == "exp" & approx == 2) {
                l2 <- l2 - QQQQ(m, nr, lambda, df$time1)
            }
        }
    }

    l <- l1 + l2

    if(print)  cat(l1, l2, l, fill=T)

    l
}
