#' Correlation diagnostics
#'
#' Comparison of correlations of predicted and true final ranls
#'
#' @param data_df dataframe of data
#' @param kendall if true, use kendall correlation per race, averages. otherwise, overall spearman

#'
#' @returns correlations based on gumbel, exponential and empirical predictions
#'
#' @examples
#'  empirical_diag(ski_data)
#'
#' @export
#'
#'
empirical_diag <- function(data_df, kendall = FALSE){

    gum_preds <- c()
    gum_fit <- readRDS("models/gum_fit")
    for(i in 1:10){
        preds <-ORM::pred(data_df,i,gum_fit, plot=FALSE)
        preds_df <- data.frame(race_number = i, rank1 = preds$Run1_pos, prob = preds$prob, rank2 = nrow(preds) - rank(preds$prob) +1, position = preds$position)
        preds_df$rank1_rev <- preds_df$rank1 %>% rank
        gum_preds <- rbind(gum_preds, preds_df)
    }

    exp_preds <- c()
    exp_fit <- readRDS("models/exp_fit")

    for(i in 1:10){
        preds <-ORM::pred(data_df,i,exp_fit, plot=FALSE)
        preds_df <- data.frame(race_number = i, rank1 = preds$Run1_pos, prob = preds$prob, rank2 = nrow(preds) - rank(preds$prob) +1, position = preds$position)
        exp_preds <- rbind(exp_preds, preds_df)
    }

    # correct for missing racers in second run
    ski_temp <- c()
    races <- unique(data_df$race)
    for(i in 1:10){
        tt <-  subset(ski_data, race == races[i])
        tt$rank1_rev <- tt$rank1 %>% rank
        ski_temp <- rbind(ski_temp, tt)
    }

    if(!kendall){
        cor_gum <- cor(gum_preds$rank2, gum_preds$position, method = "spearman")
        cor_exp <- cor(exp_preds$rank2, exp_preds$position, method = "spearman")
        cor_emp <- cor(ski_temp$position,ski_temp$rank1_rev, method = "spearman")
    }
    else{

        tau_gum <- c()
        tau_exp <- c()
        tau_emp <- c()

        for(i in 1:10){
            tau_gum[i] <- cor(gum_preds_position_list[[i]],gum_preds_rank2_list[[i]],method="kendall")
            tau_exp[i] <- cor(exp_preds_position_list[[i]],exp_preds_rank2_list[[i]],method="kendall")
            tau_emp[i] <- cor(ski_data_position_list[[i]],ski_data_rank1_list[[i]],method="kendall")
        }

        cor_gum <- mean(tau_gum)
        cor_exp <- mean(tau_exp)
        cor_emp <- mean(tau_emp)
    }

    c("gumbel" = cor_gum, "exponential" = cor_exp, "empirical" = cor_emp)
}
