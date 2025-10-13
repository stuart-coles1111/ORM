gum_fit <- ORM::fit("gum", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)
exp_fit <- ORM::fit("exp", 0, 4, ski_data,first_Run =FALSE, allranks=FALSE, maxrank=15, if_hessian=FALSE, init=0, method="BFGS", print=FALSE)

gum_preds <- c()

for(i in 1:10){
    preds <-ORM::pred(ski_data,i,gum_fit, plot=FALSE)
    preds_df <- data.frame(race_number = i, rank1 = preds$Run1_pos, prob = preds$prob, rank2 = nrow(preds) - rank(preds$prob) +1, position = preds$position)
    gum_preds <- rbind(gum_preds, preds_df)
}

exp_preds <- c()

for(i in 1:10){
    preds <-ORM::pred(ski_data,i,exp_fit, plot=FALSE)
    preds_df <- data.frame(race_number = i, rank1 = preds$Run1_pos, prob = preds$prob, rank2 = nrow(preds) - rank(preds$prob) +1, position = preds$position)
    exp_preds <- rbind(exp_preds, preds_df)
}

cor(gum_preds$rank2,gum_preds$position)
[1] 0.6822767

> cor(ski_data$position,ski_data$rank1)
[1] 0.6389551
