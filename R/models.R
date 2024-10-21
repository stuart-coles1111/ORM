#' Model definitions
#'
#' Model defintiions for EOR and GOR fits to ski data
#' @export
#'

models = list(

gum_model0 = list(
    type = "gumbel",
    npar = 1,
    alpha_ind = "alpha_ind <- 1",
    lambda1 = "lambda <-  rep(1, nrow(df))",
    lambda2 = "lambda <- rep(1, nrow(df))",
    alpha = "alpha <- rep(exp(beta[1]), n_races)"
),

gum_model1 = list(
    type = "gumbel",
    npar = 2,
    alpha_ind = "alpha_ind <- 2",
    lambda1 = "lambda <-  beta[1] * df$points",
    lambda2 = "lambda <-  beta[1] * df$points",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),

gum_model2 = list(
    type = "gumbel",
    npar = 2,
    alpha_ind = "alpha_ind <- 2",
    lambda1 = "lambda <-    beta[1] * df$bib1/30",
    lambda2 = "lambda <-    beta[1] * df$bib2/30",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),

gum_model3 = list(
    type = "gumbel",
    npar = 3,
    alpha_ind = "alpha_ind <- 3",
    lambda1 = "lambda <-   beta[1] * df$points  + beta[2] * df$bib1/30",
    lambda2 = "lambda <-   beta[1] * df$points  + beta[2] * df$bib2/30",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),

gum_model4 = list(
    type = "gumbel",
    npar = "n_races + 1",
    alpha_ind = "alpha_ind <- n_races + 1",
    lambda1 = "lambda <-   beta[race_number] * df$bib1/30",
    lambda2 = "lambda <-   beta[race_number] * df$bib2/30",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),


gum_model5 = list(
    type = "gumbel",
    npar = "n_races + 2",
    alpha_ind = "alpha_ind <- n_races + 2",
    lambda1 = "lambda <-   beta[1] * df$points  + beta[race_number + 1] * df$bib1/30",
    lambda2 = "lambda <-   beta[1] * df$points  + beta[race_number + 1] * df$bib2/30",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),

gum_model6 = list(
    type = "gumbel",
    npar = "2 * n_races + 1",
    alpha_ind = "alpha_ind <- NULL",
    lambda1 = "lambda <-   beta[1] * df$points  + beta[race_number + 1] * df$bib1/30",
    lambda2 = "lambda <-   beta[1] * df$points  + beta[race_number + 1] * df$bib2/30",
    alpha = "alpha <- exp(beta[(n_races + 2) : (2 * n_races + 1) ])"
),

gum_model7 = list(
    type = "gumbel",
    npar = "n_races + 2",
    alpha_ind = "alpha_ind <- n_races + 2",
    lambda1 = "lambda <-   beta[race_number] * df$bib1/30",
    lambda2 = "lambda <-   beta[n_races+1] * beta[race_number] * df$bib2/30",
    alpha = "alpha <- rep(exp(beta[alpha_ind]), n_races)"
),


exp_model0 = list(
    type = "exp",
    npar = 1,
    lambda1 = "lambda <-  rep(exp(beta[1]), nrow(df))",
    lambda2 = "lambda <-  rep(exp(beta[1]), nrow(df))"
),

exp_model1 = list(
    type = "exp",
    npar = 2,
    lambda1 = "lambda <-  exp(beta[1] + beta[2] * df$points)",
    lambda2 = "lambda <-  exp(beta[1] + beta[2] * df$points)"
),

exp_model2 = list(
    type = "exp",
    npar = 2,
    lambda1 = "lambda <-   exp(beta[1] + beta[2] * df$bib1/30)",
    lambda2 = "lambda <-   exp(beta[1] + beta[2] * df$bib2/30)"
),

exp_model3 = list(
    type = "exp",
    npar = 3,
    lambda1 = "lambda <-  exp(beta[1] + beta[2] * df$points + beta[3] * df$bib1/30)",
    lambda2 = "lambda <-  exp(beta[1] + beta[2] * df$points + beta[3] * df$bib2/30)"
),

exp_model4 = list(
    type = "exp",
    npar = "n_races + 1",
    lambda1 = "lambda <-   exp(beta[1] + beta[race_number + 1] * df$bib1/30)",
    lambda2 = "lambda <-   exp(beta[1] + beta[race_number + 1] * df$bib2/30)"
),

exp_model5 = list(
    type = "exp",
    npar = "n_races + 2",
    lambda1 = "lambda <-   exp(beta[1] + beta[2] * df$points  + beta[race_number + 2] * df$bib1/30)",
    lambda2 = "lambda <-   exp(beta[1] + beta[2] * df$points  + beta[race_number + 2] * df$bib2/30)"
),


exp_model6 = list(
    type = "exp",
    npar = "2 * n_races + 1",
    lambda1 = "lambda <-   exp(beta[race_number] + beta[n_races + 1] * df$points  + beta[n_races + 1 + race_number] * df$bib1/30)",
    lambda2 = "lambda <-   exp(beta[race_number] + beta[n_races + 1] * df$points  + beta[n_races + 1 + race_number] * df$bib2/30)"
)

)
