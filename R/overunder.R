#Estimate the overunder for a game featuring two teams

#' Use ols to estimate the total points scored in a game between tm1 and tm2
#'
#' @param design Design object with information from a collection of games.
#' This is a list that is the output of get_design()
#' @param tm1 A substring of the name of the first team
#' @param tm2 A substring of the name of the second team
#' @param a The confidence level of the interval we estimate
#' @param verbose Whether or not a lengthy readout of the estimate is desired
#'
#' @return A dataframe with the estimate and confidence interval
#' @export
#'
#' @examples
#' design <- get_design(regssn2021)
#' over_under_ols(design, "Rams", "Bengals")
over_under_ols <- function(design, tm1, tm2, a = 0.05, verbose = TRUE){
    X <- design$X_sum
    Y <- design$Y_sum
    Tm1 <- team_detect(design, tm1)[1]
    Tm2 <- team_detect(design, tm2)[1]
    cont <- c(1, ifelse(colnames(X)[-1] %in% c(Tm1,Tm2), 1, 0))
    #Put contrast vector c into confint to estimate Tm1 score Tm2 score
    est <- conf_int_ols(X, y = Y, cont = cont, d = 0, a = a)
    score <- round(est$est[1], digits = 1)
    return(est)
    if(verbose){
      print(paste("The expected total score for the next game between ", Tm1,
                  " and ", Tm2, " is ", score))
    }
}
