#' Estimate the point spread from a season design object
#'
#' @param data A list containing a design matrix, score differences, and teams,
#' as produced by the XY_differences function
#' @param home The name of the home team, may be a substring
#' @param away The name of the away team, may be a substring
#' @param home_effect Logical, TRUE if we want to estimate with home field advantage
#' @param a The level of the confidence interval
#' @param verbose Logical, TRUE if we want to print who the winning team will be
#'
#' @return A data frame describing the estimated point spread and the limits of a
#' confidence level
#' @export
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' point_spread_ols(List, "Patriots", "Bills")
point_spread_ols <- function(data, home, away, home_effect = TRUE, a = 0.05, verbose = TRUE){
  teams <- data$teams
  X <- data$X
  Y <- data$Y_diff
  home_indx <- which(grepl(home, teams, ignore.case = TRUE))[1]
  away_indx <- which(grepl(away, teams, ignore.case = TRUE))[1]
  HomeTm <- teams[home_indx]
  AwayTm <- teams[away_indx]
  cont <- c(ifelse(home_effect, 1, 0), rep(0, length(teams)))
  cont[colnames(X) == HomeTm] <- 1
  cont[colnames(X) == AwayTm] <- -1
  #Put contrast vector c into confint to estimate HomeTm score - AwayTm score
  est <- conf_int_ols(X, y = Y, cont = cont, d = 0, a = a)
  spread <- round(est$est[1], digits = 1)
  if (spread >=0){
    print(paste(HomeTm, "beat", AwayTm, "by", spread))
  }
  else {
    print(paste(AwayTm, "beat", HomeTm, "by", abs(spread)))
  }
  return(est)
}
