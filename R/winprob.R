#' Calculate the win probability using OLS estimators
#'
#' @param data List of season data as produced by XY_differences, contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_effect Logical, indicates if there is home field advantage
#' @param verbose Logical, indicates if we want output to read win probabilities
#'
#' @return A data frame containing the teams and their win probabilities
#' @export
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' winprob_ols(List, "Patriots", "Bills")
winprob_ols <- function(data, home, away, home_effect = TRUE, verbose = TRUE){
  X <- data$X
  Y <- data$Y_dif
  teams <- data$teams
  #Create contrast vector
  home_indx <- which(grepl(home, teams, ignore.case = TRUE))[1]
  away_indx <- which(grepl(away, teams, ignore.case = TRUE))[1]
  HomeTm <- teams[home_indx]
  AwayTm <- teams[away_indx]
  cont <- c(ifelse(home_effect, 1, 0), rep(0, length(teams)))
  cont[colnames(X) == HomeTm] <- 1
  cont[colnames(X) == AwayTm] <- -1
  reg <- lm(Y ~ X+0)
  cont <- as.matrix(cont)
  X.X <- t(X)%*%X
  df <- nrow(X) - stats::anova(reg)[1,1]
  mu <- t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%Y
  sigsq <- anova(reg)[2,3]
  sigma <- sqrt(sigsq)
  AwayProb <- pnorm(0,mean = mu, sd = sigma)
  HomeProb <- pnorm(0,mean = mu, sd = sigma, lower.tail = FALSE)
  print(paste("The estimated win probability for the ", AwayTm, "at",
              HomeTm, "is", round(AwayProb,3), sep = " " ))
  probs <- data.frame(h = HomeTm, a = AwayTm, h_spread = mu,
                      h_prob = HomeProb, a_prob = AwayProb, method = "normal")
  return(probs)
}



winprob_emp <- function(design, home, away, home_effect = TRUE, verbose = TRUE){
  X <- design$X
  Y <- design$Y_dif
  teams <- data$teams
  #Create contrast vector
  home_indx <- which(grepl(home, teams, ignore.case = TRUE))[1]
  away_indx <- which(grepl(away, teams, ignore.case = TRUE))[1]
  HomeTm <- teams[home_indx]
  AwayTm <- teams[away_indx]
  cont <- c(ifelse(home_effect, 1, 0), rep(0, length(teams)))
  cont[colnames(X) == HomeTm] <- 1
  cont[colnames(X) == AwayTm] <- -1
  reg <- lm(Y ~ X+0)
  cont <- as.matrix(cont)
  X.X <- t(X)%*%X
  df <- nrow(X) - stats::anova(reg)[1,1]
  mu <- t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%Y
  sigsq <- anova(reg)[2,3]
  sigma <- sqrt(sigsq)
  AwayProb <- pnorm(0,mean = mu, sd = sigma)
  HomeProb <- pnorm(0,mean = mu, sd = sigma, lower.tail = FALSE)
  print(paste("The estimated win probability for the ", AwayTm, "at",
              HomeTm, "is", round(AwayProb,3), sep = " " ))
  probs <- data.frame(h = HomeTm, a = AwayTm, h_spread = mu,
                      h_prob = HomeProb, a_prob = AwayProb, method = "normal")
  return(probs)
}
