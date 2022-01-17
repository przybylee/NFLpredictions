#' Predict the probability of each team beating the spread using ols estimates
#'
#' @param data List of season data as produced by XY_differences, contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param hspread Point spread for the home team
#' @param aspread Point spread for the away team
#' @param home_effect Logical, indicates if we consider home field advantage
#' @param wager Amount of money being wagered in dollars
#' @param hBL The betting line for the home team against the spread, American
#' @param aBL The betting line for the away team against the spread, American
#'
#' @return A dataframe indicating the expected value of wagering on each team to
#' beat the spread.
#' @export
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' eSpread_ols(List, "Patriots", "Bills", hspread = -5, hBL = -115, aBL = -105)
eSpread_ols <- function(data, home, away, hspread, aspread = NULL,
                        home_effect = TRUE, wager = 1, hBL = -110, aBL = -110){
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
  df <- nrow(X) - anova(reg)[1,1]
  mu <- t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%Y
  sigsq <- anova(reg)[2,3]
  sigma <- sqrt(sigsq)
  #Compute probs that each team beats the spread
  if(is.null(aspread)){aspread <- -hspread}
  AwayProb <- pnorm(aspread, mean = mu, sd = sigma)
  HomeProb <- pnorm(-hspread, mean = mu, sd = sigma, lower.tail = FALSE)
  probs <- data.frame(h = HomeTm, a = AwayTm, h_spread = mu,
                      h_prob = HomeProb, a_prob = AwayProb, method = "normal")
  aOdds <- american_to_odds(aBL)
  hOdds <- american_to_odds(hBL)
  eAway <- wager*aOdds*AwayProb - wager*HomeProb
  eHome <- wager*hOdds*HomeProb - wager*AwayProb
  EXP <- data.frame(h = HomeTm, a = AwayTm, eHome = eHome, eAway = eAway,
                    method = "normal")
  return(EXP)
}
