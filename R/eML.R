#' Calculate the expected value for wagering on each team assuming a normal
#' distribution.
#'
#' @param data List of season data as produced by XY_differences, contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param wager Amount of money being wagered in dollars
#' @param hBL The home team's moneyline given as American
#' @param aBL The away team's moneyline given as American
#' @param home_effect Logical, indicates if there is home field advantage
#'
#' @return A data frame containing the names of the two teams and the expected
#' values for wagering on either moneyline.
#' @export
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' G <- regssn2021
#' data <- XY_differences(G)
#' eML_ols(data, "Bills", "Patriots", wager = 20, hBL = -160, aBL = 180)
eML_ols <- function(data, home, away, wager = 1, hBL = -110, aBL = -110,
                           home_effect = TRUE){
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
  #Compute probs for each team winning
  AwayProb <- stats::pnorm(0, mean = mu, sd = sigma)
  HomeProb <- stats::pnorm(0, mean = mu, sd = sigma, lower.tail = FALSE)
  aOdds <- american_to_odds(aBL)
  hOdds <- american_to_odds(hBL)
  eAway <- wager*aOdds*AwayProb - wager*HomeProb
  eHome <- wager*hOdds*HomeProb - wager*AwayProb
  EXP <- data.frame(h = HomeTm, a = AwayTm, eHome = eHome, eAway = eAway,
                    method = "normal")
  return(EXP)
}


#' Calculate the expected value of wagering on either team's moneyline using the
#' empircal distribution.
#'
#' @param design  List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param wager Amount of money being wagered in dollars
#' @param hBL The home team's moneyline given as American
#' @param aBL The away team's moneyline given as American
#' @param home_effect Logical, indicates if there is home field advantage
#'
#' @return A data frame indicating the the expected values from wagering on each
#' team's moneyline based on the empircal distribution.
#' @export
#'
#' @examples
#' design <- get_design(regssn2021)
#' eML_emp(design, "Rams", "Bengals", wager = 100, hBL = -155, aBL = 185)
eML_emp <- function(design, home, away, wager = 1, hBL = -110, aBL = -110,
                    home_effect = TRUE){
  probs <- winprob_emp(design, home, away, home_effect, verbose = FALSE)
  HomeTm <- probs$h[1]
  AwayTm <- probs$a[1]
  #Compute probs for each team winning
  AwayProb <- probs$a_prob
  HomeProb <- probs$h_prob
  aOdds <- american_to_odds(aBL)
  hOdds <- american_to_odds(hBL)
  eAway <- wager*aOdds*AwayProb - wager*HomeProb
  eHome <- wager*hOdds*HomeProb - wager*AwayProb
  EXP <- data.frame(h = HomeTm, a = AwayTm, eHome = eHome, eAway = eAway,
                    method = "empirical")
  return(EXP)
}
