#' Calculate the probability of each team beating the spread assuming normal
#' errors
#'
#' @param design List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param hspread Point spread for the home team
#' @param aspread Point spread for the away team
#' @param home_effect Logical, indicates if there is home field advantage
#'
#' @return A data frame containing the teams and their win probabilities
#' @export
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' G <- regssn2021
#' List <- get_design(G)
#' spreadprob_normal(List, "Patriots", "Bills", -5)
spreadprob_normal <- function(design, home, away, hspread = 0, aspread = NULL,
                              hBL = -110, aBL = -110, home_effect = TRUE){
  X <- design$X
  Y <- design$Y_dif
  teams <- design$teams
  #If teams are given as strings, look up id numbers
  if (is.character(home)){
    home <- home %>%
      team_detect(design, .) %>%
      dplyr::pull(tm_id)
    home <- home[1]
  }
  if (is.character(away)){
    away <- away %>%
      team_detect(design, .) %>%
      dplyr::pull(tm_id)
    away <- away[1]
  }
  #Create contrast vector
  HomeTm <- teams$name[home]
  AwayTm <- teams$name[away]
  cont <- c(ifelse(home_effect, 1, 0), rep(0, length(teams$name)))
  cont[colnames(X) == HomeTm] <- 1
  cont[colnames(X) == AwayTm] <- -1
  reg <- lm(Y ~ X+0)
  cont <- as.matrix(cont)
  X.X <- t(X)%*%X
  df <- nrow(X) - stats::anova(reg)[1,1]
  mu <- t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%Y
  sigsq <- anova(reg)[2,3]
  sigma <- sqrt(sigsq)
  #Compute probs that each team beats the spread
  if(is.null(aspread)){aspread <- -hspread}
  AwayProb <- stats::pnorm(aspread, mean = mu, sd = sigma)
  HomeProb <- stats::pnorm(hspread, mean = mu, sd = sigma, lower.tail = FALSE)
  probs <- data.frame(h = HomeTm, a = AwayTm, hspread = hspread,
                      hspread_est = mu, h_prob = HomeProb, a_prob = AwayProb,
                      method = "normal")
  return(probs)
}



#' Compute win probability using the empirical distribution
#'
#' @param design List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param hspread The listed point spread for the home team
#' @param aspread The listed point spread for the away team
#' @param home_effect Logical, indicates if there is home field advantage
#'
#' @return A data frame containing the teams and their probabilities of beating
#' the spread
#'
#' @export
#'
#' @examples
#' G <- regssn2021
#' design <- get_design(G)
#' spreadprob_emp(design, "Patriots", "Bills", h_spread = 5)
spreadprob_emp <- function(design, home, away, hspread = 0, aspread = NULL,
                           home_effect = TRUE){
  X <- design$X
  Y <- design$Y_dif
  teams <- design$teams
  #If teams are given as strings, look up id numbers
  if (is.character(home)){
    home <- home %>%
      team_detect(design, .) %>%
      dplyr::pull(tm_id)
    home <- home[1]
  }
  if (is.character(away)){
    away <- away %>%
      team_detect(design, .) %>%
      dplyr::pull(tm_id)
    away <- away[1]
  }
  HomeTm <- teams$name[home]
  AwayTm <- teams$name[away]
  #Create contrast Vector
  cont <- c(ifelse(home_effect, 1, 0), rep(0, length(teams$name)))
  cont[colnames(X) == HomeTm] <- 1
  cont[colnames(X) == AwayTm] <- -1
  reg <- lm(Y ~ X+0)
  cont <- as.matrix(cont)
  X.X <- t(X)%*%X
  df <- nrow(X) - stats::anova(reg)[1,1]
  mu <- (t(cont)%*%MASS::ginv(X.X)%*%t(X)%*%Y)[1]
  # Use shifted residuals to get win prob
  res <- stats::residuals(reg)
  #If no home field advantage is assumed, make empirical distributions symmetric
  if (!home_effect){
   res <- c(res, -res)
  }
  #Count of times res allowed the home team to beat the spread
  if(is.null(aspread)){aspread <- -hspread}
  pos_res_shift <- sum(res + mu + hspread > 0)
  #Count times that res allowed the away team to beat the spread
  neg_res_shift <- sum(res+mu -aspread < 0)
  #Compute probs that each team beats the spread
  AwayProb <- neg_res_shift/length(res)
  HomeProb <- pos_res_shift/length(res)
  probs <- data.frame(h = HomeTm, a = AwayTm, hspread = hspread,
                      hspread_est = mu, h_prob = HomeProb, a_prob = AwayProb,
                      method = "empirical")
  return(probs)
}
