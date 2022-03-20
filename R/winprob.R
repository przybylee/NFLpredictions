#' Calculate the win probability using assuming a normal distribution on point
#' differential for each game
#'
#' @param design List of season data as produced by get_design(), contains
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
#' List <- get_design(G)
#' winprob_normal(List, "Patriots", "Bills")
winprob_normal <- function(design, home, away, home_effect = TRUE, verbose = TRUE){
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
  AwayProb <- stats::pnorm(0,mean = mu, sd = sigma)
  HomeProb <- stats::pnorm(0,mean = mu, sd = sigma, lower.tail = FALSE)
  print(paste("The estimated win probability for the ", AwayTm, "at",
              HomeTm, "is", round(AwayProb,3), sep = " " ))
  probs <- data.frame(h = HomeTm, a = AwayTm, h_spread = mu,
                      h_prob = HomeProb, a_prob = AwayProb, method = "normal")
  return(probs)
}



#' Compute win probability using the empirical distribution
#'
#' @param design List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_effect Logical, indicates if there is home field advantage
#' @param verbose Logical, indicates if we want output to read win probabilities
#'
#' @return A data frame containing the teams and their win probabilities
#'
#' @export
#'
#' @examples
#' G <- regssn2021
#' design <- get_design(G)
#' winprob_emp(design, "Patriots", "Bills")
winprob_emp <- function(design, home, away, home_effect = TRUE, verbose = TRUE){
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
  res_shift <- res + mu
  pos_res_shift <- sum(res_shift > 0)
  neg_res_shift <- sum(res_shift < 0)
  AwayProb <- neg_res_shift/length(res)
  HomeProb <- pos_res_shift/length(res)
if (verbose){
  "The estimated win probability for the {AwayTm} at {HomeTm} is {round(AwayProb, 3)}" %>%
    glue::glue() %>%
    print()
}
  probs <- data.frame(h = HomeTm, a = AwayTm, h_spread = mu,
                      h_prob = HomeProb, a_prob = AwayProb, method = "empirical")
  return(probs)
}
