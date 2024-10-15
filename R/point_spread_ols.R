#' Estimate the point spread from a season design object
#'
#' @param data A list containing a design matrix, score differences, and teams,
#' as produced by the XY_differences function
#' @param home A vector of home team names, may use distinct substrings
#' @param away A vector of away team names, may use distinct substrings
#' @param home_effect Logical, TRUE if we want to estimate with home field advantage
#' @param home_fit Logical, TRUE if we want to fit the OLS model with home field advantage
#' @param a The level of the confidence interval
#'
#' @details The order of teams in home and away must correspond with the games
#' to predict on, meaning the first element of `home` plays the first element of
#' `away`, and so on. The function uses the OLS method to estimate the point
#' spread and the confidence interval assumes a normal distribution of
#' point spread.
#'
#' @return A data frame describing the estimated point spread and the limits of a
#' confidence level
#' @export
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' point_spread_ols(List, "Patriots", "Bills")
point_spread_ols <- function(
    data,
    home, away,
    home_effect = TRUE,
    home_fit = TRUE,
    a = 0.05
) {
  teams <- data$teams$name

  X <- data$X
  X <- X %>% rbind(c(0, rep(1, ncol(data$X) - 1)))

  if(!home_fit) {
    #Drop the first column for home field advantage
    X <- X[,2:ncol(X)]
    home_effect <- FALSE
  }

  Y <- c(data$Y_diff, 0)

  model <- lm(Y ~ 0 + X)
  beta <- coef(model)

  home_indx <- purrr::map_int(home,
                              ~which(grepl(.x, teams, ignore.case = TRUE))[1]
  )

  away_indx <- purrr::map_int(away,
                              ~which(grepl(.x, teams, ignore.case = TRUE))[1]
  )

  HomeTm <- teams[home_indx]
  AwayTm <- teams[away_indx]

  beta_home <- beta[paste0("X", HomeTm)]
  beta_away <- beta[paste0("X", AwayTm)]
  beta_int <- 0
  if(home_effect) beta_int <- beta["Xint"]

  spread <- round(beta_int + beta_home - beta_away, digits = 1) %>% as.numeric()

  # Esimate standard deviation in the model
  deg_f <- model$df.residual
  std_dev <- sqrt(sum(model$residuals^2) / model$df.residual) %>% round(1)

  #Put contrast vector c into confint to estimate HomeTm score - AwayTm score
  output <- data.frame(Home = HomeTm,
                       Away = AwayTm,
                       spread = spread
                       ) %>%
    as_tibble() %>%
    mutate(Winner = ifelse(spread >= 0, Home, Away),
           Loser = ifelse(spread >= 0, Away, Home),
           lower = spread - stats::qt(1 - a/2, deg_f) * std_dev,
           upper = spread + stats::qt(1 - a/2, deg_f) * std_dev,
           std_dev = std_dev
           )

  output <- list(model = model, predictions = output)

  return(output)
}
