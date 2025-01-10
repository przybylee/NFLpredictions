#' Estimate the point spread from a season design object
#'
#' @param data A list containing a design matrix, score differences, and teams,
#' as produced by the XY_differences function
#' @param home A vector of home team names, may use distinct substrings
#' @param away A vector of away team names, may use distinct substrings
#' @param home_advantage_fit Logical, indicates if we want to fit the OLS model
#' with home field advantage
#' @param home_advantage_predict Logical, TRUE if we want to estimate point
#' spreads with home field advantage
#' @param alpha The level of the confidence interval
#'
#' @details The order of teams in home and away must correspond with the games
#' to predict on, meaning the first element of `home` plays the first element of
#' `away`, and so on. The function uses the OLS method to estimate the point
#' spread and the confidence interval assumes a normal distribution of
#' point spread.
#'
#' The confidence interval on the spread assumes that all games are independent
#' and the residuals are IID normal, and uses the derived t-distribution for
#' estimated differences of the team effects in each game.
#'
#' @return A data frame describing the estimated point spread and the limits of a
#' confidence level
#' @export
#'
#' @import dplyr
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' point_spread_ols(List, "Patriots", "Bills")
point_spread_ols <- function(
    data,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE,
    alpha = 0.05
) {
  teams <- data$teams$name

  X <- data$X
  X <- X %>% rbind(c(0, rep(1, ncol(data$X) - 1)))

  if(!home_advantage_fit) {
    #Drop the first column for home field advantage
    X <- X[,2:ncol(X)]
    home_advantage_predict <- FALSE
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

  # Create matrix of contrasts
  C <- matrix(0, nrow = length(beta), ncol = length(beta_home))
  for(j in 1:length(beta_home)) {
    C[home_indx[j], j] <- 1
    C[away_indx[j], j] <- -1
  }

  beta_int <- 0
  if(home_advantage_predict) {
    beta_int <- beta["Xint"]
    C[grep("Xint", names(beta)), ] <- 1

    if(beta_int < 0) {
      "Estimated home field advantage used in predictions is {round(beta_int, 1)}." %>%
        glue::glue() %>%
        futile.logger::flog.warn()
    }
  }

    spread <- round(beta_int + beta_home - beta_away, digits = 1) %>% as.numeric()

  # Esimate standard deviation in the model
  deg_f <- model$df.residual
  sigma_hat <- sqrt(sum(model$residuals^2) / model$df.residual)
  contrast_std <- sigma_hat*sqrt(diag(t(C) %*% MASS::ginv(t(X) %*% X) %*% C))

  #Put contrast vector c into confint to estimate HomeTm score - AwayTm score
  output <- data.frame(home = HomeTm,
                       away = AwayTm,
                       est_spread = spread
                       ) %>%
    as_tibble() %>%
    mutate(winner = ifelse(est_spread >= 0, home, away),
           loser = ifelse(est_spread >= 0, away, home),
           lower = est_spread - stats::qt(1 - alpha/2, deg_f) * contrast_std,
           upper = est_spread + stats::qt(1 - alpha/2, deg_f) * contrast_std,
           home_adv = beta_int,
           sigma_hat = sigma_hat
           )

  output <- list(predictions = output, estimates = beta, model = model)

  return(output)
}
