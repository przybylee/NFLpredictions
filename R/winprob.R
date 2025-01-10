#' Calculate the win probability of the home team assuming a normal
#' distribution on point differential for each game
#'
#' @param data List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_advantage_fit Logical, TRUE if we want to fit the OLS model with home field advantage
#' @param home_advantage_predict Logical, TRUE if we want to estimate point spreads with home field advantage
#' @param home_win_prob Logical, TRUE if we want to calculate the home team's win probability
#'
#' @return A data frame containing the teams and their win probabilities
#' @export
#'
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' #NONE
winprob_normal <- function(
    data,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE,
    home_win_prob = TRUE
) {
  if(home_win_prob) {
    hspread <- 0
    aspread <- NULL
  } else {
    hspread <- NULL
    aspread <- 0
  }
  output <- spreadprob_normal(data,
                              hspread, aspread,
                              home, away,
                              home_advantage_fit,
                              home_advantage_predict
  )

  # simplify the probs dataframe to just include win prob
  output$probs <- output$probs %>%
    select(-spread) %>%
    rename(prob_team = spread_team, win_prob = spread_prob)

  return(output)
}


#' Compute win probability using the empirical distribution of residuals
#'
#' @param data List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param h_spread Point spread for the home team
#' @param a_spread Point spread for the away team
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_advantage_fit Logical, TRUE if we want to fit the OLS model with home field advantage
#' @param home_advantage_predict Logical, TRUE if we want to estimate point spreads with home field advantage
#' @param symmetric Logical, indicates if we should assume that the residuals
#' are symmetric around 0
#' @param home_win_prob Logical, TRUE if we want to calculate the home team's win probability
#'
#' @return A data frame containing the teams and their win probabilities
#' @export
#'
#' @examples
#' #NONE
winprob_emp <- function(
    data,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE,
    home_win_prob = TRUE,
    symmetric = FALSE
) {
  if(home_win_prob) {
    hspread <- 0
    aspread <- NULL
  } else {
    hspread <- NULL
    aspread <- 0
  }
  output <- spreadprob_emp(data,
                           hspread, aspread,
                           home, away,
                           home_advantage_fit,
                           home_advantage_predict,
                           symmetric
  )

  # simplify the probs dataframe to just include win prob
  output$probs <- output$probs %>%
    select(-spread) %>%
    rename(prob_team = spread_team, win_prob = spread_prob)

  return(output)
}

#' Use a logistic regression to predict win probability
#'
#' @param data List of season data as produced by get_design(), contains
#' @param home Character vector of home team names
#' @param away Character vector of away team names
#' @param home_advantage_fit Logical, TRUE if we want to fit the logistic model
#' with home field advantage
#' @param home_advantage_predict Logical, TRUE if we want our prediction to
#' include the home field advantage estimated in the logistic model
#' @param home_win_prob Logical, TRUE if we want to calculate the home team's win probability,
#' otherwise we estimate the away team's win probability
#'
#' @details
#' Our logistic model only uses the binary outcome of the home team winning or losing.
#' Each team effect is estimated as a coefficient in the logistic model, and we
#' also can estiamte an intercept for the home field advantage.  If we let
#' \eqn{\beta_i} be the coefficient for team \eqn{i} and \eqn{\gamma} be the
#' intercept for home field advantage, then our model says the probability of
#' team \eqn{i} defeating team \eqn{j} is at home is given by
#' \deqn{ y_{ij} = \frac{\text{exp}(\beta_i - \beta_j + \gamma)}{1 - \text{exp}(\beta_i - \beta_j + \gamma)} }
#'
#'
#' @returns A list containing the predictions, estimates, and the model object
#' @export
#'
#' @examples #None
winprob_logistic <- function(
    data,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE,
    home_win_prob = TRUE
) {
  X <- data$X %>% rbind(c(0, rep(1, ncol(data$X) - 1)))

  teams <- data$teams$name

  if(!home_advantage_fit){
    X <- X[, 2:ncol(X)]
    home_advantage_predict <- FALSE
  }

  Y <- c(data$Y_binary, 0)

  model <- glm(Y ~ 0 + X, family = binomial(link = "logit"))

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
  if(home_advantage_predict) {
    beta_int <- beta["Xint"]

    if(beta_int < 0) {
      "Estimated home field advantage used in predictions is {round(beta_int, 1)} log odds." %>%
        glue::glue() %>%
        futile.logger::flog.warn()
    }
  }

  # Use logistic model to get probability of home team winning
  win_prob <- logistic(beta_int + beta_home - beta_away)
  if(!home_win_prob) win_prob <- 1 - win_prob

  probs <- data.frame(home = HomeTm,
                            away = AwayTm,
                            prob_team = "home",
                            win_prob = win_prob
  ) %>%
    mutate(winner = ifelse(win_prob > 0.5, home, away),
           loser = ifelse(win_prob > 0.5, away, home),
           home_advanage = logistic(beta_int)
    ) %>%
    as_tibble()

  output <- list(estimates = beta, model = model, probs = probs)

  return(output)
}
