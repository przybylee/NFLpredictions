#' Calculate the probability of each team beating the spread assuming normal
#' errors
#'
#' @param data List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param h_spread Point spread for the home team
#' @param a_spread Point spread for the away team
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_advantage_fit Logical, TRUE if we want to fit the OLS model with home field advantage
#' @param home_advantage_predict Logical, TRUE if we want to estimate point spreads with home field advantage
#'
#' @details The values for `h_spread` and `a_spread` should follow the conventions
#' of American sports books.  Beating the spread means that that a teams score, plus their
#' spread, is greater than their opponents score.  A negative spread means that we
#' are adding a handicap for a likely superior team.  A positive spread means
#' the team might be inferior, but they get extra help to beat the spread.  For
#' example, if `h_spread = 8`, we are estimating the
#' probability that the home team will lose by less than 8 points.
#'
#' Clearly, we could estimate win probability by using `h_spread = 0`.
#'
#' This function assumes that the residuals of our OLS model follow a normal
#' distribution.  Thus, if \eqn{y_j} is the home team's score minus the away team's
#' score, and \eqn{\hat{y}_j} is the predicted value of \eqn{y_j}, then the
#' probabity of the home team beating a spread \eqn{h_j} is estimated as
#' \deqn{P(y_j + h_j > 0) = P(y_j > -h_j)}
#' where \eqn{y_j \sim N(\hat{y}_j, \hat{\sigma}^2)}.  A small adjustment to this
#' formula can be made so that we can use the left tail to estimate the probability
#' of the away team beating the spread.
#'
#' @return A data frame containing the teams and their win probabilities
#' @export
#' @importFrom stats pnorm
#' @importFrom stats anova
#' @importFrom stats lm
#'
#' @examples
#' #None
spreadprob_normal <- function(
    data,
    h_spread = 0, a_spread = NULL,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE
) {
  point_spread <- point_spread_ols(data, home, away,
                                   home_advantage_fit, home_advantage_predict
  )

  model <- point_spread$model
  predictions <- point_spread$predictions
  estimates <- point_spread$estimates

  # Write the probablility of beating the spread using predictions of spreads
  probs <- predictions

  h_probs <- NULL
  if(!is.null(h_spread)) {
    h_probs <- probs %>%
      mutate(
        spread = h_spread,
        spread_team = "home",
        spread_prob = pnorm(-h_spread,
                            mean = est_spread,
                            sd = sigma_hat,
                            lower.tail = FALSE
        )
      )
  }

  a_probs <- NULL
  if(!is.null(a_spread)) {
    a_probs <- probs %>%
      mutate(
        spread = a_spread,
        spread_team = "away",
        spread_prob = pnorm(a_spread,
                            mean = est_spread,
                            sd = sigma_hat,
                            lower.tail = TRUE
        )
      )
  }

  probs <- bind_rows(h_probs, a_probs) %>%
    arrange(home,
            away,
            ifelse(spread_team == "home", spread, -spread),
            desc(spread_team),
            winner
    ) %>%
    select(home, away, starts_with("spread"), everything())

  output <- list(
    model = model,
    predictions = predictions,
    estimates = estimates,
    probs = probs
  )

  return(output)
}


#' Compute win probability using the empirical distribution
#'
#' @param data List of season data as produced by get_design(), contains
#' design matrix X, scores Y_diff, and a list of teams
#' @param h_spread The listed point spread for the home team
#' @param a_spread The listed point spread for the away team
#' @param home Name of the home team, could be a substring
#' @param away Name of the away team, could be a substring
#' @param home_advantage_fit Logical, indicates if the model should include home
#' field advantage
#' @param home_advantage_predict Logical, indicates if the model should
#' include use home field advantage when predicting point spreads
#' @param symmetric Logical, indicates if we should assume that the residuals
#' are symmetric about zero
#'
#' @details The values for `h_spread` and `a_spread` should follow the conventions
#' of American sports books.  Beating the spread means that that a teams score, plus their
#' spread, is greater than their opponents score.  A negative spread means that we
#' are adding a handicap for a likely superior team.  A positive spread means
#' the team might be inferior, but they get extra help to beat the spread.  For
#' example, if `h_spread = 8`, we are estimating the
#' probability that the home team will lose by less than 8 points.
#'
#' Clearly, we could estimate win probability by using `h_spread = 0`.
#'
#' This function uses the empirical distribution of the residuals of our OLS
#' model follow.  Thus, if \eqn{y_j} is the home team's score minus the away team's
#' score, and \eqn{\hat{y}_j} is the predicted value of \eqn{y_j}, then the
#' residual is
#' \deqn{\varepsilon_j = y_j - \hat{y}_j.}
#' When a sufficient number of games are used to fit the model, we can estimate
#' the probability of the home team beating a spread \eqn{h_j} as
#' \deqn{
#' P(y_j + h_j > 0) = P(y_j > -h_j)
#'                  = P(\varepsilon_j > -h_j - \hat{y}_j)
#'                  = 1 - F(-h_j - \hat{y}_j)
#' }
#' where \eqn{F} is the empirical CDF of the residuals.  When
#' `symmetric = TRUE`, we assume that the residuals are symmetric about zero
#' and we use both the positive and negative residuals to define the empirical
#' CDF.
#'
#' @return A data frame containing the teams and their probabilities of beating
#' the spread
#'
#' @export
#'
#' @examples
#' design <- get_design(regssn2021)
#' spreadprob_emp(design, "Patriots", "Bills", hspread = 5)
spreadprob_emp <- function(
    data,
    h_spread = 0, a_spread = NULL,
    home, away,
    home_advantage_fit = TRUE,
    home_advantage_predict = TRUE,
    symmetric = FALSE
) {
  point_spread <- point_spread_ols(data, home, away,
                                   home_advantage_fit,
                                   home_advantage_predict
  )

  model <- point_spread$model
  predictions <- point_spread$predictions
  estimates <- point_spread$estimates

  residuals <- residuals(model) %>%
    #omit the last value
    head(-1)

  if(symmetric) {
    residuals <- c(residuals, -residuals)
  }

  # # empirical density function of residuals
  # res_density <- density(residuals)
  # plot(res_density)

  # empirical cdf of residuals
  res_cdf <- ecdf(residuals)
  #res_cdf(c(-10, 0, 10))

  # Write the probablility of beating the spread using predictions of spreads
  probs <- predictions

  h_probs <- NULL
  if(!is.null(h_spread)) {
    h_probs <- probs %>%
      mutate(
        spread = h_spread,
        spread_team = "home",
        spread_prob = 1 - res_cdf(-(est_spread + h_spread))
        )
  }

  a_probs <- NULL
  if(!is.null(a_spread)) {
    a_probs <- probs %>%
      mutate(
        spread = a_spread,
        spread_team = "away",
        spread_prob = res_cdf(a_spread - est_spread)
      )
  }

  probs <- bind_rows(h_probs, a_probs) %>%
    arrange(home,
            away,
            ifelse(spread_team == "home", spread, -spread),
            desc(spread_team),
            winner
    ) %>%
    select(home, away, starts_with("spread"), everything())

  output <- list(
    model = model,
    predictions = predictions,
    estimates = estimates,
    probs = probs
  )

  return(output)
}
