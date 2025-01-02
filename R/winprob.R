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
  output <- spread_prob_normal(data, home, away,
                               hspread, aspread,
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
  output <- spread_prob_emp(data, home, away,
                            hspread, aspread,
                            home_advantage_fit,
                            home_advantage_predict
  )

  # simplify the probs dataframe to just include win prob
  output$probs <- output$probs %>%
    select(-spread) %>%
    rename(prob_team = spread_team, win_prob = spread_prob)

  return(output)
}
