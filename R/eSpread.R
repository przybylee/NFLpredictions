#' Predict the expected value of a wager for either team beating the spread
#' assuming errors follow a normal distribution.
#'
#' @param design List of season data as produced by get_design(), contains
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
#'
#' @examples
#' List <- get_design(regssn2021)
#' espread_normal(List, "Patriots", "Bills", hspread = -5, hBL = -115, aBL = -105)
espread_normal <- function(design, home, away, hspread = 0, aspread = NULL,
                        home_effect = TRUE, wager = 1, hBL = -110, aBL = -110){
  #Compute probs that each team beats the spread
  probs <- spreadprob_normal(design, home, away, hspread, aspread, home_effect)
  AwayProb <- probs$a_prob[1]
  HomeProb <- probs$h_prob[1]
  aOdds <- american_to_odds(aBL)
  hOdds <- american_to_odds(hBL)
  eAway <- wager*aOdds*AwayProb - wager*HomeProb
  eHome <- wager*hOdds*HomeProb - wager*AwayProb
  EXP <- probs %>%
    dplyr::mutate(hBL = hBL,
                  aBL = aBL,
                  wager = wager,
                  eHome = eHome,
                  eAway = eAway,
    )
  return(EXP)
}

#' Predict the expected value of a wager for either team beating the spread
#' assuming the empirical distribution
#'
#' @param design List of season data as produced by get_design(), contains
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
#'
#' @examples
#' List <- get_design(regssn2021)
#' espread_emp(List, "Patriots", "Bills", hspread = -5, hBL = -115, aBL = -105)
espread_normal <- function(design, home, away, hspread = 0, aspread = NULL,
                           home_effect = TRUE, wager = 1, hBL = -110, aBL = -110){
  #Compute probs that each team beats the spread
  probs <- spreadprob_emp(design, home, away, hspread, aspread, home_effect)
  AwayProb <- probs$a_prob[1]
  HomeProb <- probs$h_prob[1]
  aOdds <- american_to_odds(aBL)
  hOdds <- american_to_odds(hBL)
  eAway <- wager*aOdds*AwayProb - wager*HomeProb
  eHome <- wager*hOdds*HomeProb - wager*AwayProb
  EXP <- probs %>%
    dplyr::mutate(hBL = hBL,
                  aBL = aBL,
                  wager = wager,
                  eHome = eHome,
                  eAway = eAway,
    )
  return(EXP)
}
