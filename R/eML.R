#' Calculate the expected value for wagering on each team assuming a normal
#' distribution.
#'
#' @param design List of season data as produced by XY_differences, contains
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
#' data <- get_design(G)
#' eML_normal(data, "Bills", "Patriots", wager = 20, hBL = -160, aBL = 180)
eML_normal <- function(design, home, away, wager = 1, hBL = -110, aBL = -110,
                           home_effect = TRUE){
  probs <- winprob_normal(design, home, away, home_effect, verbose = FALSE)
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

