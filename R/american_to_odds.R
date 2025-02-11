#' Convert American betting line to odds
#'
#' @param line An American style betting line
#'
#' @return The proportion of the original wager paid out
#' @export
#'
#' @examples
#' american_to_odds(-110)
american_to_odds <- function(line){
  odds <- ifelse(line > 0, abs(line)/100, 100/abs(line))
  return(odds)
}

#' Convert American betting line to probability
#'
#' @param line integer indicating the team's betting line
#'
#' @returns Numeric vector of probabilities derived from home and away lines
#' @export
#'
#' @examples american_to_prob(-110, 100, home = TRUE)
american_to_prob <- function(line = 100) {
  odds <- american_to_odds(line)
  prob <- odds / (1 + odds)
  prob
}

#' Compute the logistic
#'
#' @param x A real number
#'
#' @returns A probability
#'
#' @description
#' The logistic function is given by
#' \deqn{\text{logistic}(x) = \frac{1}{1 + \exp(-x)}}
#'
#' @export
#'
#' @examples logistic(0)
logistic <- function(x){
  return(1/(1+exp(-x)))
}

#' Compute the logit
#'
#' @param p A probability between 0 and 1
#'
#' @return The logit of the probability
#' @export
#'
#' @description
#' This is the inverse of the logistic function, given by
#' \deqn{\text{logit}(p) = \text{log}(p/(1-p))}
#'
#'
#' @examples
#' # logit(0.5) is 0
#' logit(0.5)
logit <- function(p){
  return(log(p/(1-p)))
}
