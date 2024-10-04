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
  if(line > 0){odds <- abs(line)/100}
  else{odds <- 100/abs(line)}
  return(odds)
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
