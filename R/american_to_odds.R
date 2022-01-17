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
