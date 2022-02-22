#' From a collection of game scores, create a design matrix X and a vector of differences Y_diff
#' to use for linear regression.
#'
#' @param data A data frame with columns for Team, Score, Year, and Week produced by scrape_games.  Last 3 columns must be numeric.
#'
#' @return A list with 6 elements
#' @export
#'
#' @examples
#' G <- regssn2021
#' List <- XY_differences(G)
#' List$teams
XY_differences <- function(data){
  N <- length(data$Score)
  M <- length(unique(data$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  teams <- sort(unique(data$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  colnames(X) <- c("home", teams)
  Y <- rep(0, N/2)
  #Fill in cols of X with 1's where appropriate
  row = 1
  for(n in seq(2,N, by = 2)){
    X[row,"home"] <- 1
    X[row, data$Team[n-1]] <- -1
    X[row, data$Team[n]] <- 1
    Y[row] <- data$Score[n] - data$Score[n-1]
    row <- row +1
  }
  start <- paste(data$Year[1], ", week ", data$Week[1])
  end <- paste(data$Year[N], ", week ", data$Week[N])
  result <- list(X,Y, teams, data, start, end)
  names(result) <- c("X", "Y_diff", "teams", "games", "start", "end")
  return(result)
}
