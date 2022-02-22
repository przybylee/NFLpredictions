
#' Get design matrix and values from a season
#'
#' @param data A data frame with columns for Team, Score, Year, and Week
#' produced by scrape_games.  Last 3 columns must be numeric.
#'
#' @return A list of 9 components, including 2 design matrices (X, X_sum),
#' and 3 vectors of observations of season data (Y_diff, Y_sum, Y_binary), the
#' list of teams, original dataframe of scores, and date range of games.
#' @export
#'
#' @examples
#' games <- regssn2021
#' L <- get_design(games)
get_design <- function(data){
  N <- length(data$Score)
  M <- length(unique(data$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  teams <- sort(unique(data$Team))
  X <- matrix(data = 0, nrow = N/2, ncol = M+1)
  colnames(X) <- c("home", teams)
  Y_diff <- rep(0, N/2)
  Y_sum <- rep(0, N/2)
  #Fill in cols of X with 1's where appropriate
  row = 1
  for(n in seq(2,N, by = 2)){
    X[row,"home"] <- 1
    X[row, data$Team[n-1]] <- -1
    X[row, data$Team[n]] <- 1
    Y_diff[row] <- data$Score[n] - data$Score[n-1]
    Y_sum[row] <- data$Score[n] + data$Score[n-1]
    row <- row +1
  }
  Y_binary <- ifelse(Y_diff >= 0, 1, 0)
  X_sum <- abs(X)
  colnames(X)[1] <- "int"
  #start <- paste(data$Year[1], ", week ", data$Week[1])
  #end <- paste(data$Year[N], ", week ", data$Week[N])
  result <- list(X,X_sum, Y_diff, Y_sum, Y_binary, teams, data)
  names(result) <- c("X", "X_sum", "Y_diff", "Y_sum", "Y_binary", "teams",
                     "games")
  return(result)
}
