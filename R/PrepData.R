#' Remove games from data set that have missing information
#'
#' @param data A data frame with columns for Team, Score, Year, and Week
#' produced by scrape_games.  These 3 columns must be numeric.
#' @param game_ids Logical argument to indicate if we want the returned data frame
#' to include game_id numbers
#'
#' @return A cleaned version of game data with games that had incomplete data
#' removed
#' @export
#'
#' @examples
#' drop_incomplete_games(regssn2021, game_ids = TRUE)
drop_incomplete_games <- function(data, game_ids = FALSE){
 n <- length(data[,1])
 data0 <- data %>% dplyr::mutate(game_id = rep(1:(n/2), each = 2))
 complete <- stats::complete.cases(data)
 complete_games <- data0 %>%
   dplyr::mutate(complete = ifelse(complete, 1,0)) %>%
   dplyr::group_by(game_id) %>%
   dplyr::summarize(cases = sum(complete)) %>%
   dplyr::filter(cases == 2) %>%
   dplyr::pull(game_id)
 data0 <- dplyr::filter(data0, game_id %in% complete_games)
 if(!game_ids){
   data0 <- data0 %>% dplyr::select(!game_id)
 }
 return(data0)
}


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
  team_ids <- data.frame(tm_id = 1:length(teams), name = teams)
  result <- list(X,X_sum, Y_diff, Y_sum, Y_binary, team_ids, data)
  names(result) <- c("X", "X_sum", "Y_diff", "Y_sum", "Y_binary", "teams",
                     "games")
  return(result)
}


#' Search for teams in the design
#'
#' @param design Design object with information from a collection of games.
#' This is a list that is the output of get_design()
#' @param string String which contains part of a team name being searched for
#' @param ignore.case Whether or not the searc is case sensitive
#'
#' @return A vector of team names that contain string
#' @export
#'
#' @examples
#' design <- get_design(regssn2021)
#' team_detect(design, "new")
team_detect <- function(design, string, ignore.case = TRUE){
  teams <- design$teams
  match_indx <- teams$name %>%
    stringr::str_detect(stringr::regex(string, ignore_case = ignore.case))
  matches <- teams[match_indx,]
  return(matches)
}

#' Pivot games data into a wider format
#'
#' @param games A data frame with columns for Team, Score, Year, and Week where
#' each game is represented by two rows, one for each team
#'
#' @returns A data frame with columns for game_id, home_team, home_score,
#' away_team, and away_score for each game
#' @export
#'
#' @examples
#' games <- regssn2021
#' pivot_games_wide(games)
pivot_games_wide <- function(games) {
  n_games <- nrow(games)/2

  games %>%
    rename(team = Team, score = Score, year = Year, week = Week) %>%
    mutate(game_id = rep(1:n_games, each = 2),
           home_away = rep(c("home", "away"), n_games)
    ) %>%
    tidyr::pivot_wider(names_from = home_away, values_from = c("team", "score"))

}
