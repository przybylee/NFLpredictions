devtools::load_all()

# Perform validation by running the model week to week and checking the results

season <- 2024

# Get the data for the season
games0 <- scrape_games(season, 1, 17)

data <- get_design(games0)
names(data)
data$games

games <- pivot_games_wide(games0)

n_weeks <- max(games$week)

run_week <- function(week, games0) {

}
