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

run_week <- function(wk, gms) {
  # for development
  wk <- 2
  gms <- games0

  futile.logger::flog.info("Getting games for  week %i", wk)
  games_week <- gms %>% filter(Week < wk)
  data_week <- get_design(games_week)

  home <- games %>%
    filter(week == wk) %>%
    pull(team_home)

  away <- games %>%
    filter(week == wk) %>%
    pull(team_away)

  # add a catch for when the models fail to converge

  futile.logger::flog.info("Running models and making predictions")

  # OLS Normal
  winprob_normal(data_week, home, away)

  # OLS Empirical
  winprob_emp(data_week, home, away)

  # OLS Normal without home field advantage
  winprob_normal(data_week, home, away, home_advantage_fit = FALSE)

  # Logistic
  winprob_logistic(data_week, home, away)

  # Logistic without home field advantage
  winprob_logistic(data_week, home, away, home_advantage_fit = FALSE)

}
