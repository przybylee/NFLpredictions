devtools::load_all()
library(nflreadr)

# Perform validation by running the model week to week and checking the results

# season <- 2023
#
# # # Get the data for the season using orignal game scraping function
# # games0 <- scrape_games(season, 1, 19)
#
# # Get games and team names using nflreadr
# games0 <- nflreadr::load_schedules(season)
# teams <- nflreadr::load_teams(season) %>%
#   select(team_abbr, team_name)
#
# # Join team names
# games <- games0 %>%
#   rename(home_team_abbr = "home_team", away_team_abbr = "away_team") %>%
#   left_join(teams, by = c("home_team_abbr" = "team_abbr")) %>%
#   rename(home_team = team_name) %>%
#   left_join(teams, by = c("away_team_abbr" = "team_abbr")) %>%
#   rename(away_team = team_name)
#
# data <- games_wide_to_design(games)

# n_weeks <- max(games$week)

run_week <- function(wk, gms) {
  # # for development
  # wk <- 4 # Week 3 did not work, week 4 did
  # gms <- games

  futile.logger::flog.info("Getting games for  week %i", wk)
  games_pre_week <- gms %>% filter(week < wk)

  #Use this for nflreadr data
  data_week <- games_wide_to_design(games_pre_week)

  games_week <- gms %>% filter(week == wk)

  #futile.logger::flog.info("Checking rank of design matrix")
  design_rank <- data_week$X %>%
    rbind(c(0, rep(1, ncol(data_week$X) - 1))) %>%
    pracma::Rank()

  if(design_rank < ncol(data_week$X)) {
    "Design matrix is not full rank, no predictions for Week {wk}" %>%
      glue::glue() %>%
      futile.logger::flog.warn()

    return(NULL)
  }

  home <- games_week$home_team

  away <- games_week$away_team

  # add a catch for when the models fail to converge

   futile.logger::flog.info("Running models and making predictions")

  # OLS Normal
  ols_normal <- winprob_normal(data_week, home, away)
  ols_normal_preds <- ols_normal$probs %>%
    mutate(week = wk) %>%
    select(week, home, away, p_normal1 = win_prob, sp_ols1 = est_spread)

  # OLS Empirical
  ols_emp <- winprob_emp(data_week, home, away)
  ols_emp_preds <- ols_emp$probs %>%
    mutate(week = wk) %>%
    select(week, home, away, p_emp = win_prob)


  # OLS Normal without home field advantage
  ols_normal2 <- winprob_normal(
    data_week, home, away, home_advantage_fit = FALSE
  )
  ols_normal2_preds <- ols_normal2$probs %>%
    mutate(week = wk) %>%
    select(week, home, away, p_normal2 = win_prob, sp_ols2 = est_spread)

  # Logistic
  logistic <- winprob_logistic(data_week, home, away)
  logistic_preds <- logistic$probs %>%
    mutate(week = wk) %>%
    select(week, home, away, p_logistic = win_prob)

  # Logistic without home field advantage
  logistic2 <- winprob_logistic(data_week, home, away,
                                home_advantage_fit = FALSE
  )

  logistic2_preds <- logistic2$probs %>%
    mutate(week = wk) %>%
    select(week, home, away, p_logistic2 = win_prob)

  # Compbine predictions by leftjoin to weeks games using home, away, and week
  output <- games_week %>%
    left_join(ols_normal_preds,
              by = c("home_team" = "home", "away_team" = "away", "week")
    ) %>%
    left_join(ols_emp_preds,
              by = c("home_team" = "home", "away_team" = "away", "week")
    ) %>%
    left_join(ols_normal2_preds,
              by = c("home_team" = "home", "away_team" = "away", "week")
    ) %>%
    left_join(logistic_preds,
              by = c("home_team" = "home", "away_team" = "away", "week")
    ) %>%
    left_join(logistic2_preds,
              by = c("home_team" = "home", "away_team" = "away", "week")
    )

  return(output)

}

# use run week to run the model for each week of a given season
run_season <- function(season) {

  futile.logger::flog.info("Running season %i", season)
  futile.logger::flog.info("Getting schedule from nflreadr")
  games0 <- nflreadr::load_schedules(season)
  teams <- nflreadr::load_teams(season) %>%
    select(team_abbr, team_name)

  # Join team names
  games <- games0 %>%
    rename(home_team_abbr = "home_team", away_team_abbr = "away_team") %>%
    left_join(teams, by = c("home_team_abbr" = "team_abbr")) %>%
    rename(home_team = team_name) %>%
    left_join(teams, by = c("away_team_abbr" = "team_abbr")) %>%
    rename(away_team = team_name)

  data <- games_wide_to_design(games)

  n_weeks <- max(games$week)

  output <- purrr::map_dfr(2:n_weeks, ~run_week(., gms = games))
}

#results <- run_season(2023)

# use run season to run all seasons
run_all_seasons <- function(start, end) {
  purrr::map_dfr(start:end, ~run_season(.))
}



# run seasons and save results --------------------------------------------

results <- run_all_seasons(1999 , 2024)

# save results
saveRDS(results, "Research/win_prob_validation2025-02-05.rds")


