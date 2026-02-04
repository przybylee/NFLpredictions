library(nflreadr)
pkgload::load_all()

# Quick prediction for the next week's worth of games
date <- Sys.Date() # Default to current

season <- lubridate::year(date)
if(lubridate::month(date) < 9) {
  season <- season - 1
}

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

#data <- games_wide_to_design(games)

n_weeks <- max(games$week)

games_past <- games %>% filter(gameday < Sys.time())

games_upcoming <- games %>%
  filter(gameday >= lubridate::today(), gameday < lubridate::today() + 10)

design <- games_wide_to_design(games_past)

design_rank <- design$X %>%
  rbind(c(0, rep(1, ncol(design$X) - 1))) %>%
  pracma::Rank()

if(design_rank < ncol(design$X)) {
  "Design matrix is not full rank, no predictions for Week {wk}" %>%
    glue::glue() %>%
    futile.logger::flog.warn()

  #return(NULL)
}

home <- games_upcoming$home_team

away <- games_upcoming$away_team

date <- games_upcoming$gameday

# add a catch for when the models fail to converge

futile.logger::flog.info("Running models and making predictions")

# OLS Normal
ols_normal <- winprob_normal(design, home, away)
ols_normal_preds <- ols_normal$probs %>%
  select(home, away, p_normal1 = win_prob, sp_ols1 = est_spread) %>%
  mutate(date = date)

# OLS Empirical
ols_emp <- winprob_emp(design, home, away)
ols_emp_preds <- ols_emp$probs %>%
  select(home, away, p_emp = win_prob) %>%
  mutate(date = date)

# OLS Normal without home field advantage
ols_normal2 <- winprob_normal(
  design, home, away, home_advantage_fit = FALSE
)
ols_normal2_preds <- ols_normal2$probs %>%
  select(home, away, p_normal2 = win_prob, sp_ols2 = est_spread) %>%
  mutate(date = date)

# Logistic
logistic <- winprob_logistic(design, home, away)
logistic_preds <- logistic$probs %>%
  select(home, away, p_logistic = win_prob) %>%
  mutate(date = date)

# Logistic without home field advantage
logistic2 <- winprob_logistic(design, home, away, home_advantage_fit = FALSE)

logistic2_preds <- logistic2$probs %>%
  select(home, away, p_logistic2 = win_prob) %>%
  mutate(date = date)

# Combine predictions by leftjoin to weeks games using home, away, and week
output <- games_upcoming %>%
  left_join(
    ols_normal_preds,
    by = c("home_team" = "home", "away_team" = "away", "gameday" = "date")
  ) %>%
  left_join(
    ols_emp_preds,
    by = c("home_team" = "home", "away_team" = "away", "gameday" = "date")
  ) %>%
  left_join(
    ols_normal2_preds,
    by = c("home_team" = "home", "away_team" = "away", "gameday" = "date")
  ) %>%
  left_join(
    logistic_preds,
    by = c("home_team" = "home", "away_team" = "away", "gameday" = "date")
  ) %>%
  left_join(
    logistic2_preds,
    by = c("home_team" = "home", "away_team" = "away", "gameday" = "date")
  )

