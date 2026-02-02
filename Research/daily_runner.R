library(nflreadr)
pkgload::load_all()

# Quick prediction for the next week's worth of games

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


