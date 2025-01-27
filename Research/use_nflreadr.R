#install.packages("nflreadr")

library(nflreadr)
devtools::load_all()

# load the games with the nflreadr package
games0 <- load_schedules(2024)

teams <- load_teams(2024) %>%
  select(team_abbr, team_name)

games <- games0 %>%
  rename(home_team_abbr = "home_team", away_team_abbr = "away_team") %>%
  left_join(teams, by = c("home_team_abbr" = "team_abbr")) %>%
  rename(home_team = team_name) %>%
  left_join(teams, by = c("away_team_abbr" = "team_abbr")) %>%
  rename(away_team = team_name)

design <- games_wide_to_design(games)
design$teams

home <- "Chiefs"
away <- "Eagles"

?winprob_normal
point_spread_ols(data = design, home, away, home_advantage_predict = FALSE)

winprob_normal(data = design, home, away, home_advantage_predict = FALSE)
winprob_emp(data = design, home, away, home_advantage_predict = FALSE)
winprob_logistic(data = design, home, away, home_advantage_predict = FALSE)
