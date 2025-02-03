#install.packages("nflreadr")

library(nflreadr)
devtools::load_all()

season <- 2023

# compare original scrape results to nflreadr results
games0 <- scrape_games(season, 1, 19)
games0_wide <- pivot_games_wide(games0)

games1 <- nflreadr::load_schedules(season)

games <- games1 %>%
  rename(home_team_abbr = "home_team", away_team_abbr = "away_team") %>%
  left_join(teams, by = c("home_team_abbr" = "team_abbr")) %>%
  rename(home_team = team_name) %>%
  left_join(teams, by = c("away_team_abbr" = "team_abbr")) %>%
  rename(away_team = team_name)

# Check what is missing from the nflreadr data
games0_wide %>%
  anti_join(games, by = c("home_team", "away_team", "week"))
# All of the orginal scraped games are in the nflreadr data

games %>%
  anti_join(games0_wide, by = c("home_team", "away_team", "week"))
# All the games missing from the scraped data are late round playoff games


design <- games_wide_to_design(games)
design$teams

home <- "Chiefs"
away <- "Eagles"

?winprob_normal
point_spread_ols(data = design, home, away, home_advantage_predict = FALSE)

winprob_normal(data = design, home, away, home_advantage_predict = FALSE)
winprob_emp(data = design, home, away, home_advantage_predict = FALSE)
winprob_logistic(data = design, home, away, home_advantage_predict = FALSE)
