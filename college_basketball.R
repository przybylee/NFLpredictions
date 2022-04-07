data <- read.csv("scores_3-18.csv")
head(data)

#Load NFLpredictions
library(devtools)
load_all()
library(dplyr)

data <- data %>%
  mutate(Score = Final, Year = 2021) %>%
  drop_incomplete_games()

design <- get_design(data)

#Teams "NotreDame", "TexasTech"

eML_normal(design, "Duke", "Arkansas", hBL = -210, aBL = 175, home_effect = FALSE)
espread_normal(design, 140, "Miami", hspread = -6, home_effect = FALSE)
espread_emp(design, "IowaSt", "Miami", hspread = 3, home_effect = FALSE)
winprob_emp(design, "IowaSt", "Miami")

team_detect(design, "Kansas")
