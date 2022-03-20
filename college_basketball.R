data <- read.csv("scores_3-18.csv")
head(data)

#Load NFLpredictions
load_all()
library(dplyr)

data <- data %>%
  mutate(Score = Final, Year = 2021) %>%
  drop_incomplete_games()

design <- get_design(data)

#Teams "NotreDame", "TexasTech"

eML_normal(design, "MichiganSt", "Duke", hBL = 215, aBL = -265, home_effect = FALSE)
espread_normal(design, "MichiganSt", "Duke", hspread = 6.5, home_effect = FALSE)
