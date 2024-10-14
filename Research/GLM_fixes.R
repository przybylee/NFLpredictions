library(devtools)
library(dplyr)
load_all()

?scrape_games
df <- scrape_games(2023, 1, 17)
df

data <- get_design(df)
data$teams
data %>% names()

# Test improved method for getting predictions
df0 <- filter(df, Week <= 16) %>% as_tibble()

data0 <- get_design(df0)

## Test point spread regression
df %>%
  filter(Week > 16) %>%
  head()

# In week 17, Cleveland beat the Jets by 17
original <- point_spread_ols(data0, "Cleveland Browns", "New York Jets", TRUE)
point_spread_ols(data, "Cleveland Browns", "New York Jets", TRUE)
point_spread_ols(data, "Cleveland Browns", "New York Jets", FALSE)
original

# Things to do
# 1. Fix the point spread function to handle multiple games at once #DONE
# 2. Fix OLS win prob function to use new point spread ols
# 3. Fix residual win prob function to use new point spread ols
# 4. Add a GLM function to estimate win probabilities


# this run leaves one of the estimates NA because it is rank deficient
logit <- glm(data$Y_binary ~ 0 + data$X, family = binomial(link = "logit"))
names(logit)
summary(logit)

X <- rbind(data$X, c(0, rep(1, ncol(data$X) - 1)))
Y <- c(data$Y_dif, 0)

ols <- lm(Y ~ 0 + X)
summary(ols)


# Do the optimization in optim:  https://rpubs.com/gingfacekillah/btm
?optim

point_spread_ols(data, "Kansas City Chiefs", "San Francisco 49ers", FALSE)


winprob_normal(data, "Kansas City Chiefs", "San Francisco 49ers", FALSE)

home <- c("Kansas City Chiefs", "Kansas City Chiefs")
away <- c("San Francisco 49ers", "San Francisco 49ers")

winprob_normal(data, home, away, FALSE)

#Need to reconfigure the predict functions to handle multiple games at once
