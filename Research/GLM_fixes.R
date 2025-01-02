library(devtools)
#library(dplyr)
load_all()

?scrape_games
df <- scrape_games(2024, 1, 16)
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
est <- point_spread_ols(data0, "Detroit Lions", "Minnesota Vikings", TRUE)
spreadprob_normal(data0, 0, a_spread = NULL, "Detroit Lions", "Minnesota Vikings", TRUE)

est$predictions

point_spread_ols(data0, "Cleveland Browns", "New York Jets", FALSE)
original

home <- c("Browns", "Cowboys", "Bills")
away <- c("Jets", "Lions", "Patriots")

point_spread <- point_spread_ols(data, home, away, TRUE)
point_spread$predictions

h_spread <- c(15, 18, -10)
spread_probs <- spreadprob_normal(data, h_spread, a_spread = NULL,
                                  home, away,
                                  TRUE
                                  )
spread_probs$probs


spread_probs_emp <- spreadprob_emp(data, h_spread, a_spread = NULL,
                                  home, away,
                                  TRUE
                                  )
spread_probs_emp$probs

residuals <- spread_probs_emp$model$residuals %>% head(-1)

res_cdf <- ecdf(residuals)
density <- density(residuals)
plot(res_cdf)
abline(v = 0, col = "red")
abline(h = 0.5, col = "blue")

plot(density)
abline(h = 0, col = "red")
abline(v = 0, col = "blue")


mean(residuals)
median(residuals)

1- res_cdf(-8.5)

emp_model <- spread_probs_emp$model
hist(emp_model$residuals)
spreadprob_emp(data,
               h_spread = -14.8,
               a_spread = 15,
               "Green Bay Packers",
               "New Orleans Saints",
               TRUE
               )

# Things to do
# 1. Fix the point spread function to handle multiple games at once #DONE
# 2. Fix OLS win prob function to use new point spread ols #Done
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
