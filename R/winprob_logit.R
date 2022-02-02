#Experiment with over under predictions
library(NFLpredictions)

#Load some games
G <- scrape_games(wk_stop = 21)
data <- XY_differences(games)

#Fit a bradley-terry model
X <- data$X
Y_results <- ifelse(data$Y_diff >= 0, 1, 0)

bt <- glm(Y_results ~ X + 0, family = binomial(link = "logit"))
summary(bt)

cont <- c(1, rep(0, 32))
cont[colnames(X) == "Los Angeles Rams"] <- 1
cont[colnames(X) == "Cincinnati Bengals"] <- -1

#predict(bt, newdata = cont, type = "response")
betahat <- bt$coefficients

cbetahat1 <- betahat["XLos Angeles Rams"] - betahat["XCincinnati Bengals"] + 0.0897
cbetahat <- sum(na.omit(cont*betahat))
hwinprob <- 1/(1+exp(-cbetahat))
hwinprob
