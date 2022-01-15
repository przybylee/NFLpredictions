AwayTm = "Kansas City Chiefs"
HomeTm = "Tampa Bay Buccaneers"
#Enter the moneyline
line_away <- -163
line_home <- 143
spread <- -3
#colnames(X)
c <- c(1,rep(0, length(teams)))
c[colnames(X) == HomeTm] <- 1
c[colnames(X) == AwayTm] <- -1
#Put contrast vector c into confint to estimate HomeTm score - AwayTm score
conf_int(X, y = Y, cont = c, d = 0, a = 0.05)
#Using the estimated difference in team effects, compute a win probability
mu <- conf_int(X, y = Y, c = c, d = 0, a = 0.05)[1,1]
sigma <- sqrt(sigsq <- anova(lm(Y ~ X+0))[2,3])
AwayProb <- pnorm(0,mean = mu, sd = sigma)
HomeProb <- pnorm(0,mean = mu, sd = sigma, lower.tail = FALSE)
