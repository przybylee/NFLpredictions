
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NFLpredictions

<!-- badges: start -->
<!-- badges: end -->

This package functions that make make it easy to analyze betting lines
for NFL games.

## Installation

You can install the development version of NFLpredictions from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("przybylee/NFLpredictions")
```

## Example

Suppose we are considering placing a wager on a first round playoff game
between the Cardinals and the Rams.

![Alt text](README_images/SampleBettingLine.PNG)

First we load the library and collect some data. We will scrape scores
from all the games from the 2021 regular season from [Pro-Football
Reference](https://www.pro-football-reference.com/).

``` r
library(NFLpredictions)
G <- scrape_games(ssn = 2021, wk_start = 1, wk_stop = 18)
head(G)
#>                       Team Score Year Week
#> new         Dallas Cowboys    29 2021    1
#> new.1 Tampa Bay Buccaneers    31 2021    1
#> new.2  Philadelphia Eagles    32 2021    1
#> new.3      Atlanta Falcons     6 2021    1
#> new.4  Pittsburgh Steelers    23 2021    1
#> new.5        Buffalo Bills    16 2021    1
tail(G)
#>                         Team Score Year Week
#> new.538     Seattle Seahawks    38 2021   18
#> new.539    Arizona Cardinals    30 2021   18
#> new.540 Los Angeles Chargers    32 2021   18
#> new.541    Las Vegas Raiders    35 2021   18
#> new.542  San Francisco 49ers    27 2021   18
#> new.543     Los Angeles Rams    24 2021   18
```

Next, we want to use linear regression to estimate the relative
strengths of each team. We can use a simple model, where
*Y*<sub>*j*</sub> is the margin of victory for the home team in game
*j*. Then we have

*y*<sub>*j*</sub> = *τ*<sub>*h*<sub>*j*</sub></sub> − *τ*<sub>*a*<sub>*j*</sub></sub> + *η* + *ε*<sub>*j*</sub>,

where *ε*<sub>*j*</sub> is observed as the residuals in our model. The
indices *h*<sub>*j*</sub> and *a*<sub>*j*</sub> indicate the home and
away teams respectively. The effect of home field advantage is
represented by *η*. The matrix equation associated with this is

*X***β** = **y**,
where

**β** = (*η*,*τ*<sub>1</sub>,...,*τ*<sub>32</sub>)′
The design matrix *X* has a row for each game in the data set, where
each row has a 1 in the first column, a 1 in column *h*<sub>*j*</sub>,
and a  − 1 in column *a*<sub>*j*</sub>. We cannot estimate **β**
directly, but we can estimate *η* as well as the difference of any two
*τ*’s using ordinary least squares. This serves as a good predictor for
the outcome of a game. Given a data frame of games resembling `G`, we
can produce the appropriate *X* and **y** using the function
`XY_differences()`.

``` r
data <- XY_differences(G)
names(data)
#> [1] "X"      "Y_diff" "teams"  "games"  "start"  "end"
```

The object `data` is a list that contains *X* and **y**. There is also a
list of the team names appearing in the original data set.Now that the
data has been sorted, we can analyze the game of interest. For starters,
we use `point_spread_ols` to estimate the Rams’ margin of victory.

``` r
point_spread_ols(data, "Rams", "Cardinals", a = 0.05)
#> [1] "Los Angeles Rams beat Arizona Cardinals by 1.8"
#>        est  std.dev     lower    upper
#> 1 1.844102 4.444196 -6.910509 10.59871
```

Notice that since we set `a = 0.05`, we get the limits of a 95%
confidence interval for
*τ*<sub>Rams</sub> − *τ*<sub>Cardinals</sub> + *η*. This is based on the
*t* statistic when we assume
*ε*<sub>*j*</sub> ∼ *N*(0,*σ*<sup>2</sup>).
We can also use use this assumption to get win probabilities.

``` r
probs <- winprob_ols(data, "Rams", "cardinals")
#> [1] "The estimated win probability for the  Arizona Cardinals at Los Angeles Rams is 0.445"
probs
#>                  h                 a h_spread    h_prob    a_prob method
#> 1 Los Angeles Rams Arizona Cardinals 1.844102 0.5546997 0.4453003 normal
```

If we wanted to look at the chances of a team beating the spread, we
could use `spreadprob_ols()`.

It is in the interest of the sports book to set the betting lines so
each wager will have negative expectation, although they also consider
the amount of risk based on where customers are placing their bets. We
would like to consider the expected value of each of our wagers. In the
long run, we could benefit from only placing wagers with positive
expectation. To see the expected value on wagering on either of the
moneylines, we use `eML_ols()`.

``` r
eML_ols(data, "Rams", "Cardinals", wager = 40, hBL = -170, aBL = 150)
#>                  h                 a     eHome    eAway method
#> 1 Los Angeles Rams Arizona Cardinals -4.760257 4.530034 normal
```

We can also consider the expected value of betting on either team to
beat the spread.

``` r
eSpread_ols(data, "Rams", "Cardinals", hspread = -3.5, wager = 40)
#>                  h                 a     eHome    eAway method
#> 1 Los Angeles Rams Arizona Cardinals -5.571235 1.934871 normal
```

If left null, the value for `aspread`, the home teams spread is assumed
to be the opposit of `hspread`. The default values of `hBL` and `aBL`
are -110, since that is usually the standard betting line for something
with equal odds after vigorish is applied.

After considering the probabities, betting on the Cardinals moneyline
seems to have the best payout relative to our estimated probability of
this actually happening. This is still the least likely wager to pay
off.
