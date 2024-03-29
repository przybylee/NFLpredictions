% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eSpread.R
\name{espread_normal}
\alias{espread_normal}
\title{Predict the expected value of a wager for either team beating the spread
assuming errors follow a normal distribution.}
\usage{
espread_normal(
  design,
  home,
  away,
  hspread = 0,
  aspread = NULL,
  home_effect = TRUE,
  wager = 1,
  hBL = -110,
  aBL = -110
)
}
\arguments{
\item{design}{List of season data as produced by get_design(), contains
design matrix X, scores Y_diff, and a list of teams}

\item{home}{Name of the home team, could be a substring}

\item{away}{Name of the away team, could be a substring}

\item{hspread}{Point spread for the home team}

\item{aspread}{Point spread for the away team}

\item{home_effect}{Logical, indicates if we consider home field advantage}

\item{wager}{Amount of money being wagered in dollars}

\item{hBL}{The betting line for the home team against the spread, American}

\item{aBL}{The betting line for the away team against the spread, American}
}
\value{
A dataframe indicating the expected value of wagering on each team to
beat the spread.
}
\description{
Predict the expected value of a wager for either team beating the spread
assuming errors follow a normal distribution.
}
\examples{
List <- get_design(regssn2021)
espread_normal(List, "Patriots", "Bills", hspread = -5, hBL = -115, aBL = -105)
}
