% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrepData.R
\name{team_detect}
\alias{team_detect}
\title{Search for teams in the design}
\usage{
team_detect(design, string, ignore.case = TRUE)
}
\arguments{
\item{design}{Design object with information from a collection of games.
This is a list that is the output of get_design()}

\item{string}{String which contains part of a team name being searched for}

\item{ignore.case}{Whether or not the searc is case sensitive}
}
\value{
A vector of team names that contain string
}
\description{
Search for teams in the design
}
\examples{
design <- get_design(regssn2021)
team_detect(design, "new")
}
