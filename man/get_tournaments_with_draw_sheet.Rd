% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_calls.R
\name{get_tournaments_with_draw_sheet}
\alias{get_tournaments_with_draw_sheet}
\title{Get a list of tournaments from TournamentApi, append filters column, unnest and then
append draw sheet column}
\usage{
get_tournaments_with_draw_sheet(circuit, start_date, days)
}
\arguments{
\item{circuit}{The circuit to query on: WCT, MT, WT, JT}

\item{start_date}{Date to take tournaments from (can be string)}

\item{days}{Number of days to include in query (integer)}
}
\value{
A dataframe of all tournaments, events, and matches
}
\description{
Get a list of tournaments from TournamentApi, append filters column, unnest and then
append draw sheet column
}
\examples{
get_tournaments("WCT", "2020-01-01", 31)
}
