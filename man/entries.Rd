% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{entries}
\alias{entries}
\title{Tournament entrants}
\format{
A data.table:
\describe{
  \item{tournament}{A character, giving the tournament name.}
  \item{entrant_number}{An integer, giving the order in which the entrant
  signed up, with 1 signing up first.}
  \item{player}{A character, giving the player's name.}
  \item{entry_type}{A character, giving the entrant type if the tournament requires one, and \code{""} if not.}
  \item{win}{An integer, giving the number of games the player won.}
  \item{loss}{An integer, giving the number of games the player lost.}
  \item{bye}{An integer, giving the number of rounds in which the player was given a bye.}
}
}
\source{
Original forum games on \url{http://forums.sirlingames.com/}. An
  online copy is kept at
  \url{https://docs.google.com/spreadsheets/d/1lMTR9hYlfLQCQwWA4B4c4a4xcyDRS-CeavLs48Q2mJI/edit?usp=sharing}.
   Other games are a tidied version of
  \url{https://docs.google.com/spreadsheets/d/1xAClI8QRwdoK47UYnt4oC8B4h9PsAeRzxpViVKm53_0/edit?usp=sharing}.
}
\usage{
entries
}
\description{
Entrants for each recorded tournament, plus an entrant number according to
order of entry, if known.
}
\keyword{datasets}
