#' Recorded match
#'
#' Tournament match information for games played on the forum, plus information
#' for tournament and casual face-to-face matches recorded by Metalize.
#'
#' @format A data.table:
#' \describe{
#'   \item{start}{A Date, giving the day the match started.}
#'   \item{end}{A Date, giving the day the match finished.}
#'   \item{tournament}{A character, giving the tournament name.}
#'   \item{round}{An integer, giving the tournament round.}
#'   \item{round_match_number}{An integer, giving the listed number of the match
#'   within the round.}
#'   \item{player1}{A character, giving the name of Player 1.}
#'   \item{player2}{A character, giving the name of Player 2.}
#'   \item{deck1}{A character, giving the name of Player 1's deck.}
#'   \item{deck2}{A character, giving the name of Player 2's deck.}
#'   \item{map}{A character, giving the name of any map used, or \code{NA} if
#'   non used.}
#'   \item{victor}{A character, giving the winning player's name.}
#'   \item{victory}{A character, giving the victory type: \code{timeout}
#'   indicates a match that was abjudicated due to time, \code{normal} indicates
#'   a match that ended in concession or a base kill, \code{forfeit} indicates a
#'   match ended due to irrecoverable play errors.}
#'   \item{format}{A character, giving how the game was played. This is usually
#'   \code{forum}.}
#'   \item{unknown_order}{A logical: \code{TRUE} if the order of the players is
#'   unknown, i.e. \code{player1} might actually be Player 2. \code{NA}
#'   otherwise.}
#'   \item{recorder}{A character, giving the forum name of the person who
#'   recorded the match.}
#'   \item{notes}{A character, giving any notes on the match the recorder felt
#'   like giving.}
#' }
#' @source Original forum games on \url{http://forums.sirlingames.com/}. An
#'   online copy is kept at
#'   \url{https://docs.google.com/spreadsheets/d/1lMTR9hYlfLQCQwWA4B4c4a4xcyDRS-CeavLs48Q2mJI/edit?usp=sharing}.
#'    Other games are a tidied version of
#'   \url{https://docs.google.com/spreadsheets/d/1xAClI8QRwdoK47UYnt4oC8B4h9PsAeRzxpViVKm53_0/edit?usp=sharing}.
"matches"

#' Common Codex deck nicknames
"nicknames"

#' Specification-starter lookup table
"starters"

#' Tournament entract deck information for forum games
#'
#' Tournament decks chosen by entrants, for games played on the forum and
#' face-to-face tournaments recorded by Metalize.
#'
#' @format A data.table:
#' \describe{
#'   \item{tournament}{A character, giving the tournament name.}
#'   \item{entrant_number}{An integer, giving the order in which the entrant
#'   signed up, with 1 signing up first.}
#'   \item{player}{A character, giving the player's name.}
#'   \item{deck}{A character, giving the player's deck's name in standardised form, using nicknames.}
#'   \item{win}{An integer, giving the number of games the player won.}
#'   \item{loss}{An integer, giving the number of games the player lost.}
#'   \item{bye}{An integer, giving the number of rounds in which the player was given a bye.}
#' }
#' @source Original forum games on \url{http://forums.sirlingames.com/}. An
#'   online copy is kept at
#'   \url{https://docs.google.com/spreadsheets/d/1lMTR9hYlfLQCQwWA4B4c4a4xcyDRS-CeavLs48Q2mJI/edit?usp=sharing}.
#'    Other games are a tidied version of
#'   \url{https://docs.google.com/spreadsheets/d/1xAClI8QRwdoK47UYnt4oC8B4h9PsAeRzxpViVKm53_0/edit?usp=sharing}.
"decks"


#' Tournament entrants
#'
#' Entrants for each recorded tournament, plus an entrant number according to
#' order of entry, if known.
#'
#' @format A data.table:
#' \describe{
#'   \item{tournament}{A character, giving the tournament name.}
#'   \item{entrant_number}{An integer, giving the order in which the entrant
#'   signed up, with 1 signing up first.}
#'   \item{player}{A character, giving the player's name.}
#'   \item{entry_type}{A character, giving the entrant type if the tournament requires one, and \code{""} if not.}
#'   \item{win}{An integer, giving the number of games the player won.}
#'   \item{loss}{An integer, giving the number of games the player lost.}
#'   \item{bye}{An integer, giving the number of rounds in which the player was given a bye.}
#' }
#' @source Original forum games on \url{http://forums.sirlingames.com/}. An
#'   online copy is kept at
#'   \url{https://docs.google.com/spreadsheets/d/1lMTR9hYlfLQCQwWA4B4c4a4xcyDRS-CeavLs48Q2mJI/edit?usp=sharing}.
#'    Other games are a tidied version of
#'   \url{https://docs.google.com/spreadsheets/d/1xAClI8QRwdoK47UYnt4oC8B4h9PsAeRzxpViVKm53_0/edit?usp=sharing}.
"entries"
