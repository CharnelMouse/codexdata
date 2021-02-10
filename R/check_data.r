check_primary_keys_unique <- function(
  dt,
  colnames
) {
  if (length(colnames) == 0)
    stop("colnames cannot be length zero")
  if (any(!is.element(colnames, colnames(dt))))
    stop("colnames must exist in dt")
  primary <- dt[, ..colnames]
  dup <- duplicated(primary)
  if (any(dup)) {
    dups <- unique(primary[dup])
    error_list <- paste(
      apply(
        dups,
        1L,
        function(y) paste(colnames, y, sep = ": ", collapse = "\n")
      ),
      collapse = "\n\n"
    )
    stop(paste0("there are duplicated primary keys:\n", error_list))
  }
}

check_foreign_keys <- function(
  dt,
  ref,
  keys,
  ref_keys = keys,
  optional = FALSE
) {
  if (length(keys) == 0)
    stop("require at least one key")
  if (length(keys) != length(ref_keys))
    stop("keys and ref_keys must be same length")
  dt_miss <- setdiff(keys, colnames(dt))
  if (length(dt_miss) > 0)
    stop(paste("there are keys not in dt:", paste(dt_miss, collapse = ", ")))
  ref_miss <- setdiff(ref_keys, colnames(ref))
  if (length(ref_miss) > 0)
    stop(paste("there are keys not in ref:", paste(ref_miss, collapse = ", ")))
  value_miss <- stats::setNames(
    Map(
      function(key, ref_key) {
        setdiff(
          dt[[key]],
          c(
            ref[[ref_key]],
            if (optional) NA
          )
        )
      },
      keys,
      ref_keys
    ),
    keys
  )
  value_miss <- value_miss[vapply(value_miss, length, integer(1)) > 0L]
  if (length(unlist(value_miss)) > 0)
    stop(paste0(
      "there are values not in ref:\n",
      paste(
        names(value_miss),
        vapply(value_miss, paste, character(1), collapse = ", "),
        sep = ": ",
        collapse = "\n"
      )
    ))
}

check_no_required_values_missing <- function(
  dt,
  optional = character()
) {
  missing <- lapply(dt[, -..optional], function(x) which(is.na(x)))
  stripped_missing <- missing[vapply(missing, length, integer(1)) > 0L]
  if (length((stripped_missing)) > 0)
    stop(paste0(
      "there are missing required values in the following rows:\n",
      paste(
        names(stripped_missing),
        vapply(stripped_missing, paste, character(1), collapse = ", "),
        sep = ": ",
        collapse = "\n"
      )
    ))
}

get_player_win_stats <- function(
  matches
) {
  matches <- as.data.table(matches)[!is.na(victor)]
  played <- melt(
    matches,
    measure.vars = c("player1", "player2"),
    variable.name = "order",
    value.name = "player"
  )[, .(played = .N), by = c("tournament", "player")]
  won <- matches[
    , .(won = .N), by = c("tournament", "victor")
  ][
    , .(tournament, player = victor, won)
  ]
  last_round <- melt(
    matches,
    measure.vars = c("player1", "player2"),
    variable.name = "order",
    value.name = "player"
  )[
    , .(last_round = max(round)), by = c("tournament", "player")
  ]
  merge(
    merge(
      played, won,
      by = c("tournament", "player"),
      all = TRUE
    ),
    last_round,
    by = c("tournament", "player"),
    all = TRUE
  )[, won := ifelse(is.na(won), 0L, won)
  ][, c("lost", "byes") := list(played - won, last_round - played)
  ][order(tournament, -won, lost, -byes)
  ][, c("tournament", "player", "last_round", "played", "won", "lost", "byes")]
}

check_player_stats_agree <- function(
  match_stats,
  decks,
  include_nas = TRUE
) {
  deck_stats <- as.data.table(decks)[, c("tournament", "player", "win", "loss")]
  agreements <- merge(
    as.data.table(match_stats)[, -c("played")],
    as.data.table(deck_stats),
    by = c("tournament", "player")
  )[, c("wins_match", "losses_match") := list(won == win, lost == loss)]
  if (!include_nas)
    agreements <- agreements[!is.na(wins_match) & !is.na(losses_match)]
  agreements[is.na(wins_match) | !wins_match | is.na(losses_match) | !losses_match]
}

check_player_sticks_to_tournament_deck <- function(
  matches,
  decks,
  entry_rules
) {
  if (any(is.na(decks$deck)))
    stop("there are missing decks")
  fixed_deck_tournaments <- entry_rules[
    ,
    .(fixed = all(fixed_deck == "yes")), by = "tournament"
  ][
    fixed == TRUE, tournament
  ]
  expected <- decks[
    is.element(tournament, fixed_deck_tournaments),
    c("tournament", "player", "deck")
  ]
  used_decks <- matches[
    is.element(tournament, fixed_deck_tournaments),
    .(
      tournament, round,
      player = c(player1, player2),
      deck = c(deck1, deck2)
    )
  ]
  used_decks[
    !expected,
    on = c("tournament", "player", "deck")
  ]
}
