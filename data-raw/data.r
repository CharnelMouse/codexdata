library(data.table)
library(magrittr)
source("R/check_data.r")
source("R/convert_deck_names.r")

starters <- fread("data-raw/starters.csv")
check_primary_keys_unique(starters, "starter")
check_no_required_values_missing(starters)
stopifnot(all(is.element(starters$base, c("no", "yes"))))
maps <- fread("data-raw/maps.csv", sep = ",")
check_primary_keys_unique(maps, "map")
check_no_required_values_missing(maps)
players <- fread("data-raw/players.csv")
check_primary_keys_unique(players, "player")
check_no_required_values_missing(players)
tournaments <- fread("data-raw/tournaments.csv")
check_primary_keys_unique(tournaments, "tournament")
check_no_required_values_missing(tournaments, "notes")
stopifnot(all(is.element(tournaments$type, c("casual", "tournament", "series"))))
deck_types <- fread("data-raw/deck_types.csv")
check_primary_keys_unique(deck_types, "deck_type")

aliases <- fread("data-raw/aliases.csv")
check_primary_keys_unique(aliases, "alias")
check_foreign_keys(aliases, players, "player")
check_no_required_values_missing(aliases)
specs <- fread("data-raw/specs.csv")
check_primary_keys_unique(specs, "spec")
check_foreign_keys(specs, starters, "starter")
check_no_required_values_missing(specs)
stopifnot(all(is.element(specs$base, c("no", "yes"))))
entry_rules <- fread("data-raw/entry_rules.csv")
check_primary_keys_unique(entry_rules, c("tournament", "entry_type"))
check_foreign_keys(entry_rules, tournaments, "tournament")
check_foreign_keys(entry_rules, deck_types, "deck_type")
check_no_required_values_missing(entry_rules, c("entry_type", "max_decks"))
stopifnot(all(is.element(entry_rules$fixed_deck, c("no", "yes"))))
stopifnot(all(entry_rules$max_decks >= entry_rules$min_decks, na.rm = TRUE))
stopifnot(all(is.element(entry_rules$driver, c("no", "yes"))))
matches <- fread("data-raw/matches.csv", colClasses = list(Date = c("start", "end")), na.strings = "")
matches[, c("player1", "player2", "victor") := lapply(.SD,
                                                      function(x) {
                                                        ifelse(!is.element(x, aliases$alias),
                                                               x,
                                                               aliases[match(x, alias), player])
                                                      }),
        .SDcols = c("player1", "player2", "victor")]
check_primary_keys_unique(matches, c("end", "tournament", "round", "round_match_number"))
check_foreign_keys(matches, maps, "map", optional = TRUE)
check_foreign_keys(matches[, .(player = c(player1, player2))], players, "player")
check_foreign_keys(matches[, .(player = victor)], players, "player", optional = TRUE)
check_foreign_keys(matches, tournaments, "tournament")
check_no_required_values_missing(matches,
                                 c("end", "deck1", "deck2", "map", "victor", "victory", "unknown_order", "notes"))
stopifnot(all(matches$end >= matches$start, na.rm = TRUE))
stopifnot(all(matches[, is.na(victor) | victor == player1 | victor == player2]))
stopifnot(all(is.element(matches$victory, c("normal", "timeout", "forfeit", NA_character_))))
stopifnot(all(is.element(matches$format, c("forum", "tabletopia", "face-to-face"))))
stopifnot(all(is.element(matches$unknown_order, c("TRUE", NA_character_))))

decks <- fread("data-raw/decks.csv")
decks[, c("player") := lapply(.SD,
                              function(x) {
                                ifelse(!is.element(x, aliases$alias),
                                       x,
                                       aliases[match(x, alias), player])
                              }),
      .SDcols = c("player")]
check_primary_keys_unique(decks, c("tournament", "player", "deck_number"))
check_foreign_keys(decks, tournaments, "tournament")
check_foreign_keys(decks, players, "player")
check_no_required_values_missing(decks)
nicknames <- fread("data-raw/nicknames.csv")
check_primary_keys_unique(nicknames, "nickname")
check_foreign_keys(nicknames, starters, "starter")
check_foreign_keys(nicknames[, .(spec = c(spec1, spec2, spec3))], specs, "spec")
check_no_required_values_missing(nicknames)
entries <- fread("data-raw/entries.csv")
entries[, c("player") := lapply(.SD,
                                function(x) {
                                  ifelse(!is.element(x, aliases$alias),
                                         x,
                                         aliases[match(x, alias), player])
                                }),
        .SDcols = c("player")]
check_primary_keys_unique(entries, c("tournament", "player"))
check_foreign_keys(entries, players, "player")
check_foreign_keys(entries, tournaments, "tournament")
check_foreign_keys(entries, entry_rules, "entry_type")
check_no_required_values_missing(entries, c("win", "loss", "bye"))
stopifnot(nrow(fsetdiff(entries[, c("tournament", "entry_type")], entry_rules[, c("tournament", "entry_type")])) == 0L)

standardised_nicknames <- nicknames[, .(nickname,
                                        name = standardise_deck_name(paste(spec1, spec2, spec3, starter, sep = "/"),
                                                                     specs))]
if (nrow(check_player_stats_agree(get_player_win_stats(matches), entries, include_nas = FALSE)) > 0L)
  stop("Player stats do not agree")
if (nrow(check_player_sticks_to_tournament_deck(matches, decks, entry_rules)) > 0L)
  stop("Player deck changes within a fixed-deck tournament")
if (any(!is.na(matches$deck1) & matches$deck1 != standardise_deck_name(matches$deck1, specs, standardised_nicknames)) ||
    any(!is.na(matches$deck2) & matches$deck2 != standardise_deck_name(matches$deck2, specs, standardised_nicknames)))
  stop("there are non-standardised names in matches")
matches[, c("deck1", "deck2") := Map(standardise_deck_name, list(deck1, deck2),
                                     MoreArgs = list(starters = specs, nicknames = standardised_nicknames))]
check_primary_keys_unique(matches, c("end", "tournament", "round", "round_match_number"))
fwrite(matches, "data-raw/matches.csv")
if (any(!is.na(decks$deck) & decks$deck != standardise_deck_name(decks$deck, specs, standardised_nicknames)))
  stop("there are non-standardised names in decks")

nicknames <- standardised_nicknames
starters <- specs
usethis::use_data(nicknames, starters, entries, overwrite = TRUE)

monocolour_deck_names <- paste0("Mono", setdiff(starters$starter, "Neutral"))
monocolour_deck_components <- prepare_deck_names_for_modelling(monocolour_deck_names, specs, nicknames)
possible_spec_trios <- utils::combn(specs$spec, 3L, paste, collapse = "/")
draft_decks <-  standardise_deck_name(apply(expand.grid(possible_spec_trios, unique(specs$starter)),
                                            1, paste, collapse = "/"),
                                      specs)
multicolour_decks <- draft_decks[stringr::str_count(draft_decks, "/") == 2L]
draft_deck_names <- standardise_deck_name(draft_decks, specs, nicknames)
multicolour_deck_names <- standardise_deck_name(multicolour_decks, specs, nicknames)

draft_deck_components <- prepare_deck_names_for_modelling(draft_deck_names, specs, nicknames)
multicolour_deck_components <- prepare_deck_names_for_modelling(multicolour_deck_names, specs, nicknames)
usethis::use_data(monocolour_deck_names, monocolour_deck_components,
                  multicolour_deck_names, multicolour_deck_components,
                  draft_deck_names, draft_deck_components,
                  overwrite = TRUE,
                  internal = TRUE)

metal_matches <- fread("data-raw/metalize matches.csv")
check_primary_keys_unique(metal_matches, "GameID")
metal_tournaments <- fread("data-raw/metalize tournaments.csv")
check_primary_keys_unique(metal_tournaments, "EventID")

metal_matches[`Player1 Deck` == "Wgute", `Player1 Deck` := "White"]
Map(function(x, y) metal_matches[,
                                 c("Player1 Deck", "Player2 Deck") := Map(stringr::str_replace,
                                                                          list(`Player1 Deck`, `Player2 Deck`),
                                                                          x, y)],
    c("Ninjutsu", "Finesse", "Finess"),
    c("Ninjitsu", "Finess", "Finesse"))
metal_matches[, c("Player1", "Player2", "Victor") := lapply(.SD,
                                                            function(x) {
                                                              ifelse(!is.element(x, aliases$alias),
                                                                     x,
                                                                     aliases[match(x, alias), player])
                                                            }),
              .SDcols = c("Player1", "Player2", "Victor")]
clean_tournaments <- metal_tournaments[, .(EventID, `event date` = as.Date(Date, "%d.%m.%Y"),
                                           `Tournament?`,
                                           format = Medium,
                                           tournament = Description)]
clean_tournaments[format == "Offline", format := "face-to-face"]

metalize_matches <- merge(clean_tournaments,
                          metal_matches,
                          by = "EventID")[, .(event = EventID, `event date`,
                                              `Tournament?`, format, tournament,
                                              game = GameID,
                                              player1 = Player1, player2 = Player2, victor = Victor,
                                              deck1 = ifelse(stringr::str_detect(`Player1 Deck`, "/"),
                                                             vapply(stringr::str_split(`Player1 Deck`, "/"),
                                                                    function(x) paste(c(x[-1], x[1]), collapse = "/"),
                                                                    character(1)),
                                                             paste0("Mono", `Player1 Deck`)),
                                              deck2 = ifelse(stringr::str_detect(`Player2 Deck`, "/"),
                                                             vapply(stringr::str_split(`Player2 Deck`, "/"),
                                                                    function(x) paste(c(x[-1], x[1]), collapse = "/"),
                                                                    character(1)),
                                                             paste0("Mono", `Player2 Deck`)),
                                              `victory type` = Victory_Type, round = Round,
                                              notes = Description)
                                          ][, .(start = `event date`,
                                                end = `event date`,
                                                tournament = ifelse(`Tournament?` == "Casual",
                                                                    "Casual",
                                                                    tournament),
                                                round, round_match_number = NA,
                                                player1, player2,
                                                deck1 = standardise_deck_name(stringr::str_remove_all(deck1, "\\[|\\]"),
                                                                              specs,
                                                                              nicknames),
                                                deck2 = standardise_deck_name(stringr::str_remove_all(deck2, "\\[|\\]"),
                                                                              specs,
                                                                              nicknames),
                                                map = NA_character_,
                                                victor,
                                                victory = `victory type`, format,
                                                unknown_order = NA, recorder = "LeonidG", notes)]
if (any(metalize_matches$end < metalize_matches$start))
  stop("there are Metalize matches that end before they start!")
Map(function(x, y) metalize_matches[victor == x, victor := y],
    c("IvanD", "BorisB", "Rita"),
    c("IvanP", "Boris", "RitaP"))
metalize_matches[stringr::str_detect(tournament, "Monocolour") & is.na(round), round := 3L]
metalize_matches[, deck1 := standardise_deck_name(deck1, starters, nicknames)]
if (metalize_matches[tournament != "Casual" & !is.element(victor, c(player1, player2)), .N] > 0L)
  stop("Invalid victors")

metalize_decks <- unique(metalize_matches[,
                                          .(tournament,
                                            player = c(player1, player2),
                                            deck = c(deck1, deck2))])[,
                                                                      deck_number := seq.int(.N),
                                                                      by = c("tournament", "player")
                                                                      ][, .(tournament, player, deck_number, deck)]

check_primary_keys_unique(metalize_matches[tournament != "Casual"], names(metalize_matches))
if (any(metalize_matches[, !is.na(victor) & !is.element(victor, c(player1, player2))]))
  stop("there are victors that don't match a player name")
dummy_metalize_entry_rules <- metalize_matches[, .(tournament = setdiff(unique(tournament), "Casual"),
                                                   fixed_deck = "yes")]
if (nrow(check_player_sticks_to_tournament_deck(metalize_matches, metalize_decks, dummy_metalize_entry_rules)) > 0L)
  stop("Player deck changes within a fixed-deck tournament")
if (any(!is.na(metalize_matches$deck1) &
        metalize_matches$deck1 != standardise_deck_name(metalize_matches$deck1, specs, nicknames)) ||
    any(!is.na(metalize_matches$deck2) &
        metalize_matches$deck2 != standardise_deck_name(metalize_matches$deck2, specs, nicknames)))
  stop("there are non-standardised names")

check_primary_keys_unique(metalize_decks, c("tournament", "player", "deck_number"))

fwrite(metalize_matches, "data-raw/fixed metalize matches.csv")
fwrite(metalize_decks, "data-raw/fixed metalize decks.csv")

matches <- rbind(matches, metalize_matches)[order(end, tournament, round, round_match_number)]
decks <- rbind(decks, metalize_decks)[order(tournament)]
usethis::use_data(matches, overwrite = TRUE)
usethis::use_data(decks, overwrite = TRUE)
