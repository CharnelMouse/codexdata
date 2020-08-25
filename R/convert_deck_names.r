#' Standardise recorded deck names, using any given nicknames
#'
#' @param name a character vector, containing deck names or nicknames.
#' @param starters a data.table, containing columns for spec names and their associated starter colour.
#' @param nicknames an optional data.table, containing standardised deck names
#'   and their associated nicknames. This is used to convert given nicknames to
#'   their deck names.
#' @param return_nicknames if TRUE, returns a deck's nickname instead of its
#'   name if there is one in \code{nicknames}.
#'
#' @return a character vector of the same length as \code{name}, containing
#'   standardised versions of the deck names, or their nicknames when there is
#'   one.
#' @export
standardise_deck_name <- function(name, starters = starters,
                                  nicknames = data.table(name = character(0), nickname = character(0)),
                                  return_nicknames = TRUE) {
  if (!is.character(name))
    stop("input is not a character vector")
  if (length(name) == 0)
    return(character(0))
  name <- convert_from_nicknames(name, nicknames)
  check_slash_count(name)
  check_for_late_brackets(name)
  check_bracket_placement(name)
  split <- stringr::str_split(remove_brackets(name), "/") %>%
    lapply(stringr::str_to_title)
  check_all_specs_valid(split, starters)
  split_with_starter_removed_where_possible <- lapply(split, remove_starters_if_matched, starters)
  cleaned <- sort_and_collapse(split_with_starter_removed_where_possible, starters)
  if (!return_nicknames)
    cleaned
  else
    check_for_nicknames(cleaned, nicknames)
}

convert_from_nicknames <- function(name, nicknames) {
  matches <- match(name, nicknames$nickname)
  ifelse(is.na(matches) | is.na(name), name, nicknames$name[matches])
}

check_slash_count <- function(name) {
  without_brackets <- stringr::str_remove_all(name, "[\\[\\]]")
  wrong_slashes <- !is.element(stringr::str_count(without_brackets, "/"), c(2, 3)) & !is.na(name)
  stop_if_nonempty(name[wrong_slashes], "names with wrong number of slashes", which(wrong_slashes))
}

check_for_late_brackets <- function(name) {
  late_start <- stringr::str_detect(name, "[\\w/]\\[") & !is.na(name)
  stop_if_nonempty(name[late_start], "names with bracket pair started after beginning", which(late_start))
}

check_bracket_placement <- function(name) {
  brackets_present <- isTRUE(stringr::str_detect(name, "\\[[\\w/]*\\]"))
  if (any(brackets_present[!is.na(brackets_present)])) {
    invalid_brackets <- brackets_present & !stringr::str_detect(name, "^\\[[\\w/]+\\](/|$)")
    stop_if_nonempty(name[invalid_brackets], "names with incorrect bracket placement", which(invalid_brackets))
  }
}

check_all_specs_valid <- function(split, starters) {
  specs_invalid <- !vapply(split, function(x) all(is.na(x)) ||
                             (all(x[1:3] %in% starters$spec) &&
                                (length(x) == 3 || is.element(x[4], starters$starter))),
                           logical(1L))
  stop_if_nonempty(vapply(split[specs_invalid], function(x) paste(x, collapse = "/"), character(1L)),
                   "names with unrecognised spec/starter names", which(specs_invalid))
  split
}

stop_if_nonempty <- function(x, msg, indices = NULL) {
  if (length(x) > 0L) {
    if (is.null(indices))
      stop(paste0(msg, ": ", paste(x, collapse = ", ")))
    else
      stop(paste0(msg, "\n",
                  paste0(indices, ": ", x, collapse = "\n")))
  }
  NULL
}

remove_starters_if_matched <- function(split_name, starters) {
  if (length(split_name) == 3)
    return(split_name)
  spec_starters <- starters$starter[match(split_name[1:3], starters$spec)]
  matching <- which(spec_starters == split_name[4])
  if (length(matching) == 0)
    return(split_name)
  c(split_name[matching], split_name[setdiff(1:3, matching)])
}

sort_and_collapse <- function(split, starters) {
  split_starter <- lapply(split, function(x) if (length(x) == 4) x[4] else starters$starter[match(x[1], starters$spec)])
  split_spec_ids <- lapply(split, match, starters$spec)
  split_starters <- lapply(split_spec_ids, function(n) starters$starter[n])
  which_specs_matched_to_deck_starter <- Map(function(x, y) which(x == y),
                                             split_starters,
                                             split_starter)
  starter_specs <- Map(function(x, y) x[y], split, which_specs_matched_to_deck_starter)
  nonstarter_specs <- Map(function(x, y) x[setdiff(1:3, y)], split, which_specs_matched_to_deck_starter)
  first <- lapply(starter_specs,
                  function(x) if (length(x) == 0) character(0) else paste(stringr::str_sort(x), collapse = "/"))
  second <- lapply(nonstarter_specs,
                   function(x) if (length(x) == 0) character(0) else paste(stringr::str_sort(x), collapse = "/"))
  mapply(function(first, second, starter) {
    if (any(stringr::str_detect(first, "NA"), stringr::str_detect(second, "NA")))
      return(NA)
    if (length(first) == 0)
      return(stringr::str_c(paste(second, collapse = "/"), "/", starter))
    if (length(second) == 0)
      return(stringr::str_c("[", paste(first, collapse = "/"), "]"))
    stringr::str_c("[", paste(first, collapse = "/"), "]/", paste(second, collapse = "/"))
    },
    first, second, split_starter)
}

check_for_nicknames <- function(cleaned_names, nicknames) {
  matches <- match(cleaned_names, nicknames$name)
  ifelse(is.na(matches), cleaned_names, nicknames$nickname[matches])
}

#' Extract specs and starters from deck (nick)names for modelling
#'
#' @param name a character vector, containing deck names in standardised form, or their nicknames
#' @inheritParams standardise_deck_name
#'
#' @return a data.table, containing a column for the deck's starter name and three columns for its spec names.
#' @export
prepare_deck_names_for_modelling <- function(name, starters,
                                             nicknames = data.table(name = character(0), nickname = character(0))) {
  if (length(name) == 0)
    return(data.table(starter = character(0), spec1 = character(0), spec2 = character(0), spec3 = character(0)))
  nickname_matches <- match(name, nicknames$nickname)
  name <- ifelse(is.na(nickname_matches), name, nicknames$name[nickname_matches])
  split <- stringr::str_split(remove_brackets(name), "/", n = 4, simplify = TRUE)
  data.table(starter = ifelse(split[, 4] == "", starters$starter[match(split[, 1], starters$spec)], split[, 4]),
             spec1 = split[, 1], spec2 = split[, 2], spec3 = split[, 3])
}

remove_brackets <- function(name) {
  stringr::str_remove_all(name, "[\\[\\]]")
}
