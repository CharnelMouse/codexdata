#' Standardise recorded deck names, using any given nicknames
#'
#' @param names a character vector, containing deck names or nicknames.
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
standardise_deck_name <- function(names, starters = starters,
                                  nicknames = data.table(name = character(), nickname = character()),
                                  return_nicknames = TRUE) {
  if (!is.character(names))
    stop("input is not a character vector")
  if (length(names) == 0)
    return(character())
  base_names <- convert_from_nicknames(names, nicknames)
  check_slash_count(base_names)
  check_for_late_brackets(base_names)
  check_bracket_placement(base_names)
  components <- remove_brackets(base_names) %>%
    strsplit("/") %>%
    lapply(stringr::str_to_title)
  check_all_deck_components_valid(components, starters)
  cleaned <- lapply(components, remove_starters_if_matched, starters) %>%
    add_deck_delimiters(starters)
  if (!return_nicknames)
    cleaned
  else
    replace_with_nickname(cleaned, nicknames)
}

convert_from_nicknames <- function(name, nicknames) {
  matches <- match(name, nicknames$nickname)
  ifelse(is.na(matches) | is.na(name), name, nicknames$name[matches])
}

check_slash_count <- function(name) {
  without_brackets <- gsub("[\\[\\]]", "", name)
  wrong_slashes <- !is.element(stringr::str_count(without_brackets, "/"), c(2, 3)) & !is.na(name)
  stop_if_nonempty(name[wrong_slashes], "names with wrong number of slashes", which(wrong_slashes))
}

check_for_late_brackets <- function(name) {
  late_start <- grepl("[\\w/]\\[", name) & !is.na(name)
  stop_if_nonempty(name[late_start], "names with bracket pair started after beginning", which(late_start))
}

check_bracket_placement <- function(name) {
  brackets_present <- stringr::str_detect(name, "\\[[\\w/]*\\]")
  if (any(brackets_present[!is.na(brackets_present)])) {
    invalid_brackets <- brackets_present & !stringr::str_detect(name, "^\\[[\\w/]+\\](/|$)")
    stop_if_nonempty(name[invalid_brackets], "names with incorrect bracket placement", which(invalid_brackets))
  }
}

check_all_deck_components_valid <- function(split, starters) {
  specs_invalid <- vapply(split,
                          function(x) any(!is.na(x)) &&
                            (!all(x[1:3] %in% starters$spec) ||
                               (length(x) != 3 &&
                                  (length(x) != 4 || !is.element(x[4], starters$starter)))),
                          logical(1))
  stop_if_nonempty(vapply(split[specs_invalid], function(x) paste(x, collapse = "/"), character(1)),
                   "names with unrecognised spec/starter names", which(specs_invalid))
}

stop_if_nonempty <- function(x, msg, indices = NULL) {
  if (length(x) > 0) {
    if (is.null(indices))
      stop(paste0(msg, ": ", paste(x, collapse = ", ")))
    else
      stop(paste0(msg, "\n",
                  paste0(indices, ": ", x, collapse = "\n")))
  }
}

remove_starters_if_matched <- function(split_name, starters) {
  if (length(split_name) == 3)
    return(split_name)
  spec_starters <- starters$starter[match(split_name[1:3], starters$spec)]
  match_given_starter <- which(spec_starters == split_name[4])
  if (length(match_given_starter) == 0)
    return(split_name)
  c(split_name[match_given_starter], split_name[setdiff(1:3, match_given_starter)])
}

add_deck_delimiters <- function(split, starters) {
  split_starter <- vapply(split,
                          function(x)
                            if (length(x) == 4)
                              x[4]
                          else
                            starters$starter[match(x[1], starters$spec)],
                          character(1))
  split_spec_ids <- lapply(split, match, starters$spec)
  split_starters <- lapply(split_spec_ids, function(n) starters$starter[n])
  which_specs_matched_to_deck_starter <- Map(function(x, y) which(x == y),
                                             split_starters,
                                             split_starter)
  starter_specs <- Map(`[`, split, which_specs_matched_to_deck_starter)
  nonstarter_specs <- Map(function(x, y) x[setdiff(1:3, y)], split, which_specs_matched_to_deck_starter)
  starter_spec_strings <- lapply(starter_specs,
                                 function(x) paste(sort(x), collapse = "/"))
  nonstarter_spec_strings <- lapply(nonstarter_specs,
                                    function(x) paste(sort(x), collapse = "/"))
  mapply(function(first, second, starter) {
    if (any(grepl("NA", c(first, second))))
      return(NA)
    if (nchar(first) == 0)
      return(stringr::str_c(paste(second, collapse = "/"), "/", starter))
    if (nchar(second) == 0)
      return(stringr::str_c("[", paste(first, collapse = "/"), "]"))
    stringr::str_c("[", paste(first, collapse = "/"), "]/", paste(second, collapse = "/"))
    },
    starter_spec_strings, nonstarter_spec_strings, split_starter)
}

replace_with_nickname <- function(cleaned_names, nicknames) {
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
                                             nicknames = data.table(name = character(),
                                                                    nickname = character())) {
  if (length(name) == 0)
    return(data.table(starter = character(),
                      spec1 = character(),
                      spec2 = character(),
                      spec3 = character()))
  nickname_matches <- match(name, nicknames$nickname)
  name <- ifelse(is.na(nickname_matches),
                 name,
                 nicknames$name[nickname_matches])
  split <- stringr::str_split(remove_brackets(name), "/", n = 4, simplify = TRUE)
  data.table(starter = ifelse(split[, 4] == "",
                              starters$starter[match(split[, 1], starters$spec)],
                              split[, 4]),
             spec1 = split[, 1],
             spec2 = split[, 2],
             spec3 = split[, 3])
}

remove_brackets <- function(name) {
  stringr::str_remove_all(name, "[\\[\\]]")
}
