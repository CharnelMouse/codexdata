context("data validation")

describe("check_primary_keys_unique()", {
  it("expects at least one column name for primary key", {
    expect_error(check_primary_keys_unique(data.table(a = integer()), character()),
                 "^colnames cannot be length zero$")
  })
  it("expects colnames to be in dt", {
    expect_error(check_primary_keys_unique(data.table(a = integer()), "b"),
                 "^colnames must exist in dt$")
  })
  it("returns NULL (pass) if dt has no rows", {
    expect_null(check_primary_keys_unique(data.table(a = integer()), "a"))
  })
  it("returns NULL (pass) if dt has no rows where primary key is duplicated", {
    expect_null(check_primary_keys_unique(data.table(a = 1L), "a"))
    expect_null(check_primary_keys_unique(data.table(a = 1:2, b = 1L), "a"))
  })
  it("returns duplicated keys if any in error message", {
    expect_error(check_primary_keys_unique(data.table(a = rep(1L, 2L)), "a"),
                 "^there are duplicated primary keys:\na: 1$")
    expect_error(check_primary_keys_unique(data.table(a = rep(1L, 3L)), "a"),
                 "^there are duplicated primary keys:\na: 1$")
    expect_error(check_primary_keys_unique(data.table(a = rep(1:2, 2L)), "a"),
                 "^there are duplicated primary keys:\na: 1\n\na: 2$")
  })
  it("checks for unique sets over multiple-column keys", {
    expect_error(check_primary_keys_unique(data.table(a = rep(1:2, 3L),
                                                      b = c(1L, 1L, 2L, 2L, 2L, 3L)),
                                           c("a", "b")),
                 "^there are duplicated primary keys:\na: 1\nb: 2")
  })
})

describe("check_foreign_keys()", {
  it("expects at least one key name", {
    expect_error(check_foreign_keys(data.table(a = character()),
                                    data.table(a = character()),
                                    character()),
                 "^require at least one key$")
  })
  it("expects key to exist in table and reference table", {
    expect_error(check_foreign_keys(data.table(a = character()),
                                    data.table(b = character()),
                                    "b"),
                 "^there are keys not in dt: b$")
    expect_error(check_foreign_keys(data.table(a = character()),
                                    data.table(b = character()),
                                    "a"),
                 "^there are keys not in ref: a$")
  })
  it("returns error with values in dt that don't exist in ref", {
    expect_error(check_foreign_keys(data.table(a = 1:3),
                                    data.table(a = 1:2),
                                    "a"),
                 "^there are values not in ref:\na: 3$")
    expect_error(check_foreign_keys(data.table(a = 1:4,
                                               b = 1:2),
                                    data.table(a = c(1:2, 1L),
                                               b = 1:3),
                                    c("a", "b")),
                 "^there are values not in ref:\na: 3, 4$")
  })
  it("returns NULL if foreign keys all pull values out of ref", {
    expect_null(check_foreign_keys(data.table(a = integer()),
                                   data.table(a = integer()),
                                   "a"))
  })
  it("returns error for NA values by default", {
    expect_error(check_foreign_keys(data.table(a = NA_character_),
                                    data.table(a = "a"),
                                    "a"),
                 "^there are values not in ref:\na: NA$")
  })
  it("allows NA in addition to ref values if optional = TRUE", {
    expect_null(check_foreign_keys(data.table(a = NA_character_),
                                   data.table(a = "a"),
                                   "a",
                                   optional = TRUE))
  })
})

describe("check_no_required_values_missing", {
  it("expects NA values to only occur in given columns, allowed where normalization would hinder use", {
    dt <- data.table(a = NA)
    expect_null(check_no_required_values_missing(dt, "a"))
    expect_error(check_no_required_values_missing(dt),
                 "^there are missing required values in the following rows:\na: 1$")
    expect_error(check_no_required_values_missing(data.table(a = 1:2, b = c(1, NA), c = NA)),
                 "^there are missing required values in the following rows:\nb: 2\nc: 1, 2$")
  })
})
