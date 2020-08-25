context("deck name conversion")

describe("standardise_deck_name()", {
  test_lookup <- data.table(spec = c("Anarchy", "Blood", "Fire", "Balance", "Feral", "Growth"),
                            starter = rep(c("Red", "Green"), each = 3))
  std <- function(x, ...) {
    standardise_deck_name(x, test_lookup, ...)
  }
  it("requires a character input", {
    msg <- "input is not a character vector"
    expect_error(std(NULL), msg)
  })
  it("returns zero-length input", {
    expect_identical(std(character(0)), character(0))
  })
  it("returns NAs", {
    expect_identical(std(c(NA_character_, "Anarchy/Blood/Growth")), c(NA_character_, "[Anarchy/Blood]/Growth"))
  })
  it("returns a vector of the same length", {
    expect_length(std("Anarchy/Blood/Growth"), 1)
    expect_length(std(c("Anarchy/Blood/Growth", "Anarchy/Blood/Growth")), 2)
  })
  it("expects names to have two or three forward slashes, with letters between and on either side", {
    expect_error(std("Anarchy"))
    expect_error(std("Anarchy/Growth"))
    expect_silent(std("Anarchy/Blood/Growth/Red"))
    expect_error(std("Anarchy/Blood/"))
    expect_error(std("Anarchy//Blood"))
    expect_error(std("Anarchy/1/Blood"))
  })
  it("requires any square brackets to open at the start, and close before a slash or the end", {
    expect_error(std("[]Anarchy/Growth/Blood"))
    expect_error(std("Anarchy/Gro[]wth/Blood"))
    expect_error(std("[Anarch]y/Growth/Blood"))
    expect_error(std("[Anarchy/]Growth/Blood"))
    expect_error(std("Anarchy/[Growth]/Blood"))
    expect_error(std("Anarchy/[Growth/Blood]"))
    expect_error(std("[Anarchy]/[Feral/Growth]"))
    expect_identical(std("[Anarchy]/Growth/Blood"), "[Anarchy/Blood]/Growth")
    expect_identical(std("[Anarchy/Blood/Fire]"), "[Anarchy/Blood/Fire]")
  })
  it("expects spec names from the given spec/starter lookup", {
    expect_error(std("Anarchy/Blood/foo"))
    expect_error(std("Anarchy/foo/Blood"))
    expect_error(std("Anarchy/Blood/Fire/foo"))
  })
  it("removes the starter and moves specs with that starter to the front, if any specs match the starter", {
    expect_identical(std("Anarchy/Blood/Fire/Red"), "[Anarchy/Blood/Fire]")
    expect_identical(std("Balance/Blood/Growth/Red"), "[Blood]/Balance/Growth")
  })
  it("moves specs with same starter as first spec to front, sorts both groups, brackets the first, if no explicit starter", {
    expect_identical(std("Anarchy/Blood/Fire"), "[Anarchy/Blood/Fire]")
    expect_identical(std("Fire/Anarchy/Blood"), "[Anarchy/Blood/Fire]")
    expect_identical(std("Anarchy/Blood/Growth"), "[Anarchy/Blood]/Growth")
    expect_identical(std("Anarchy/Growth/Blood"), "[Anarchy/Blood]/Growth")
    expect_identical(std("Anarchy/Growth/Balance"), "[Anarchy]/Balance/Growth")
  })
  it("moves specs grouped with a given starter to front, sorts both groups, brackets the first", {
    expect_identical(std("Anarchy/Blood/Fire/Red"), "[Anarchy/Blood/Fire]")
    expect_identical(std("Fire/Anarchy/Blood/Red"), "[Anarchy/Blood/Fire]")
    expect_identical(std("Anarchy/Blood/Growth/Red"), "[Anarchy/Blood]/Growth")
    expect_identical(std("Anarchy/Growth/Blood/Red"), "[Anarchy/Blood]/Growth")
    expect_identical(std("Anarchy/Growth/Balance/Red"), "[Anarchy]/Balance/Growth")
  })
  it("sorts all specs together and keeps the starter explicit if there are no matches", {
    expect_identical(std("Anarchy/Blood/Fire/Green"), "Anarchy/Blood/Fire/Green")
    expect_identical(std("Blood/Anarchy/Fire/Green"), "Anarchy/Blood/Fire/Green")
  })
  it("replaces finished name with a nickname, if any given and a match is found", {
    expect_identical(std("Anarchy/Blood/Fire",
                         nicknames = data.table(name = "[Anarchy/Blood/Fire]", nickname = "MonoRed")),
                     "MonoRed")
  })
  it("returns given nicknames if return_nicknames is TRUE", {
    expect_identical(std("MonoRed", nicknames = data.table(name = "[Anarchy/Blood/Fire]", nickname = "MonoRed")),
                     "MonoRed")
    expect_identical(std(c("MonoRed", "Anarchy/Growth/Blood"),
                         nicknames = data.table(name = "[Anarchy/Blood/Fire]", nickname = "MonoRed")),
                     c("MonoRed", "[Anarchy/Blood]/Growth"))
    expect_identical(std(c("MonoRed", "Anarchy/Growth/Blood"),
                         nicknames = data.table(name = "[Anarchy/Blood/Fire]", nickname = "MonoRed"),
                         return_nicknames = FALSE),
                     c("[Anarchy/Blood/Fire]", "[Anarchy/Blood]/Growth"))
  })
  it("can take specs with incorrect capitalisation", {
    expect_identical(std("anarcHy/BlOod/fire/GrEen"), "Anarchy/Blood/Fire/Green")
  })
})

describe("prepare_deck_names_for_modelling()", {
  test_lookup <- data.table(spec = c("Anarchy", "Blood", "Fire", "Balance", "Feral", "Growth"),
                            starter = rep(c("Red", "Green"), each = 3))
  test_nicknames <- data.table(name = "[Anarchy/Blood/Fire]", nickname = "MonoRed")
  it("returns a nx0 data.table if given a length-0 vector of names", {
    expect_identical(prepare_deck_names_for_modelling(character(0), test_lookup),
                     data.table(starter = character(0), spec1 = character(0),
                                spec2 = character(0), spec3 = character(0)))
  })
  it("returns a nx4 data.table for names, giving starter and specs for each deck", {
    expect_identical(prepare_deck_names_for_modelling("[Anarchy/Blood/Fire]", test_lookup),
                     data.table(starter = "Red", spec1 = "Anarchy", spec2 = "Blood", spec3 = "Fire"))
    expect_identical(prepare_deck_names_for_modelling("Anarchy/Blood/Fire/Green", test_lookup),
                     data.table(starter = "Green", spec1 = "Anarchy", spec2 = "Blood", spec3 = "Fire"))
  })
  it("treats any nicknames as their respective deck names", {
    expect_identical(prepare_deck_names_for_modelling("MonoRed", test_lookup, test_nicknames),
                     data.table(starter = "Red", spec1 = "Anarchy", spec2 = "Blood", spec3 = "Fire"))
  })
})
