test_that("split_dates adds DAY, MONTH and YEAR", {
  out <- split_dates(toy_checklists(), preview = FALSE)
  expect_equal(out$MONTH[1:3], c(1L, 7L, 7L))
  expect_equal(out$YEAR[5], 2021L)
  expect_equal(out$DAY[4], 1L)
})

test_that("add_season is hemisphere-aware", {
  df <- data.frame(MONTH = c(1, 7, 1, 7), LATITUDE = c(-30, -30, 30, 30))
  out <- add_season(df)
  expect_equal(out$season, c("Summer", "Winter", "Winter", "Summer"))
})

test_that("add_season handles the equator as requested", {
  df <- data.frame(MONTH = c(1, 1), LATITUDE = c(0, -5))
  expect_true(is.na(add_season(df)$season[1]))
  expect_equal(add_season(df, equator = "north")$season[1], "Winter")
  expect_equal(add_season(df, equator = "south")$season[1], "Summer")
})

test_that("add_season and filter_season check their inputs", {
  expect_error(add_season(data.frame(MONTH = 1)), "LATITUDE")
  expect_error(filter_season(data.frame(a = 1)), "season")
  df <- add_season(data.frame(MONTH = 1:12, LATITUDE = -30))
  out <- filter_season(df, keep = "Summer")
  expect_equal(sort(out$MONTH), c(1, 2, 12))
})
