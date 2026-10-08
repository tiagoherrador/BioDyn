test_that("plot_effort returns a ggplot with total and presence panels", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  df <- add_season(split_dates(toy_checklists(), preview = FALSE))
  df$presence <- ifelse(df[["SAMPLING EVENT IDENTIFIER"]] %in% c("S1", "S2", "S5"), "Present", "Absent")
  p <- plot_effort(df)
  expect_s3_class(p, "ggplot")
  expect_setequal(levels(p$data$type), c("total", "presence"))
})

test_that("plot_effort checks for required columns", {
  expect_error(plot_effort(data.frame(a = 1)), "Missing columns")
})

test_that("season palettes are named and colour-blind friendly hex codes", {
  expect_setequal(names(season_palette), c("Summer", "Autumn", "Winter", "Spring"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", season_palette)))
  expect_identical(colores_season, season_palette)
})

test_that("make_numeric_coordinates drops rows with invalid coordinates", {
  df <- data.frame(LATITUDE = c("-30", "x", "-31"), LONGITUDE = c("-60", "-61", NA))
  out <- suppressMessages(suppressWarnings(make_numeric_coordinates(df)))
  expect_equal(nrow(out), 1)
  expect_true(is.numeric(out$LATITUDE))
})
