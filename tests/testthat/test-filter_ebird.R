test_that("filter_ebird applies the effort, protocol and completeness filters", {
  out <- filter_ebird(toy_checklists(), max_distance_km = 10, max_observers = 10,
                      max_duration_min = 300, protocols = c("Stationary", "Traveling"),
                      drop_duplicates = FALSE)
  # S4 incidental, S5 25 km, S6 15 observers, S7 400 min, S8 not complete -> removed
  expect_equal(sort(out[["SAMPLING EVENT IDENTIFIER"]]), c("S1", "S2", "S3"))
})

test_that("filter_ebird can keep incomplete checklists", {
  out <- filter_ebird(toy_checklists(), max_distance_km = 10, max_observers = 10,
                      max_duration_min = 300, protocols = c("Stationary", "Traveling"),
                      require_all_species = FALSE, drop_duplicates = FALSE)
  # S8 passes every effort filter but is not a complete checklist
  expect_true("S8" %in% out[["SAMPLING EVENT IDENTIFIER"]])
})

test_that("filter_ebird reports missing columns", {
  expect_error(filter_ebird(data.frame(a = 1)), "Missing required columns")
})
