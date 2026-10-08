test_that("build_data marks checklists with a sighting as present and the rest as absent", {
  f <- write_toy_files(withr::local_tempdir())
  out <- build_data(f[["sightings"]], f[["checklists"]])
  expect_equal(nrow(out), 8)                          # every checklist is kept
  expect_equal(sort(out[["SAMPLING EVENT IDENTIFIER"]][out$presence == "Present"]),
               c("S1", "S2", "S5"))
  expect_equal(sum(out$presence == "Absent"), 5)
})

test_that("build_data honours custom labels and column name", {
  f <- write_toy_files(withr::local_tempdir())
  out <- build_data(f[["sightings"]], f[["checklists"]],
                    pres_col = "detected", label_pos = "yes", label_neg = "no")
  expect_true("detected" %in% names(out))
  expect_setequal(unique(out$detected), c("yes", "no"))
})

test_that("build_data fails clearly when the id column is missing", {
  f <- write_toy_files(withr::local_tempdir())
  expect_error(build_data(f[["sightings"]], f[["checklists"]], id_col = "NOPE"),
               "not found")
})
