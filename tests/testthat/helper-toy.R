# Tiny hand-made eBird-like tables. Nothing here is real eBird data.
toy_checklists <- function() {
  data.frame(
    `SAMPLING EVENT IDENTIFIER` = paste0("S", 1:8),
    `OBSERVATION DATE` = c("2020-01-15", "2020-07-10", "2020-07-11", "2020-12-01",
                           "2021-03-20", "2021-06-30", "2021-09-05", "2021-09-06"),
    LATITUDE  = c(-45, -30, -30.5, -44, -35, -29, -38, -38.2),
    LONGITUDE = c(-70, -62, -62.1, -69, -64, -63, -65, -65.1),
    `PROTOCOL NAME` = c("Traveling", "Stationary", "Traveling", "Incidental",
                        "Traveling", "Stationary", "Traveling", "Traveling"),
    `EFFORT DISTANCE KM` = c(2, 0, 3, 1, 25, 0, 4, 4),
    `NUMBER OBSERVERS` = c(1, 2, 1, 1, 1, 15, 2, 2),
    `DURATION MINUTES` = c(60, 30, 90, 20, 100, 60, 400, 45),
    `ALL SPECIES REPORTED` = c(1, 1, 1, 1, 1, 1, 1, 0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

toy_sightings <- function() {
  data.frame(
    `SAMPLING EVENT IDENTIFIER` = c("S1", "S2", "S5"),
    `COMMON NAME` = "Toy tyrant",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

write_toy_files <- function(dir) {
  sp <- file.path(dir, "sightings.txt"); cp <- file.path(dir, "checklists.txt")
  utils::write.table(toy_sightings(), sp, sep = "\t", row.names = FALSE, quote = FALSE)
  utils::write.table(toy_checklists(), cp, sep = "\t", row.names = FALSE, quote = FALSE)
  c(sightings = sp, checklists = cp)
}
