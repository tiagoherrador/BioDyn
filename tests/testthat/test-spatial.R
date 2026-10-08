toy_points <- function() {
  data.frame(
    LATITUDE  = c(-30, -30.2, -31, -40, -45),
    LONGITUDE = c(-60, -60.1, -60, -65, -70),
    presence  = c("Present", "Absent", "Absent", "Absent", "Absent"),
    stringsAsFactors = FALSE
  )
}

test_that("filter_radius keeps presences and absences close to a presence", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  out <- filter_radius(toy_points(), radius_km = 50, interactive = FALSE)
  # the 2nd point is ~15 km from the presence; the 3rd is ~110 km; the rest are far
  expect_equal(nrow(out), 2)
  expect_equal(out$presence, c("Present", "Absent"))
  expect_true(all(out$within_radius))
  expect_equal(out$dist_min_km[1], 0)
  expect_lt(out$dist_min_km[2], 50)
})

test_that("filter_radius distances are close to the true great-circle distance", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  df <- toy_points()[1:2, ]
  out <- filter_radius(df, radius_km = 1000, interactive = FALSE)
  # haversine
  rad <- pi / 180
  a <- sin((df$LATITUDE[2] - df$LATITUDE[1]) * rad / 2)^2 +
    cos(df$LATITUDE[1] * rad) * cos(df$LATITUDE[2] * rad) *
    sin((df$LONGITUDE[2] - df$LONGITUDE[1]) * rad / 2)^2
  true_km <- 2 * 6371 * asin(sqrt(a))
  expect_equal(out$dist_min_km[2], true_km, tolerance = 0.01)   # within 1 %
})

test_that("filter_radius without presences warns and keeps nothing", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  df <- toy_points(); df$presence <- "Absent"
  expect_warning(out <- filter_radius(df, radius_km = 50, interactive = FALSE), "No presences")
  expect_equal(nrow(out), 0)
})

test_that("pick_metric_epsg_from_pts picks the southern UTM zone", {
  expect_equal(BioDyn:::pick_metric_epsg_from_pts(-60.5, -30), 32720)
  expect_equal(BioDyn:::pick_metric_epsg_from_pts(10.5, 45), 32632)
})

test_that("filter_ecoregion keeps points inside the polygon", {
  skip_if_not_installed("sf")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-62, -32), c(-58, -32), c(-58, -29), c(-62, -29), c(-62, -32)))),
    crs = 4326))
  out <- filter_ecoregion(toy_points(), region_sf = poly, buffer_km = 0, interactive = FALSE)
  expect_equal(nrow(out), 3)                    # the first three points fall in the box
})
