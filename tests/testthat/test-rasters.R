make_toy_raster <- function(path, value_fun) {
  r <- terra::rast(xmin = -72, xmax = -56, ymin = -46, ymax = -26, resolution = 0.5, crs = "EPSG:4326")
  xy <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  terra::values(r) <- value_fun(xy[, 1], xy[, 2])
  terra::writeRaster(r, path, overwrite = TRUE)
  path
}

test_that("extract_rasters adds one numeric column per raster with the right values", {
  d <- withr::local_tempdir()
  lat_r <- make_toy_raster(file.path(d, "lat.tif"), function(x, y) y)   # value = latitude
  lon_r <- make_toy_raster(file.path(d, "lon.tif"), function(x, y) x)   # value = longitude
  pts <- data.frame(LATITUDE = c(-30.1, -40.2), LONGITUDE = c(-60.1, -65.3))
  out <- suppressMessages(extract_rasters(pts, c(latv = lat_r, lonv = lon_r)))
  expect_equal(out$latv, pts$LATITUDE, tolerance = 0.5)   # pixel is 0.5 degrees wide
  expect_equal(out$lonv, pts$LONGITUDE, tolerance = 0.5)
  expect_equal(nrow(out), 2)
})

test_that("extract_rasters gives NA for rows without coordinates and does not drop rows", {
  d <- withr::local_tempdir()
  r <- make_toy_raster(file.path(d, "lat.tif"), function(x, y) y)
  pts <- data.frame(LATITUDE = c(-30, NA), LONGITUDE = c(-60, -61))
  out <- suppressWarnings(suppressMessages(extract_rasters(pts, c(v = r))))
  expect_equal(nrow(out), 2)
  expect_false(is.na(out$v[1]))
  expect_true(is.na(out$v[2]))
})

test_that("extract_rasters requires a named raster list and warns on missing files", {
  pts <- data.frame(LATITUDE = -30, LONGITUDE = -60)
  expect_error(extract_rasters(pts, c("a.tif")), "named")
  expect_warning(suppressMessages(extract_rasters(pts, c(v = "does_not_exist.tif"))), "not found")
})

test_that("elev_minmax returns the min and max elevation inside each polygon", {
  skip_if_not_installed("exactextractr"); skip_if_not_installed("raster")
  d <- withr::local_tempdir()
  dem <- make_toy_raster(file.path(d, "dem.tif"), function(x, y) (y + 46) * 100)   # 0 at -46, 2000 at -26
  polys <- sf::st_sf(id = c("a", "b"), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-70, -45), c(-60, -45), c(-60, -40), c(-70, -40), c(-70, -45)))),
    sf::st_polygon(list(rbind(c(-70, -35), c(-60, -35), c(-60, -30), c(-70, -30), c(-70, -35)))),
    crs = 4326))
  out <- suppressWarnings(elev_minmax(dem, polys, polygon_id_col = "id"))
  expect_equal(out$poly_id, c("a", "b"))
  expect_lt(out$elev_max[1], out$elev_min[2])        # polygon b is higher than a
  expect_equal(out$elev_min[1], 125, tolerance = 0.05)  # lowest cell centre inside a is -44.75
})
