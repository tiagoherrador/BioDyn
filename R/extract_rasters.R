#' Extract CHELSA environmental variables at point locations
#'
#' Given a set of point observations (either as a data.frame or a CSV file),
#' this function extracts values from a set of CHELSA raster files and
#' appends them as new columns. Temperature variables (BIO1 to BIO11, CHELSA v2.1)
#' are assumed to already be scaled correctly by \pkg{terra} when metadata is present.
#'
#' @param data A data.frame with longitude/latitude columns, or a character
#'   string with the path to a CSV file readable by \code{utils::read.csv()}.
#' @param raster_list Named character vector or list. Names are the desired
#'   output column names; values are file paths to raster files.
#' @param out_file Optional character string. If not \code{NULL}, the
#'   resulting data.frame is written to this path as CSV.
#' @param lon_col Character string. Name of the longitude column.
#'   Default: \code{"LONGITUDE"}.
#' @param lat_col Character string. Name of the latitude column.
#'   Default: \code{"LATITUDE"}.
#'
#' @return A data.frame equal to the input data, with one additional numeric
#'   column per raster in \code{raster_list}. Rows with missing coordinates
#'   receive \code{NA} in the corresponding environmental variables. If
#'   \code{out_file} is not \code{NULL}, the result is also written to CSV.
#'
#' @examples
#' \dontrun{
#' chelsa_files <- c(
#'   bio1  = "CHELSA_bio1.tif",
#'   bio12 = "CHELSA_bio12.tif"
#' )
#'
#' df_chelsa <- extract_rasters(
#'   data        = filtered_radius,
#'   raster_list = chelsa_files,
#'   out_file    = "datos_con_variables.csv"
#' )
#' }
#'
#' @export
#' @importFrom sf st_as_sf st_transform st_coordinates st_crs
#' @importFrom terra rast extract vect crs
extract_rasters <- function(data,
                    raster_list,
                    out_file = NULL,
                    lon_col = "LONGITUDE",
                    lat_col = "LATITUDE") {

  # -------------------------
  # Load data.frame or CSV
  # -------------------------
  if (is.data.frame(data)) {
    df <- data
  } else if (is.character(data) && length(data) == 1L && file.exists(data)) {
    df <- utils::read.csv(data, check.names = FALSE)
  } else {
    stop("`data` must be a data.frame or an existing file path.")
  }

  if (!lon_col %in% names(df) || !lat_col %in% names(df)) {
    stop("Columns '", lon_col, "' and/or '", lat_col, "' not found in input data.")
  }

  # -------------------------
  # Prepare coordinates
  # -------------------------
  df[[lon_col]] <- suppressWarnings(as.numeric(df[[lon_col]]))
  df[[lat_col]] <- suppressWarnings(as.numeric(df[[lat_col]]))

  valid_coords <- !is.na(df[[lon_col]]) & !is.na(df[[lat_col]])
  if (!all(valid_coords)) {
    warning(
      sum(!valid_coords),
      " rows have NA coordinates; environmental variables will be NA for those rows."
    )
  }

  pts_valid <- df[valid_coords, , drop = FALSE]

  if (nrow(pts_valid) > 0) {
    sf_pts <- sf::st_as_sf(
      pts_valid,
      coords = c(lon_col, lat_col),
      crs    = 4326
    )
  } else {
    sf_pts <- NULL
  }

  # Ensure raster_list is named
  if (is.null(names(raster_list)) || any(names(raster_list) == "")) {
    stop("`raster_list` must be a *named* vector or list (names = output column names).")
  }

  # -------------------------
  # Loop over rasters
  # -------------------------
  for (var_name in names(raster_list)) {
    raster_path <- raster_list[[var_name]]
    message("Processing: ", var_name)

    df[[var_name]] <- NA_real_

    if (is.null(sf_pts) || nrow(sf_pts) == 0) {
      warning("No valid coordinates; skipping extraction for '", var_name, "'.")
      next
    }

    if (!file.exists(raster_path)) {
      warning("  File not found: ", raster_path, ". Column filled with NA.")
      next
    }

    vals_full <- rep(NA_real_, nrow(df))

    r <- tryCatch(
      terra::rast(raster_path),
      error = function(e) {
        warning("Failed to read raster '", raster_path, "': ", e$message)
        return(NULL)
      }
    )
    if (is.null(r)) next

    raster_crs <- terra::crs(r, proj = TRUE)
    if (is.na(raster_crs) || raster_crs == "") {
      warning("Raster CRS is missing; assuming WGS84.")
      raster_crs <- "EPSG:4326"
    }

    pts_proj <- tryCatch(
      sf::st_transform(sf_pts, raster_crs),
      error = function(e) {
        warning("CRS transform failed for '", raster_path, "': ", e$message)
        return(NULL)
      }
    )
    if (is.null(pts_proj)) next

    v_pts <- terra::vect(pts_proj)

    ext <- tryCatch(
      terra::extract(r, v_pts),
      error = function(e) {
        warning("terra::extract() failed for '", var_name, "': ", e$message)
        return(NULL)
      }
    )
    if (is.null(ext)) next

    # Typically, terra::extract returns a data.frame with ID + values
    vals <- ext[[2L]]

    # Detect CHELSA temperature BIO1 to BIO11 by filename (lowercase)
    base_name <- tolower(basename(raster_path))
    is_temp <- grepl("^bio([1-9]|1[0-1])(\\D|$)", base_name)

    if (is_temp) {
      # CHELSA v2.1 temperature variables usually include scale/offset metadata
      # that terra applies automatically, so no manual rescaling is performed.
      vals <- vals
    }

    vals_full[valid_coords] <- vals
    df[[var_name]] <- vals_full

    valid_vals <- vals[!is.na(vals)]
    if (length(valid_vals) > 0L) {
      message("  Extracted: ", length(valid_vals), "/", length(vals), " values")
      rng <- range(valid_vals, na.rm = TRUE)
      message(sprintf("  Range: %.2f to %.2f", rng[1], rng[2]))
      message(sprintf("  Mean: %.2f", mean(valid_vals)))
    } else {
      message("  No valid values extracted.")
    }
  }

  # -------------------------
  # Optional write to CSV
  # -------------------------
  if (!is.null(out_file)) {
    utils::write.csv(df, file = out_file, row.names = FALSE)
    message("Data written to: ", normalizePath(out_file, winslash = "/"))
  }

  df
}

