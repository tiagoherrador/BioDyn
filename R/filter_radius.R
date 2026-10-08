#' Plot / map presences and absences around a radius and filter by distance
#'
#' This function visualizes presences and absences relative to a distance
#' threshold around presences, and returns a filtered \code{data.frame}
#' containing all presences and absences within the specified radius.
#'
#' For each absence, the distance to the nearest presence is computed
#' (using the \pkg{RANN} package if \code{dist_col} does not already exist).
#' Points are then classified as:
#' \itemize{
#'   \item Presences
#'   \item Absences within radius (kept)
#'   \item Absences outside radius (discarded)
#' }
#'
#' If \code{interactive = FALSE} (default), a static map is drawn using
#' \pkg{ggplot2}. If \code{interactive = TRUE}, an interactive map with
#' zoom and pan is drawn using \pkg{leaflet}.
#'
#' @param data A \code{data.frame} containing at least latitude/longitude
#'   and a presence/absence column (coordinates in decimal degrees,
#'   WGS84 / EPSG:4326).
#' @param lat_col Character scalar. Name of the latitude column.
#'   Default: \code{"LATITUDE"}.
#' @param lon_col Character scalar. Name of the longitude column.
#'   Default: \code{"LONGITUDE"}.
#' @param pres_col Character scalar. Name of the presence/absence column.
#' @param pres_label Character scalar. Value in \code{pres_col} that
#'   identifies presences.
#' @param radius_km Numeric scalar. Maximum distance (km) used to keep
#'   absences near presences. Default: 100.
#' @param dist_col Character scalar. Name of the column where the minimum
#'   distance to presences will be stored or read from. If this column
#'   already exists in \code{data}, it is reused (coerced to numeric) and
#'   distances are not recomputed. Default: \code{"dist_min_km"}.
#' @param base_sf Optional \code{sf} object to be used as a base layer
#'   (e.g., country or ecoregion boundaries).
#' @param show_union Logical. If TRUE, plot the union of all buffers
#'   around presences.
#' @param max_buffers Integer scalar. Maximum number of presence points
#'   to buffer in a single batch (for memory optimization). Default: 500.
#' @param title Character scalar. Plot title.
#' @param fig_size Numeric vector of length 2: width and height (in inches)
#'   passed to \code{ggsave()} if \code{save_path} is not \code{NULL}.
#' @param col_pres Color for presences.
#' @param col_abs_in Color for absences within radius.
#' @param col_abs_out Color for absences outside radius.
#' @param col_buffer Color for buffer union boundary.
#' @param alpha_pts Numeric scalar between 0 and 1. Point transparency.
#' @param interactive Logical. If TRUE, draw an interactive \pkg{leaflet}
#'   map instead of a static \pkg{ggplot2} map.
#' @param save_path Optional character scalar. If not \code{NULL}, the plot
#'   is saved: PNG for static plots (ggplot2) or HTML widget for interactive
#'   plots (leaflet).
#'
#' @return A \code{data.frame} containing:
#'   \itemize{
#'     \item All original columns from \code{data} for presences and absences
#'           within the specified radius.
#'     \item Column \code{dist_col}: distance (km) to nearest presence.
#'     \item Column \code{within_radius}: \code{TRUE} for presences and
#'           absences within radius, \code{FALSE} otherwise.
#'   }
#'
#' @examples
#' \dontrun{
#' # Static map
#' filtered <- filter_radius(
#'   data       = data,
#'   pres_col   = "presence",
#'   pres_label = "Present",
#'   radius_km  = 50
#' )
#'
#' # Interactive map (zoomable)
#' filtered_int <- filter_radius(
#'   data        = data,
#'   pres_col    = "presence",
#'   pres_label  = "Present",
#'   radius_km   = 50,
#'   interactive = TRUE
#' )
#' }
#'
#' @export
#' @importFrom sf st_as_sf st_transform st_coordinates st_buffer st_union st_crs
#' @importFrom ggplot2 ggplot geom_sf theme_minimal labs ggtitle
#' @importFrom RANN nn2
filter_radius <- function(data,
                          lat_col = "LATITUDE",
                          lon_col = "LONGITUDE",
                          pres_col = "presence",
                          pres_label = "Present",
                          radius_km = 100,
                          dist_col = "dist_min_km",
                          base_sf = NULL,
                          show_union = TRUE,
                          max_buffers = 500,
                          title = "Filtered checklists by threshold",
                          fig_size = c(9, 9),
                          col_pres = "#298c8c",
                          col_abs_in = "#9fc8c8",
                          col_abs_out = "lightgray",
                          col_buffer = "black",
                          alpha_pts = 0.85,
                          interactive = FALSE,
                          save_path = NULL) {

  ## ---- Basic checks ----
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }

  needed <- c(lat_col, lon_col, pres_col)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0L) {
    stop("Missing required columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # Coerce lat/lon to numeric, drop rows with NA coords
  data_local <- data
  data_local[[lat_col]] <- suppressWarnings(as.numeric(as.character(data_local[[lat_col]])))
  data_local[[lon_col]] <- suppressWarnings(as.numeric(as.character(data_local[[lon_col]])))

  na_coords <- is.na(data_local[[lat_col]]) | is.na(data_local[[lon_col]])
  if (any(na_coords)) {
    warning(sum(na_coords), " rows dropped due to NA coordinates.")
    data_local <- data_local[!na_coords, , drop = FALSE]
  }

  if (nrow(data_local) == 0L) {
    warning("No rows with valid coordinates. Returning empty data.frame.")
    data_local[[dist_col]] <- numeric(0L)
    data_local[["within_radius"]] <- logical(0L)
    return(data_local)
  }

  ## ---- Presence/absence classification (robust to NA and case/whitespace) ----
  pres_label_norm <- tolower(trimws(as.character(pres_label)))
  pres_vec_norm <- tolower(trimws(as.character(data_local[[pres_col]])))
  es_pos <- pres_vec_norm == pres_label_norm
  es_pos[is.na(es_pos)] <- FALSE     # treat unknown as not presence
  es_neg <- !es_pos

  ## ---- Compute distance to nearest presence if needed ----
  if (!dist_col %in% names(data_local)) {
    if (!any(es_pos)) {
      warning("No presences found (no rows with `pres_col == pres_label`). Setting all distances to Inf.")
      data_local[[dist_col]] <- rep(Inf, nrow(data_local))
    } else {
      if (!requireNamespace("RANN", quietly = TRUE)) {
        stop("Package 'RANN' is required for nearest-neighbour distance computation. Please install it.")
      }

      # Build sf (WGS84) and transform to local UTM for metric distances
      epsg <- pick_metric_epsg_from_pts(
        lon = data_local[[lon_col]],
        lat = data_local[[lat_col]]
      )

      sf_all <- sf::st_as_sf(
        data_local,
        coords = c(lon_col, lat_col),
        crs = 4326,
        remove = FALSE
      )

      sf_all_utm <- sf::st_transform(sf_all, epsg)
      coords_utm <- sf::st_coordinates(sf_all_utm)

      pos_idx <- which(es_pos)
      neg_idx <- which(es_neg)

      # Initialize distance column
      data_local[[dist_col]] <- NA_real_

      # Presences have 0 distance
      if (length(pos_idx) > 0L) {
        data_local[[dist_col]][pos_idx] <- 0
      }

      # If there are absences to query, compute nearest-neighbour distances
      if (length(neg_idx) > 0L) {
        pos_coords <- coords_utm[pos_idx, , drop = FALSE]
        neg_coords <- coords_utm[neg_idx, , drop = FALSE]

        # Defensive: ensure matrices have correct dimensions
        if (nrow(pos_coords) < 1L) {
          stop("Internal error: no presence coordinates available for distance computation.")
        }

        nn <- RANN::nn2(
          data  = pos_coords,
          query = neg_coords,
          k     = 1
        )

        dist_m  <- as.numeric(nn$nn.dists[, 1])
        dist_km <- dist_m / 1000

        data_local[[dist_col]][neg_idx] <- dist_km
      }
    }
  } else {
    # Ensure numeric
    data_local[[dist_col]] <- suppressWarnings(as.numeric(data_local[[dist_col]]))
  }

  ## ---- Flags within/outside radius (avoid NA comparisons) ----
  es_pos <- pres_vec_norm == pres_label_norm
  es_pos[is.na(es_pos)] <- FALSE
  es_neg <- !es_pos

  neg_kept <- es_neg & !is.na(data_local[[dist_col]]) & (data_local[[dist_col]] < radius_km)
  neg_out  <- es_neg & !is.na(data_local[[dist_col]]) & (data_local[[dist_col]] >= radius_km)

  data_local[["within_radius"]] <- FALSE
  data_local[["within_radius"]][es_pos | neg_kept] <- TRUE

  ## ---- Prepare sf objects for plotting ----
  gdf <- sf::st_as_sf(
    data_local,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )

  gdf_pos      <- if (any(es_pos))   gdf[es_pos, , drop = FALSE]   else gdf[integer(0), , drop = FALSE]
  gdf_neg_kept <- if (any(neg_kept)) gdf[neg_kept, , drop = FALSE] else gdf[integer(0), , drop = FALSE]
  gdf_neg_out  <- if (any(neg_out))  gdf[neg_out, , drop = FALSE]  else gdf[integer(0), , drop = FALSE]

  ## ---- Union of buffers (optional) ----
  union_buffer_wgs84 <- NULL
  if (show_union && any(es_pos) && radius_km > 0) {
    epsg <- pick_metric_epsg_from_pts(
      lon = data_local[[lon_col]],
      lat = data_local[[lat_col]]
    )
    gdf_pos_utm <- sf::st_transform(gdf_pos, epsg)

    n_pos <- nrow(gdf_pos_utm)
    if (n_pos == 0L) {
      union_buffer_wgs84 <- NULL
    } else if (n_pos > max_buffers) {
      # chunking to limit memory peak
      chunk_size <- max(1L, ceiling(n_pos / max_buffers))
      chunk_idxs <- split(seq_len(n_pos), ceiling(seq_len(n_pos) / chunk_size))
      unions <- vector("list", length(chunk_idxs))
      for (i in seq_along(chunk_idxs)) {
        part <- gdf_pos_utm[chunk_idxs[[i]], , drop = FALSE]
        buf  <- sf::st_buffer(part, dist = radius_km * 1000)
        unions[[i]] <- sf::st_union(buf)
      }
      union_metric       <- sf::st_union(do.call(c, unions))
      union_buffer_wgs84 <- sf::st_transform(union_metric, 4326)
    } else {
      buf_all            <- sf::st_buffer(gdf_pos_utm, dist = radius_km * 1000)
      union_metric       <- sf::st_union(buf_all)
      union_buffer_wgs84 <- sf::st_transform(union_metric, 4326)
    }
  }

  ## ---- Filtered data.frame (return value) ----
  keep_rows <- es_pos | neg_kept
  data_ret <- data_local[keep_rows, , drop = FALSE]
  rownames(data_ret) <- NULL

  ## ---- Plotting: interactive vs static ----
  if (interactive) {
    # Leaflet interactive map
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      warning("Package 'leaflet' is required for interactive = TRUE. Falling back to static ggplot.")
    } else {
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      # Base layer
      if (!is.null(base_sf) && inherits(base_sf, "sf") && nrow(base_sf) > 0L) {
        base_4326 <- sf::st_transform(base_sf, 4326)
        m <- m |>
          leaflet::addPolygons(
            data   = base_4326,
            color  = "black",
            weight = 1,
            fill   = FALSE,
            opacity = 0.6
          )
      }

      # Points: absences outside
      if (nrow(gdf_neg_out) > 0L) {
        m <- m |>
          leaflet::addCircleMarkers(
            data        = gdf_neg_out,
            radius      = 3,
            color       = col_abs_out,
            stroke      = FALSE,
            fillOpacity = 0.35
          )
      }

      # Points: absences within radius
      if (nrow(gdf_neg_kept) > 0L) {
        m <- m |>
          leaflet::addCircleMarkers(
            data        = gdf_neg_kept,
            radius      = 4,
            color       = col_abs_in,
            stroke      = FALSE,
            fillOpacity = alpha_pts
          )
      }

      # Points: presences
      if (nrow(gdf_pos) > 0L) {
        m <- m |>
          leaflet::addCircleMarkers(
            data        = gdf_pos,
            radius      = 5,
            color       = col_pres,
            stroke      = FALSE,
            fillOpacity = alpha_pts
          )
      }

      # Buffer boundary (on top)
      if (!is.null(union_buffer_wgs84)) {
        m <- m |>
          leaflet::addPolygons(
            data   = union_buffer_wgs84,
            color  = col_buffer,
            weight = 2,
            fill   = FALSE
          )
      }

      print(m)

      if (!is.null(save_path)) {
        if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
          warning("Package 'htmlwidgets' is required to save interactive maps.")
        } else {
          htmlwidgets::saveWidget(m, save_path, selfcontained = TRUE)
        }
      }

      return(data_ret)
    }
  }

  ## ---- Static ggplot (fallback or interactive = FALSE) ----
  p <- ggplot2::ggplot()

  # 1. Base map
  if (!is.null(base_sf) && inherits(base_sf, "sf") && nrow(base_sf) > 0L) {
    base_4326 <- sf::st_transform(base_sf, 4326)
    p <- p + ggplot2::geom_sf(
      data  = base_4326,
      color = "black",
      fill  = NA,
      size  = 0.4,
      alpha = 0.6
    )
  }

  # 2. OUTSIDE absences (grey)
  if (nrow(gdf_neg_out) > 0L) {
    p <- p + ggplot2::geom_sf(
      data  = gdf_neg_out,
      color = col_abs_out,
      alpha = 0.35,
      size  = 1.5
    )
  }

  # 3. INSIDE absences
  if (nrow(gdf_neg_kept) > 0L) {
    p <- p + ggplot2::geom_sf(
      data  = gdf_neg_kept,
      color = col_abs_in,
      alpha = alpha_pts,
      size  = 2
    )
  }

  # 4. PRESENCES
  if (nrow(gdf_pos) > 0L) {
    p <- p + ggplot2::geom_sf(
      data  = gdf_pos,
      color = col_pres,
      alpha = alpha_pts,
      size  = 2.5
    )
  }

  # 5. BUFFER (on top)
  if (!is.null(union_buffer_wgs84)) {
    p <- p + ggplot2::geom_sf(
      data        = union_buffer_wgs84,
      color       = col_buffer,
      fill        = NA,
      linewidth   = 0.8,
      inherit.aes = FALSE
    )
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::ggtitle(title)

  print(p)

  if (!is.null(save_path)) {
    ggplot2::ggsave(
      filename = save_path,
      plot     = p,
      width    = fig_size[1],
      height   = fig_size[2],
      dpi      = 300
    )
  }

  data_ret
}





#' Pick a suitable metric EPSG (UTM) from point coordinates
#'
#' Given longitude and latitude vectors in WGS84 (EPSG:4326),
#' this helper function selects an appropriate UTM EPSG code
#' based on the median location of the points.
#'
#' The function is intended for internal use in spatial operations
#' that require metric distances (e.g. buffering, nearest-neighbour
#' distance computation).
#'
#' @param lon Numeric vector of longitudes (decimal degrees).
#' @param lat Numeric vector of latitudes (decimal degrees).
#'
#' @return Integer. EPSG code corresponding to the appropriate UTM zone
#'   (EPSG:326xx for Northern Hemisphere, EPSG:327xx for Southern Hemisphere).
#'
#' @keywords internal
pick_metric_epsg_from_pts <- function(lon, lat) {

  # ---- Basic checks ----
  if (missing(lon) || missing(lat)) {
    stop("Both `lon` and `lat` must be provided.")
  }

  lon <- suppressWarnings(as.numeric(lon))
  lat <- suppressWarnings(as.numeric(lat))

  ok <- is.finite(lon) & is.finite(lat)
  lon <- lon[ok]
  lat <- lat[ok]

  if (length(lon) == 0L || length(lat) == 0L) {
    stop("No valid coordinate pairs provided.")
  }

  # ---- Median location ----
  lon_med <- stats::median(lon)
  lat_med <- stats::median(lat)

  # ---- UTM zone calculation ----
  zone <- floor((lon_med + 180) / 6) + 1

  if (zone < 1 || zone > 60) {
    stop("Computed UTM zone is out of valid range (1-60).")
  }

  # EPSG: 326xx = Northern Hemisphere, 327xx = Southern Hemisphere
  if (lat_med >= 0) {
    epsg <- 32600 + zone
  } else {
    epsg <- 32700 + zone
  }

  epsg
}
