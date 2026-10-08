#' Filter checklists by ecoregion polygon (fast spatial filter + map)
#'
#' Filters checklists (lat/lon) using a region polygon, with a fast bbox
#' pre-filter, precise spatial test, optional UTM buffering, optional
#' simplification, and a static (ggplot2) or interactive (leaflet) map.
#'
#' The region can be provided either as a path to a vector file
#' (shapefile/GeoJSON/etc.) or as an sf object. Points are expected in
#' decimal degrees (EPSG:4326). Buffering and simplification are done in a
#' local metric (UTM) projection determined from the median of the points.
#'
#' @param data A data.frame with latitude/longitude columns.
#' @param eco_path Path to a vector file readable by sf::st_read. If
#'   provided, \code{region_sf} is ignored.
#' @param region_sf An \code{sf} object with one or more polygon features.
#' @param lat_col Name of the latitude column in \code{data}. Default: "LATITUDE".
#' @param lon_col Name of the longitude column in \code{data}. Default: "LONGITUDE".
#' @param pres_col Name of presence column in \code{data} (used when
#'   \code{keep_presences=TRUE}). Default: "presence".
#' @param pres_label Character scalar. Label for presence rows in \code{pres_col}.
#'   Default: "Present".
#' @param keep_presences Logical; if TRUE, always keep rows labelled as
#'   presence in \code{pres_col} even if they are outside the region.
#'   Default: TRUE.
#' @param inside Logical; if TRUE keep points inside the region, if FALSE
#'   keep points outside the region. Default: TRUE.
#' @param buffer_km Numeric; buffer to apply to the region in kilometres
#'   (applied in a local metric CRS). Default: 0 (no buffer).
#' @param simplify_tolerance_m Numeric; if provided (>0), simplify the
#'   region using this tolerance (meters, applied in local metric CRS).
#'   Default: NULL (no simplification).
#' @param interactive Logical; if TRUE attempt to show an interactive
#'   leaflet map (requires the leaflet package). Default: FALSE.
#' @param base_color Color for region outline. Default: "black".
#' @param pres_color Color for presence points. Default: "#298c8c".
#' @param abs_color Color for absence points. Default: "#9fc8c8".
#' @param context_color Color for excluded/context points. Default: "lightgray".
#' @param title Plot title for static map. Default: "Filtered checklists by ecoregion".
#' @param save_path Optional path to save the produced map. For interactive
#'   maps this will save an HTML widget (requires htmlwidgets). For static
#'   maps this will call ggplot2::ggsave.
#'
#' @return A data.frame with the subset of rows from \code{data} that passed
#'   the filter. Row names are reset.
#'
#' @export
filter_ecoregion <- function(data,
                             eco_path = NULL,
                             region_sf = NULL,
                             lat_col = "LATITUDE",
                             lon_col = "LONGITUDE",
                             pres_col = "presence",
                             pres_label = "Present",
                             keep_presences = FALSE,
                             inside = TRUE,
                             buffer_km = 0,
                             simplify_tolerance_m = NULL,
                             interactive = FALSE,
                             base_color = "black",
                             pres_color = "#298c8c",
                             abs_color = "#9fc8c8",
                             context_color = "lightgray",
                             title = "Filtered checklists by ecoregion",
                             save_path = NULL) {

  # ---------------------------
  # Validate and prepare input
  # ---------------------------
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }

  if (!(lat_col %in% names(data) && lon_col %in% names(data))) {
    stop("Latitude/longitude columns not found in `data`.")
  }

  data_local <- data

  # Coerce coordinates safely (factors -> character -> numeric)
  data_local[[lat_col]] <- suppressWarnings(as.numeric(as.character(data_local[[lat_col]])))
  data_local[[lon_col]] <- suppressWarnings(as.numeric(as.character(data_local[[lon_col]])))

  data_local <- data_local[stats::complete.cases(data_local[, c(lat_col, lon_col)]), , drop = FALSE]

  if (nrow(data_local) == 0L) {
    warning("No valid coordinates found in `data`; returning empty data.frame.")
    return(data_local)
  }

  # ---------------------------
  # Load region
  # ---------------------------
  if (is.null(region_sf)) {
    if (is.null(eco_path)) {
      stop("Provide either `eco_path` or `region_sf`.")
    }
    region <- sf::st_read(eco_path, quiet = TRUE)
  } else {
    region <- region_sf
  }

  if (nrow(region) == 0L) {
    stop("`region` contains no features.")
  }

  # Ensure region is in WGS84 (4326). Use a safe check for CRS.
  crs_region <- sf::st_crs(region)
  if (is.na(crs_region)) {
    region <- sf::st_set_crs(region, 4326)
  } else {
    region <- sf::st_transform(region, 4326)
  }

  # Keep a copy of the full input region for plotting contour
  region_all <- region

  # Dissolve into a single geometry for filtering
  region_union <- sf::st_union(region)

  # --------------------------------------
  # Buffer in km (using UTM local system)
  # --------------------------------------
  if (!is.null(buffer_km) && buffer_km != 0) {
    epsg <- pick_metric_epsg_from_pts(
      lon = data_local[[lon_col]],
      lat = data_local[[lat_col]]
    )

    region_union <- region_union |>
      sf::st_transform(epsg) |>
      sf::st_buffer(buffer_km * 1000) |>
      sf::st_transform(4326)
  }

  # --------------------------------
  # Optional simplification (meters)
  # --------------------------------
  if (!is.null(simplify_tolerance_m) && simplify_tolerance_m > 0) {
    epsg <- pick_metric_epsg_from_pts(
      lon = data_local[[lon_col]],
      lat = data_local[[lat_col]]
    )

    region_union <- region_union |>
      sf::st_transform(epsg) |>
      sf::st_simplify(
        dTolerance = simplify_tolerance_m,
        preserveTopology = TRUE
      ) |>
      sf::st_transform(4326)
  }

  # ---------------------------
  # Convert points to sf (WGS84)
  # ---------------------------
  pts <- sf::st_as_sf(
    data_local,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )

  # ---------------------------
  # Fast bounding box filter
  # ---------------------------
  bb <- sf::st_bbox(region_union)
  lon <- data_local[[lon_col]]
  lat <- data_local[[lat_col]]

  if (any(is.na(bb))) {
    warning("Region bounding box is NA; skipping bbox pre-filter.")
    mask_bbox <- rep(TRUE, length(lon))
  } else {
    mask_bbox <- lon >= bb["xmin"] &
      lon <= bb["xmax"] &
      lat >= bb["ymin"] &
      lat <= bb["ymax"]
  }

  # ---------------------------------
  # Precise test only for bbox hits
  # ---------------------------------
  mask_inside <- rep(FALSE, nrow(data_local))

  if (any(mask_bbox)) {
    pts_cand <- pts[mask_bbox, , drop = FALSE]

    # st_covers returns matrix: 1 row (region) x N points
    covers <- sf::st_covers(region_union, pts_cand, sparse = FALSE)

    if (is.matrix(covers) && nrow(covers) >= 1L) {
      mask_inside[mask_bbox] <- as.logical(covers[1, , drop = TRUE])
    } else {
      # Defensive fallback: try st_within
      covers2 <- sf::st_within(pts_cand, region_union, sparse = FALSE)

      if (is.matrix(covers2) && ncol(covers2) >= 1L) {
        mask_inside[mask_bbox] <- as.logical(rowSums(covers2) > 0)
      } else {
        warning("Spatial predicate returned unexpected result. Treating candidates as outside.")
      }
    }
  }

  # Inside/outside choice
  mask_region <- if (inside) mask_inside else !mask_inside

  # Keep presences (if requested) regardless of spatial mask
  if (keep_presences && pres_col %in% names(data_local)) {
    is_pos <- as.character(data_local[[pres_col]]) == pres_label
    mask_final <- mask_region | is_pos
  } else {
    mask_final <- mask_region
  }

  data_out <- data_local[mask_final, , drop = FALSE]
  rownames(data_out) <- NULL

  # -----------------------------------------------------
  # -----------------   PLOT SECTION   ------------------
  # -----------------------------------------------------

  # INTERACTIVE MAP (leaflet)
  if (interactive) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      warning("Package 'leaflet' is required for interactive = TRUE. Falling back to static map.")
    } else {
      pts_out <- sf::st_as_sf(
        data_out,
        coords = c(lon_col, lat_col),
        crs = 4326,
        remove = FALSE
      )

      data_excluded <- data_local[!mask_final, , drop = FALSE]
      pts_excluded <- sf::st_as_sf(
        data_excluded,
        coords = c(lon_col, lat_col),
        crs = 4326,
        remove = FALSE
      )

      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

      if (nrow(data_excluded) > 0L) {
        m <- m |>
          leaflet::addCircleMarkers(
            data = pts_excluded,
            radius = 3,
            color = context_color,
            stroke = FALSE,
            fillOpacity = 0.35
          )
      }

      if (nrow(data_out) > 0L) {
        if (pres_col %in% names(data_out)) {
          is_pos <- as.character(data_out[[pres_col]]) == pres_label

          if (any(!is_pos)) {
            m <- m |>
              leaflet::addCircleMarkers(
                data = pts_out[!is_pos, , drop = FALSE],
                radius = 4,
                color = abs_color,
                stroke = FALSE,
                fillOpacity = 0.75
              )
          }

          if (any(is_pos)) {
            m <- m |>
              leaflet::addCircleMarkers(
                data = pts_out[is_pos, , drop = FALSE],
                radius = 5,
                color = pres_color,
                stroke = FALSE,
                fillOpacity = 0.9
              )
          }
        } else {
          m <- m |>
            leaflet::addCircleMarkers(
              data = pts_out,
              radius = 4,
              color = pres_color,
              stroke = FALSE,
              fillOpacity = 0.8
            )
        }
      }

      # Region contour
      m <- m |>
        leaflet::addPolygons(
          data = sf::st_as_sf(region_union),
          color = base_color,
          weight = 2,
          opacity = 1,
          fill = FALSE
        )

      print(m)

      if (!is.null(save_path)) {
        if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
          warning("htmlwidgets needed to save leaflet maps.")
        } else {
          htmlwidgets::saveWidget(m, save_path, selfcontained = TRUE)
        }
      }

      return(data_out)
    }
  }

  # STATIC GGPLOT MAP (leaflet not available or interactive = FALSE)
  region_df <- sf::st_as_sf(region_all)

  data_excluded <- data_local[!mask_final, , drop = FALSE]
  pts_excluded <- sf::st_as_sf(
    data_excluded,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )

  pts_out <- sf::st_as_sf(
    data_out,
    coords = c(lon_col, lat_col),
    crs = 4326,
    remove = FALSE
  )

  p <- ggplot2::ggplot()

  # Region full outline (optional: if you want all polygons, uncomment)
  # p <- p + ggplot2::geom_sf(data = region_df, fill = NA, color = base_color, linewidth = 0.2)

  if (nrow(data_excluded) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = pts_excluded,
        color = context_color,
        alpha = 0.25,
        size = 1
      )
  }

  if (nrow(data_out) > 0L) {
    if (pres_col %in% names(data_out)) {
      is_pos <- as.character(data_out[[pres_col]]) == pres_label

      if (any(!is_pos)) {
        p <- p +
          ggplot2::geom_sf(
            data = pts_out[!is_pos, , drop = FALSE],
            color = abs_color,
            alpha = 0.8,
            size = 1.8
          )
      }

      if (any(is_pos)) {
        p <- p +
          ggplot2::geom_sf(
            data = pts_out[is_pos, , drop = FALSE],
            color = pres_color,
            alpha = 0.9,
            size = 2.1
          )
      }
    } else {
      p <- p +
        ggplot2::geom_sf(
          data = pts_out,
          color = pres_color,
          size = 2
        )
    }
  }

  p <- p +
    ggplot2::geom_sf(
      data = sf::st_as_sf(region_union),
      fill = NA,
      color = base_color,
      linewidth = 0.3
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggplot2::ggtitle(title)

  print(p)

  if (!is.null(save_path)) {
    tryCatch(
      ggplot2::ggsave(save_path, p, width = 8, height = 8, dpi = 300),
      error = function(e) warning("Failed to save static plot: ", conditionMessage(e))
    )
  }

  data_out
}
